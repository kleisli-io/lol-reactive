;;;; Regression test for defcomponent-with-api auto-registration.
;;;;
;;;; The generated constructor built the pandoriclet closure but never called
;;;; register-component, so every API route's (find-component id) lookup
;;;; returned NIL and the handler responded "Component not found". The whole
;;;; API surface for components defined via defcomponent-with-api was inert.
;;;;
;;;; The fixture is interned in :lol-web/fullstack so the macro-template
;;;; expansion (which references find-component, render, etc. as bare symbols)
;;;; resolves them in the same package the macro author wrote them in.

(in-package :lol-web/fullstack)

(defcomponent-with-api regression-api-bug19-component ()
  :state ((counter 0))
  :actions ((incr () (incf counter)))
  :render "<div>probe</div>")

;; Fixture for the auth + ownership tests below. Two instance shapes
;; come from the same component definition — one bound to a principal
;; via :principal-binding, one public.
(defcomponent-with-api auth-probe-foo ()
  :state ((counter 0))
  :actions ((bump () (incf counter)))
  :render "<div/>")

;; Exercises the generated <NAME>-ACTION attribute emitter.
(defisomorphic-component escape-probe-iso ()
  :state ((n 0))
  :actions ((bump () (incf n)))
  :render "<div/>")

(in-package :lol-web/fullstack/test)
(in-suite :lol-web/fullstack/test)

(test regression-defcomponent-with-api-registers-instance
  "An instance from defcomponent-with-api is findable via find-component"
  (let* ((c (lol-web/fullstack::regression-api-bug19-component))
         (id (funcall c :id)))
    (unwind-protect
         (progn
           (is (eq c (find-component id))
               "find-component returns the same closure as the constructor")
           (is (equal '(:counter 0) (funcall c :state))
               "state surface unchanged by registration"))
      (unregister-component id))))

(test regression-hydration-runtime-uses-property-access-on-el
  "Generated hydration runtime accesses el.getAttribute(), not the
   concatenated identifier elgetAttribute. (ps:@ elget-attribute)
   compiles to a single bare symbol that throws ReferenceError at
   first hydration; the correct (ps:@ el get-attribute) emits the
   property access."
  (let ((js (lol-web/fullstack::hydration-runtime-js)))
    (is (null (search "elgetAttribute" js))
        "elgetAttribute concat-typo present — must be el.getAttribute")
    (is (search "el.getAttribute" js)
        "expected el.getAttribute() in hydrate-all body")))

;;; ============================================================================
;;; component-api :set-state / :action body-key coercion — bounded pool
;;; ============================================================================

(test regression-component-api-body-key-bounds-keyword-pool
  "1000 distinct hostile :key / :action payloads must not grow the
   keyword pool."
  (let ((baseline (length (apropos-list "" :keyword))))
    (loop for i below 1000 do
          (lol-web/fullstack::safe-coerce-keyword
           (format nil "attacker-set-state-key-~D-~D" i (random 999999))))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D"
          baseline after)))
  (is (eq 'lol-web/escape:safe-coerce-keyword
          (find-symbol "SAFE-COERCE-KEYWORD" :lol-web/fullstack))
      "safe-coerce-keyword must be imported into :lol-web/fullstack"))

;;; ============================================================================
;;; CSPRNG component-ids and principal-binding ownership gate
;;; ============================================================================

(test regression-generate-component-id-not-monotonic
  "Consecutive IDs differ in the random suffix, not by an incremented
   counter. 1000 IDs share the prefix but every random suffix is unique."
  (let* ((ids (loop repeat 1000 collect (generate-component-id 'probe)))
         (suffixes (mapcar (lambda (id) (subseq id (1+ (position #\- id)))) ids)))
    (is (= 1000 (length (remove-duplicates suffixes :test #'string=)))
        "expected 1000 distinct suffixes, got ~D"
        (length (remove-duplicates suffixes :test #'string=)))
    (is (every (lambda (s) (= 32 (length s))) suffixes)
        "every suffix must be 32 hex chars (128 bits)")))

(test regression-component-principal-binding-stored-on-register
  "register-component :principal-binding stores the opaque value;
   component-principal-binding returns it; absent binding ⇒ NIL."
  (let ((bound-id (generate-component-id 'probe))
        (free-id  (generate-component-id 'probe))
        (closure  (lambda () :probe)))
    (unwind-protect
         (progn
           (register-component bound-id closure :principal-binding :alice)
           (register-component free-id closure)
           (is (eq :alice (component-principal-binding bound-id))
               "binding round-trips eq through the registry")
           (is (null (component-principal-binding free-id))
               "no :principal-binding ⇒ NIL"))
      (unregister-component bound-id)
      (unregister-component free-id))))

(test regression-component-api-owner-can-dispatch-non-owner-cannot
  "When the component has a principal-binding, %principal-owns-component-p
   is T only for the binding's principal. Public (no-binding) instances
   always pass."
  (let* ((alice-c (lol-web/fullstack::auth-probe-foo :principal-binding :alice))
         (public-c (lol-web/fullstack::auth-probe-foo))
         (alice-id  (funcall alice-c :id))
         (public-id (funcall public-c :id)))
    (unwind-protect
         (let ((mk-hooks (lambda (p)
                           (list :lol-web.auth.hooks
                                 (cons (lambda () t) (lambda () p))))))
           (let ((lol-web/server:*env* (funcall mk-hooks :alice)))
             (is (lol-web/fullstack::%principal-owns-component-p alice-id)
                 "owner :alice must own their component"))
           (let ((lol-web/server:*env* (funcall mk-hooks :mallory)))
             (is (null (lol-web/fullstack::%principal-owns-component-p alice-id))
                 ":mallory must not own :alice's component"))
           (let ((lol-web/server:*env* (funcall mk-hooks :anyone)))
             (is (lol-web/fullstack::%principal-owns-component-p public-id)
                 "public component (no binding) is always owned")))
      (unregister-component alice-id)
      (unregister-component public-id))))

;;; ============================================================================
;;; HMAC-signed hydration state — round-trip + tamper rejection
;;; ============================================================================

(defun %hydration-secret (label)
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for c across label
          for i from 0 below 32
          do (setf (aref key i) (char-code c)))
    key))

(test regression-sign-verify-roundtrip
  "Round-tripping the same payload + key recovers the payload with :OK."
  (let* ((key (%hydration-secret "primary"))
         (payload `((:key . "counter") (:value . 42)))
         (signed (sign-hydration-state payload key)))
    (is (stringp (cdr (assoc :payload signed)))
        ":payload field must be a JSON string")
    (is (stringp (cdr (assoc :tag signed)))
        ":tag field must be a hex string")
    (multiple-value-bind (decoded status) (verify-hydration-state signed key)
      (is (eq :ok status) "verify must succeed under matching key, got ~A" status)
      (is (equal "counter" (cdr (assoc :key decoded)))
          ":key round-trips through encode/verify/decode")
      (is (= 42 (cdr (assoc :value decoded)))
          ":value round-trips through encode/verify/decode"))))

(test regression-sign-without-key-signals
  "Sign with a NIL secret-key must signal — fail-closed at mint time."
  (signals error (sign-hydration-state '((:key . "x")) nil)))

(test regression-verify-without-key-yields-no-key
  "Verify against a NIL secret-key must return (NIL :NO-KEY) without
   inspecting the envelope."
  (multiple-value-bind (payload status)
      (verify-hydration-state '((:payload . "x") (:tag . "abcd")) nil)
    (is (null payload))
    (is (eq :no-key status))))

(test regression-verify-tampered-tag-rejects
  "Flipping one character of the tag must yield :BAD-TAG."
  (let* ((key (%hydration-secret "primary"))
         (signed (sign-hydration-state '((:key . "counter") (:value . 1)) key))
         (orig-tag (cdr (assoc :tag signed)))
         (tampered-tag (concatenate 'string
                                    (string (if (char= (char orig-tag 0) #\0) #\1 #\0))
                                    (subseq orig-tag 1)))
         (tampered (list (assoc :payload signed)
                         (cons :tag tampered-tag))))
    (multiple-value-bind (payload status) (verify-hydration-state tampered key)
      (is (null payload))
      (is (eq :bad-tag status) "tampered tag must yield :BAD-TAG, got ~A" status))))

(test regression-verify-tampered-payload-rejects
  "Mutating the payload string without updating the tag must yield
   :BAD-TAG — the whole point of the envelope."
  (let* ((key (%hydration-secret "primary"))
         (signed (sign-hydration-state '((:key . "counter") (:value . 1)) key))
         (tampered (list (cons :payload "{\"key\":\"counter\",\"value\":999}")
                         (assoc :tag signed))))
    (multiple-value-bind (payload status) (verify-hydration-state tampered key)
      (is (null payload))
      (is (eq :bad-tag status) "tampered payload must yield :BAD-TAG, got ~A" status))))

(test regression-verify-wrong-key-rejects
  "An envelope signed under one key must not verify under another."
  (let* ((alice-key (%hydration-secret "alice"))
         (mallory-key (%hydration-secret "mallory"))
         (signed (sign-hydration-state '((:key . "counter") (:value . 1)) alice-key)))
    (multiple-value-bind (payload status)
        (verify-hydration-state signed mallory-key)
      (is (null payload))
      (is (eq :bad-tag status) "cross-key verify must yield :BAD-TAG, got ~A" status))))

(test regression-verify-missing-fields-rejects
  "Envelopes missing :PAYLOAD or :TAG must yield :MISSING-TAG."
  (let ((key (%hydration-secret "primary")))
    (is (eq :missing-tag
            (nth-value 1 (verify-hydration-state nil key)))
        "NIL envelope returns :MISSING-TAG")
    (is (eq :missing-tag
            (nth-value 1 (verify-hydration-state '((:payload . "p")) key)))
        "envelope without :TAG returns :MISSING-TAG")
    (is (eq :missing-tag
            (nth-value 1 (verify-hydration-state '((:tag . "t")) key)))
        "envelope without :PAYLOAD returns :MISSING-TAG")))

;;; ============================================================================
;;; component-api /api/dispatch — args-shape validation
;;; ============================================================================

(in-package :lol-web/fullstack)

(defcomponent-with-api dispatch-validation-probe ()
  :state ((counter 0))
  :actions ((bump-by (n) (incf counter n)))
  :render "<div/>")

(in-package :lol-web/fullstack/test)

(defun %dispatch-handler ()
  "Resolve the live POST /api/dispatch handler so tests do not depend
   on whatever routing scaffolding the consumer wires up."
  (gethash (cons :post "/api/dispatch") lol-web/server:*routes*))

(defun %public-dispatch-component ()
  "Construct a fresh public dispatch-validation-probe instance. Public
   instances skip the auth gate so the args-shape branch is reachable
   without standing up a session/principal in the test env."
  (lol-web/fullstack::dispatch-validation-probe))

(defun %dispatch-with-body (handler body-json &key (principal :test))
  "Invoke HANDLER (a defroute-generated, zero-arg handler) with BODY-JSON
   staged in *env*'s request-body cache and a no-op auth-hooks cons
   installed under PRINCIPAL. Returns the response body as one string,
   pulled out of the Clack triple (STATUS HEADERS BODY-LIST). Body chunks
   are concatenated since Clack permits a list of strings.

   The auth hooks must be present even for public components: the
   generic /api/dispatch handler runs %with-component-auth
   unconditionally; only the per-action handlers honour :public T."
  (let ((lol-web/server:*env*
          (list :lol/cached-body-json body-json
                :lol-web.auth.hooks
                (cons (lambda () t) (lambda () principal)))))
    (let* ((raw (funcall handler))
           (body (third raw)))
      (apply #'concatenate 'string body))))

(test regression-defcomponent-action-rejects-non-list-args
  "An :args field that is not a list must be refused with the neutral
   invalid-arguments shape; APPLY must never run with non-list args."
  (let* ((probe (%public-dispatch-component))
         (id    (funcall probe :id)))
    (unwind-protect
         (let* ((handler (%dispatch-handler))
                (body-json `((:component-id . ,id)
                             (:action . "bump-by")
                             (:args . "not-a-list")))
                (body-str (%dispatch-with-body handler body-json))
                (parsed (lol-web/server:decode-json-string body-str)))
           (is (eq nil (cdr (assoc :success parsed)))
               "success must be false")
           (is (string= "Invalid arguments" (cdr (assoc :error parsed)))
               "error must be the neutral invalid-arguments shape, got ~S"
               (cdr (assoc :error parsed)))
           (is (= 0 (funcall probe :state :counter))
               "counter must not have advanced — APPLY never ran"))
      (lol-web/core:unregister-component id))))

(test regression-defcomponent-action-rejects-arity-mismatch
  "Args length not matching the registered action arity must be refused.
   The arity registry is populated at defcomponent-with-api expansion."
  (let* ((probe (%public-dispatch-component))
         (id    (funcall probe :id)))
    (unwind-protect
         (let* ((handler (%dispatch-handler))
                (body-json `((:component-id . ,id)
                             (:action . "bump-by")
                             (:args . (1 2 3))))
                (body-str (%dispatch-with-body handler body-json))
                (parsed (lol-web/server:decode-json-string body-str)))
           (is (eq nil (cdr (assoc :success parsed))))
           (is (string= "Invalid arguments" (cdr (assoc :error parsed))))
           (is (= 0 (funcall probe :state :counter))
               "counter must not have advanced — arity gate fired"))
      (lol-web/core:unregister-component id))))

(test regression-defcomponent-action-rejects-type-mismatch
  "When the action handler raises (e.g., +-on-a-string), the handler-case
   converts the crash into the neutral invalid-arguments shape so a
   bad JSON body cannot kill the worker thread."
  (let* ((probe (%public-dispatch-component))
         (id    (funcall probe :id)))
    (unwind-protect
         (let* ((handler (%dispatch-handler))
                (body-json `((:component-id . ,id)
                             (:action . "bump-by")
                             (:args . ("not-a-number"))))
                (body-str (%dispatch-with-body handler body-json))
                (parsed (lol-web/server:decode-json-string body-str)))
           (is (eq nil (cdr (assoc :success parsed))))
           (is (string= "Invalid arguments" (cdr (assoc :error parsed))))
           (is (= 0 (funcall probe :state :counter))
               "counter must not have advanced — handler crash was caught"))
      (lol-web/core:unregister-component id))))

(test regression-component-api-not-found-vs-forbidden-indistinguishable
  "Wire response for 'no such component' and 'principal not authorised'
   must be byte-identical. Distinguishable strings let a probe enumerate
   component IDs by status."
  (let* ((alice (lol-web/fullstack::auth-probe-foo :principal-binding :alice))
         (alice-id (funcall alice :id)))
    (unwind-protect
         (let* ((handler (%dispatch-handler))
                ;; Not-found probe: an id that was never registered.
                (not-found-body `((:component-id . "nonexistent-id-xyz")
                                  (:action . "bump")
                                  (:args . ())))
                ;; Cross-owner probe: alice's id queried as mallory.
                (forbidden-body `((:component-id . ,alice-id)
                                  (:action . "bump")
                                  (:args . ())))
                (not-found-resp (%dispatch-with-body handler not-found-body
                                                     :principal :mallory))
                (forbidden-resp (%dispatch-with-body handler forbidden-body
                                                     :principal :mallory)))
           (is (string= not-found-resp forbidden-resp)
               "responses must be byte-identical; ~S vs ~S"
               not-found-resp forbidden-resp))
      (lol-web/core:unregister-component alice-id))))

(test regression-isomorphic-id-name-escaped
  "with-hydration-wrapper routes component-id and component-name through
   safe-attr, so a breakout id cannot close the data-component-id
   attribute and inject markup; the body HTML survives verbatim."
  (let ((out (lol-web/fullstack::with-hydration-wrapper
              "x\"><script>alert(1)</script>" "widget" nil "<p>body</p>")))
    (is (search "&quot;" out))
    (is (search "&lt;script" out))
    (is (null (search "\"><script" out)))
    (is (search "<p>body</p>" out))))

(test regression-hydration-script-neutralizes-close-script
  "generate-hydration-script splices component-id into a <script> body;
   neutralize-script-close defuses a literal </script> so the id cannot
   end the script element early and execute grafted markup. A clean id is
   spliced verbatim so hydration still resolves the container."
  (let ((js (lol-web/fullstack::generate-hydration-script
             "</script><script>alert(1)</script>" 'widget nil nil)))
    (is (null (search "</script" js))
        "no live </script close tag may survive in the script body"))
  (let ((js (lol-web/fullstack::generate-hydration-script
             "comp-123" 'widget nil nil)))
    (is (search "comp-123" js)
        "a clean id is spliced verbatim for the hydration selector")))

(test regression-client-action-attr-escapes-action-name
  "client-action-attr routes the action name through safe-attr, so a `\"`
   in the name cannot close data-action and graft a sibling attribute.
   The two-arg (data-args) shape escapes the name identically."
  (let ((out (lol-web/fullstack::client-action-attr "x\" onclick=\"alert(1)")))
    (is (search "&quot;" out))
    (is (null (search "\" onclick=\"alert" out))))
  (let ((out (lol-web/fullstack::client-action-attr "x\" onclick=\"alert(1)" 7)))
    (is (search "&quot;" out))
    (is (null (search "\" onclick=\"alert" out)))
    (is (search "data-args=" out) "the data-args pair still emits")))

(test regression-name-action-escapes-action-name
  "The generated <NAME>-ACTION emitter routes its action name through
   safe-attr identically to client-action-attr."
  (let ((out (lol-web/fullstack::escape-probe-iso-action "x\" onclick=\"alert(1)")))
    (is (search "&quot;" out))
    (is (null (search "\" onclick=\"alert" out)))))

