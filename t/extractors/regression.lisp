;;;; Regression tests for :lol-web/extractors.
;;;;
;;;; Covers: extractor-spec round-trip; resolve-extractor methods for the
;;;; built-in kinds (:path :query :body :header :json-body); required-p /
;;;; default policy; coercion (integer, boolean, keyword); coercion failure
;;;; signals 400, missing required signals 422, unknown kind signals 500;
;;;; defhandler registration into *routes* + *handler-metadata*; end-to-end
;;;; dispatch via route-handler; with-error-handling translates extractor
;;;; conditions to the right status.

(in-package :lol-web/extractors/test)
(in-suite :lol-web/extractors/test)

;;; ============================================================================
;;; extractor-spec — struct round-trip
;;; ============================================================================

(test regression-extractor-spec-round-trip
  "make-extractor-spec accepts every documented slot and reads them back."
  (let ((spec (make-extractor-spec :name 'limit :kind :query
                                   :type 'integer :required-p nil
                                   :default (lambda () 20)
                                   :source-string "page-size"
                                   :custom-resolver 'my-resolver)))
    (is (eq 'limit (extractor-spec-name spec)))
    (is (eq :query (extractor-spec-kind spec)))
    (is (eq 'integer (extractor-spec-type spec)))
    (is (null (extractor-spec-required-p spec)))
    (is (functionp (extractor-spec-default spec)))
    (is (= 20 (funcall (extractor-spec-default spec))))
    (is (string= "page-size" (extractor-spec-source-string spec)))
    (is (eq 'my-resolver (extractor-spec-custom-resolver spec)))))

;;; ============================================================================
;;; resolve-extractor — :path
;;; ============================================================================

(test regression-resolve-path-extractor-present
  "resolve-extractor :path reads the named segment from *path-params*
   and coerces per :type."
  (let ((spec (make-extractor-spec :name 'id :kind :path :type 'integer))
        (path-params '(("id" . "42"))))
    (is (= 42 (resolve-extractor :path spec nil path-params)))))

(test regression-resolve-path-extractor-missing-required
  "Missing required :path extractor signals MISSING-EXTRACTOR-INPUT (422)."
  (let ((spec (make-extractor-spec :name 'id :kind :path :type 'integer)))
    (signals missing-extractor-input
      (resolve-extractor :path spec nil nil))))

(test regression-resolve-path-extractor-source-override
  ":source overrides the default lookup key."
  (let ((spec (make-extractor-spec :name 'user-id :kind :path :type 'integer
                                   :source-string "id"))
        (path-params '(("id" . "7"))))
    (is (= 7 (resolve-extractor :path spec nil path-params)))))

;;; ============================================================================
;;; resolve-extractor — :query
;;; ============================================================================

(test regression-resolve-query-extractor-present
  "resolve-extractor :query reads :query-parameters from env and coerces."
  (let ((spec (make-extractor-spec :name 'q :kind :query :type 'string))
        (env '(:query-parameters (("q" . "hello")))))
    (is (string= "hello" (resolve-extractor :query spec env nil)))))

(test regression-resolve-query-extractor-default-applied
  "Missing optional :query extractor with :default falls through to the thunk."
  (let ((spec (make-extractor-spec :name 'limit :kind :query :type 'integer
                                   :required-p nil
                                   :default (lambda () 20)))
        (env '(:query-parameters ())))
    (is (= 20 (resolve-extractor :query spec env nil)))))

(test regression-resolve-query-extractor-default-not-coerced
  "The default value is returned uncoerced — even if it would not parse."
  (let ((spec (make-extractor-spec :name 'limit :kind :query :type 'integer
                                   :required-p nil
                                   :default (lambda () nil)))
        (env '(:query-parameters ())))
    ;; default is NIL → resolves to NIL even though :type is integer
    (is (null (resolve-extractor :query spec env nil)))))

(test regression-resolve-query-extractor-optional-no-default-returns-nil
  "Optional extractor with no default and missing input returns NIL."
  (let ((spec (make-extractor-spec :name 'q :kind :query :type 'string
                                   :required-p nil))
        (env '(:query-parameters ())))
    (is (null (resolve-extractor :query spec env nil)))))

;;; ============================================================================
;;; resolve-extractor — :body
;;; ============================================================================

(test regression-resolve-body-extractor-present
  "resolve-extractor :body reads :body-parameters from env."
  (let ((spec (make-extractor-spec :name 'name :kind :body :type 'string))
        (env '(:body-parameters (("name" . "alice")))))
    (is (string= "alice" (resolve-extractor :body spec env nil)))))

;;; ============================================================================
;;; resolve-extractor — :header
;;; ============================================================================

(test regression-resolve-header-extractor-with-source
  "Headers are stored under :headers as a hash-table keyed by lowercase
   name; :source must match (case-insensitive — resolver downcases)."
  (let* ((headers (make-hash-table :test 'equal))
         (spec (make-extractor-spec :name 'api-key :kind :header
                                    :type 'string
                                    :source-string "X-API-Key"))
         (env (progn
                (setf (gethash "x-api-key" headers) "secret-token-abc")
                (list :headers headers))))
    (is (string= "secret-token-abc"
                 (resolve-extractor :header spec env nil)))))

(test regression-resolve-header-extractor-missing-required
  "Missing required :header signals MISSING-EXTRACTOR-INPUT (422)."
  (let* ((headers (make-hash-table :test 'equal))
         (spec (make-extractor-spec :name 'api-key :kind :header
                                    :type 'string
                                    :source-string "X-API-Key"))
         (env (list :headers headers)))
    (signals missing-extractor-input
      (resolve-extractor :header spec env nil))))

;;; ============================================================================
;;; resolve-extractor — :json-body
;;; ============================================================================

(test regression-resolve-json-body-extractor-present
  ":json-body returns the parsed alist via parse-request-json. We seed the
   :lol/cached-body-json key directly so the extractor exercises the cached
   path without re-encoding bytes — equivalence to the byte-decode path is
   already covered in :lol-web/server's regression suite."
  (let* ((parsed '((:name . "alice") (:count . 3)))
         (spec (make-extractor-spec :name 'body :kind :json-body :type t))
         (env (list :lol/cached-body-json parsed))
         (resolved (resolve-extractor :json-body spec env nil)))
    (is (eq parsed resolved)
        ":json-body should return the cached parsed value as-is")))

;;; ============================================================================
;;; Coercion — success and failure paths
;;; ============================================================================

(test regression-coerce-integer-success-and-failure
  "INTEGER coercion parses digit strings; non-numeric input signals 400."
  (let ((spec (make-extractor-spec :name 'n :kind :query :type 'integer)))
    (is (= 42 (resolve-extractor :query spec
                                 '(:query-parameters (("n" . "42")))
                                 nil)))
    (signals extractor-coercion-error
      (resolve-extractor :query spec
                         '(:query-parameters (("n" . "not-a-number")))
                         nil))))

(test regression-coercion-error-status-is-400
  "extractor-coercion-error subclasses http-error with status 400."
  (let ((c (make-condition 'extractor-coercion-error
                           :raw-value "x" :target-type 'integer)))
    (is (= 400 (http-error-status c)))
    (is (typep c 'lol-web/server:http-error))
    (is (typep c 'extractor-error))))

(test regression-missing-input-status-is-422
  "missing-extractor-input subclasses http-error with status 422."
  (let ((c (make-condition 'missing-extractor-input)))
    (is (= 422 (http-error-status c)))
    (is (typep c 'lol-web/server:http-error))
    (is (typep c 'extractor-error))))

(test regression-coerce-boolean-accepts-common-truthy-strings
  "BOOLEAN coercion accepts true/false, 1/0, yes/no, on/off case-insensitively."
  (let ((spec (make-extractor-spec :name 'flag :kind :query :type 'boolean)))
    (flet ((bool (raw)
             (resolve-extractor :query spec
                                `(:query-parameters (("flag" . ,raw)))
                                nil)))
      (is (eq t   (bool "true")))
      (is (eq t   (bool "TRUE")))
      (is (eq t   (bool "1")))
      (is (eq t   (bool "yes")))
      (is (eq t   (bool "on")))
      (is (eq nil (bool "false")))
      (is (eq nil (bool "0")))
      (is (eq nil (bool "no")))
      (signals extractor-coercion-error
        (bool "maybe")))))

(test regression-coerce-keyword-interns-upcased
  "KEYWORD coercion upcases and interns into the keyword package."
  (let ((spec (make-extractor-spec :name 'mode :kind :query :type 'keyword)))
    (is (eq :live (resolve-extractor :query spec
                                     '(:query-parameters (("mode" . "live")))
                                     nil)))
    (is (eq :draft (resolve-extractor :query spec
                                      '(:query-parameters (("mode" . "DRAFT")))
                                      nil)))))

(test regression-coerce-unsupported-type-signals-coercion-error
  "Type=T passes through; an unknown TYPE keyword signals coercion-error."
  (let ((spec (make-extractor-spec :name 'x :kind :query :type 'list-of-things)))
    (signals extractor-coercion-error
      (resolve-extractor :query spec
                         '(:query-parameters (("x" . "anything")))
                         nil))))

;;; ============================================================================
;;; Default RESOLVE-EXTRACTOR method — extractor-not-registered
;;; ============================================================================

(test regression-unknown-kind-signals-not-registered
  "RESOLVE-EXTRACTOR's default method signals EXTRACTOR-NOT-REGISTERED (500)
   for any KIND with no eql-method."
  (let ((spec (make-extractor-spec :name 'x :kind :totally-made-up :type t)))
    (signals extractor-not-registered
      (resolve-extractor :totally-made-up spec nil nil)))
  (let ((c (make-condition 'extractor-not-registered)))
    (is (= 500 (http-error-status c)))))

;;; ============================================================================
;;; Custom resolver — :resolver bypasses kind dispatch
;;; ============================================================================

(defun %test-custom-resolver (spec env path-params)
  (declare (ignore env path-params))
  (format nil "custom:~A" (extractor-spec-name spec)))

(test regression-custom-resolver-bypasses-kind-dispatch
  "When CUSTOM-RESOLVER is set, the named function is called instead of
   dispatching on KIND. The :around method on (kind t) intercepts before
   any kind-specific method."
  (let ((spec (make-extractor-spec :name 'thing :kind :path :type t
                                   :custom-resolver
                                   '%test-custom-resolver)))
    (is (string= "custom:THING"
                 (resolve-extractor :path spec nil nil)))))

;;; ============================================================================
;;; defhandler — registers in *routes* and *handler-metadata*
;;; ============================================================================

(defhandler regression-extractors-handler "/regression/extractors/get-user/:id"
    (:method :get :content-type "application/json")
    ((id :path :type integer))
  (encode-json-string (list (cons :id id))))

(test regression-defhandler-registers-route-and-metadata
  "defhandler stores the per-request handler in *routes* and the metadata
   plist in *handler-metadata*, both keyed by (cons METHOD PATH)."
  (let ((key (cons :get "/regression/extractors/get-user/:id")))
    (is (functionp (gethash key *routes*))
        "*routes* must contain a zero-arg handler under the route key")
    (let ((meta (handler-metadata :get "/regression/extractors/get-user/:id")))
      (is (consp meta) "metadata plist must be present")
      (is (eq 'regression-extractors-handler (getf meta :name)))
      (is (eq :get (getf meta :method)))
      (is (string= "/regression/extractors/get-user/:id" (getf meta :path)))
      (let ((specs (getf meta :extractors)))
        (is (= 1 (length specs)))
        (is (eq 'id (extractor-spec-name (first specs))))
        (is (eq :path (extractor-spec-kind (first specs))))
        (is (eq 'integer (extractor-spec-type (first specs))))
        (is (extractor-spec-required-p (first specs)))))))

(test regression-defhandler-dispatchable-end-to-end
  "Calling the registered handler with a synthetic env and path-params binds
   the resolved values into the body and returns the user-built response."
  (let* ((handler (gethash (cons :get "/regression/extractors/get-user/:id")
                           *routes*))
         (lol-web/server:*env* '(:query-parameters () :body-parameters ()))
         (lol-web/server:*path-params* '(("id" . "99")))
         (response (funcall handler)))
    (is (consp response) "handler must return a Clack response list")
    (is (= 200 (first response)) "successful coerce → 200")
    (let ((body (third response)))
      (is (search "\"id\":99"
                  (with-output-to-string (s)
                    (dolist (chunk body) (princ chunk s))))
          "response body should JSON-encode (:id . 99) — got ~S" body))))

;;; ============================================================================
;;; with-error-handling — extractor conditions translate to right status
;;; ============================================================================

(defhandler regression-extractors-coercion "/regression/extractors/parse/:n"
    (:method :get)
    ((n :path :type integer))
  (format nil "n=~A" n))

(test regression-defhandler-coercion-failure-becomes-400
  "An extractor-coercion-error inside the body is caught by the dispatcher's
   with-error-handling wrapper and translated to a 400 response. We invoke
   the wrapper directly here since the test fixture bypasses route-handler."
  (let* ((handler (gethash (cons :get "/regression/extractors/parse/:n")
                           *routes*))
         (lol-web/server:*env* '(:query-parameters () :body-parameters ()))
         (lol-web/server:*path-params* '(("n" . "not-an-int")))
         (*error-output* (make-broadcast-stream))
         (*error-log-path* nil)
         (response (with-error-handling "regression"
                     (funcall handler))))
    (is (= 400 (first response))
        "non-numeric :path parameter must produce 400, got ~A" (first response))))

(defhandler regression-extractors-required "/regression/extractors/required-q"
    (:method :get)
    ((q :query :type string))
  (format nil "q=~A" q))

(test regression-defhandler-missing-required-becomes-422
  "Missing required extractor input → 422 Unprocessable Entity, via the
   dispatcher's with-error-handling translation."
  (let* ((handler (gethash (cons :get "/regression/extractors/required-q")
                           *routes*))
         (lol-web/server:*env* '(:query-parameters () :body-parameters ()))
         (lol-web/server:*path-params* nil)
         (*error-output* (make-broadcast-stream))
         (*error-log-path* nil)
         (response (with-error-handling "regression"
                     (funcall handler))))
    (is (= 422 (first response))
        "missing required :query parameter must produce 422, got ~A" (first response))))

;;; ============================================================================
;;; defhandler — once-only PATH and :METHOD evaluation
;;; ============================================================================

(defvar *regression-once-only-path-eval-count* 0)
(defvar *regression-once-only-method-eval-count* 0)

(defun %regression-once-only-path ()
  (incf *regression-once-only-path-eval-count*)
  "/regression/extractors/once-only")

(defun %regression-once-only-method ()
  (incf *regression-once-only-method-eval-count*)
  :get)

(test regression-defhandler-evaluates-path-and-method-exactly-once
  "PATH and :METHOD forms are referenced at multiple sites in the expansion
   (metadata key, metadata plist, defroute call, return values). Once-only
   protection via defmacro!'s o!path and the let-bound g!method must collapse
   evaluation to exactly one each, even with side-effecting forms."
  (setf *regression-once-only-path-eval-count* 0
        *regression-once-only-method-eval-count* 0)
  (defhandler regression-once-only-handler
      (%regression-once-only-path)
      (:method (%regression-once-only-method))
      ()
    "ok")
  (is (= 1 *regression-once-only-path-eval-count*)
      "PATH form must be evaluated exactly once, was ~A times"
      *regression-once-only-path-eval-count*)
  (is (= 1 *regression-once-only-method-eval-count*)
      ":METHOD form must be evaluated exactly once, was ~A times"
      *regression-once-only-method-eval-count*)
  ;; Sanity: registration actually landed.
  (is (functionp (gethash (cons :get "/regression/extractors/once-only")
                          *routes*))
      "handler must be registered under (:get . path) after expansion"))

;;; ============================================================================
;;; Coercion handles non-string raw inputs (e.g. :json-body fields)
;;; ============================================================================

(test regression-coerce-handles-already-typed-raw-values
  "When a :json-body or custom-resolver extractor yields an already-typed
   value (integer for :type integer, keyword for :type keyword, etc.),
   the per-type coercer must accept it rather than choke on a non-string."
  (flet ((apply-policy (type raw)
           (lol-web/extractors::%apply-spec-policy
            (make-extractor-spec :name 'x :kind :json-body :type type)
            raw)))
    (is (= 42 (apply-policy 'integer 42)))
    (is (eq t (apply-policy 'boolean t)))
    (is (eq :foo (apply-policy 'keyword :foo)))
    (is (eq 'bar (apply-policy 'symbol 'bar)))))

;;; ============================================================================
;;; Sentinel — startup-time extractor-registry validation
;;; ============================================================================

(test regression-sentinel-fires-on-unregistered-kind
  "%validate-handler-metadata-or-error walks *HANDLER-METADATA* and signals
   EXTRACTOR-NOT-REGISTERED when any registered handler references a KIND with
   no RESOLVE-EXTRACTOR method. The condition's body names the offending route
   and extractor."
  (let ((bogus-meta (make-hash-table :test 'equal)))
    (setf (gethash (cons :get "/regression/sentinel/bogus") bogus-meta)
          (list :name 'regression-sentinel-handler
                :method :get
                :path "/regression/sentinel/bogus"
                :extractors (list (make-extractor-spec
                                   :name 'x
                                   :kind :totally-fictional-kind
                                   :type t))
                :options nil))
    (handler-case
        (progn
          (lol-web/extractors::%validate-handler-metadata-or-error bogus-meta)
          (is nil "expected EXTRACTOR-NOT-REGISTERED, none signalled"))
      (extractor-not-registered (c)
        (is (eq :totally-fictional-kind (extractor-error-kind c))
            "condition must carry the offending KIND")
        (is (eq 'x (extractor-error-name c))
            "condition must carry the offending extractor NAME")
        (let ((body (lol-web/server:http-error-body c)))
          (is (search "/regression/sentinel/bogus" body)
              "condition body should name the offending route, got ~S" body)
          (is (search ":TOTALLY-FICTIONAL-KIND" (string-upcase body))
              "condition body should name the offending kind, got ~S" body))))))

(test regression-sentinel-passes-when-all-extractors-registered
  "When every registered handler's extractors have a RESOLVE-EXTRACTOR method
   (or an fbound CUSTOM-RESOLVER), the sentinel returns the validated count
   instead of signalling."
  (let ((good-meta (make-hash-table :test 'equal)))
    (setf (gethash (cons :get "/regression/sentinel/ok") good-meta)
          (list :name 'regression-sentinel-ok
                :method :get
                :path "/regression/sentinel/ok"
                :extractors (list (make-extractor-spec
                                   :name 'id :kind :path :type 'integer)
                                  (make-extractor-spec
                                   :name 'q :kind :query :type 'string))
                :options nil))
    (is (= 2 (lol-web/extractors::%validate-handler-metadata-or-error good-meta))
        "sentinel must return the count of validated specs")))

(test regression-sentinel-detects-unbound-custom-resolver
  "If a handler declares :resolver with a symbol that isn't fbound, the
   sentinel signals — startup must refuse rather than 500-ing on first
   request through that route."
  (let ((meta (make-hash-table :test 'equal)))
    (setf (gethash (cons :get "/regression/sentinel/bad-resolver") meta)
          (list :name 'r
                :method :get
                :path "/regression/sentinel/bad-resolver"
                :extractors (list (make-extractor-spec
                                   :name 'x :kind :magic
                                   :custom-resolver
                                   'sentinel-no-such-fn-anywhere))
                :options nil))
    (signals extractor-not-registered
      (lol-web/extractors::%validate-handler-metadata-or-error meta))))

(test regression-sentinel-installed-on-before-server-start-hook
  "The sentinel function must be on *BEFORE-SERVER-START-HOOK* at file load
   time so START-SERVER picks it up automatically — no explicit registration
   required from extractors users."
  (is (member 'lol-web/extractors::%validate-handler-metadata-or-error
              lol-web/server:*before-server-start-hook*
              :test #'eq)
      "extractor sentinel symbol should be on the pre-server-start hook"))

(test regression-sentinel-permits-fbound-custom-resolver-with-unregistered-kind
  "When :resolver names an fbound function, the sentinel ignores the KIND
   axis entirely — the resolver's :around-method intercept already bypasses
   kind dispatch. Avoids spurious failure for one-off custom extractors."
  (let ((meta (make-hash-table :test 'equal)))
    (setf (gethash (cons :get "/regression/sentinel/custom-only") meta)
          (list :name 'cr
                :method :get
                :path "/regression/sentinel/custom-only"
                :extractors (list (make-extractor-spec
                                   :name 'x :kind :totally-fictional-kind
                                   :custom-resolver '%test-custom-resolver))
                :options nil))
    (is (= 1 (lol-web/extractors::%validate-handler-metadata-or-error meta)))))

;;; ============================================================================
;;; :type :keyword / :type :symbol coercion — bounded keyword pool
;;; ============================================================================

(test regression-coerce-keyword-bounds-keyword-pool
  "1000 distinct hostile :type :keyword inputs must not grow the keyword
   pool. Unknown inputs signal extractor-coercion-error (400); the test
   catches them so the loop completes."
  (let ((baseline (length (apropos-list "" :keyword)))
        (spec (make-extractor-spec :name 'k :kind :query :type 'keyword
                                   :required-p t :source-string "k")))
    (loop for i below 1000 do
          (handler-case
              (lol-web/extractors::%coerce-keyword
               (format nil "attackerKeywordInput~D~D" i (random 999999))
               spec)
            (extractor-coercion-error () nil)))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D"
          baseline after))))

(test regression-coerce-symbol-bounds-keyword-pool
  "Same bound for %coerce-symbol — :type :symbol shares the keyword pool
   with %coerce-keyword."
  (let ((baseline (length (apropos-list "" :keyword)))
        (spec (make-extractor-spec :name 's :kind :query :type 'symbol
                                   :required-p t :source-string "s")))
    (loop for i below 1000 do
          (handler-case
              (lol-web/extractors::%coerce-symbol
               (format nil "attackerSymbolInput~D~D" i (random 999999))
               spec)
            (extractor-coercion-error () nil)))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D"
          baseline after))))

(test regression-coerce-symbol-string-yields-keyword
  ":type :symbol coercion of a string resolves to a KEYWORD interned in the
   keyword package. Keywords satisfy SYMBOLP so the SYMBOL type contract
   holds, but a string-typed :symbol parameter never yields a non-keyword
   symbol — the resolved value is eq the corresponding keyword literal."
  ;; The :coerce-symbol-string-target literal below interns the target in
  ;; :keyword at read time, so the find-symbol-bounded coercion resolves it.
  (let* ((spec (make-extractor-spec :name 's :kind :query :type 'symbol
                                    :required-p t :source-string "s"))
         (result (resolve-extractor
                  :query spec
                  '(:query-parameters (("s" . "coerce-symbol-string-target")))
                  nil)))
    (is (keywordp result)
        ":type :symbol on a string must yield a keyword, got ~S" result)
    (is (eq :coerce-symbol-string-target result)
        "resolved value must be eq the interned keyword, got ~S" result)
    (is (symbolp result)
        "the keyword still satisfies SYMBOLP (the :symbol contract)")))

;;; ============================================================================
;;; Coercion-error wire body is generic — no input-echo / name+type oracle
;;; ============================================================================

(test regression-coercion-error-body-omits-raw-value
  "extractor-coercion-error's wire BODY is a generic message that neither
   echoes the submitted value nor names the failing extractor, so the
   text/plain 400 is not an input-echo / extractor-name+type oracle for an
   unauthenticated client. The full diagnostic (raw value + extractor
   identity) lives only in the condition REPORT, for server logs."
  (let ((c (handler-case
               (resolve-extractor
                :query
                (make-extractor-spec :name 'secret-param-name :kind :query
                                     :type 'integer)
                '(:query-parameters (("secret-param-name" . "sentinel-raw-12abc")))
                nil)
             (extractor-coercion-error (e) e))))
    (is (typep c 'extractor-coercion-error)
        "a non-numeric value must signal extractor-coercion-error")
    (let ((body (lol-web/server:http-error-body c)))
      (is (null (search "sentinel-raw-12abc" body))
          "wire body must NOT echo the submitted value, got ~S" body)
      (is (null (search "SECRET-PARAM-NAME" (string-upcase body)))
          "wire body must NOT name the failing extractor, got ~S" body))
    (let ((report (princ-to-string c)))
      (is (search "sentinel-raw-12abc" report)
          "the report (server log) DOES carry the raw value for diagnosis")
      (is (search "SECRET-PARAM-NAME" (string-upcase report))
          "the report names the failing extractor for diagnosis"))))
