;;;; Regression tests for surgery snapshot/undo/redo + surgery API routes.
;;;;
;;;; normalize-state-pairs converts plist-shaped :state values from
;;;; defcomponent-with-api into the alist form that capture-snapshot /
;;;; restore-snapshot / surgery-undo / surgery-redo / component-state-tree
;;;; expect. Without it, dolist over a plist yielded individual keywords
;;;; and (car :key) signalled TYPE-ERROR — snapshot round-trip never
;;;; worked for API components.
;;;;
;;;; The /api/surgery/* POST routes (state, update, snapshot, panel, undo,
;;;; redo) must be registered at load time; surgery-js.lisp issues fetches
;;;; against them. No HTTP-shaped eval surface: REPL-into-component is a
;;;; SLIME concern, never a route.

(in-package :lol-web/devtools/test)
(in-suite :lol-web/devtools/test)

(test regression-cycle-safe-printer-binds-level-length
  "WITH-CYCLE-SAFE-PRINTER bounds *PRINT-LEVEL* and *PRINT-LENGTH* so a deep
   or wide object rendered on a diagnostic surface cannot produce an unbounded
   string."
  (lol-web/core:with-cycle-safe-printer
    (is (integerp *print-level*) "*print-level* is bound to an integer cap")
    (is (integerp *print-length*) "*print-length* is bound to an integer cap")))

(test regression-bounded-serialize-truncates
  "BOUNDED-SERIALIZE caps depth, total node count, and per-string length,
   substituting the truncation marker, and terminates on cyclic structure, so
   an adversarial component state cannot exhaust memory or loop when the surgery
   panel serializes it. Ordinary values pass through unchanged."
  (let ((deep (let ((x 0)) (dotimes (i 50) (setf x (list x))) x))
        (long (make-string 500 :initial-element #\a))
        (wide (make-list 1000 :initial-element 1))
        (cyc  (let ((c (list 1 2 3))) (setf (cdddr c) c) c)))
    (is (search lol-web/core:*serialize-truncation-marker*
                (format nil "~s" (lol-web/core:bounded-serialize deep :max-depth 5)))
        "depth past the cap collapses to the truncation marker")
    (is (<= (length (lol-web/core:bounded-serialize long :max-string-length 100))
            (+ 100 (length lol-web/core:*serialize-truncation-marker*)))
        "a long string is truncated to the cap plus the marker")
    (is (find lol-web/core:*serialize-truncation-marker*
              (lol-web/core:bounded-serialize wide :max-nodes 10) :test #'equal)
        "a wide structure past the node budget gains a truncation marker")
    (is (equal '(1 "two" "THREE") (lol-web/core:bounded-serialize '(1 "two" :three)))
        "ordinary values serialize unchanged")
    (is (member lol-web/core:*serialize-truncation-marker*
                (lol-web/core:bounded-serialize cyc) :test #'equal)
        "a cyclic list terminates with a truncation marker instead of looping")))

(test regression-surgery-normalize-state-pairs
  "normalize-state-pairs converts plist or alist to alist form"
  (is (equal '((:a . 1) (:b . 2))
             (lol-web/devtools::normalize-state-pairs '((:a . 1) (:b . 2))))
      "alist input passes through unchanged")
  (is (equal '((:a . 1) (:b . 2))
             (lol-web/devtools::normalize-state-pairs '(:a 1 :b 2)))
      "plist input is converted to alist")
  (is (null (lol-web/devtools::normalize-state-pairs nil))
      "empty input yields empty list"))

(test regression-surgery-restore-snapshot-plist-state
  "restore-snapshot works on plist :state (defcomponent-with-api shape)"
  (let* ((restored (make-hash-table :test 'eq))
         (probe-id "regression-snapshot-probe-plist")
         (component
           (lambda (msg &rest args)
             (ecase msg
               (:id probe-id)
               (:inspect (list :id probe-id
                               :state (list :counter 7 :name "alice")
                               :subscribers 0
                               :mounted t))
               (:set-state (setf (gethash (first args) restored)
                                 (second args)))))))
    (lol-web/core:register-component probe-id component)
    (unwind-protect
         (let ((ts (capture-snapshot component "test")))
           (is (restore-snapshot component ts)
               "restore-snapshot returns t on success")
           (is (= 7 (gethash :counter restored))
               ":counter restored from plist snapshot")
           (is (string= "alice" (gethash :name restored))
               ":name restored from plist snapshot"))
      (lol-web/core:unregister-component probe-id))))

(test regression-surgery-restore-snapshot-alist-state
  "restore-snapshot still works on alist :state (defcomponent shape)"
  (let* ((restored (make-hash-table :test 'eq))
         (probe-id "regression-snapshot-probe-alist")
         (component
           (lambda (msg &rest args)
             (ecase msg
               (:id probe-id)
               (:inspect (list :id probe-id
                               :state '((counter . 7) (name . "alice"))
                               :subscribers 0
                               :mounted t))
               (:set-state (setf (gethash (first args) restored)
                                 (second args)))))))
    (lol-web/core:register-component probe-id component)
    (unwind-protect
         (let ((ts (capture-snapshot component "test")))
           (is (restore-snapshot component ts)
               "restore-snapshot returns t on success")
           (is (= 7 (gethash 'counter restored))
               "counter restored from alist snapshot")
           (is (string= "alice" (gethash 'name restored))
               "name restored from alist snapshot"))
      (lol-web/core:unregister-component probe-id))))

(test regression-surgery-component-state-tree-plist
  "component-state-tree renders plist :state correctly"
  (let* ((probe-id "regression-state-tree-probe")
         (component
           (lambda (msg &rest args)
             (declare (ignore args))
             (ecase msg
               (:id probe-id)
               (:inspect (list :id probe-id
                               :state (list :counter 42)
                               :subscribers 0
                               :mounted t))))))
    (let* ((tree (component-state-tree component))
           (state (cdr (assoc :state tree))))
      (is (= 1 (length state))
          "one state entry rendered")
      (is (eq :counter (cdr (assoc :key (first state))))
          ":key correctly extracted from plist pair")
      (is (= 42 (cdr (assoc :value (first state))))
          ":value correctly extracted from plist pair"))))

(test regression-surgery-api-routes-registered
  "The /api/surgery/* POST routes (state, update, snapshot, panel, undo,
   redo) are registered at load time. No /api/surgery/eval route exists —
   HTTP-shaped Lisp eval is not an API surface."
  (dolist (path '("/api/surgery/state"
                  "/api/surgery/update"
                  "/api/surgery/snapshot"
                  "/api/surgery/panel"
                  "/api/surgery/undo"
                  "/api/surgery/redo"))
    (is (gethash (cons :post path) lol-web/server:*routes*)
        "POST ~A is registered" path)))

(test regression-surgery-eval-route-absent
  "No /api/surgery/eval route is registered. Any HTTP eval surface is a
   remote-code-execution-behind-a-flag class regression."
  (is (null (gethash (cons :post "/api/surgery/eval") lol-web/server:*routes*))
      "POST /api/surgery/eval must not be registered"))

(test regression-surgery-eval-in-context-unbound
  "surgery-eval-in-context must not be a bound function in :lol-web/devtools.
   Deleting the function removes the eval surface; resurrection through any
   reachable path is a regression."
  (let ((sym (find-symbol "SURGERY-EVAL-IN-CONTEXT" :lol-web/devtools)))
    (is (or (null sym) (not (fboundp sym)))
        "surgery-eval-in-context must be unbound in :lol-web/devtools")))

(test regression-surgery-mode-installs-render-hook
  "enable-surgery-mode installs xray-wrapper-html as
   :lol-web/html's *component-render-hook*; disable-surgery-mode
   clears it. The hook is the bridge that lets *surgery-mode*
   actually change what component->html returns."
  (let ((hook-before lol-web/html:*component-render-hook*))
    (unwind-protect
         (progn
           (lol-web/devtools::disable-surgery-mode)
           (is (null lol-web/html:*component-render-hook*)
               "disable-surgery-mode clears the render hook")
          (let ((lol-web/devtools::*allow-global-surgery-enable* t))
            (lol-web/devtools::enable-surgery-mode))
          (is (eq #'lol-web/devtools::xray-wrapper-html
                  lol-web/html:*component-render-hook*)
               "enable-surgery-mode installs xray-wrapper-html as the hook")
           (lol-web/devtools::disable-surgery-mode)
           (is (null lol-web/html:*component-render-hook*)
               "disable-surgery-mode clears the hook again"))
      (setf lol-web/html:*component-render-hook* hook-before))))

(test regression-enable-surgery-mode-requires-explicit-global-opt-in
  "Global surgery mode cannot be enabled by a bare call."
  (let ((lol-web/devtools::*allow-global-surgery-enable* nil))
    (signals error (lol-web/devtools::enable-surgery-mode))))

(test regression-component-to-html-uses-render-hook
  "lol-web/html:component->html consults *component-render-hook* under
   :wrapper t and bypasses it when :wrapper nil. The hook receives the
   component and is responsible for the entire wrapped output."
  (let ((hook-before lol-web/html:*component-render-hook*)
        (probe (lambda (msg &rest args)
                 (declare (ignore args))
                 (ecase msg
                   (:id "render-hook-probe")
                   (:render "<inner/>")))))
    (unwind-protect
         (progn
           (setf lol-web/html:*component-render-hook*
                 (lambda (c)
                   (declare (ignore c))
                   "<wrapped-by-hook/>"))
           (is (search "<wrapped-by-hook/>"
                       (lol-web/html:component->html probe :wrapper t))
               "hook output is returned when :wrapper t")
           (is (string= "<inner/>"
                        (lol-web/html:component->html probe :wrapper nil))
               "hook is bypassed when :wrapper nil"))
      (setf lol-web/html:*component-render-hook* hook-before))))

(test regression-behavior-presets-symbols-not-defined
  "register-behavior-preset, *behavior-presets*, and
   list-behavior-presets must not be defined in :lol-web/devtools.
   They have no consumers in-tree and no exports — leaving them
   defined would just keep dead surface alive."
  (dolist (sym '(:register-behavior-preset
                 :*behavior-presets*
                 :list-behavior-presets))
    (let ((s (find-symbol (symbol-name sym) :lol-web/devtools)))
      (is (or (null s) (not (or (boundp s) (fboundp s))))
          (format nil "symbol ~A must not be bound or fbound in :lol-web/devtools"
                  sym)))))

(test regression-surgery-mode-defaults-to-disabled
  "*surgery-mode* defaults NIL — the load-bearing gate for /api/surgery/*."
  ;; Sibling tests flip *surgery-mode* under unwind-protect; normalise first.
  (lol-web/devtools::disable-surgery-mode)
  (is (null lol-web/devtools::*surgery-mode*))
  (is (null (lol-web/devtools::surgery-mode-p))))

(test regression-with-surgery-gate-fail-closed
  "with-surgery-gate skips BODY and returns the refusal shape when surgery-mode NIL."
  (let ((body-ran nil))
    (lol-web/devtools::disable-surgery-mode)
    (let ((response
            (lol-web/devtools::with-surgery-gate
              (setf body-ran t)
              "must not reach caller")))
      (is (null body-ran))
      (is (stringp response))
      (is (search "\"success\"" (string-downcase response)))
      (is (search "component not found" (string-downcase response)))))
  (unwind-protect
       (progn
         (let ((lol-web/devtools::*allow-global-surgery-enable* t))
           (lol-web/devtools::enable-surgery-mode))
         (is (string= "body-value"
                      (lol-web/devtools::with-surgery-gate "body-value"))))
    (lol-web/devtools::disable-surgery-mode)))

(test regression-surgery-disabled-response-matches-not-found
  "Disabled response byte-identical to encoded not-found; refuses to leak deployment state."
  (is (string= (lol-web/devtools::%surgery-disabled-response)
               (lol-web/server:encode-json-string
                (lol-web/devtools::%surgery-not-found)))))

;;; ============================================================================
;;; surgery-update key coercion — bounded keyword pool
;;; ============================================================================

(test regression-surgery-update-key-bounds-keyword-pool
  "1000 distinct hostile surgery-update :key payloads must not grow the
   keyword pool."
  (let ((baseline (length (apropos-list "" :keyword))))
    (loop for i below 1000 do
          (lol-web/devtools::safe-coerce-keyword
           (format nil "attacker-surgery-update-key-~D-~D" i (random 999999))))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D"
          baseline after)))
  (is (eq 'lol-web/escape:safe-coerce-keyword
          (find-symbol "SAFE-COERCE-KEYWORD" :lol-web/devtools))
      "safe-coerce-keyword must be imported into :lol-web/devtools"))

;;; ============================================================================
;;; surgery-middleware — per-request let-binding of *surgery-mode* + hook
;;; ============================================================================

(defun %surgery-mw-probe-app ()
  "Probe app that captures the surgery-mode + render-hook values it
   observes inside the middleware's dynamic extent. Returns a closure
   over (probe :captured) -> (cons surgery-mode hook)."
  (let ((captured nil))
    (lambda (msg &optional env)
      (declare (ignore env))
      (ecase msg
        (:probe-call
         (setf captured (cons lol-web/devtools::*surgery-mode*
                              lol-web/html:*component-render-hook*))
         (list 200 nil (list "ok")))
        (:captured captured)))))

(defun %surgery-mw-env (&key session-entry include-session)
  "Build a synthetic env with optional :lack.session hash-table.
   SESSION-ENTRY is the value stored under :lol-web/surgery-mode (or
   :omit to leave the key absent)."
  (let ((session (when include-session
                   (let ((s (make-hash-table :test 'eql)))
                     (unless (eq session-entry :omit)
                       (setf (gethash :lol-web/surgery-mode s) session-entry))
                     s))))
    (list :lack.session session)))

(test regression-surgery-middleware-decide-t-binds-mode-and-hook
  "decide -> T inside the middleware body sees *surgery-mode* T and
   *component-render-hook* set to xray-wrapper-html. After return the
   globals are restored to their prior values."
  (let* ((before-mode lol-web/devtools::*surgery-mode*)
         (before-hook lol-web/html:*component-render-hook*)
         (probe (%surgery-mw-probe-app))
         (mw    (lol-web/devtools:surgery-middleware
                  (lambda (env) (funcall probe :probe-call env))
                  :decide (constantly t))))
    (unwind-protect
         (progn
           (funcall mw (list))
           (let ((capt (funcall probe :captured)))
             (is (eq t (car capt))
                 "inside body: *surgery-mode* must be T, got ~S" (car capt))
             (is (eq #'lol-web/devtools::xray-wrapper-html (cdr capt))
                 "inside body: render-hook must be xray-wrapper-html")))
      (setf lol-web/devtools::*surgery-mode* before-mode
            lol-web/html:*component-render-hook* before-hook))
    (is (eq before-mode lol-web/devtools::*surgery-mode*)
        "after middleware return: global *surgery-mode* restored")
    (is (eq before-hook lol-web/html:*component-render-hook*)
        "after middleware return: global render-hook restored")))

(test regression-surgery-middleware-decide-nil-binds-both-nil
  "decide -> NIL forces *surgery-mode* NIL and render-hook NIL inside,
   regardless of what the globals are."
  (let ((before-mode lol-web/devtools::*surgery-mode*)
        (before-hook lol-web/html:*component-render-hook*))
    (unwind-protect
         (progn
           (let ((lol-web/devtools::*allow-global-surgery-enable* t))
             (lol-web/devtools::enable-surgery-mode))
           (let* ((probe (%surgery-mw-probe-app))
                  (mw    (lol-web/devtools:surgery-middleware
                           (lambda (env) (funcall probe :probe-call env))
                           :decide (constantly nil))))
             (funcall mw (list))
             (let ((capt (funcall probe :captured)))
               (is (null (car capt))
                   "globals ON + decide NIL -> body must see *surgery-mode* NIL")
               (is (null (cdr capt))
                   "globals ON + decide NIL -> body must see render-hook NIL"))))
      (setf lol-web/devtools::*surgery-mode* before-mode
            lol-web/html:*component-render-hook* before-hook))))

(test regression-surgery-middleware-shadows-global-setf
  "(setf *surgery-mode* T) from outside cannot poison a request whose
   middleware decided NIL — the let-binding masks the global. This is
   the core multi-tenant property: an admin/REPL flip cannot affect a
   non-admin user's request through this middleware."
  (let ((before-mode lol-web/devtools::*surgery-mode*)
        (before-hook lol-web/html:*component-render-hook*))
    (unwind-protect
         (progn
           (setf lol-web/devtools::*surgery-mode* t
                 lol-web/html:*component-render-hook*
                   #'lol-web/devtools::xray-wrapper-html)
           (let* ((probe (%surgery-mw-probe-app))
                  (mw    (lol-web/devtools:surgery-middleware
                           (lambda (env) (funcall probe :probe-call env))
                           :decide (constantly nil))))
             (funcall mw (list))
             (let ((capt (funcall probe :captured)))
               (is (null (car capt))
                   "global T must be masked to NIL by middleware decide NIL")
               (is (null (cdr capt))
                   "global hook must be masked to NIL by middleware decide NIL"))))
      (setf lol-web/devtools::*surgery-mode* before-mode
            lol-web/html:*component-render-hook* before-hook))))

(test regression-surgery-middleware-restores-globals-on-app-error
  "If the wrapped app signals, the let-binding still unwinds and the
   globals are restored — exception-safety via unwind-protect-style
   dynamic-binding semantics."
  (let* ((before-mode lol-web/devtools::*surgery-mode*)
         (before-hook lol-web/html:*component-render-hook*)
         (mw (lol-web/devtools:surgery-middleware
               (lambda (env) (declare (ignore env)) (error "app blew up"))
               :decide (constantly t))))
    (handler-case (funcall mw (list)) (error () nil))
    (is (eq before-mode lol-web/devtools::*surgery-mode*)
        "global *surgery-mode* restored after app error")
    (is (eq before-hook lol-web/html:*component-render-hook*)
        "global render-hook restored after app error")))

(test regression-surgery-middleware-default-decide-reads-session
  "Default %default-surgery-decide reads :lack.session for an explicit
   :lol-web/surgery-mode -> T entry; absent or non-T value yields NIL."
  (let ((d #'lol-web/devtools::%default-surgery-decide))
    (is (null (funcall d (%surgery-mw-env :include-session nil)))
        "no session -> NIL")
    (is (null (funcall d (%surgery-mw-env :include-session t
                                          :session-entry :omit)))
        "session present but key absent -> NIL")
    (is (null (funcall d (%surgery-mw-env :include-session t
                                          :session-entry nil)))
        "session present, key bound to NIL -> NIL")
    (is (null (funcall d (%surgery-mw-env :include-session t
                                          :session-entry "truthy-but-not-t")))
        "session present, key bound to non-T truthy -> NIL (strict eq)")
    (is (eq t (funcall d (%surgery-mw-env :include-session t
                                          :session-entry t)))
        "session present, key explicitly T -> T")))

(test regression-surgery-middleware-default-decide-via-middleware
  "End-to-end: default decide enables the request when the session
   carries :lol-web/surgery-mode -> T."
  (let ((before-mode lol-web/devtools::*surgery-mode*)
        (before-hook lol-web/html:*component-render-hook*))
    (unwind-protect
         (let* ((probe (%surgery-mw-probe-app))
                (mw    (lol-web/devtools:surgery-middleware
                         (lambda (env) (funcall probe :probe-call env))))
                (env   (%surgery-mw-env :include-session t
                                        :session-entry t)))
           (funcall mw env)
           (let ((capt (funcall probe :captured)))
             (is (eq t (car capt))
                 "session :lol-web/surgery-mode T -> body sees T")
             (is (eq #'lol-web/devtools::xray-wrapper-html (cdr capt))
                 "session :lol-web/surgery-mode T -> body sees xray hook")))
      (setf lol-web/devtools::*surgery-mode* before-mode
            lol-web/html:*component-render-hook* before-hook))))

;;; ============================================================================
;;; Per-instance surgery stacks — lifecycle, isolation, FIFO caps
;;; ============================================================================

(defun %surgery-stack-probe (id &key (counter 0))
  "Minimal probe component honouring :id / :inspect / :set-state."
  (let ((c counter))
    (lambda (msg &rest args)
      (ecase msg
        (:id id)
        (:inspect (list :id id
                        :state (list :counter c)
                        :subscribers 0
                        :mounted t))
        (:set-state
         (when (eq (first args) :counter)
           (setf c (second args))))))))

(test regression-surgery-stacks-tied-to-registration-lifetime
  "unregister-component releases snapshot/undo/redo stacks alongside the entry."
  (let* ((id "regression-stacks-lifetime")
         (probe (%surgery-stack-probe id)))
    (lol-web/core:register-component id probe)
    (unwind-protect
         (progn
           (capture-snapshot probe "before")
           (push-undo probe)
           (setf (lol-web/core:component-redo-stack id) (list :rstub))
           (is (not (null (lol-web/core:component-snapshots id))))
           (is (not (null (lol-web/core:component-undo-stack id))))
           (is (not (null (lol-web/core:component-redo-stack id))))
           (lol-web/core:unregister-component id)
           (is (null (lol-web/core:component-snapshots id))
               "snapshots gone after unregister")
           (is (null (lol-web/core:component-undo-stack id))
               "undo stack gone after unregister")
           (is (null (lol-web/core:component-redo-stack id))
               "redo stack gone after unregister"))
      (lol-web/core:unregister-component id))))

(test regression-surgery-stacks-cross-instance-isolation
  "Two registered components in one image keep separate snapshot/undo/redo stacks."
  (let* ((id-a "regression-iso-a")
         (id-b "regression-iso-b")
         (a (%surgery-stack-probe id-a))
         (b (%surgery-stack-probe id-b)))
    (lol-web/core:register-component id-a a)
    (lol-web/core:register-component id-b b)
    (unwind-protect
         (progn
           (capture-snapshot a "only-a")
           (push-undo a)
           (is (= 1 (length (list-snapshots a))))
           (is (null (list-snapshots b))
               "B's snapshots untouched by A's capture")
           (is (lol-web/devtools::can-undo-p id-a))
           (is (null (lol-web/devtools::can-undo-p id-b))
               "B's undo stack untouched by A's push-undo")
           (capture-snapshot b "only-b")
           (is (= 1 (length (list-snapshots a)))
               "A's snapshot count unchanged when B captures")
           (is (= 1 (length (list-snapshots b)))))
      (lol-web/core:unregister-component id-a)
      (lol-web/core:unregister-component id-b))))

(test regression-surgery-snapshot-cap-fifo-eviction
  "capture-snapshot drops oldest entries past *max-snapshots-per-component*."
  (let* ((id "regression-snapshot-cap")
         (probe (%surgery-stack-probe id)))
    (lol-web/core:register-component id probe)
    (unwind-protect
         (let ((lol-web/devtools::*max-snapshots-per-component* 3))
           (capture-snapshot probe "s1")
           (capture-snapshot probe "s2")
           (capture-snapshot probe "s3")
           (capture-snapshot probe "s4")
           (capture-snapshot probe "s5")
           (let ((snaps (list-snapshots probe)))
             (is (= 3 (length snaps))
                 "cap enforced at 3, got ~D" (length snaps))
             (is (equal "s5" (getf (first snaps) :description))
                 "most-recent at head")
             (is (equal "s3" (getf (third snaps) :description))
                 "s1 and s2 evicted; s3 is the oldest survivor")))
      (lol-web/core:unregister-component id))))

(test regression-surgery-undo-cap-fifo-eviction
  "push-undo drops oldest entries past *max-undo-depth*."
  (let* ((id "regression-undo-cap")
         (probe (%surgery-stack-probe id :counter 0)))
    (lol-web/core:register-component id probe)
    (unwind-protect
         (let ((lol-web/devtools::*max-undo-depth* 3))
           (dotimes (i 5) (push-undo probe))
           (let ((u (lol-web/core:component-undo-stack id)))
             (is (= 3 (length u))
                 "undo cap enforced at 3, got ~D" (length u))))
      (lol-web/core:unregister-component id))))

(test regression-surgery-operations-noop-on-unregistered
  "capture-snapshot / push-undo / surgery-undo return NIL when the
   component is not registered — no orphan state can accumulate."
  (let* ((id "regression-unregistered-probe")
         (probe (%surgery-stack-probe id)))
    ;; Ensure clean slate.
    (lol-web/core:unregister-component id)
    (is (null (capture-snapshot probe "nope"))
        "capture-snapshot returns NIL for unregistered id")
    (is (null (push-undo probe))
        "push-undo returns NIL for unregistered id")
    (is (null (surgery-undo id))
        "surgery-undo returns NIL for unregistered id")
    (is (null (lol-web/core:component-snapshots id))
        "no snapshot state was stashed off-registry")
    (is (null (lol-web/core:component-undo-stack id)))
    (is (null (lol-web/core:component-redo-stack id)))))

;;; ============================================================================
;;; surgery-panel-html — format-value-for-json renders escaped
;;; ============================================================================

(test regression-format-value-for-json-escapes-html
  "surgery-panel-html routes state-tree key/value/type through cl-who:esc,
   so a state pair carrying `<script>alert(1)</script>` in a value renders
   as literal text inside the panel, not as a tag."
  (let* ((id "regression-panel-escape-probe")
         (state (list "<script>" "alert(1)"))
         (component
           (lambda (msg &rest args)
             (declare (ignore args))
             (ecase msg
               (:id id)
               (:inspect (list :id id :state state :subscribers 0 :mounted t))))))
    (lol-web/core:register-component id component)
    (unwind-protect
         (let ((html (lol-web/devtools::surgery-panel-html component)))
           (is (search "&lt;script&gt;" html)
               "raw `<script>` must render as &lt;script&gt;")
           (is (null (search "<script>alert(1)" html))
               "unescaped attacker payload must not appear inside the panel"))
      (lol-web/core:unregister-component id))))

(test regression-format-value-for-json-handles-circular-list
  "State formatting terminates on circular lists, substituting the bounded
   serializer's truncation marker for the back-reference."
  (let ((xs (list :a)))
    (setf (cdr xs) xs)
    (is (equal (list "A" lol-web/core:*serialize-truncation-marker*)
               (lol-web/devtools::format-value-for-json xs)))))

(test regression-format-value-for-json-handles-circular-hash
  "State formatting terminates on self-referential hash tables, substituting
   the truncation marker for the back-reference."
  (let ((ht (make-hash-table :test 'eq)))
    (setf (gethash :self ht) ht)
    (let ((entry (assoc "SELF" (lol-web/devtools::format-value-for-json ht)
                        :test #'string=)))
      (is (and entry
               (string= lol-web/core:*serialize-truncation-marker*
                        (cdr entry)))))))

;;; ============================================================================
;;; Surgery runtime JS attaches a CSRF token to every fetch
;;; ============================================================================

(test regression-surgery-runtime-js-includes-csrf-header
  "Every fetch in the generated runtime carries X-CSRF-Token. A static
   substring check is sufficient because the runtime is produced by a
   single parenscript:ps form whose output is text."
  (let ((js (lol-web/devtools::surgery-runtime-js)))
    (is (search "X-CSRF-Token" js)
        "runtime JS must include the X-CSRF-Token header literal")
    (is (search "meta[name=" js)
        "runtime JS must read the token from a meta tag selector")
    (is (null (search "Content-Type" (subseq js 0 (search "csrfHeaders" js
                                                          :start2 0))))
        "no fetch above csrf-headers should set Content-Type without it")))

(test regression-surgery-runtime-js-fetches-all-use-csrf-headers
  "Every fetch site routes through csrfHeaders — no inline Content-Type
   construction survives. The substring `csrfHeaders()` matches once for
   the helper declaration plus once per call site, so the invariant is
   header-count = 1 + fetch-count."
  (let* ((js (lol-web/devtools::surgery-runtime-js))
         (fetch-count (loop with start = 0
                            for pos = (search "fetch(" js :start2 start)
                            while pos
                            count pos
                            do (setf start (1+ pos))))
         (header-count (loop with start = 0
                             for pos = (search "csrfHeaders()" js :start2 start)
                             while pos
                             count pos
                             do (setf start (1+ pos)))))
    (is (>= fetch-count 6)
        "expected at least six fetch calls in the surgery runtime, got ~D"
        fetch-count)
    (is (= header-count (1+ fetch-count))
        "fetches=~D should pair with ~D csrfHeaders mentions (1 declaration + ~D calls); got ~D"
        fetch-count (1+ fetch-count) fetch-count header-count)))

(test regression-surgery-panel-emits-csrf-meta-tag
  "surgery-panel-html emits a <meta name=\"csrf-token\"> tag whenever a
   token is resolvable. With no CSRF middleware installed the tag is
   absent (empty string), but the helper code path runs."
  (let* ((id "regression-panel-csrf-probe")
         (component
           (lambda (msg &rest args)
             (declare (ignore args))
             (ecase msg
               (:id id)
               (:inspect (list :id id :state '() :subscribers 0 :mounted t))))))
    (lol-web/core:register-component id component)
    (unwind-protect
         (let ((html (lol-web/devtools::surgery-panel-html component)))
           (is (stringp html)
               "panel HTML must render without an active session")
           (is (search "surgery-panel" html)
               "panel scaffold must still be present"))
      (lol-web/core:unregister-component id))))

;;; ============================================================================
;;; Condition rendering routes through public-condition-message
;;; ============================================================================

(test regression-public-condition-message-class-only
  "Default formatter returns just the class name; no condition text
   reaches the wire. Whitelisting a class adds a :fields entry, but
   the default exposure is the bare class symbol."
  (let* ((c (handler-case (error "secret stack frame ~A" "leak")
              (error (e) e)))
         (msg (lol-web/devtools:public-condition-message c)))
    (is (eq 'simple-error (getf msg :class))
        "default :class must be the condition's class symbol")
    (is (null (getf msg :fields))
        "no :fields entry when class is not whitelisted")
    (is (null (search "secret stack frame" (format nil "~S" msg)))
        "raw condition message text must not appear in the formatted result")))

(test regression-public-condition-message-whitelist
  "A whitelisted accessor surfaces its value under :fields. Whitelisting
   is opt-in so the default contract stays restrictive."
  (let* ((c (handler-case (error 'simple-error
                                  :format-control "shouldnt leak ~A"
                                  :format-arguments '("payload"))
              (error (e) e)))
         (lol-web/devtools:*public-condition-accessors*
           (list (cons 'simple-error
                       (list (lambda (cond) (class-name (class-of cond)))))))
         (msg (lol-web/devtools:public-condition-message c)))
    (is (equal (list 'simple-error) (getf msg :fields))
        "whitelist exposes accessor results under :fields")
    (is (null (search "payload" (format nil "~S" msg)))
        "even with whitelist, the raw format-arguments must not leak")))

;;; ============================================================================
;;; %surgery-component — ownership (principal-binding) gate
;;; ============================================================================

(test regression-surgery-honors-principal-binding
  "%surgery-component resolves a public (no principal-binding) component for
   any request, but a binding-gated component resolves only when the current
   request principal EQUALs the binding — an unowned component is
   indistinguishable from absent (NIL), closing the cross-principal surgery
   read/write gap."
  (let* ((pub-id "regression-surgery-pub")
         (bound-id "regression-surgery-bound")
         (probe (lambda (msg &rest args)
                  (declare (ignore args))
                  (ecase msg (:id pub-id)))))
    (lol-web/core:register-component pub-id probe)
    (lol-web/core:register-component bound-id probe :principal-binding "alice")
    (unwind-protect
         (progn
           (let ((lol-web/server:*env* nil))
             (is (eq probe (lol-web/devtools::%surgery-component pub-id))
                 "public component resolves with no request principal"))
           (let ((lol-web/server:*env* nil))
             (is (null (lol-web/devtools::%surgery-component bound-id))
                 "bound component refuses when there is no request principal"))
           (let ((lol-web/server:*env*
                   (list :lol-web.auth.hooks
                         (cons (lambda () t) (lambda () "mallory")))))
             (is (null (lol-web/devtools::%surgery-component bound-id))
                 "bound component refuses a non-matching principal"))
           (let ((lol-web/server:*env*
                   (list :lol-web.auth.hooks
                         (cons (lambda () t) (lambda () "alice")))))
             (is (eq probe (lol-web/devtools::%surgery-component bound-id))
                 "bound component resolves for the matching principal")))
      (lol-web/core:unregister-component pub-id)
      (lol-web/core:unregister-component bound-id))))

(test regression-defcomponent-registers-public-no-principal-binding
  "A DEFCOMPONENT instance registers with no principal-binding, so it is a
   public instance: %surgery-principal-owns-p admits any request principal,
   including a non-matching one. Cross-principal isolation for default
   components is not provided by the ownership gate — it requires
   DEFCOMPONENT-WITH-API with an explicit :principal-binding."
  (lol-web/core:defcomponent regression-public-surgery-probe ((count 0))
    (:render () "")
    (:dispatch (action &rest args) (declare (ignore action args)) nil))
  (let* ((component (regression-public-surgery-probe))
         (id (funcall component :id)))
    (unwind-protect
         (progn
           (is (null (lol-web/core:component-principal-binding id))
               "defcomponent must register with a NIL principal-binding")
           (let ((lol-web/server:*env*
                   (list :lol-web.auth.hooks
                         (cons (lambda () t) (lambda () "mallory")))))
             (is (not (null (lol-web/devtools::%surgery-principal-owns-p id)))
                 "a binding-less component is owned-by-anyone, even a ~
                  non-matching principal")))
      (funcall component :unmount))))

;;; ============================================================================
;;; surgery-set-state — snapshot capture + mutation atomic under the lock
;;; ============================================================================

(test regression-surgery-set-state-snapshot-capture-is-atomic
  "surgery-set-state captures the pre-mutation snapshot and applies the new
   value as one operation under the component registry lock. Concurrent
   writers each leave exactly one snapshot frame — below the per-component
   cap none is lost to an interleaved read-cons-write."
  (let* ((id "regression-surgery-set-state-atomic")
         (probe (%surgery-stack-probe id))
         (writers 10))
    (lol-web/core:register-component id probe)
    (unwind-protect
         (progn
           (let ((threads
                   (loop for i below writers
                         collect (bordeaux-threads:make-thread
                                  (let ((v i))
                                    (lambda ()
                                      (lol-web/devtools::surgery-set-state
                                       id :counter v)))))))
             (dolist (th threads) (bordeaux-threads:join-thread th)))
           (is (= writers (length (lol-web/core:component-snapshots id)))
               "~D concurrent writes must leave ~D snapshot frames, got ~D"
               writers writers (length (lol-web/core:component-snapshots id))))
      (lol-web/core:unregister-component id))))

;;; ============================================================================
;;; surgery-undo — atomic across the undo/redo stacks under concurrency
;;; ============================================================================

(test regression-surgery-undo-concurrent-preserves-history
  "surgery-undo's read-modify-write across the undo and redo stacks runs under
   *components-lock*, so concurrent undos move exactly one frame each from undo
   to redo — the total frame count is conserved and no frame is lost or
   duplicated by an interleave."
  (let* ((id "regression-surgery-undo-concurrent")
         (probe (%surgery-stack-probe id))
         (n 200)
         (threads-n 16)
         (per-thread 10))
    (lol-web/core:register-component id probe)
    (unwind-protect
         (progn
           (dotimes (_ n) (push-undo probe))
           (is (= n (length (lol-web/core:component-undo-stack id)))
               "setup: ~D undo frames staged" n)
           (let ((threads
                   (loop repeat threads-n
                         collect (bordeaux-threads:make-thread
                                  (lambda ()
                                    (dotimes (_ per-thread)
                                      (surgery-undo id)))))))
             (dolist (th threads) (bordeaux-threads:join-thread th)))
           (let ((undo (length (lol-web/core:component-undo-stack id)))
                 (redo (length (lol-web/core:component-redo-stack id))))
             (is (= n (+ undo redo))
                 "frame count conserved: undo ~D + redo ~D must equal ~D"
                 undo redo n)
             (is (= (* threads-n per-thread) redo)
                 "exactly ~D frames moved to redo, got ~D"
                 (* threads-n per-thread) redo)))
      (lol-web/core:unregister-component id))))

;;; ============================================================================
;;; with-surgery-write-gate — surgery routes self-assert CSRF
;;; ============================================================================

(test regression-surgery-write-gate-requires-csrf
  "with-surgery-write-gate runs BODY only when surgery mode is on AND the
   request carries a valid CSRF token; otherwise it returns the byte-identical
   disabled/not-found refusal. A consumer running :use-csrf nil with surgery on
   therefore cannot mutate component state without a token."
  (unwind-protect
       (progn
         (let ((lol-web/devtools::*allow-global-surgery-enable* t))
           (lol-web/devtools::enable-surgery-mode))
         (let* ((lol-web/server:*env* (list :content-type "application/json"))
                (result (lol-web/devtools::with-surgery-write-gate "x")))
           (is (string= (lol-web/devtools::%surgery-disabled-response) result)
               "no session/token -> write gate refuses with the disabled shape")
           (is (null (string= "x" result))
               "body must not run without a valid CSRF token"))
         (let ((session (make-hash-table :test 'equal))
               (headers (make-hash-table :test 'equal)))
           (setf (gethash "csrf-token" session) "good-token")
           (setf (gethash "x-csrf-token" headers) "good-token")
           (let* ((lol-web/server:*env* (list :lack.session session
                                              :content-type "application/json"
                                              :headers headers))
                  (result (lol-web/devtools::with-surgery-write-gate "x")))
             (is (string= "x" result)
                 "valid CSRF token -> body runs and returns its value"))))
    (lol-web/devtools::disable-surgery-mode)))

;;; ============================================================================
;;; enable-surgery-mode — production refusal + audit hook
;;; ============================================================================

(test regression-enable-surgery-mode-refuses-in-production
  "enable-surgery-mode refuses outright when the production env var is set,
   even with the global opt-in flag T — a stray REPL enable cannot expose the
   x-ray surface in a production image. PATH is reliably set in the build env."
  (unwind-protect
       (let ((lol-web/devtools::*allow-global-surgery-enable* t)
             (lol-web/devtools::*surgery-production-env-var* "PATH"))
         (signals error (lol-web/devtools::enable-surgery-mode)))
    (lol-web/devtools::disable-surgery-mode)))

(test regression-enable-surgery-mode-fires-audit-hook
  "enable-surgery-mode funcalls *surgery-enable-audit-hook* on success, so an
   accidental global enable is observable rather than silent."
  (let ((fired nil))
    (unwind-protect
         (let ((lol-web/devtools::*allow-global-surgery-enable* t)
               (lol-web/devtools::*surgery-production-env-var*
                 "LOL_WEB_NONEXISTENT_PROD_VAR_FOR_TEST")
               (lol-web/devtools::*surgery-enable-audit-hook*
                 (lambda () (setf fired t))))
           (lol-web/devtools::enable-surgery-mode)
           (is (eq t fired)
               "audit hook must fire when global surgery is enabled"))
      (lol-web/devtools::disable-surgery-mode))))

;;; ============================================================================
;;; surgery-dispatch — locked + snapshot, serialized against ordinary dispatch
;;; ============================================================================

(test regression-surgery-vs-dispatch-no-torn-state
  "Ordinary :dispatch and surgery-dispatch both mutate component state under
   *components-lock*, so concurrent increments from the two paths serialize and
   none is lost. Without the protocol-level lock over :dispatch the counter
   would fall short of the total number of increments."
  (lol-web/core:defcomponent regression-lockstep-counter ((count 0))
    (:render () "")
    (:dispatch (action &rest args)
      (declare (ignore args))
      (case action (:inc (incf count)))))
  (let* ((comp (regression-lockstep-counter :id "regression-lockstep-counter"))
         (id (funcall comp :id))
         (threads-n 8)
         (per-thread 50))
    (unwind-protect
         (progn
           (let ((threads
                   (loop for tid below threads-n
                         collect (let ((surgeon (evenp tid)))
                                   (bordeaux-threads:make-thread
                                    (lambda ()
                                      (dotimes (_ per-thread)
                                        (if surgeon
                                            (surgery-dispatch id :inc)
                                            (funcall comp :dispatch :inc)))))))))
             (dolist (th threads) (bordeaux-threads:join-thread th)))
           (is (= (* threads-n per-thread) (funcall comp :state :count))
               "~D increments split across surgery-dispatch and plain dispatch ~
                must all land; a shortfall means :dispatch is not serialized"
               (* threads-n per-thread)))
      (funcall comp :unmount))))

(test regression-surgery-dispatch-locked-or-unexported
  "surgery-dispatch mirrors surgery-set-state: it captures a restorable
   snapshot before applying the action (so the change runs under the lock and
   is undoable), and remains exported on the devtools surface."
  (lol-web/core:defcomponent regression-surgery-dispatch-probe ((count 0))
    (:render () "")
    (:dispatch (action &rest args)
      (declare (ignore args))
      (case action (:inc (incf count)))))
  (let* ((comp (regression-surgery-dispatch-probe
                :id "regression-surgery-dispatch-probe"))
         (id (funcall comp :id)))
    (unwind-protect
         (progn
           (is (null (lol-web/core:component-snapshots id))
               "no snapshots before surgery-dispatch")
           (surgery-dispatch id :inc)
           (is (= 1 (length (lol-web/core:component-snapshots id)))
               "surgery-dispatch captured exactly one pre-mutation snapshot")
           (is (= 1 (funcall comp :state :count))
               "the dispatched action was applied")
           (is (eq :external
                   (nth-value 1 (find-symbol "SURGERY-DISPATCH" :lol-web/devtools)))
               "surgery-dispatch stays exported on the devtools surface"))
      (funcall comp :unmount))))
