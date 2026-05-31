;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/FULLSTACK; Base: 10 -*-
;;;; fullstack/component-api.lisp - Components with Auto-Generated API Endpoints
;;;;
;;;; PURPOSE:
;;;;   Single component definition generates both server-side rendering
;;;;   and API endpoints for client-side interaction. No manual route
;;;;   registration required.
;;;;
;;;; KEY MACRO:
;;;;   DEFCOMPONENT-WITH-API - Define a component with auto-generated REST API
;;;;
;;;; GENERATED API:
;;;;   POST /api/{component-name}/{action-name}
;;;;   POST /api/{component-name}/state - Get/set state
;;;;   POST /api/{component-name}/render - Re-render component

(in-package :lol-web/fullstack)

;;; ============================================================================
;;; COMPONENT-API REGISTRY
;;; ============================================================================

(defparameter *api-components* (make-hash-table :test 'equal)
  "Registry of API-enabled components: name -> (component . routes)")

(defun register-api-component (name component routes)
  "Register an API-enabled component."
  (setf (gethash name *api-components*)
        (cons component routes)))

(defun find-api-component (name)
  "Find an API-enabled component by name."
  (car (gethash name *api-components*)))

(defun list-api-routes (name)
  "List all routes for an API component."
  (cdr (gethash name *api-components*)))

;;; ============================================================================
;;; ROUTE GENERATION HELPERS
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kebab-to-path (symbol)
    "Convert a kebab-case symbol to URL path segment."
    (string-downcase (substitute #\- #\_ (symbol-name symbol))))

  (defun generate-api-path (component-name action-name)
    "Generate API endpoint path."
    (format nil "/api/~A/~A"
            (kebab-to-path component-name)
            (kebab-to-path action-name)))

  (defun extract-action-name (action-spec)
    "Extract action name from action specification."
    (if (consp action-spec)
        (car action-spec)
        action-spec))

  (defun extract-action-params (action-spec)
    "Extract action parameters from action specification."
    (if (consp action-spec)
        (cadr action-spec)
        nil))

  (defun extract-action-body (action-spec)
    "Extract action body from action specification."
    (if (consp action-spec)
        (cddr action-spec)
        nil)))

;;; ============================================================================
;;; NEUTRAL RESPONSE SHAPES
;;;
;;; "Component not found" and "Forbidden" used to be distinguishable on
;;; the wire, which let a probe enumerate component IDs by status string.
;;; Both authz refusal and existence refusal now collapse to the same
;;; opaque "Not available" shape. Caller-bug refusals (bad action name,
;;; malformed args) keep distinct error strings — those leak nothing
;;; about the registry.
;;; ============================================================================

(defun %component-unavailable-response ()
  "Neutral response for both 'component not found' and 'principal not
   authorised'. Identical bytes for both arms denies the oracle."
  '((:success . nil) (:error . "Not available")))

(defun %component-unavailable-json ()
  "Encoded form of the neutral unavailable response."
  (encode-json-string (%component-unavailable-response)))

(defun %invalid-args-response ()
  "Caller-bug refusal: malformed :args, arity mismatch, or action
   handler crash. Separate from %component-unavailable so genuine
   misuse is distinguishable from authz/oracle gates."
  '((:success . nil) (:error . "Invalid arguments")))

;;; ============================================================================
;;; ACTION ARITY REGISTRY
;;;
;;; The /api/dispatch handler accepts a free-form :args list from the
;;; client; without a known arity it cannot reject an arity mismatch
;;; before applying. defcomponent-with-api registers the expected arity
;;; for each (component-name . action-keyword) pair at expansion so the
;;; dispatcher can refuse the bad shape ahead of APPLY.
;;; ============================================================================

(defparameter *action-arities* (make-hash-table :test 'equal)
  "Hash from (COMPONENT-NAME-SYMBOL . ACTION-KEYWORD) to integer arity.
   Populated at defcomponent-with-api expansion; consulted by the
   /api/dispatch handler to refuse arity mismatches before APPLY runs.")

(defun register-action-arity (component-name action-key arity)
  "Record ARITY for the (COMPONENT-NAME . ACTION-KEY) pair. Subsequent
   redefinitions overwrite (REPL reload of a defcomponent-with-api form
   updates the entry in place)."
  (setf (gethash (cons component-name action-key) *action-arities*) arity))

(defun action-arity (component-name action-key)
  "Return the recorded arity for (COMPONENT-NAME . ACTION-KEY), or NIL
   when no entry exists."
  (gethash (cons component-name action-key) *action-arities*))

;;; ============================================================================
;;; AUTHORIZATION HELPER
;;; ============================================================================

(defun %principal-owns-component-p (component-id)
  "Verify the current request's principal owns COMPONENT-ID. Returns T
   when the component has no principal-binding (public instance) or
   when the binding is EQUAL to (current-principal). Consumer chooses
   binding shape; opaque to the framework."
  (let ((binding (component-principal-binding component-id)))
    (or (null binding)
        (equal binding (lol-web/server:current-principal)))))

(defmacro %with-component-auth ((component-id) &body body)
  "Wrap BODY in with-auth and refuse with the neutral unavailable
   shape when the resolved principal does not own COMPONENT-ID. The
   auth gate runs first; an unauthenticated caller never reaches the
   ownership check. Both 'no auth' and 'wrong principal' return bytes
   identical to 'component not found' — the wire cannot distinguish."
  (let ((cid (gensym "CID")))
    `(let ((,cid ,component-id))
       (lol-web/server:with-auth ()
         (if (%principal-owns-component-p ,cid)
             (progn ,@body)
             (%component-unavailable-json))))))

(defun %extract-signed-set-state (body-json)
  "Pull the signed envelope off BODY-JSON and verify it against the
   per-request hydration key. Returns (VALUES KEY VALUE STATUS) where
   STATUS is :OK plus a keyword KEY and the payload VALUE, otherwise
   STATUS is :NO-KEY / :MISSING-TAG / :BAD-TAG / :INVALID-KEY and the
   first two values are NIL. Refuses absent envelopes and payload shapes
   the per-component handler cannot consume."
  (let ((secret-key (%request-hydration-key))
        (signed (cdr (assoc :signed body-json))))
    (multiple-value-bind (payload status)
        (verify-hydration-state signed secret-key)
      (case status
        (:ok
         (let* ((raw-key (cdr (assoc :key payload)))
                (key (and (stringp raw-key) (safe-coerce-keyword raw-key)))
                (value (cdr (assoc :value payload))))
           (if key
               (values key value :ok)
               (values nil nil :invalid-key))))
        (t (values nil nil status))))))

(defun %set-state-refusal (status)
  "Translate a verify-hydration-state STATUS into the JSON refusal body."
  (encode-json-string
    `((:success . nil)
      (:error . ,(ecase status
                   (:no-key "Hydration secret-key not configured")
                   (:missing-tag "Missing or malformed signed envelope")
                   (:bad-tag "Signed envelope failed verification")
                   (:invalid-key "Invalid or unknown :key"))))))

;;; ============================================================================
;;; DEFCOMPONENT-WITH-API MACRO
;;; ============================================================================

(defmacro! defcomponent-with-api (name (&rest props) &key state actions render public)
  "Define a component with auto-generated API endpoints.

   NAME: Component name (becomes part of API path)
   PROPS: Component properties (keyword arguments)
   STATE: List of (state-name initial-value) pairs
   ACTIONS: List of (action-name (params) &body) - generates POST routes
   RENDER: Render expression using state and props

   Generated API endpoints:
   - POST /api/{name}/{action} - For each action
   - POST /api/{name}/get-state - Get current state
   - POST /api/{name}/set-state - Set state value
   - POST /api/{name}/render - Re-render component

   Example:
     (defcomponent-with-api task-list ()
       :state ((tasks '())
               (filter :all))
       :actions ((add-task (text)
                   (push (list :id (gensym) :text text :done nil) tasks))
                 (toggle-task (id)
                   (let ((task (find id tasks :key (lambda (t) (getf t :id)))))
                     (when task (setf (getf task :done) (not (getf task :done))))))
                 (delete-task (id)
                   (setf tasks (remove id tasks :key (lambda (t) (getf t :id))))))
       :render (htm-str
                 (:ul :class \"task-list\"
                   (dolist (task tasks)
                     (htm (:li :class (if (getf task :done) \"done\" \"\")
                            (cl-who:esc (getf task :text))))))))"
  (let* ((action-names (mapcar #'extract-action-name actions))
         (component-path (kebab-to-path name))
         (routes-var (symb "*" name "-ROUTES*"))
         (wrap-auth (lambda (cid-form body)
                      (if public
                          body
                          `(%with-component-auth (,cid-form) ,body)))))
    `(progn
       ;; Define the component. principal-binding is opaque consumer data;
       ;; NIL leaves the instance reachable by any authenticated caller,
       ;; non-NIL gates dispatch on (equal binding (current-principal)).
       (defun ,name (&key (id (generate-component-id ',name))
                          principal-binding
                          ,@props)
         (let (,@(mapcar (lambda (s) `(,(car s) ,(cadr s))) state))
           (let ((,g!self
                  (pandoriclet ((id id)
                                ,@(mapcar (lambda (s) `(,(car s) ,(car s))) state)
                                ,@(mapcar (lambda (p) `(,p ,p)) props))
                    (dlambda
                      (:id () id)

                      (:render ()
                       ,render)

                      (:state (&optional key)
                       (if key
                           ;; Accept either the bare symbol or the same-name
                           ;; keyword so callers that route external strings
                           ;; through SAFE-COERCE-KEYWORD hit the same slot
                           ;; as direct Lisp callers.
                           (ecase key
                             ,@(mapcar (lambda (s)
                                         `((,(car s)
                                            ,(intern (symbol-name (car s)) :keyword))
                                           ,(car s)))
                                       state))
                           (list ,@(mapcan (lambda (s)
                                             `(,(intern (symbol-name (car s)) :keyword)
                                               ,(car s)))
                                           state))))

                      (:set-state (key value)
                       (with-components-lock
                         (ecase key
                           ,@(mapcar (lambda (s)
                                       `((,(car s)
                                          ,(intern (symbol-name (car s)) :keyword))
                                         (setf ,(car s) value)))
                                     state)))
                       value)

                      (:dispatch (action &rest args)
                       (with-components-lock
                         (ecase action
                           ,@(mapcar (lambda (act)
                                       (let ((act-name (extract-action-name act))
                                             (act-params (extract-action-params act))
                                             (act-body (extract-action-body act)))
                                         `((,act-name)
                                           (destructuring-bind ,act-params args
                                             ,@act-body))))
                                     actions))))

                      (:props ()
                       (list ,@(mapcan (lambda (p)
                                         `(,(intern (symbol-name p) :keyword) ,p))
                                       props)))

                      (:inspect ()
                       (list :id id
                             :component ',name
                             :state (list ,@(mapcan (lambda (s)
                                                      `(,(intern (symbol-name (car s)) :keyword)
                                                        ,(car s)))
                                                    state))
                             :props (list ,@(mapcan (lambda (p)
                                                      `(,(intern (symbol-name p) :keyword) ,p))
                                                    props))
                             :actions ',action-names))))))
             ;; Register so the API routes (which look up via find-component)
             ;; can locate this instance. defcomponent does the equivalent in
             ;; its :mount handler — defcomponent-with-api has no :mount, so
             ;; register inline at construction time.
             (register-component id ,g!self :principal-binding principal-binding)
             ,g!self)))

       ;; Record action arities so /api/dispatch can refuse arity
       ;; mismatches before APPLY runs. Re-evaluating the form (REPL
       ;; reload) overwrites the entry in place.
       ,@(mapcar (lambda (act)
                   (let ((act-name (extract-action-name act))
                         (act-params (extract-action-params act)))
                     `(register-action-arity
                       ',name
                       ,(intern (symbol-name act-name) :keyword)
                       ,(length act-params))))
                 actions)

       ;; Register API routes. Each handler's body is wrapped in
       ;; %with-component-auth unless :public T was supplied; the wrapper
       ;; fails closed when no auth middleware is installed and rejects
       ;; cross-owner dispatch when the component carries a binding.
       ,@(mapcar (lambda (act)
                   (let* ((act-name (extract-action-name act))
                          (act-params (extract-action-params act))
                          (api-path (generate-api-path name act-name))
                          (handler-name (symb name '- act-name '-handler))
                          (cid (gensym "CID")))
                     `(defhandler ,handler-name ,api-path
                          (:method :post :content-type "application/json")
                          ((body-json :json-body :required nil))
                        (let ((,cid (cdr (assoc :component-id body-json))))
                          ,(funcall wrap-auth cid
                              `(let ((component (find-component ,cid)))
                                 (if (null component)
                                     (%component-unavailable-json)
                                     (handler-case
                                         (let ((args (list ,@(mapcar
                                                              (lambda (p)
                                                                `(cdr (assoc
                                                                       ,(intern (symbol-name p) :keyword)
                                                                       body-json)))
                                                              act-params))))
                                           (apply component :dispatch ',act-name args)
                                           (encode-json-string
                                             (list :success t
                                                   :html (funcall component :render)
                                                   :state (funcall component :state))))
                                       (error ()
                                         (encode-json-string
                                           (%invalid-args-response)))))))))))
                 actions)

       ;; State getter route
       (defhandler ,(symb name '-get-state-handler)
           ,(format nil "/api/~A/get-state" component-path)
           (:method :post :content-type "application/json")
           ((body-json :json-body :required nil))
         (let ((,g!cid (cdr (assoc :component-id body-json))))
           ,(funcall wrap-auth g!cid
               `(let ((component (find-component ,g!cid)))
                  (if component
                      (encode-json-string
                        (list :success t :state (funcall component :state)))
                      (%component-unavailable-json))))))

       ;; State setter route. Requires an HMAC-signed envelope so a
       ;; tampered client cannot forge (key, value) writes; the legal
       ;; mint path is via lol-web/fullstack:sign-hydration-state.
       (defhandler ,(symb name '-set-state-handler)
           ,(format nil "/api/~A/set-state" component-path)
           (:method :post :content-type "application/json")
           ((body-json :json-body :required nil))
         (let ((,g!cid (cdr (assoc :component-id body-json))))
           ,(funcall wrap-auth g!cid
               `(multiple-value-bind (key value status)
                    (%extract-signed-set-state body-json)
                  (if (eq status :ok)
                      (let ((component (find-component ,g!cid)))
                        (if component
                            (progn
                              (funcall component :set-state key value)
                              (encode-json-string
                                (list :success t
                                      :html (funcall component :render)
                                      :state (funcall component :state))))
                            (%component-unavailable-json)))
                      (%set-state-refusal status))))))

       ;; Render route
       (defhandler ,(symb name '-render-handler)
           ,(format nil "/api/~A/render" component-path)
           (:method :post :content-type "application/json")
           ((body-json :json-body :required nil))
         (let ((,g!cid (cdr (assoc :component-id body-json))))
           ,(funcall wrap-auth g!cid
               `(let ((component (find-component ,g!cid)))
                  (if component
                      (encode-json-string
                        (list :success t :html (funcall component :render)))
                      (%component-unavailable-json))))))

       ;; Store route list for introspection
       (defparameter ,routes-var
         ',(cons (format nil "/api/~A/get-state" component-path)
                 (cons (format nil "/api/~A/set-state" component-path)
                       (cons (format nil "/api/~A/render" component-path)
                             (mapcar (lambda (act)
                                       (generate-api-path name (extract-action-name act)))
                                     actions)))))

       ',name)))

;;; ============================================================================
;;; CLIENT-SIDE API HELPERS
;;; ============================================================================

(defun generate-api-client-js (component-name actions)
  "Generate JavaScript client for component API."
  (parenscript:ps*
    `(defvar ,(intern (format nil "~A-API" (string-upcase component-name)))
       (create
        ,@(mapcan
           (lambda (act)
             (let ((act-name (extract-action-name act))
                   (act-params (extract-action-params act)))
               `(,(intern (kebab-to-path act-name) :keyword)
                 (lambda (component-id ,@act-params)
                   (fetch ,(generate-api-path component-name act-name)
                          (ps:create :method "POST"
                                  :headers (ps:create "Content-Type" "application/json")
                                  :body ((ps:@ -j-s-o-n stringify)
                                         (ps:create :component-id component-id
                                                 ,@(mapcan (lambda (p)
                                                             `(,(intern (kebab-to-path p) :keyword) ,p))
                                                           act-params)))))))))
           actions)
        :get-state (lambda (component-id)
                     (fetch ,(format nil "/api/~A/get-state" (kebab-to-path component-name))
                            (ps:create :method "POST"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body ((ps:@ -j-s-o-n stringify)
                                           (ps:create :component-id component-id)))))
        :set-state (lambda (component-id key value)
                     (fetch ,(format nil "/api/~A/set-state" (kebab-to-path component-name))
                            (ps:create :method "POST"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body ((ps:@ -j-s-o-n stringify)
                                           (ps:create :component-id component-id
                                                   :key key
                                                   :value value)))))
        :render (lambda (component-id)
                  (fetch ,(format nil "/api/~A/render" (kebab-to-path component-name))
                         (ps:create :method "POST"
                                 :headers (ps:create "Content-Type" "application/json")
                                 :body ((ps:@ -j-s-o-n stringify)
                                        (ps:create :component-id component-id)))))))))

(defun api-client-script-tag (component-name actions)
  "Generate script tag with API client."
  (format nil "<script>~A</script>"
          (generate-api-client-js component-name actions)))

;;; ============================================================================
;;; INTROSPECTION
;;; ============================================================================

(defun list-api-components ()
  "List all registered API components."
  (let ((components nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k components))
             *api-components*)
    components))

(defun inspect-api-component (name)
  "Inspect an API component's configuration."
  (let ((entry (gethash name *api-components*)))
    (when entry
      (list :name name
            :routes (cdr entry)))))

;;; ============================================================================
;;; BUILT-IN COMPONENT-API ROUTES
;;;
;;; Generic per-component dispatch endpoints registered at load time. These
;;; are the manually-defined siblings of the per-component routes that
;;; defcomponent-with-api auto-generates: any component placed in the
;;; component registry (by register-component / defcomponent) becomes
;;; reachable through these endpoints without further per-component wiring.
;;;
;;; Lives in :lol-web/fullstack rather than :lol-web/server because they
;;; depend on the component-system protocol (find-component, :dispatch,
;;; :set-state, :inspect, render-component) — server substrate has no
;;; concept of "component". Server stays component-agnostic; an application
;;; that doesn't load fullstack also doesn't pay for these routes.
;;; ============================================================================

(defhandler component-api-dispatch-handler "/api/dispatch"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Dispatch an action to a component. Validates :args is a list before
   APPLY, and refuses arity mismatch when the component's action arity
   is registered. Action handler crashes are caught and turned into a
   neutral 'invalid arguments' response so a worker thread never dies
   from a malformed JSON body."
  (let ((component-id (cdr (assoc :component-id body-json))))
    (%with-component-auth (component-id)
      (let* ((raw-action (cdr (assoc :action body-json)))
             (action (and (stringp raw-action) (safe-coerce-keyword raw-action)))
             (raw-args (cdr (assoc :args body-json)))
             (component (find-component component-id)))
        (cond
          ((not component) (%component-unavailable-json))
          ((not action)
           (encode-json-string
             '((:success . nil) (:error . "Invalid or unknown :action"))))
          ((not (listp raw-args))
           (encode-json-string (%invalid-args-response)))
          (t
           (let* ((inspect (funcall component :inspect))
                  (cname (getf inspect :component))
                  (expected (and cname (action-arity cname action))))
             (cond
               ((and expected (/= (length raw-args) expected))
                (encode-json-string (%invalid-args-response)))
               (t
                (handler-case
                    (progn
                      (apply #'funcall component :dispatch action raw-args)
                      (encode-json-string
                        `((:success . t)
                          (:html . ,(render-component component)))))
                  (error ()
                    (encode-json-string (%invalid-args-response)))))))))))))

(defhandler component-api-set-state-handler "/api/set-state"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Set state on a component. Requires an HMAC-signed envelope under the
   :SIGNED key carrying (:KEY ... :VALUE ...); verification reads the
   key configured on MAKE-APP via :HYDRATION-SECRET-KEY."
  (let ((component-id (cdr (assoc :component-id body-json))))
    (%with-component-auth (component-id)
      (multiple-value-bind (key value status)
          (%extract-signed-set-state body-json)
        (if (eq status :ok)
            (let ((component (find-component component-id)))
              (if component
                  (progn
                    (funcall component :set-state key value)
                    (encode-json-string
                      `((:success . t)
                        (:html . ,(render-component component)))))
                  (%component-unavailable-json)))
            (%set-state-refusal status))))))

(defhandler component-api-component-state-handler "/api/component-state"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Get component state for inspection."
  (let ((component-id (cdr (assoc :component-id body-json))))
    (%with-component-auth (component-id)
      (let ((component (find-component component-id)))
        (if component
            (encode-json-string (funcall component :inspect))
            (%component-unavailable-json))))))
