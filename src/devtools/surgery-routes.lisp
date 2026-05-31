;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/DEVTOOLS; Base: 10 -*-
;;;; devtools/surgery-routes.lisp - HTTP routes backing the surgery panel UI.
;;;;
;;;; surgery-js.lisp issues fetches against /api/surgery/{state,update,
;;;; snapshot,panel,undo,redo}. The handlers in surgery.lisp implement the
;;;; behaviour (component-state-tree, surgery-set-state, capture-snapshot,
;;;; restore-snapshot, surgery-panel-html, surgery-undo, surgery-redo).
;;;; HTTP-surface eval was removed: REPL-into-component is a SLIME concern,
;;;; not an HTTP route. defhandler calls live here so route registration
;;;; follows surgery.lisp (handlers) and the extractor protocol in load order.

(in-package :lol-web/devtools)

(defun %surgery-not-found ()
  "Standard 'component not registered' response shape."
  '((:success . nil) (:error . "Component not found")))

(defun %surgery-disabled-response ()
  "Encoded refusal byte-identical to (encode-json-string (%surgery-not-found))
   so callers cannot distinguish 'surgery disabled' from 'component unknown'."
  (encode-json-string (%surgery-not-found)))

(defmacro with-surgery-gate (&body body)
  "Evaluate BODY only when (surgery-mode-p); otherwise return %surgery-disabled-response.

   Read routes (/state, /panel) use this gate, NOT WITH-SURGERY-WRITE-GATE:
   they echo component state but mutate nothing, so by design they do not
   require a CSRF token. The read exposure is bounded — surgery mode is off by
   default, %SURGERY-COMPONENT enforces the principal-ownership gate, the
   response is BOUNDED-SERIALIZE-capped, and component ids are 128-bit
   CSPRNG-unguessable — so a guessable-id state echo is not reachable on a
   default deployment. Writes use WITH-SURGERY-WRITE-GATE (which adds CSRF)."
  `(if (surgery-mode-p)
       (progn ,@body)
       (%surgery-disabled-response)))

(defmacro with-surgery-write-gate (&body body)
  "Like WITH-SURGERY-GATE but additionally self-asserts CSRF: BODY runs only
   when surgery mode is on AND the request carries a valid CSRF token
   (request-csrf-valid-p); otherwise returns %surgery-disabled-response.

   Write-capable surgery routes use this instead of WITH-SURGERY-GATE so they
   do not delegate CSRF entirely to app-level csrf-middleware — a consumer
   running :use-csrf nil with surgery enabled then cannot mutate component
   state cross-origin. The surgery panel JS already sends the token via the
   X-CSRF-Token header, so the legitimate UI is unaffected. The refusal is
   byte-identical to the disabled/not-found shape."
  `(if (and (surgery-mode-p) (lol-web/server:request-csrf-valid-p))
       (progn ,@body)
       (%surgery-disabled-response)))

(defun %surgery-principal-owns-p (component-id)
  "T when the current request's principal may operate on COMPONENT-ID: the
   component carries no principal-binding (public instance) or its binding is
   EQUAL to (current-principal). Mirrors the fullstack component-API ownership
   gate so surgery honours the same ownership contract the rest of the
   framework upholds.

   Posture: a NIL binding is owned-by-anyone by design, not a fail-open
   defect. DEFCOMPONENT instances register with no binding, so they are
   public — their isolation rests on CSPRNG-unguessable ids, the surgery-mode
   gate, and CSRF, not on this check. A consumer needing cross-principal
   isolation defines the component with DEFCOMPONENT-WITH-API and supplies a
   :principal-binding, after which this gate refuses every non-matching
   principal."
  (let ((binding (lol-web/core:component-principal-binding component-id)))
    (or (null binding)
        (equal binding (lol-web/server:current-principal)))))

(defmacro with-surgery-error-shield (&body body)
  "Wrap BODY in a HANDLER-CASE that turns any signalled ERROR into a
   wire-safe refusal carrying only the condition's class name (via
   PUBLIC-CONDITION-MESSAGE). The raw printed report never reaches the
   response — surgery's privilege level makes condition-text echo a
   credible state-leak vector."
  (let ((c (gensym "C")))
    `(handler-case (progn ,@body)
       (error (,c)
         (encode-json-string
           `((:success . nil)
             (:error . "Surgery handler failed")
             (:condition . ,(public-condition-message ,c))))))))

(defun %surgery-component (component-id)
  "Resolve a surgery POST's :component-id to a live component the current
   principal owns, or NIL. An unowned (cross-principal) component is
   indistinguishable from an absent one, so every route's not-found shape
   doubles as the cross-principal refusal — closing the surgery ownership
   gap without adding a distinct deny response that would itself be an
   ownership oracle."
  (when component-id
    (let ((component (find-component component-id)))
      (when (and component (%surgery-principal-owns-p component-id))
        component))))

(defhandler surgery-state-handler "/api/surgery/state"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Return the component-state-tree for the panel's state inspector."
  (with-surgery-gate
    (with-surgery-error-shield
      (encode-json-string
        (let* ((component-id (cdr (assoc :component-id body-json)))
               (component (%surgery-component component-id)))
          (if component
              `((:success . t)
                (:state . ,(component-state-tree component)))
              (%surgery-not-found)))))))

(defhandler surgery-update-handler "/api/surgery/update"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Apply a surgical state change. Body: {component-id, key, value}."
  (with-surgery-write-gate
    (with-surgery-error-shield
      (encode-json-string
        (let* ((component-id (cdr (assoc :component-id body-json)))
               (raw-key (cdr (assoc :key body-json)))
               (key (and (stringp raw-key) (safe-coerce-keyword raw-key)))
               (value (cdr (assoc :value body-json)))
               (component (%surgery-component component-id)))
          (cond
            ((not component) (%surgery-not-found))
            ((not key) '((:success . nil) (:error . "Missing :key in request body")))
            (t (let ((tree (surgery-set-state component-id key value)))
                 `((:success . t)
                   (:html . ,(funcall component :render))
                   (:state . ,tree))))))))))

(defhandler surgery-snapshot-handler "/api/surgery/snapshot"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Capture or restore a snapshot. Body: {component-id, action, ...}.
   :action 'capture' uses :description; :action 'restore' uses :timestamp."
  (with-surgery-write-gate
    (with-surgery-error-shield
      (encode-json-string
        (let* ((component-id (cdr (assoc :component-id body-json)))
               (action (cdr (assoc :action body-json)))
               (component (%surgery-component component-id)))
          (cond
            ((not component) (%surgery-not-found))
            ((string= action "capture")
             (let ((ts (capture-snapshot component
                                         (cdr (assoc :description body-json)))))
               `((:success . t) (:timestamp . ,ts))))
            ((string= action "restore")
             (let ((ts (cdr (assoc :timestamp body-json))))
               (if (and ts (restore-snapshot component ts))
                   `((:success . t)
                     (:html . ,(funcall component :render))
                     (:state . ,(component-state-tree component)))
                   '((:success . nil) (:error . "Snapshot not found")))))
            (t '((:success . nil) (:error . "Unknown :action — expected 'capture' or 'restore'")))))))))

(defhandler surgery-panel-handler "/api/surgery/panel"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Return the surgery panel HTML for the requested component."
  (with-surgery-gate
    (with-surgery-error-shield
      (encode-json-string
        (let* ((component-id (cdr (assoc :component-id body-json)))
               (component (%surgery-component component-id)))
          (if component
              `((:success . t)
                (:panel-html . ,(surgery-panel-html component)))
              (%surgery-not-found)))))))

(defhandler surgery-undo-handler "/api/surgery/undo"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Undo the most recent surgical change."
  (with-surgery-write-gate
    (with-surgery-error-shield
      (encode-json-string
        (let* ((component-id (cdr (assoc :component-id body-json)))
               (component (%surgery-component component-id)))
          (cond
            ((not component) (%surgery-not-found))
            ((not (can-undo-p component-id))
             '((:success . nil) (:error . "Nothing to undo")))
            (t (let ((tree (surgery-undo component-id)))
                 `((:success . t)
                   (:html . ,(funcall component :render))
                   (:state . ,tree))))))))))

(defhandler surgery-redo-handler "/api/surgery/redo"
    (:method :post :content-type "application/json")
    ((body-json :json-body :required nil))
  "Redo a previously undone change."
  (with-surgery-write-gate
    (with-surgery-error-shield
      (encode-json-string
        (let* ((component-id (cdr (assoc :component-id body-json)))
               (component (%surgery-component component-id)))
          (cond
            ((not component) (%surgery-not-found))
            ((not (can-redo-p component-id))
             '((:success . nil) (:error . "Nothing to redo")))
            (t (let ((tree (surgery-redo component-id)))
                 `((:success . t)
                   (:html . ,(funcall component :render))
                   (:state . ,tree))))))))))
