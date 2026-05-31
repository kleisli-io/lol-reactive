;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/DEVTOOLS; Base: 10 -*-
;;;; Component Surgery - X-ray inspection and live modification of pandoric closures
;;;;
;;;; This is what makes LOL-REACTIVE unique: the ability to reach into any
;;;; component's closure, inspect its state, modify values, and even hotswap
;;;; behavior - all while the component is running.

(in-package :lol-web/devtools)

;;; ============================================================================
;;; SNAPSHOT / UNDO / REDO
;;;
;;; Snapshot, undo, and redo stacks live on the component's registry entry
;;; (see :lol-web/core's COMPONENT-SNAPSHOTS / COMPONENT-UNDO-STACK /
;;; COMPONENT-REDO-STACK); they die with UNREGISTER-COMPONENT. Each stack
;;; is FIFO-capped per the corresponding *MAX-* defparameter below.
;;; Operations on unregistered components are silent no-ops returning NIL.
;;; ============================================================================

(defparameter *max-snapshots-per-component* 20
  "Per-component snapshot cap; FIFO eviction past this depth.")

(defparameter *max-undo-depth* 1000
  "Per-component undo cap; FIFO eviction past this depth.")

(defparameter *max-redo-depth* 1000
  "Per-component redo cap; FIFO eviction past this depth.")

(defun %trim-stack (stack cap)
  "Drop oldest entries past CAP. STACK is most-recent-first."
  (if (and cap (> (length stack) cap))
      (subseq stack 0 cap)
      stack))

(defun normalize-state-pairs (pairs)
  "Normalize state PAIRS to alist form ((key . value) ...).

   Components produced by DEFCOMPONENT (core) report :state as an alist of cons
   pairs; components produced by DEFCOMPONENT-WITH-API (fullstack) report :state
   as a flat plist of alternating keyword/value. Surgery code consumes both via
   this normalizer."
  (cond
    ((null pairs) nil)
    ((consp (first pairs)) pairs)
    (t (loop for (key value) on pairs by #'cddr
             collect (cons key value)))))

(defun capture-snapshot (component &optional description)
  "Capture current state as a restorable snapshot. Returns the
   timestamp ID, or NIL when COMPONENT is not registered. The
   read-modify-write over the snapshot stack runs under the component
   registry lock, so a concurrent capture or surgery mutation cannot
   drop or duplicate a snapshot frame."
  (lol-web/core:with-components-lock
    (let* ((component-id (funcall component :id))
           (entry (gethash component-id lol-web/core:*components*)))
      (when entry
        (let* ((state (funcall component :inspect))
               (timestamp (get-universal-time))
               (snapshot (list :timestamp timestamp
                               :description (or description "Manual snapshot")
                               :state (getf state :state))))
          (setf (lol-web/core:component-snapshots component-id)
                (%trim-stack (cons snapshot
                                   (lol-web/core:component-snapshots component-id))
                             *max-snapshots-per-component*))
          timestamp)))))

(defun list-snapshots (component)
  "List all snapshots for COMPONENT (closure or string ID)."
  (let ((component-id (if (stringp component)
                          component
                          (funcall component :id))))
    (lol-web/core:component-snapshots component-id)))

(defun find-snapshot (component timestamp)
  "Find a specific snapshot by timestamp."
  (let ((snapshots (list-snapshots component)))
    (find timestamp snapshots :key (lambda (s) (getf s :timestamp)))))

(defun restore-snapshot (component timestamp)
  "Restore component to a previous snapshot state. The lookup and the
   per-variable updates run under the component registry lock so a
   concurrent surgery write, undo, or redo cannot interleave and leave the
   component half-restored."
  (lol-web/core:with-components-lock
    (let ((snapshot (find-snapshot component timestamp)))
      (when snapshot
        (dolist (pair (normalize-state-pairs (getf snapshot :state)))
          (funcall component :set-state (car pair) (cdr pair)))
        t))))

(defun clear-snapshots (component)
  "Clear all snapshots for COMPONENT (closure or string ID)."
  (let ((component-id (if (stringp component)
                          component
                          (funcall component :id))))
    (setf (lol-web/core:component-snapshots component-id) nil)))

;;; ============================================================================
;;; STATE TREE EXTRACTION
;;;
;;; Convert pandoric closure state to JSON-serializable format for the UI.
;;; ============================================================================

(defun component-state-tree (component)
  "Extract complete state tree from a component for JSON serialization."
  (let ((inspection (funcall component :inspect)))
    `((:id . ,(getf inspection :id))
      (:mounted . ,(getf inspection :mounted))
      (:subscribers . ,(getf inspection :subscribers))
      (:state . ,(mapcar (lambda (pair)
                           `((:key . ,(car pair))
                             (:value . ,(format-value-for-json (cdr pair)))
                             (:type . ,(type-of-value (cdr pair)))))
                         (normalize-state-pairs (getf inspection :state)))))))

(defun format-value-for-json (value)
  "Format a Lisp value into a bounded, cycle-safe JSON-serializable tree.
   Delegates to BOUNDED-SERIALIZE so a deep, wide, or cyclic component state
   cannot exhaust memory or loop when the surgery panel inspects it."
  (bounded-serialize value))

(defun type-of-value (value)
  "Return a string describing the type for UI display."
  (typecase value
    (null "nil")
    ((eql t) "boolean")
    (integer "integer")
    (float "float")
    (string "string")
    (keyword "keyword")
    (symbol "symbol")
    (cons "list")
    (hash-table "hash-table")
    (function "function")
    (t (format nil "~a" (type-of value)))))

;;; ============================================================================
;;; PUBLIC CONDITION FORMATTER
;;;
;;; Surgery handlers can raise arbitrary conditions; the wire response must
;;; not echo the condition's printed report because that can leak internal
;;; paths, secrets, and stack-frame details to whoever can flip surgery
;;; mode. PUBLIC-CONDITION-MESSAGE returns just the class name unless the
;;; class is whitelisted in *PUBLIC-CONDITION-ACCESSORS*.
;;; ============================================================================

(defparameter *public-condition-accessors* nil
  "Alist of (CONDITION-CLASS-NAME . (ACCESSOR-FN ...)). Each ACCESSOR-FN
   receives the condition and must return a value that is itself safe to
   echo (no FORMAT calls over the condition). Default empty: only the
   class name is returned. Extend at consumer discretion when a specific
   condition class has known-safe accessors.")

(defun public-condition-message (c)
  "Render condition C as a wire-safe alist. Always includes (:CLASS ...);
   includes (:FIELDS ...) only when C's class is whitelisted in
   *PUBLIC-CONDITION-ACCESSORS*. Never invokes the condition's report
   function and never reads slots that are not on the whitelist."
  (let* ((class-name (class-name (class-of c)))
         (accessors (cdr (assoc class-name *public-condition-accessors*))))
    (if accessors
        (list :class class-name
              :fields (mapcar (lambda (acc) (funcall acc c)) accessors))
        (list :class class-name))))

;;; ============================================================================
;;; SURGERY OPERATIONS
;;;
;;; The core "x-ray" functionality - reaching into closures and modifying them.
;;; ============================================================================

(defun surgery-get-state (component-id key)
  "Get a specific state value from a component."
  (let ((component (find-component component-id)))
    (when component
      (funcall component :state key))))

(defun surgery-set-state (component-id key value)
  "Set a specific state value in a component.
   This is the 'magic' - directly modifying closure state. The snapshot
   capture and the state mutation run as one operation under the component
   registry lock — the same lock SURGERY-UNDO/SURGERY-REDO hold — so a
   concurrent undo, redo, or surgery write cannot interleave between the
   pre-mutation snapshot and the mutation and tear the history."
  (lol-web/core:with-components-lock
    (let ((component (find-component component-id)))
      (when component
        (capture-snapshot component "Before surgery")
        (funcall component :set-state key value)
        (component-state-tree component)))))

(defun surgery-dispatch (component-id action &rest args)
  "Dispatch an action to a component under the registry lock, capturing a
   restorable snapshot first so the change is undoable and cannot interleave
   with a concurrent surgery write — mirroring SURGERY-SET-STATE. Returns the
   updated state tree, or NIL when COMPONENT-ID is not registered."
  (lol-web/core:with-components-lock
    (let ((component (find-component component-id)))
      (when component
        (capture-snapshot component "Before surgery")
        (apply component :dispatch action args)
        (component-state-tree component)))))

;;; ============================================================================
;;; SURGERY X-RAY COMPONENT
;;;
;;; A meta-component that wraps other components and provides x-ray UI.
;;; ============================================================================

(defun xray-wrapper-html (component)
  "Generate wrapper HTML that adds x-ray functionality to a component."
  (let ((id (funcall component :id))
        (inner-html (funcall component :render)))
    (cl-who:with-html-output-to-string (s)
      (:div :class "xray-wrapper group relative"
            :data-component-id id
            :data-xray-enabled "true"
        ;; X-ray toggle button (appears on hover)
        (:button :class "xray-toggle absolute top-2 right-2 opacity-0 group-hover:opacity-100 transition-opacity bg-brutal-accent text-brutal-bg px-2 py-1 text-xs font-bold border-2 border-brutal-bg shadow-brutal-sm z-50"
                 :onclick (parenscript:ps* `(funcall toggle-xray ,id))
          "X-RAY")
        ;; Component content
        (:div :class "xray-content"
          (cl-who:str inner-html))))))

(defun %surgery-csrf-meta-tag ()
  "Emit `<meta name=\"csrf-token\" content=\"...\">` for the current
   session's CSRF token. Returns the empty string when no token is
   resolvable (no session, csrf middleware not installed). The runtime
   JS reads this tag at fetch time so token rotation propagates without
   a panel re-render."
  (let ((token (ignore-errors (lol-web/server:get-csrf-token))))
    (if (and token (stringp token))
        (format nil "<meta name=\"csrf-token\" content=\"~A\">"
                (lol-web/escape:escape-attribute token))
        "")))

(defun surgery-panel-html (component)
  "Generate the surgery panel HTML for a component."
  (let* ((id (funcall component :id))
         (state-tree (component-state-tree component))
         (snapshots (list-snapshots id)))
    (cl-who:with-html-output-to-string (s)
      (cl-who:str (%surgery-csrf-meta-tag))
      (:div :class "surgery-panel fixed right-0 top-0 h-full w-96 bg-brutal-surface border-l-4 border-brutal-accent p-4 overflow-y-auto z-50 transform translate-x-full transition-transform"
            :id (format nil "surgery-panel-~a" id)
            :data-for-component id
        ;; Header
        (:div :class "flex justify-between items-center mb-4 pb-4 border-b-2 border-brutal-muted"
          (:h3 :class "text-brutal-primary font-bold"
            "(SURGERY)")
          (:button :class "text-brutal-accent hover:text-brutal-error"
                   :onclick (parenscript:ps* `(funcall close-xray ,id))
            "CLOSE"))

        ;; Component ID
        (:div :class "mb-4 text-brutal-muted text-sm"
          (:span :class "text-brutal-secondary" "ID: ")
          (cl-who:esc (princ-to-string id)))

        ;; State Inspector
        (:div :class "mb-6"
          (:h4 :class "text-brutal-secondary font-bold mb-2" "STATE")
          (:div :class "bg-brutal-bg p-3 border-2 border-brutal-primary font-mono text-sm"
            (dolist (var (cdr (assoc :state state-tree)))
              (let ((key (cdr (assoc :key var)))
                    (value (cdr (assoc :value var)))
                    (vtype (cdr (assoc :type var))))
                (cl-who:htm
                 (:div :class "flex justify-between items-center py-1 border-b border-brutal-surface"
                   (:span :class "text-brutal-accent" (cl-who:esc (princ-to-string key)))
                   (:span :class "text-brutal-text cursor-pointer hover:bg-brutal-surface px-2"
                          :onclick (parenscript:ps* `(funcall edit-state ,id ,key))
                          :data-state-key key
                     (cl-who:esc (format nil "~a" value)))
                   (:span :class "text-brutal-muted text-xs" (cl-who:esc (princ-to-string vtype)))))))))

        ;; Snapshots
        (:div :class "mb-6"
          (:h4 :class "text-brutal-secondary font-bold mb-2"
            "SNAPSHOTS "
            (:span :class "text-brutal-muted text-sm"
              (cl-who:fmt "(~a)" (length snapshots))))
          (:button :class "brutal-btn bg-brutal-primary text-brutal-bg px-4 py-2 text-xs mb-2 w-full"
                   :onclick (parenscript:ps* `(funcall capture-snapshot ,id))
            "CAPTURE")
          (:div :class "space-y-2 max-h-40 overflow-y-auto"
            (dolist (snap (subseq snapshots 0 (min 5 (length snapshots))))
              (cl-who:htm
               (:div :class "flex justify-between items-center bg-brutal-bg p-2 text-xs border border-brutal-muted"
                 (:span :class "text-brutal-muted"
                   (cl-who:str (format-timestamp (getf snap :timestamp))))
                 (:button :class "text-brutal-accent hover:text-brutal-primary"
                          :onclick (let ((timestamp (getf snap :timestamp)))
                                     (parenscript:ps* `(funcall restore-snapshot ,id ,timestamp)))
                   "RESTORE"))))))

        ;; Actions
        (:div
          (:h4 :class "text-brutal-secondary font-bold mb-2" "ACTIONS")
          (:div :class "grid grid-cols-2 gap-2"
            (:button :class "brutal-btn bg-brutal-surface-el text-brutal-text px-2 py-2 text-xs border-2 border-brutal-muted"
                     :onclick (parenscript:ps* `(funcall dispatch ,id ":render"))
              "RE-RENDER")
            (:button :class "brutal-btn bg-brutal-surface-el text-brutal-text px-2 py-2 text-xs border-2 border-brutal-muted"
                     :onclick (parenscript:ps* `(funcall inspect-component ,id))
              "INSPECT")))))))

(defun format-timestamp (universal-time)
  "Format a universal time for display."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore year month day))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

;;; ============================================================================
;;; SURGERY MODE
;;;
;;; surgery-middleware let-binds *surgery-mode* and
;;; lol-web/html:*component-render-hook* per request so the toggle is
;;; thread-local inside any middleware-gated request body. The
;;; enable/disable pair stays usable from REPL/test (no middleware).
;;; ============================================================================

(defparameter *surgery-mode* nil
  "Default surgery (x-ray) state read where no surgery-middleware
   binding shadows it.")

(defparameter *allow-global-surgery-enable* nil
  "Global enable-surgery-mode opt-in. Prefer surgery-middleware.")

(defparameter *surgery-production-env-var* "LOL_WEB_PRODUCTION"
  "Name of the environment variable whose presence makes
   ENABLE-SURGERY-MODE refuse. Guards only the image-global enable — the
   per-request SURGERY-MIDDLEWARE path is unaffected.")

(defparameter *surgery-enable-audit-hook* nil
  "When non-NIL, a function of no arguments funcalled whenever
   ENABLE-SURGERY-MODE installs the global render hook. A production image
   sets this to log/alert so an accidental global enable is observable
   rather than silent. SURGERY-MIDDLEWARE does not fire it.")

(defun %surgery-production-p ()
  "True iff the environment variable named by *SURGERY-PRODUCTION-ENV-VAR*
   is set to a non-empty value."
  (let ((v (uiop:getenv *surgery-production-env-var*)))
    (and v (plusp (length v)))))

(defun enable-surgery-mode ()
  "Set the *surgery-mode* default to T and install xray-wrapper-html as the
   global *component-render-hook* — leaking per-component closure state into
   every rendered response process-wide. Shadowed inside surgery-middleware.

   Refuses when *ALLOW-GLOBAL-SURGERY-ENABLE* is NIL, and refuses outright
   when the production env var (*SURGERY-PRODUCTION-ENV-VAR*) is set, so a
   stray REPL enable cannot turn on the x-ray surface in a production image.
   Fires *SURGERY-ENABLE-AUDIT-HOOK* on success."
  (unless *allow-global-surgery-enable*
    (error "enable-surgery-mode requires *allow-global-surgery-enable* to be T"))
  (when (%surgery-production-p)
    (error "enable-surgery-mode refused: ~A is set. Global surgery exposes ~
            component closure state in every response; use surgery-middleware ~
            for per-request, dynamically-scoped enablement instead."
           *surgery-production-env-var*))
  (setf *surgery-mode* t)
  (setf lol-web/html:*component-render-hook* #'xray-wrapper-html)
  (when *surgery-enable-audit-hook*
    (funcall *surgery-enable-audit-hook*))
  t)

(defun disable-surgery-mode ()
  "Counterpart to enable-surgery-mode: clear both globals."
  (setf *surgery-mode* nil)
  (setf lol-web/html:*component-render-hook* nil))

(defun surgery-mode-p ()
  "T iff surgery mode is active in the current dynamic extent."
  *surgery-mode*)

(defun %default-surgery-decide (env)
  "Default surgery-middleware decision: T iff session holds
   :lol-web/surgery-mode -> T. NIL when no session is present."
  (let ((session (getf env :lack.session)))
    (when (hash-table-p session)
      (eq t (gethash :lol-web/surgery-mode session)))))

(defun surgery-middleware (app &key (decide #'%default-surgery-decide))
  "Lack middleware that scopes *surgery-mode* and
   lol-web/html:*component-render-hook* per request via CL dynamic
   binding, so the toggle cannot leak between concurrent requests.

   DECIDE is a one-argument function over the request env; T enables
   surgery for this request, NIL disables. Default reads :lack.session
   for an explicit :lol-web/surgery-mode entry, so the middleware must
   sit downstream of session middleware in the Lack chain. The render
   hook is bound to xray-wrapper-html when DECIDE returns T, NIL
   otherwise — neither global is consulted inside the request body."
  (lambda (env)
    (let ((wants (and decide (funcall decide env))))
      (let ((*surgery-mode*                       (and wants t))
            (lol-web/html:*component-render-hook* (when wants
                                                    #'xray-wrapper-html)))
        (funcall app env)))))

;;; ============================================================================
;;; COMPONENT METADATA
;;;
;;; Track component types for behavior presets and documentation.
;;; ============================================================================

(defparameter *component-metadata* (make-hash-table :test 'equal)
  "Component ID -> metadata about the component.")

(defun register-component-metadata (id metadata)
  "Store metadata about a component for surgery UI."
  (setf (gethash id *component-metadata*) metadata))

(defun get-component-metadata (id)
  "Get metadata for a component."
  (gethash id *component-metadata*))

;;; ============================================================================
;;; UNDO/REDO
;;; ============================================================================

(defun push-undo (component)
  "Push current state onto the undo stack and clear redo. No-op when
   COMPONENT is not registered. The read-modify-write over the undo and
   redo stacks runs under the component registry lock, matching
   SURGERY-UNDO/SURGERY-REDO so a frame cannot be lost to an interleave."
  (lol-web/core:with-components-lock
    (let* ((id (funcall component :id))
           (entry (gethash id lol-web/core:*components*)))
      (when entry
        (let ((state (funcall component :inspect)))
          (setf (lol-web/core:component-undo-stack id)
                (%trim-stack (cons state (lol-web/core:component-undo-stack id))
                             *max-undo-depth*))
          (setf (lol-web/core:component-redo-stack id) nil))))))

(defun can-undo-p (component-id)
  "Check if undo is available."
  (not (null (lol-web/core:component-undo-stack component-id))))

(defun can-redo-p (component-id)
  "Check if redo is available."
  (not (null (lol-web/core:component-redo-stack component-id))))

(defun surgery-undo (component-id)
  "Undo the last change to a component. The read-modify-write across the
   undo and redo stacks runs under *COMPONENTS-LOCK* so two concurrent undos
   for the same ID cannot interleave and lose or duplicate a history frame."
  (lol-web/core:with-components-lock
    (let ((component (find-component component-id)))
      (when (and component (can-undo-p component-id))
        (let* ((current (funcall component :inspect))
               (undo-stack (lol-web/core:component-undo-stack component-id))
               (prev-state (first undo-stack)))
          (setf (lol-web/core:component-redo-stack component-id)
                (%trim-stack (cons current
                                   (lol-web/core:component-redo-stack component-id))
                             *max-redo-depth*))
          (setf (lol-web/core:component-undo-stack component-id) (rest undo-stack))
          (dolist (pair (normalize-state-pairs (getf prev-state :state)))
            (funcall component :set-state (car pair) (cdr pair))))
        (component-state-tree component)))))

(defun surgery-redo (component-id)
  "Redo a previously undone change. The read-modify-write runs under
   *COMPONENTS-LOCK* for the same atomicity reason as SURGERY-UNDO."
  (lol-web/core:with-components-lock
    (let ((component (find-component component-id)))
      (when (and component (can-redo-p component-id))
        (let* ((current (funcall component :inspect))
               (redo-stack (lol-web/core:component-redo-stack component-id))
               (next-state (first redo-stack)))
          (setf (lol-web/core:component-undo-stack component-id)
                (%trim-stack (cons current
                                   (lol-web/core:component-undo-stack component-id))
                             *max-undo-depth*))
          (setf (lol-web/core:component-redo-stack component-id) (rest redo-stack))
          (dolist (pair (normalize-state-pairs (getf next-state :state)))
            (funcall component :set-state (car pair) (cdr pair))))
        (component-state-tree component)))))
