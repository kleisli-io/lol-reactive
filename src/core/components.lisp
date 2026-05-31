;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/CORE; Base: 10 -*-
;;;; Core component system using Let Over Lambda patterns
;;;; Components are pandoric closures with reactive state

(in-package :lol-web/core)

;;; ============================================================================
;;; COMPONENT REGISTRY
;;; ============================================================================

(defparameter *components* (make-hash-table :test 'equal)
  "Registry of all component instances by ID.")

(defvar *components-lock* (bordeaux-threads:make-recursive-lock "lol-web components registry")
  "Serialises component registry reads/writes AND every component state
   mutation (:set-state / :dispatch / surgery), so a surgery op cannot
   interleave with ordinary dispatch traffic and tear a snapshot.")

(defvar *deferred-notifications* nil
  "Bound by the OUTERMOST WITH-COMPONENTS-LOCK frame to a one-element list
   whose car accumulates (component . subscribers) pairs. They are delivered
   AFTER that frame releases *COMPONENTS-LOCK*, so NOTIFY-SUBSCRIBERS never
   runs while the lock is held — even when a surgery op (holding the lock)
   nests an ordinary :set-state. NIL outside any frame.")

(defmacro! with-components-lock (&body body)
  "Hold *COMPONENTS-LOCK* across BODY so a compound read-modify-write over a
   component's registry entry or state slot is atomic with respect to
   concurrent registry operations and other mutations. The lock is recursive,
   so per-accessor acquisitions inside BODY nest harmlessly.

   Subscriber notifications queued via %NOTIFY-OR-DEFER during BODY are held
   until the outermost frame releases the lock, then delivered, so a
   subscriber that takes a second lock cannot ABBA-deadlock against the
   component lock."
  `(let* ((,g!outermost (null *deferred-notifications*))
          (*deferred-notifications* (or *deferred-notifications* (list nil))))
     (if ,g!outermost
         (multiple-value-prog1
             (bordeaux-threads:with-recursive-lock-held (*components-lock*)
               ,@body)
           (%flush-deferred-notifications))
         (bordeaux-threads:with-recursive-lock-held (*components-lock*)
           ,@body))))

(defstruct (component-entry (:conc-name %component-entry-))
  "Registry entry: component closure, opaque principal-binding gating
   ownership, and four mutable per-instance stacks that die with
   UNREGISTER-COMPONENT. ORIGINALS is the optimistic-update rollback
   store keyed by the per-element id the client invents at apply time."
  (component         nil :read-only t)
  (principal-binding nil :read-only t)
  (snapshots         nil)
  (undo-stack        nil)
  (redo-stack        nil)
  (originals         nil))

(defun register-component (id component &key principal-binding)
  "Register COMPONENT under ID. PRINCIPAL-BINDING is an opaque
   consumer-supplied value compared by EQUAL against the current
   request's principal at lookup time. NIL ⇒ no ownership check."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (setf (gethash id *components*)
          (make-component-entry :component component
                                :principal-binding principal-binding)))
  component)

(defun find-component (id)
  "Find a component closure by ID, or NIL if absent."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (%component-entry-component entry)))))

(defun component-principal-binding (id)
  "Return the opaque principal-binding stored under ID, or NIL."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (%component-entry-principal-binding entry)))))

(defun component-snapshots (id)
  "Snapshot list for ID, or NIL if unregistered."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (%component-entry-snapshots entry)))))

(defun (setf component-snapshots) (value id)
  "No-op when ID is unregistered; returns VALUE either way."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (setf (%component-entry-snapshots entry) value))
      value)))

(defun component-undo-stack (id)
  "Undo stack for ID, or NIL if unregistered."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (%component-entry-undo-stack entry)))))

(defun (setf component-undo-stack) (value id)
  "No-op when ID is unregistered; returns VALUE either way."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (setf (%component-entry-undo-stack entry) value))
      value)))

(defun component-redo-stack (id)
  "Redo stack for ID, or NIL if unregistered."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (%component-entry-redo-stack entry)))))

(defun (setf component-redo-stack) (value id)
  "No-op when ID is unregistered; returns VALUE either way."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (setf (%component-entry-redo-stack entry) value))
      value)))

(defun component-originals (id)
  "Optimistic-update originals store for ID, or NIL if unregistered."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (%component-entry-originals entry)))))

(defun (setf component-originals) (value id)
  "No-op when ID is unregistered; returns VALUE either way."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (let ((entry (gethash id *components*)))
      (when entry (setf (%component-entry-originals entry) value))
      value)))

(defun unregister-component (id)
  "Remove a component from the registry; entry slots are released."
  (bordeaux-threads:with-recursive-lock-held (*components-lock*)
    (remhash id *components*)))

;;; ============================================================================
;;; COMPONENT PROTOCOL
;;;
;;; Components respond to these messages:
;;;   :render () -> HTML string
;;;   :state (key) -> value
;;;   :set-state (key value) -> value
;;;   :dispatch (action &rest args) -> result
;;;   :subscribe (callback) -> unsubscribe-fn
;;;   :id () -> component ID
;;; ============================================================================

;; Helper functions needed at macro-expansion time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-handler (key body)
    "Find a handler form like (:render () ...) in body."
    (find-if (lambda (form)
               (and (listp form)
                    (eq (car form) key)))
             body))

  (defun generate-component-id (component-name)
    "Generate a CSPRNG-backed component ID. 128 bits of entropy from the
     OS CSPRNG so a component instance is not enumerable from its
     prefix."
    (format nil "~a-~a" component-name (random-bytes-hex 16)))

  (defun %extract-leading-declares (body)
    "Split BODY into (values DECLARES REST), peeling leading (declare ...)
     forms off the front so callers can place them in a valid declaration
     position (top of a lambda/let body, not inside a progn)."
    (loop for tail on body
          for form = (car tail)
          while (and (consp form) (eq (car form) 'declare))
          collect form into declares
          finally (return (values declares tail)))))

(defmacro! defcomponent (name (&rest state-vars) &body body)
  "Define a reactive component using pandoric closures.

   STATE-VARS are (name initial-value) pairs that become pandoric-accessible.
   BODY should contain:
     (:render () ...) - Returns HTML via cl-who
     (:dispatch (action &rest args) ...) - Handle actions
     (:on-mount () ...) - Called when component mounts (optional)
     (:on-unmount () ...) - Called when component unmounts (optional)

   Example:
   (defcomponent counter ((count 0))
     (:render ()
       (with-html-output-to-string (s)
         (:div :class (brutal-card-classes)
           (:span (str count)))))
     (:dispatch (action &rest args)
       (case action
         (:increment (incf count))
         (:decrement (decf count)))))"
  (let* ((render-form (find-handler :render body))
         (dispatch-form (find-handler :dispatch body))
         (mount-form (find-handler :on-mount body))
         (unmount-form (find-handler :on-unmount body))
         (state-names (mapcar #'car state-vars)))
    `(defun ,name (&key (id (generate-component-id ',name)) ,@state-vars)
       (let ((,g!subscribers '())
             (,g!mounted nil))
         (pandoriclet ((id id)
                       ;; Bind pandoric vars to the function params (not defaults)
                       ,@(mapcar (lambda (sv) (list (car sv) (car sv))) state-vars)
                       (subscribers ,g!subscribers)
                       (mounted ,g!mounted))
           (let ((,g!self nil))
             (setf ,g!self
                   (dlambda
                     ;; Core protocol
                     (:id () id)

                     (:render ()
                      ,(if render-form
                           `(progn ,@(cddr render-form))
                           `(error "No :render handler defined")))

                     (:state (key)
                      ;; Case labels carry both the bare symbol (direct Lisp
                      ;; callers, surgery) and the same-name keyword (callers
                      ;; routing untrusted strings through SAFE-COERCE-KEYWORD).
                      (case key
                        ,@(mapcar (lambda (name)
                                    `((,name ,(intern (symbol-name name) :keyword))
                                      ,name))
                                  state-names)
                        (t (error "Unknown state key: ~a" key))))

                     (:set-state (key value)
                      (case key
                        ,@(mapcar (lambda (name)
                                    `((,name ,(intern (symbol-name name) :keyword))
                                      (with-components-lock
                                        (setf ,name value)
                                        (%notify-or-defer ,g!self (copy-list subscribers)))
                                      value))
                                  state-names)
                        (t (error "Unknown state key: ~a" key))))

                     ;; Lift leading (declare ...) forms out of user-body so they land
                     ;; at the top of the dispatch lambda body (a valid declaration
                     ;; position) rather than inside the inner progn (which is not).
                     ,(if dispatch-form
                          (let ((user-params (cadr dispatch-form))
                                (user-body (cddr dispatch-form)))
                            (multiple-value-bind (decls rest)
                                (%extract-leading-declares user-body)
                              `(:dispatch ,user-params
                                ,@decls
                                (with-components-lock
                                  (let ((,g!result (progn ,@rest)))
                                    (%notify-or-defer ,g!self (copy-list subscribers))
                                    ,g!result)))))
                          `(:dispatch (action &rest args)
                            (error "No :dispatch handler defined")))

                     (:subscribe (callback)
                      (with-components-lock (push callback subscribers))
                      ;; Return unsubscribe function (a closure!)
                      (lambda ()
                        (with-components-lock
                          (setf subscribers (remove callback subscribers)))))

                     (:mount ()
                      (unless mounted
                        (setf mounted t)
                        (register-component id ,g!self)
                        ,(when mount-form
                           `(progn ,@(cddr mount-form)))))

                     (:unmount ()
                      (when mounted
                        (setf mounted nil)
                        (unregister-component id)
                        ,(when unmount-form
                           `(progn ,@(cddr unmount-form)))))

                     (:mounted-p () mounted)

                     ;; Debug/introspection
                     (:inspect ()
                      (list :id id
                            :state (list ,@(mapcar (lambda (name)
                                                     `(cons ',name ,name))
                                                   state-names))
                            :subscribers (length subscribers)
                            :mounted mounted))

                     (t (&rest args)
                      (error "Unknown component message: ~a" args))))
             ;; Auto-mount and return the component
             (funcall ,g!self :mount)
             ,g!self))))))

(defun notify-subscribers (component subscribers)
  "Notify all subscribers of state change."
  (dolist (callback subscribers)
    (funcall callback component)))

(defun %notify-or-defer (component subscribers)
  "Deliver a subscriber notification now, or — when called inside a
   WITH-COMPONENTS-LOCK frame — defer it to that frame's post-release flush,
   so NOTIFY-SUBSCRIBERS never fires while *COMPONENTS-LOCK* is held.
   SUBSCRIBERS must already be a caller-owned snapshot."
  (if *deferred-notifications*
      (push (cons component subscribers) (car *deferred-notifications*))
      (notify-subscribers component subscribers)))

(defun %flush-deferred-notifications ()
  "Deliver and clear the notifications queued in the current frame's holder.
   Called by the outermost WITH-COMPONENTS-LOCK after the lock is released."
  (let ((queued (nreverse (car *deferred-notifications*))))
    (setf (car *deferred-notifications*) nil)
    (dolist (entry queued)
      (notify-subscribers (car entry) (cdr entry)))))

;;; ============================================================================
;;; COMPONENT STATE ACCESS MACRO
;;; ============================================================================

(defmacro with-component-state ((&rest state-keys) component &body body)
  "Access component state with lexical bindings.
   Uses Let Over Lambda's with-pandoric under the hood."
  `(with-pandoric ,state-keys ,component
     ,@body))

;;; ============================================================================
;;; HIGHER-ORDER COMPONENTS (Let Over Lambda Style)
;;; ============================================================================

(defmacro! defhoc (name (wrapped-component &rest extra-state) &body body)
  "Define a Higher-Order Component - a component that wraps another.
   Uses pandoric access to reach into the wrapped component's state.

   Example:
   (defhoc with-loading (component (loading nil))
     (:render ()
       (if loading
           (render-loading-spinner)
           (funcall component :render)))
     (:dispatch (action &rest args)
       (case action
         (:set-loading (setf loading (car args)))
         (t (apply component :dispatch action args)))))"
  `(defun ,name (,wrapped-component &key ,@extra-state)
     (let ((,g!inner ,wrapped-component))
       (pandoriclet ((inner ,g!inner) ,@extra-state)
         (dlambda
           ,@body
           ;; Default: delegate to inner component
           (t (&rest args)
            (apply inner args)))))))

;;; ============================================================================
;;; REACTIVE STATE CONTAINER (Inspired by React's useState)
;;; ============================================================================

(defun make-reactive-state (initial-value)
  "Create a reactive state container using pandoric closures.
   Returns (getter setter subscriber) functions.

   This is the 'Let Over Lambda' pattern in its purest form:
   A closure (getter) over a let binding, with pandoric access (setter)."
  (let ((value initial-value)
        (subscribers '()))
    (values
     ;; Getter
     (lambda () value)
     ;; Setter
     (lambda (new-value)
       (setf value new-value)
       (dolist (sub subscribers)
         (funcall sub new-value))
       value)
     ;; Subscribe
     (lambda (callback)
       (push callback subscribers)
       (lambda ()
         (setf subscribers (remove callback subscribers)))))))

;;; ============================================================================
;;; ANAPHORIC COMPONENT HELPERS
;;; ============================================================================

(defmacro arender (component &body transform)
  "Anaphoric render - binds IT to the render result for transformation."
  `(let ((it (funcall ,component :render)))
     ,@transform))

(defmacro awith-state (component state-key &body body)
  "Anaphoric state access - binds IT to the state value."
  `(let ((it (funcall ,component :state ,state-key)))
     ,@body))
