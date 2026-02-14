;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; LOL-REACTIVE State Management - Higher-level patterns built on signals
;;;;
;;;; - Store: Redux-like centralized state
;;;; - Evolving Component: Self-modifying behavior with hot-swap
;;;; - Component Factory: Dynamic component creation at runtime

(in-package :lol-reactive)

;;; ============================================================================
;;; STORE - Redux-like Centralized State
;;; ============================================================================

(defun make-store (initial-state)
  "Create a Redux-like store with multiple pandoric signals.

   INITIAL-STATE is an alist of (KEY . VALUE) pairs.
   Each key becomes a pandoric signal with full introspection.

   Messages:
     :get (key) - Get value (reactive)
     :set (key value) - Set value
     :reducer (action-type fn) - Register action handler
     :dispatch (action-type &rest args) - Dispatch action to reducer
     :state () - Get all state as alist
     :inspect () - Full introspection of all signals

   Example:
     (let ((store (make-store '((score . 0) (lives . 3)))))
       ;; Register reducer
       (funcall store :reducer :gain-points
         (lambda (action points)
           (declare (ignore action))
           `((score . ,(+ (funcall store :get 'score) points)))))
       ;; Dispatch action
       (funcall store :dispatch :gain-points 100)
       (funcall store :get 'score))  ; => 100"
  (let ((signals (make-hash-table :test 'eq))
        (reducers (make-hash-table :test 'eq))
        (middleware '()))
    ;; Initialize signals for each state key
    (dolist (pair initial-state)
      (setf (gethash (car pair) signals)
            (make-pandoric-signal (car pair) (cdr pair))))
    (dlambda
      ;; Get a value (reactive)
      (:get (key)
       (let ((sig (gethash key signals)))
         (when sig (funcall sig :get))))

      ;; Set a value
      (:set (key value)
       (let ((sig (gethash key signals)))
         (if sig
             (funcall sig :set value)
             ;; Create new signal if doesn't exist
             (setf (gethash key signals)
                   (make-pandoric-signal key value)))))

      ;; Register a reducer
      (:reducer (action-type fn)
       (setf (gethash action-type reducers) fn))

      ;; Dispatch an action (Redux-style)
      (:dispatch (action-type &rest payload)
       (let ((reducer (gethash action-type reducers)))
         (when reducer
           (let ((new-state (apply reducer action-type payload)))
             ;; Update signals from new state
             (dolist (pair new-state)
               (let ((sig (gethash (car pair) signals)))
                 (when sig
                   (funcall sig :set (cdr pair)))))))))

      ;; Add middleware
      (:use (middleware-fn)
       (push middleware-fn middleware))

      ;; Get all state as alist
      (:state ()
       (let ((state '()))
         (maphash (lambda (key sig)
                    (push (cons key (funcall sig :peek)) state))
                  signals)
         state))

      ;; Full introspection
      (:inspect ()
       (let ((info '()))
         (maphash (lambda (key sig)
                    (push (cons key (funcall sig :inspect)) info))
                  signals)
         info)))))

;;; ============================================================================
;;; EVOLVING COMPONENT - Self-Modifying Behavior
;;; ============================================================================

(defun make-evolving-component (name initial-state behavior)
  "Create a component that can hot-swap its own behavior at runtime.

   The component has:
   - Reactive state (pandoric signals)
   - Dispatch function (can be replaced via :evolve)
   - Full introspection
   - Behavior history with rollback

   Messages:
     :get (key) - Get state value
     :set (key value) - Set state value
     :dispatch (action &rest args) - Dispatch to current behavior
     :render () - Render using current behavior
     :evolve (new-behavior &optional description) - Hot-swap behavior
     :devolve () - Roll back to previous behavior
     :state () - Get all state as alist
     :inspect () - Full introspection

   Example:
     (let ((widget (make-evolving-component :my-widget
                     '((count . 0))
                     (lambda (action &rest args)
                       (case action
                         (:render \"<div>v1</div>\")
                         (:inc ...))))))
       (funcall widget :evolve
         (lambda (action &rest args)
           (case action
             (:render \"<div class='v2'>new!</div>\")
             ...))
         \"v2: new design\")
       (funcall widget :devolve)) ; roll back to v1"
  (let ((state-signals (make-hash-table))
        (current-behavior behavior)
        (behavior-history '())
        (render-count 0)
        (created (get-universal-time)))
    ;; Initialize state signals
    (dolist (pair initial-state)
      (setf (gethash (car pair) state-signals)
            (make-pandoric-signal (car pair) (cdr pair))))
    (dlambda
      ;; Get state value
      (:get (key)
       (let ((sig (gethash key state-signals)))
         (when sig (funcall sig :get))))

      ;; Set state value
      (:set (key value)
       (let ((sig (gethash key state-signals)))
         (if sig
             (funcall sig :set value)
             (setf (gethash key state-signals)
                   (make-pandoric-signal key value)))))

      ;; Dispatch through current behavior
      (:dispatch (action &rest args)
       (apply current-behavior action args))

      ;; Hot-swap behavior!
      (:evolve (new-behavior &optional description)
       (push (list (get-universal-time)
                   description
                   current-behavior)
             behavior-history)
       (setf current-behavior new-behavior)
       (format nil "Evolved to: ~a" (or description "new behavior")))

      ;; Roll back behavior
      (:devolve ()
       (when behavior-history
         (let ((prev (pop behavior-history)))
           (setf current-behavior (third prev))
           (format nil "Rolled back (was: ~a)" (second prev)))))

      ;; Render using current behavior
      (:render ()
       (incf render-count)
       (funcall current-behavior :render))

      ;; Get all state as alist
      (:state ()
       (let ((result '()))
         (maphash (lambda (k sig)
                    (push (cons k (funcall sig :peek)) result))
                  state-signals)
         result))

      ;; Full introspection
      (:inspect ()
       (let ((state-info '()))
         (maphash (lambda (k sig)
                    (push (cons k (funcall sig :inspect)) state-info))
                  state-signals)
         (list :name name
               :state state-info
               :render-count render-count
               :behavior-versions (1+ (length behavior-history))
               :age (- (get-universal-time) created)))))))

;;; ============================================================================
;;; COMPONENT FACTORY - Dynamic Component Creation
;;; ============================================================================

(defparameter *factory-registry* (make-hash-table :test 'equal)
  "Global registry for factory-spawned components.")

(defun make-component-factory ()
  "Create a meta-component that creates other components at runtime.

   The factory maintains:
   - Template registry (named component definitions)
   - Instance registry (spawned components)
   - Creation statistics

   Messages:
     :define-template (name initial-state behavior-generator) - Register template
     :spawn (template-name instance-id &optional state-overrides) - Create instance
     :instance (id) - Get instance by ID
     :destroy (id) - Remove instance
     :instances () - List all instance IDs
     :broadcast (action &rest args) - Send action to all instances
     :stats () - Factory statistics

   Example:
     (let ((factory (make-component-factory)))
       ;; Define template
       (funcall factory :define-template :counter
         '((count . 0))
         (lambda (id)
           (lambda (action &rest args) ...)))
       ;; Spawn instances
       (funcall factory :spawn :counter :counter-1)
       (funcall factory :spawn :counter :counter-2 '((count . 100)))
       ;; Broadcast to all
       (funcall factory :broadcast :increment))"
  (let ((templates (make-hash-table :test 'equal))
        (instances (make-hash-table :test 'equal))
        (creation-count 0))
    (dlambda
      ;; Define a component template
      (:define-template (name initial-state behavior-generator)
       (setf (gethash name templates)
             (list :initial-state initial-state
                   :behavior-generator behavior-generator))
       (format nil "Template '~a' registered" name))

      ;; Spawn an instance from a template
      (:spawn (template-name instance-id &optional state-overrides)
       (let ((template (gethash template-name templates)))
         (when template
           (incf creation-count)
           (let* ((base-state (getf template :initial-state))
                  (merged-state (append state-overrides base-state))
                  (instance (make-evolving-component
                             instance-id
                             merged-state
                             (funcall (getf template :behavior-generator)
                                      instance-id))))
             (setf (gethash instance-id instances) instance
                   (gethash instance-id *factory-registry*) instance)
             instance-id))))

      ;; Get an instance
      (:instance (id)
       (gethash id instances))

      ;; Destroy an instance
      (:destroy (id)
       (remhash id instances)
       (remhash id *factory-registry*))

      ;; List all instances
      (:instances ()
       (let ((ids '()))
         (maphash (lambda (k v)
                    (declare (ignore v))
                    (push k ids))
                  instances)
         ids))

      ;; Broadcast action to all instances
      (:broadcast (action &rest args)
       (let ((results '()))
         (maphash (lambda (id instance)
                    (push (cons id (apply instance :dispatch action args))
                          results))
                  instances)
         results))

      ;; Factory statistics
      (:stats ()
       (list :templates (hash-table-count templates)
             :instances (hash-table-count instances)
             :total-created creation-count)))))
