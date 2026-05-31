;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/CORE; Base: 10 -*-
;;;; LOL-REACTIVE Signals - Fine-grained reactive primitives
;;;;
;;;; Core reactive engine:
;;;; - Signals: Auto-tracking reactive values
;;;; - Effects: Side effects with automatic dependency tracking
;;;; - Computed: Derived values that cache and auto-update
;;;; - Batch: Group multiple updates
;;;; - Pandoric Signal: Introspectable reactive value (Let Over Lambda)
;;;;
;;;; All primitives use Let Over Lambda patterns (pandoric closures, dlambda)

(in-package :lol-web/core)

;;; ============================================================================
;;; DEPENDENCY TRACKING INFRASTRUCTURE
;;; ============================================================================

;;; Dispatch primitives are per-thread + per-request: each new thread starts
;;; with fresh bindings via bordeaux-threads:*default-special-bindings*, and
;;; WITH-REACTIVE-CONTEXT layers a fresh dynamic extent around every request.
;;; Subscribers tables and signal value cells remain shared by closure —
;;; cross-thread signal-graph sharing still needs WITH-LOL-WEB-THREAD-SAFETY.

(defparameter *current-effect* nil
  "Currently executing effect; accessing a signal during its run subscribes
   the effect to future changes.")

(defparameter *current-effect-register* nil
  "Unary function recording the subscribers tables the current effect attaches
   to, so the effect's disposer can later unsubscribe.")

(defparameter *batch-depth* 0
  "When > 0, signal updates defer their effects into *pending-effects*
   instead of running immediately.")

(defparameter *pending-effects* '()
  "Effects deferred during the active batch; drained when *batch-depth*
   returns to zero.")

(dolist (entry '((*current-effect* . nil)
                 (*current-effect-register* . nil)
                 (*batch-depth* . 0)
                 (*pending-effects* . nil)))
  (pushnew entry bordeaux-threads:*default-special-bindings*
           :key #'car :test #'eq))

(defmacro with-reactive-context (&body body)
  "Fresh dynamic extent for the dispatch primitives so one request's mutation
   cannot leak into the next on a reused thread."
  `(let ((*current-effect* nil)
         (*current-effect-register* nil)
         (*batch-depth* 0)
         (*pending-effects* '()))
     ,@body))

(defparameter *max-signal-history* 100
  "Default maximum history entries for pandoric signals.")

(defvar *thread-safety-lock*
  (bordeaux-threads:make-lock "lol-reactive thread-safety")
  "Coarse lock acquired by WITH-LOL-WEB-THREAD-SAFETY. One lock guards every
   reactive operation wrapped in the macro, so two threads cannot interleave
   their effect runs, batches, or signal updates on shared signal state.")

(defmacro with-lol-web-thread-safety (&body body)
  "Serialise BODY on *thread-safety-lock* — required when two threads share
   a signal graph, because subscribers tables and value cells are shared by
   closure capture. Single-thread use needs no wrapper."
  `(bordeaux-threads:with-lock-held (*thread-safety-lock*)
     ,@body))

(defun track-subscription (subscribers)
  "Subscribe the currently-executing effect to changes in this reactive source.
   No-op outside an effect. When the effect tracks subscriptions (via
   *current-effect-register*), also register the subscribers table with the
   effect so its disposer can unsubscribe."
  (when *current-effect*
    (setf (gethash *current-effect* subscribers) t)
    (when *current-effect-register*
      (funcall *current-effect-register* subscribers))))

;;; ============================================================================
;;; SIGNAL - Basic Reactive Value with Auto-Tracking
;;; ============================================================================

(defun make-signal (initial-value)
  "Create a reactive signal with automatic dependency tracking.

   Returns two values: (GETTER SETTER)

   The GETTER returns the current value. When called during an effect's
   execution, it automatically subscribes that effect to future changes.

   The SETTER updates the value and triggers all subscribed effects.

   Example:
     (multiple-value-bind (count set-count) (make-signal 0)
       (make-effect
         (lambda ()
           (format t \"Count: ~a~%\" (funcall count))
           nil))  ; return nil = no cleanup
       (funcall set-count 1)  ; prints \"Count: 1\"
       (funcall set-count 2)) ; prints \"Count: 2\""
  (let ((value initial-value)
        (subscribers (make-hash-table :test 'eq)))
    (values
     ;; Getter - tracks dependency when called during effect
     (lambda ()
       (track-subscription subscribers)
       value)
     ;; Setter - triggers all subscribed effects
     (lambda (new-value)
       (unless (equal value new-value)
         (setf value new-value)
         (if (> *batch-depth* 0)
             ;; Defer effects during batch
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (pushnew effect *pending-effects*))
                      subscribers)
             ;; Run immediately
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (when effect (funcall effect)))
                      subscribers)))
       value))))

;;; ============================================================================
;;; EFFECT - Side Effect with Auto-Tracking
;;; ============================================================================

(defun make-effect (fn)
  "Create a reactive effect that auto-tracks signal dependencies.

   FN is called immediately and whenever any signal it accesses changes.
   FN can return a cleanup function that runs before re-execution or disposal.

   Returns a DISPOSE function to stop the effect.

   Example:
     (multiple-value-bind (name set-name) (make-signal \"Alice\")
       (let ((dispose (make-effect
                        (lambda ()
                          (format t \"Hello, ~a!~%\" (funcall name))
                          ;; Return cleanup function (optional)
                          (lambda () (format t \"Cleaning up~%\"))))))
         (funcall set-name \"Bob\")   ; prints cleanup, then \"Hello, Bob!\"
         (funcall dispose)))          ; prints cleanup, stops tracking"
  (let ((effect-fn nil)
        (cleanup-fn nil)
        (tracked-subscribers '()))
    (flet ((register (subs)
             (pushnew subs tracked-subscribers :test #'eq))
           (drop-all-subscriptions ()
             (dolist (subs tracked-subscribers)
               (remhash effect-fn subs))
             (setf tracked-subscribers '())))
      (setf effect-fn
            (lambda ()
              ;; Run cleanup from previous execution
              (when (functionp cleanup-fn)
                (funcall cleanup-fn)
                (setf cleanup-fn nil))
              ;; Track this effect and record its subscriptions during execution
              (let ((*current-effect* effect-fn)
                    (*current-effect-register* #'register))
                (setf cleanup-fn (funcall fn)))))
      ;; Run immediately to establish initial dependencies
      (funcall effect-fn)
      ;; Return disposer — clears every subscribers table the effect appeared in
      ;; before nullifying effect-fn. Without unsubscribe a disposed effect keeps
      ;; running on every signal change because the subscriber table still keys
      ;; on the closure object.
      (lambda ()
        (when (functionp cleanup-fn)
          (funcall cleanup-fn)
          (setf cleanup-fn nil))
        (drop-all-subscriptions)
        (setf effect-fn nil)))))

;;; ============================================================================
;;; BATCH - Group Multiple Updates
;;; ============================================================================

(defmacro batch (&body body)
  "Batch multiple signal updates - effects run once at end.

   Example:
     (batch
       (funcall set-x 1)
       (funcall set-y 2)
       (funcall set-z 3))
     ; Effects depending on x, y, z run once, not three times"
  `(progn
     (incf *batch-depth*)
     (unwind-protect
          (progn ,@body)
       (decf *batch-depth*)
       (when (zerop *batch-depth*)
         (let ((effects *pending-effects*))
           (setf *pending-effects* '())
           (dolist (effect effects)
             (when effect (funcall effect))))))))

;;; ============================================================================
;;; COMPUTED - Derived Reactive Value
;;; ============================================================================

(defun make-computed (compute-fn &key (test #'equal))
  "Create a computed value that automatically tracks dependencies.

   The value is derived by calling COMPUTE-FN, which can access signals.
   The result is cached and only recomputed when dependencies change.
   Downstream effects are only notified if the computed value actually changed
   (compared using TEST, default EQUAL).

   TEST: Equality function for comparing values. Common choices:
     - EQUAL (default): Structural equality for strings, lists, numbers
     - EQUALP: Case-insensitive strings, hash-tables, arrays
     - EQ: Reference equality (like JavaScript ===)

   Returns a GETTER function.

   Example:
     (multiple-value-bind (first-name set-first) (make-signal \"John\")
       (multiple-value-bind (last-name set-last) (make-signal \"Doe\")
         (let ((full-name (make-computed
                            (lambda ()
                              (format nil \"~a ~a\"
                                      (funcall first-name)
                                      (funcall last-name))))))
           (funcall full-name)        ; => \"John Doe\"
           (funcall set-first \"Jane\")
           (funcall full-name))))     ; => \"Jane Doe\"

   Example with custom test (for hash-table results):
     (make-computed (lambda () (build-state-hash-table)) :test #'equalp)"
  (let* ((value nil)
         (subscribers (make-hash-table :test 'eq))
         ;; Capture and retain the disposer so the recompute effect can be torn
         ;; down. Previously the disposer was discarded — the internal effect
         ;; remained alive for the lifetime of every source signal it tracked.
         (dispose (make-effect
                   (lambda ()
                     (let ((new-value (funcall compute-fn)))
                       (unless (funcall test value new-value)
                         (setf value new-value)
                         (maphash (lambda (effect v)
                                    (declare (ignore v))
                                    (when effect (funcall effect)))
                                  subscribers)))
                     nil))))
    (values
     ;; Getter
     (lambda ()
       (track-subscription subscribers)
       value)
     ;; Disposer — call to stop tracking source signals.
     dispose)))

;;; ============================================================================
;;; PANDORIC SIGNAL - Introspectable Reactive Value
;;; ============================================================================

(defun make-pandoric-signal (name initial-value &key (max-history *max-signal-history*))
  "Create a signal with full introspection capabilities via dlambda.

   This is the 'Let Over Lambda' version of signals - the closure's
   internal state can be inspected and modified from outside.

   MAX-HISTORY: Maximum history entries to keep. NIL uses *MAX-SIGNAL-HISTORY*.

   Messages:
     :get () - Get value (tracks dependency)
     :set (value) - Set value (triggers effects, respects batch)
     :peek () - Get value without tracking
     :history () - Get list of previous values with timestamps
     :undo () - Restore previous value
     :inspect () - Get full introspection data
     :subscriber-count () - Number of subscribed effects

   Example:
     (let ((score (make-pandoric-signal :player-score 0)))
       (funcall score :set 100)
       (funcall score :set 200)
       (funcall score :history)  ; => ((time . 0) (time . 100))
       (funcall score :undo)     ; => 100
       (funcall score :inspect)) ; => (:name :player-score :value 100 ...)

   Example with custom history limit:
     (make-pandoric-signal :mouse-pos '(0 . 0) :max-history 10)
     (make-pandoric-signal :debug-state nil :max-history nil)"
  (let ((value initial-value)
        (subscribers (make-hash-table :test 'eq))
        (history '())
        (history-limit (or max-history *max-signal-history*))
        (created-at (get-universal-time)))
    (dlambda
      ;; Get value (reactive - tracks dependency)
      (:get ()
       (track-subscription subscribers)
       value)

      ;; Set value (triggers effects, respects batch)
      (:set (new-value)
       (unless (equal value new-value)
         (push (cons (get-universal-time) value) history)
         ;; Enforce history limit
         (when (and history-limit (> (length history) history-limit))
           (setf (cdr (nthcdr (1- history-limit) history)) nil))
         (setf value new-value)
         ;; Respect batch depth like regular signals
         (if (> *batch-depth* 0)
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (pushnew effect *pending-effects*))
                      subscribers)
             (maphash (lambda (effect v)
                        (declare (ignore v))
                        (when effect (funcall effect)))
                      subscribers)))
       value)

      ;; Peek without tracking
      (:peek () value)

      ;; Get history of previous values
      (:history () (reverse history))

      ;; Undo to previous value (also respects batch)
      (:undo ()
       (when history
         (let ((prev (pop history)))
           (setf value (cdr prev))
           (if (> *batch-depth* 0)
               (maphash (lambda (e v)
                          (declare (ignore v))
                          (pushnew e *pending-effects*))
                        subscribers)
               (maphash (lambda (e v)
                          (declare (ignore v))
                          (when e (funcall e)))
                        subscribers)))
         value))

      ;; Full introspection
      (:inspect ()
       (list :name name
             :value value
             :subscribers (hash-table-count subscribers)
             :history-length (length history)
             :history-limit history-limit
             :age (- (get-universal-time) created-at)))

      ;; Subscriber count
      (:subscriber-count ()
       (hash-table-count subscribers))

      ;; Reset to new value, clearing history (also respects batch)
      (:reset (new-value)
       (setf history '()
             value new-value)
       (if (> *batch-depth* 0)
           (maphash (lambda (e v)
                      (declare (ignore v))
                      (pushnew e *pending-effects*))
                    subscribers)
           (maphash (lambda (e v)
                      (declare (ignore v))
                      (when e (funcall e)))
                    subscribers))
       value))))
