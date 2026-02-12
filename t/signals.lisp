;;;; LOL-REACTIVE Test Suite - Signals
;;;;
;;;; Tests for make-signal, make-computed, make-effect, batch, and make-store.

(in-package :lol-reactive.tests)
(in-suite :signals)

;;; ============================================================================
;;; MAKE-SIGNAL TESTS
;;; ============================================================================

(test signal-basic-creation
  "Signal can be created with initial value"
  (multiple-value-bind (getter setter) (lol-reactive:make-signal 42)
    (is (functionp getter))
    (is (functionp setter))
    (is (= 42 (funcall getter)))))

(test signal-set-value
  "Signal setter updates the value"
  (multiple-value-bind (getter setter) (lol-reactive:make-signal 0)
    (is (= 0 (funcall getter)))
    (funcall setter 100)
    (is (= 100 (funcall getter)))
    (funcall setter -50)
    (is (= -50 (funcall getter)))))

(test signal-various-types
  "Signal can hold various Lisp types"
  ;; String
  (multiple-value-bind (getter setter) (lol-reactive:make-signal "hello")
    (is (string= "hello" (funcall getter)))
    (funcall setter "world")
    (is (string= "world" (funcall getter))))
  ;; List
  (multiple-value-bind (getter setter) (lol-reactive:make-signal '(1 2 3))
    (is (equal '(1 2 3) (funcall getter)))
    (funcall setter '(a b c))
    (is (equal '(a b c) (funcall getter))))
  ;; NIL
  (multiple-value-bind (getter setter) (lol-reactive:make-signal nil)
    (is (null (funcall getter)))
    (funcall setter t)
    (is (eq t (funcall getter)))))

;;; ============================================================================
;;; MAKE-COMPUTED TESTS
;;; ============================================================================

(test computed-basic
  "Computed value derives from signal"
  (multiple-value-bind (a set-a) (lol-reactive:make-signal 10)
    (let ((doubled (lol-reactive:make-computed (lambda () (* 2 (funcall a))))))
      (is (= 20 (funcall doubled)))
      (funcall set-a 5)
      (is (= 10 (funcall doubled))))))

(test computed-multiple-dependencies
  "Computed can depend on multiple signals"
  (multiple-value-bind (a set-a) (lol-reactive:make-signal 10)
    (multiple-value-bind (b set-b) (lol-reactive:make-signal 20)
      (let ((sum (lol-reactive:make-computed
                   (lambda () (+ (funcall a) (funcall b))))))
        (is (= 30 (funcall sum)))
        (funcall set-a 5)
        (is (= 25 (funcall sum)))
        (funcall set-b 100)
        (is (= 105 (funcall sum)))))))

(test computed-chained
  "Computed values can depend on other computed values"
  (multiple-value-bind (base set-base) (lol-reactive:make-signal 10)
    (let* ((doubled (lol-reactive:make-computed
                      (lambda () (* 2 (funcall base)))))
           (quadrupled (lol-reactive:make-computed
                         (lambda () (* 2 (funcall doubled))))))
      (is (= 40 (funcall quadrupled)))
      (funcall set-base 5)
      (is (= 20 (funcall quadrupled))))))

;;; ============================================================================
;;; MAKE-EFFECT TESTS
;;; ============================================================================

(test effect-runs-on-creation
  "Effect runs once when created"
  (let ((run-count 0))
    (multiple-value-bind (val set-val) (lol-reactive:make-signal 0)
      (declare (ignore set-val))
      (lol-reactive:make-effect
        (lambda ()
          (funcall val)  ; Track dependency
          (incf run-count)))
      (is (= 1 run-count)))))

(test effect-runs-on-change
  "Effect runs when signal changes"
  (let ((run-count 0))
    (multiple-value-bind (val set-val) (lol-reactive:make-signal 0)
      (lol-reactive:make-effect
        (lambda ()
          (funcall val)  ; Track dependency
          (incf run-count)))
      (is (= 1 run-count))  ; Initial run
      (funcall set-val 1)
      (is (= 2 run-count))  ; After first change
      (funcall set-val 2)
      (is (= 3 run-count))))) ; After second change

;;; ============================================================================
;;; MAKE-STORE TESTS
;;; ============================================================================

(test store-basic-creation
  "Store can be created with initial state"
  (let ((store (lol-reactive:make-store '((count . 0) (name . "test")))))
    (is (functionp store))
    (is (= 0 (funcall store :get 'count)))
    (is (string= "test" (funcall store :get 'name)))))

(test store-set-state
  "Store can set individual keys"
  (let ((store (lol-reactive:make-store '((count . 0)))))
    (is (= 0 (funcall store :get 'count)))
    (funcall store :set 'count 42)
    (is (= 42 (funcall store :get 'count)))))

(test store-state-returns-all
  "Store :state message returns all state"
  (let ((store (lol-reactive:make-store '((a . 1) (b . 2)))))
    (let ((state (funcall store :state)))
      (is (listp state))
      (is (= 1 (cdr (assoc 'a state))))
      (is (= 2 (cdr (assoc 'b state)))))))

(test store-inspect
  "Store :inspect returns detailed info"
  (let ((store (lol-reactive:make-store '((x . 100)))))
    (let ((inspection (funcall store :inspect)))
      (is (listp inspection)))))
