(in-package :lol-web/core/test)
(in-suite :lol-web/core/test)

;;; ============================================================================
;;; MAKE-SIGNAL
;;; ============================================================================

(test signal-basic-creation
  "Signal can be created with initial value"
  (multiple-value-bind (getter setter) (make-signal 42)
    (is (functionp getter))
    (is (functionp setter))
    (is (= 42 (funcall getter)))))

(test signal-set-value
  "Signal setter updates the value"
  (multiple-value-bind (getter setter) (make-signal 0)
    (is (= 0 (funcall getter)))
    (funcall setter 100)
    (is (= 100 (funcall getter)))
    (funcall setter -50)
    (is (= -50 (funcall getter)))))

(test signal-various-types
  "Signal can hold various Lisp types"
  (multiple-value-bind (getter setter) (make-signal "hello")
    (is (string= "hello" (funcall getter)))
    (funcall setter "world")
    (is (string= "world" (funcall getter))))
  (multiple-value-bind (getter setter) (make-signal '(1 2 3))
    (is (equal '(1 2 3) (funcall getter)))
    (funcall setter '(a b c))
    (is (equal '(a b c) (funcall getter))))
  (multiple-value-bind (getter setter) (make-signal nil)
    (is (null (funcall getter)))
    (funcall setter t)
    (is (eq t (funcall getter)))))

;;; ============================================================================
;;; MAKE-COMPUTED
;;; ============================================================================

(test computed-basic
  "Computed value derives from signal"
  (multiple-value-bind (a set-a) (make-signal 10)
    (let ((doubled (make-computed (lambda () (* 2 (funcall a))))))
      (is (= 20 (funcall doubled)))
      (funcall set-a 5)
      (is (= 10 (funcall doubled))))))

(test computed-multiple-dependencies
  "Computed can depend on multiple signals"
  (multiple-value-bind (a set-a) (make-signal 10)
    (multiple-value-bind (b set-b) (make-signal 20)
      (let ((sum (make-computed
                   (lambda () (+ (funcall a) (funcall b))))))
        (is (= 30 (funcall sum)))
        (funcall set-a 5)
        (is (= 25 (funcall sum)))
        (funcall set-b 100)
        (is (= 105 (funcall sum)))))))

(test computed-chained
  "Computed values can depend on other computed values"
  (multiple-value-bind (base set-base) (make-signal 10)
    (let* ((doubled (make-computed
                      (lambda () (* 2 (funcall base)))))
           (quadrupled (make-computed
                         (lambda () (* 2 (funcall doubled))))))
      (is (= 40 (funcall quadrupled)))
      (funcall set-base 5)
      (is (= 20 (funcall quadrupled))))))

;;; ============================================================================
;;; MAKE-EFFECT
;;; ============================================================================

(test effect-runs-on-creation
  "Effect runs once when created"
  (let ((run-count 0))
    (multiple-value-bind (val set-val) (make-signal 0)
      (declare (ignore set-val))
      (make-effect
        (lambda ()
          (funcall val)
          (incf run-count)))
      (is (= 1 run-count)))))

(test effect-runs-on-change
  "Effect runs when signal changes"
  (let ((run-count 0))
    (multiple-value-bind (val set-val) (make-signal 0)
      (make-effect
        (lambda ()
          (funcall val)
          (incf run-count)))
      (is (= 1 run-count))
      (funcall set-val 1)
      (is (= 2 run-count))
      (funcall set-val 2)
      (is (= 3 run-count)))))

;;; ============================================================================
;;; MAKE-STORE
;;; ============================================================================

(test store-basic-creation
  "Store can be created with initial state"
  (let ((store (make-store '((count . 0) (name . "test")))))
    (is (functionp store))
    (is (= 0 (funcall store :get 'count)))
    (is (string= "test" (funcall store :get 'name)))))

(test store-set-state
  "Store can set individual keys"
  (let ((store (make-store '((count . 0)))))
    (is (= 0 (funcall store :get 'count)))
    (funcall store :set 'count 42)
    (is (= 42 (funcall store :get 'count)))))

(test store-state-returns-all
  "Store :state message returns all state"
  (let ((store (make-store '((a . 1) (b . 2)))))
    (let ((state (funcall store :state)))
      (is (listp state))
      (is (= 1 (cdr (assoc 'a state))))
      (is (= 2 (cdr (assoc 'b state)))))))

(test store-inspect
  "Store :inspect returns detailed info"
  (let ((store (make-store '((x . 100)))))
    (let ((inspection (funcall store :inspect)))
      (is (listp inspection)))))

(test regression-pandoric-signal-nil-history-uses-default-limit
  "Passing :max-history NIL does not create an unbounded history."
  (let ((lol-web/core::*max-signal-history* 3))
    (let ((signal (make-pandoric-signal :bounded nil :max-history nil)))
      (dotimes (i 10)
        (funcall signal :set i))
      (is (= 3 (length (funcall signal :history))))
      (is (= 3 (getf (funcall signal :inspect) :history-limit))))))
