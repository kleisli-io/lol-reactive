;;;; Regression tests for opt-in cross-thread safety + per-request isolation
;;;; of the reactive dispatch primitives (src/core/signals.lisp).

(in-package :lol-web/core/test)
(in-suite :lol-web/core/test)

(test regression-with-lol-web-thread-safety-is-macro
  "with-lol-web-thread-safety is exported from :lol-web/core as a macro"
  (is (macro-function 'with-lol-web-thread-safety)))

(test regression-with-lol-web-thread-safety-serialises-rmw
  "Wrapper serialises read-modify-write on a shared signal across threads"
  (multiple-value-bind (counter set-counter) (make-signal 0)
    (let* ((per-thread 200)
           (n-threads 16)
           (expected (* per-thread n-threads))
           (threads
             (loop repeat n-threads
                   collect (bordeaux-threads:make-thread
                             (lambda ()
                               (dotimes (_ per-thread)
                                 (with-lol-web-thread-safety
                                   (funcall set-counter
                                            (1+ (funcall counter))))))))))
      (dolist (th threads) (bordeaux-threads:join-thread th))
      (is (= expected (funcall counter))
          "expected ~D increments, got ~D — wrapper failed to serialise"
          expected (funcall counter)))))

(test regression-with-reactive-context-resets-dispatch-primitives
  (let ((lol-web/core::*current-effect* 'outer)
        (lol-web/core::*current-effect-register* 'outer-register)
        (lol-web/core::*batch-depth* 42)
        (lol-web/core::*pending-effects* '(outer-pending)))
    (with-reactive-context
      (is (null lol-web/core::*current-effect*))
      (is (null lol-web/core::*current-effect-register*))
      (is (zerop lol-web/core::*batch-depth*))
      (is (null lol-web/core::*pending-effects*)))
    (is (eq 'outer lol-web/core::*current-effect*))
    (is (= 42 lol-web/core::*batch-depth*))))

(test regression-batch-isolates-depth-across-threads
  (let* ((n-threads 16)
         (per-thread 200)
         (observed (make-array n-threads :initial-element nil)))
    (let ((threads
            (loop for tid from 0 below n-threads
                  collect (let ((tid tid))
                            (bordeaux-threads:make-thread
                             (lambda ()
                               (let ((seen nil))
                                 (dotimes (_ per-thread)
                                   (with-reactive-context
                                     (batch
                                       (push lol-web/core::*batch-depth* seen))))
                                 (setf (aref observed tid) seen))))))))
      (dolist (th threads) (bordeaux-threads:join-thread th)))
    (let ((all (loop for v across observed append v)))
      (is (= (* n-threads per-thread) (length all)))
      (is (every (lambda (d) (= d 1)) all)
          "every batch must observe depth 1 inside its own dynamic extent"))))
