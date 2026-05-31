(in-package :lol-web/core/test)
(in-suite :lol-web/core/test)

(test component-register-and-find
  "Components can be registered and found by ID"
  (let ((test-component (lambda () "test")))
    (register-component "test-comp-1" test-component)
    (is (eq test-component (find-component "test-comp-1")))
    (unregister-component "test-comp-1")
    (is (null (find-component "test-comp-1")))))

(test component-find-nonexistent
  "Finding nonexistent component returns NIL"
  (is (null (find-component "nonexistent-comp-xyz"))))

(test defcomponent-creates-function
  "defcomponent macro is available"
  (is (fboundp 'defcomponent)))

(test generate-component-id-unique-sequential
  "generate-component-id returns distinct IDs in sequence (atomic-incf monotonic)"
  (let* ((n 200)
         (ids (loop repeat n
                    collect (lol-web/core::generate-component-id 'test-comp))))
    (is (= n (length ids)))
    (is (= n (length (remove-duplicates ids :test #'string=)))
        "duplicate IDs: counter is not monotonic")))

(test generate-component-id-unique-concurrent
  "generate-component-id stays unique across threads (atomic-incf race-free)"
  (let* ((n-threads 8)
         (per-thread 250)
         (ids-by-thread (make-array n-threads :initial-element nil))
         (threads
           (loop for tid from 0 below n-threads
                 collect (let ((tid tid))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (setf (aref ids-by-thread tid)
                                    (loop repeat per-thread
                                          collect (lol-web/core::generate-component-id 'race-comp)))))))))
    (mapc #'bordeaux-threads:join-thread threads)
    (let* ((all-ids (loop for v across ids-by-thread append v))
           (unique  (remove-duplicates all-ids :test #'string=)))
      (is (= (* n-threads per-thread) (length all-ids)))
      (is (= (length all-ids) (length unique))
          "concurrent generation produced colliding IDs"))))

(defcomponent declare-ignore-counter ((count 0))
  (:render () (princ-to-string count))
  (:dispatch (action &rest args)
    (declare (ignore args))
    (case action (:inc (incf count)) (:dec (decf count)))))

(test defcomponent-dispatch-accepts-leading-declares
  (let ((c (declare-ignore-counter :id "regression-declare-1" :count 5)))
    (is (string= "5" (funcall c :render)))
    (is (= 6 (funcall c :dispatch :inc)))
    (is (= 5 (funcall c :dispatch :dec :spurious-extra-arg)))
    (is (string= "5" (funcall c :render)))
    (unregister-component "regression-declare-1")))

(test regression-component-registry-concurrent-register-find-unregister
  "Concurrent registry operations stay coherent under the registry lock."
  (let* ((n-threads 8)
         (per-thread 80)
         (failures nil)
         (failure-lock (bordeaux-threads:make-lock "component-registry-test-failures"))
         (threads
           (loop for tid from 0 below n-threads
                 collect (let ((tid tid))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (loop for i from 0 below per-thread
                                    for id = (format nil "component-lock-~D-~D" tid i)
                                    for component = (lambda () id)
                                    do (handler-case
                                           (progn
                                             (register-component id component
                                                                 :principal-binding tid)
                                             (unless (eq component (find-component id))
                                               (error "component lookup mismatch"))
                                             (unless (eql tid (component-principal-binding id))
                                               (error "principal binding mismatch"))
                                             (unregister-component id)
                                             (when (find-component id)
                                               (error "component survived unregister")))
                                         (error (e)
                                           (bordeaux-threads:with-lock-held (failure-lock)
                                             (push e failures)))))))))))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (null failures) "registry failures: ~S" failures)))

(test regression-context-registry-concurrent-register-and-list
  "Context registry reads and writes run under a recursive lock."
  (let* ((n-threads 6)
         (per-thread 50)
         (failures nil)
         (failure-lock (bordeaux-threads:make-lock "context-registry-test-failures"))
         (threads
           (loop for tid from 0 below n-threads
                 collect (let ((tid tid))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (loop for i from 0 below per-thread
                                    for name = (intern (format nil "CONTEXT-LOCK-~D-~D" tid i)
                                                       :lol-web/core/test)
                                    do (handler-case
                                           (progn
                                             (lol-web/core::register-context
                                              name '*probe* :default "doc")
                                             (unless (get-context-info name)
                                               (error "context lookup missing"))
                                             (list-contexts))
                                         (error (e)
                                           (bordeaux-threads:with-lock-held (failure-lock)
                                             (push e failures)))))))))))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (null failures) "context registry failures: ~S" failures)))

(test regression-notify-subscribers-after-lock-release
  "Subscriber notifications run AFTER *components-lock* is released, never while
   it is held. Inside an outer with-components-lock frame the :set-state notify
   is deferred until the frame unwinds — so a subscriber that takes a second
   lock cannot ABBA-deadlock against the component lock. The ordinary (no outer
   frame) :set-state path still delivers, also post-release."
  (let ((events '()))
    (defcomponent regression-l6-notify-probe ((k 0))
      (:render () "")
      (:dispatch (action &rest args) (declare (ignore action args)) nil))
    (let* ((comp (regression-l6-notify-probe :id "regression-l6-notify"))
           (unsub (funcall comp :subscribe
                           (lambda (c) (declare (ignore c)) (push :notify events)))))
      (unwind-protect
           (progn
             (with-components-lock
               (funcall comp :set-state :k 1)
               (push :inside events))
             (is (equal '(:inside :notify) (reverse events))
                 "the deferred notify fires after the outer lock body, not under it")
             (setf events '())
             (funcall comp :set-state :k 2)
             (is (equal '(:notify) events)
                 "the ordinary :set-state path still notifies"))
        (funcall unsub)
        (funcall comp :unmount)))))
