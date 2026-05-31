;;;; Regression tests for :lol-web/wizards covering OpenAPI-surface controls.
;;;;
;;;; defwizard registers GET+POST metadata in *handler-metadata* so that the
;;;; OpenAPI emitter's :internal filter can drop wizard endpoints from the
;;;; published surface. These tests pin the default-internal behaviour and
;;;; the opt-out path.

(in-package :lol-web/wizards/test)
(in-suite :lol-web/wizards/test)

(defun %wizard-path-keys (spec)
  "Helper: return the list of path strings from a BUILD-OPENAPI-SPEC alist."
  (let ((paths (cdr (assoc "paths" spec :test #'string=))))
    (mapcar #'car paths)))

(test regression-defwizard-internal-excluded-from-openapi
  "By default DEFWIZARD marks its GET+POST routes :internal t in
   *handler-metadata*. BUILD-OPENAPI-SPEC's :internal filter drops the
   wizard from the emitted paths object even when the path is listed
   in :ONLY-PATHS."
  (defwizard regression-internal-wizard ()
    :steps ((:name :a :title "A"
             :form (lambda (data) (declare (ignore data)) ""))))
  (let* ((spec (lol-web/openapi:build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/wizard/regression-internal-wizard")))
         (keys (%wizard-path-keys spec)))
    (is (not (member "/wizard/regression-internal-wizard" keys :test #'string=))
        "default-internal wizard must be excluded from emitted spec, got ~S" keys)))

(test regression-defwizard-internal-nil-includes-in-openapi
  "Passing :INTERNAL NIL opts the wizard back into the published surface;
   both GET and POST methods are registered, so the path-item carries
   two operations."
  (defwizard regression-public-wizard ()
    :internal nil
    :steps ((:name :a :title "A"
             :form (lambda (data) (declare (ignore data)) ""))))
  (let* ((spec (lol-web/openapi:build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/wizard/regression-public-wizard")))
         (paths (cdr (assoc "paths" spec :test #'string=)))
         (path (cdr (assoc "/wizard/regression-public-wizard" paths
                            :test #'string=)))
         (methods (mapcar #'car path)))
    (is (not (null path)) ":internal nil wizard must appear in emitted spec")
    (is (member "get" methods :test #'string=))
    (is (member "post" methods :test #'string=))))

;;; ============================================================================
;;; Cross-wizard authorization
;;; ============================================================================

(defwizard regression-xwiz-a ()
  :steps ((:name :a :title "A"
           :form (lambda (data) (declare (ignore data)) ""))))

(defwizard regression-xwiz-b ()
  :steps ((:name :b :title "B"
           :form (lambda (data) (declare (ignore data)) ""))))

(test regression-cross-wizard-session-id-rejected
  "A wizard-A session-id presented to wizard-B's route must not
   authorize the submission.  With *wizard-sessions* namespaced by
   wizard-name, the wizard-B lookup misses; the auto-spawn branch then
   refuses :complete and :back because only :next may spawn."
  (let* ((state-a (lol-web/wizards::make-wizard-state
                   'regression-xwiz-a
                   (list (list :name :a :title "A"))
                   :owner-token nil)))
    (lol-web/wizards::store-wizard-session state-a)
    (unwind-protect
         (multiple-value-bind (status reason)
             (process-wizard-submission 'regression-xwiz-b
                                        (funcall state-a :id)
                                        :complete
                                        '())
           (declare (ignore reason))
           (is (eq status :forbidden)
               "cross-wizard :complete must be :forbidden, got ~S" status))
      (lol-web/wizards::remove-wizard-session
       'regression-xwiz-a (funcall state-a :id)))))

;;; ============================================================================
;;; Concurrent session-registry mutation
;;; ============================================================================

(test regression-wizard-sessions-concurrent-write-no-corruption
  "64 threads each calling store-wizard-session must produce exactly 64
   new entries in *wizard-sessions* — no insert lost to a race on the
   underlying hash table."
  (defwizard regression-conc-wizard ()
    :steps ((:name :a :title "A"
             :form (lambda (data) (declare (ignore data)) ""))))
  (let ((before (hash-table-count lol-web/wizards::*wizard-sessions*))
        (threads nil)
        (ids nil)
        (ids-lock (bordeaux-threads:make-lock "regression-conc-ids")))
    (unwind-protect
         (progn
           (dotimes (_ 64)
             (push (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((state (lol-web/wizards::make-wizard-state
                                    'regression-conc-wizard
                                    (list (list :name :a :title "A"))
                                    :owner-token nil)))
                        (lol-web/wizards::store-wizard-session state)
                        (bordeaux-threads:with-lock-held (ids-lock)
                          (push (funcall state :id) ids)))))
                   threads))
           (dolist (th threads) (bordeaux-threads:join-thread th))
           (let ((after (hash-table-count lol-web/wizards::*wizard-sessions*)))
             (is (= 64 (- after before))
                 "expected 64 new sessions, got ~D" (- after before))))
      (dolist (id ids)
        (lol-web/wizards::remove-wizard-session 'regression-conc-wizard id)))))

;;; ============================================================================
;;; Snapshot cleanup under concurrent insertion
;;; ============================================================================

(test regression-cleanup-stale-sessions-no-iteration-crash
  "cleanup-stale-sessions snapshots keys under lock before iterating,
   so a concurrent inserter must not crash the cleanup pass."
  (defwizard regression-cleanup-wizard ()
    :steps ((:name :a :title "A"
             :form (lambda (data) (declare (ignore data)) ""))))
  (let ((stop nil)
        (ids nil)
        (ids-lock (bordeaux-threads:make-lock "regression-cleanup-ids")))
    (let ((inserter (bordeaux-threads:make-thread
                     (lambda ()
                       (loop until stop
                             for state = (lol-web/wizards::make-wizard-state
                                          'regression-cleanup-wizard
                                          (list (list :name :a :title "A"))
                                          :owner-token nil)
                             do (lol-web/wizards::store-wizard-session state)
                                (bordeaux-threads:with-lock-held (ids-lock)
                                  (push (funcall state :id) ids))
                                (sleep 0.001))))))
      (unwind-protect
           (is (eq :ok
                   (handler-case
                       (progn
                         (dotimes (_ 8)
                           (lol-web/wizards:cleanup-stale-sessions 0))
                         :ok)
                     (error () :signaled)))
               "cleanup must not crash under concurrent inserts")
        (setf stop t)
        (bordeaux-threads:join-thread inserter)
        (dolist (id ids)
          (lol-web/wizards::remove-wizard-session
           'regression-cleanup-wizard id))))))

;;; ============================================================================
;;; CSRF token surfaced in the auto-generated GET form
;;; ============================================================================

(defwizard regression-csrf-form-wizard ()
  :internal nil
  :steps ((:name :a :title "A"
           :form (lambda (data) (declare (ignore data)) "<p>hi</p>"))))

(test regression-wizard-get-form-includes-csrf-token
  "The auto-generated GET handler emits an <input name=\"csrf-token\">
   inside the form, so a subsequent POST passes with-csrf-validation."
  (lack/test:testing-app
      (lol-web/server:make-app :use-csrf nil
                               :use-static nil
                               :use-accesslog nil)
    (let ((body (lack/test:request "/wizard/regression-csrf-form-wizard")))
      (is (search "name=\"csrf-token\"" body)
          "GET response body must carry csrf-token input, got ~S" body))))

;;; ============================================================================
;;; GET handler allocates no session state (FIND-C-M01)
;;; ============================================================================

(defwizard regression-noalloc-wizard ()
  :internal nil
  :steps ((:name :a :title "A"
           :form (lambda (data) (declare (ignore data)) ""))))

(test regression-wizard-get-handler-does-not-allocate-state
  "Five GET requests to a wizard's route must not insert any rows into
   *wizard-sessions*; allocation now happens only on the first POST."
  (let ((before (hash-table-count lol-web/wizards::*wizard-sessions*)))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil
                                 :use-static nil
                                 :use-accesslog nil)
      (dotimes (_ 5)
        (lack/test:request "/wizard/regression-noalloc-wizard")))
    (let ((after (hash-table-count lol-web/wizards::*wizard-sessions*)))
      (is (= before after)
          "GET handler must not allocate sessions; ~D -> ~D" before after))))

;;; ============================================================================
;;; Per-instance lock serialises wizard-state messages (step-skip TOCTOU)
;;; ============================================================================

(test regression-wizard-state-concurrent-advance-no-corruption
  "make-wizard-state serialises every message through a per-instance
   recursive lock, so concurrent same-session advances cannot race the
   bounds guard in :next-step and push current-step past the final step.
   The exposed :lock is a stable, re-entrant lock the submission handler
   holds across the read-then-mutate compound."
  (let* ((steps (list (list :name :a :title "A")
                      (list :name :b :title "B")
                      (list :name :c :title "C")))
         (state (lol-web/wizards::make-wizard-state
                 'regression-conc-advance steps :owner-token nil))
         (lock (funcall state :lock)))
    (is (eq lock (funcall state :lock))
        ":lock must return the one per-instance lock object")
    ;; Evaluate the lock-held body outside IS: fiveam's IS decomposes
    ;; (op . args) as a function call, which would mis-handle the
    ;; WITH-RECURSIVE-LOCK-HELD macro and call (LOCK) as a function.
    (let ((held (bordeaux-threads:with-recursive-lock-held (lock) :held)))
      (is (eq :held held)
          ":lock must return a usable recursive lock"))
    (let ((threads nil))
      (dotimes (_ 64)
        (push (bordeaux-threads:make-thread
               (lambda () (dotimes (_ 8) (funcall state :next-step))))
              threads))
      (dolist (th threads) (bordeaux-threads:join-thread th)))
    (let ((final (funcall state :current-step))
          (total (funcall state :total-steps)))
      (is (<= 0 final (1- total))
          "current-step ~D escaped bounds [0, ~D] under concurrent advance"
          final (1- total))
      (is (= final (1- total))
          "concurrent advance must still saturate at the final step, got ~D"
          final))))
