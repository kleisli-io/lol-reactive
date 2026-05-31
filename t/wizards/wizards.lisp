(in-package :lol-web/wizards/test)
(in-suite :lol-web/wizards/test)

;;; ============================================================================
;;; Wizard registration
;;; ============================================================================

(test register-wizard-exists
  "register-wizard function exists"
  (is (fboundp 'register-wizard)))

(test list-wizards-exists
  "list-wizards function exists"
  (is (fboundp 'list-wizards)))

(test get-wizard-spec-exists
  "get-wizard-spec function exists"
  (is (fboundp 'get-wizard-spec)))

;;; ============================================================================
;;; Wizard sessions
;;; ============================================================================

(test start-wizard-exists
  "start-wizard function exists"
  (is (fboundp 'start-wizard)))

(test get-wizard-session-exists
  "get-wizard-session function exists"
  (is (fboundp 'get-wizard-session)))

(test process-wizard-submission-exists
  "process-wizard-submission function exists"
  (is (fboundp 'process-wizard-submission)))

;;; ============================================================================
;;; Wizard rendering
;;; ============================================================================

(test render-wizard-step-exists
  "render-wizard-step function exists"
  (is (fboundp 'render-wizard-step)))

(test render-wizard-complete-exists
  "render-wizard-complete function exists"
  (is (fboundp 'render-wizard-complete)))

;;; ============================================================================
;;; Form-field helpers — boundedness and HTML output
;;; ============================================================================

(test wizard-text-field-exists
  "wizard-text-field function exists"
  (is (fboundp 'wizard-text-field)))

(test wizard-select-field-exists
  "wizard-select-field function exists"
  (is (fboundp 'wizard-select-field)))

(test wizard-radio-group-exists
  "wizard-radio-group function exists"
  (is (fboundp 'wizard-radio-group)))

(test wizard-text-field-renders-html
  "wizard-text-field produces HTML output"
  (let ((html (wizard-text-field "username" :label "Username")))
    (is (stringp html))
    (is (search "username" html))
    (is (search "input" html :test #'char-equal))))

(test wizard-select-field-renders-options
  "wizard-select-field includes options"
  (let ((html (wizard-select-field "country"
                '(("us" . "United States") ("uk" . "United Kingdom"))
                :label "Country")))
    (is (stringp html))
    (is (search "select" html :test #'char-equal))
    (is (search "option" html :test #'char-equal))))

;;; ============================================================================
;;; Session-binding regression: another user cannot drive someone else's wizard
;;; ============================================================================

(defun %make-fake-session (&optional initial-pairs)
  "Build a hash-table that mimics the Lack session contract enough for
   session-get / session-set to round-trip."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on initial-pairs by #'cddr
          do (setf (gethash k h) v))
    h))

(defmacro with-fake-session ((session) &body body)
  "Bind lol-web/server:*env* with the given session table installed
   under :lack.session, so session-get / session-set work without a
   live Hunchentoot acceptor."
  `(let ((lol-web/server:*env* (list :lack.session ,session)))
     ,@body))

(test regression-wizard-session-hijack-attempt-rejected
  "An attacker who guesses a victim's wizard-session-id but submits from
   their own Lack session must be refused (status :forbidden), not
   served the victim's in-progress wizard. process-wizard-submission
   compares the wizard's owner-token to the token stored in the
   submitter's session under wizard-owner-key."
  (lol-web/wizards::register-wizard
   'regression-hijack-probe
   (list :steps (list (list :name :a :title "A"
                            :form (lambda (data)
                                    (declare (ignore data))
                                    "<input name='x'/>")))
         :on-complete nil))
  (let* ((victim-session (%make-fake-session))
         (attacker-session (%make-fake-session))
         (wizard nil)
         (wid nil))
    ;; Victim starts a wizard. start-wizard stores the owner-token in
    ;; the victim's session.
    (with-fake-session (victim-session)
      (setf wizard (lol-web/wizards::start-wizard 'regression-hijack-probe))
      (setf wid (funcall wizard :id)))
    (unwind-protect
         (progn
           ;; Attacker presents the victim's wizard id from their own
           ;; session — the owner-token lookup fails.
           (with-fake-session (attacker-session)
             (multiple-value-bind (status result)
                 (lol-web/wizards::process-wizard-submission
                  'regression-hijack-probe wid :next nil)
               (is (eq status :forbidden)
                   "attacker submission must be :forbidden")
               (is (stringp result)
                   "forbidden response carries an explanation string")))
           ;; Victim, submitting from their own session, is still
           ;; allowed. Use :back so we don't trip step-validation.
           (with-fake-session (victim-session)
             (multiple-value-bind (status result)
                 (lol-web/wizards::process-wizard-submission
                  'regression-hijack-probe wid :back nil)
               (declare (ignore result))
               (is (eq status :continue)
                   "victim submission still :continue from owning session"))))
      (lol-web/wizards::remove-wizard-session 'regression-hijack-probe wid))))

;;; ============================================================================
;;; Wizard form-data + action coercion — bounded keyword pool
;;; ============================================================================

(test regression-wizard-form-data-loop-bounds-keyword-pool
  "1000 distinct hostile wizard form-field names must not grow the
   keyword pool; the loop here mirrors the defwizard macro's POST body."
  (let ((baseline (length (apropos-list "" :keyword)))
        (fake-post-params
          (loop for i below 1000
                collect (cons (format nil "attacker-wizard-field-~D-~D"
                                      i (random 999999))
                              "v"))))
    (let ((form-data
            (loop for (k . v) in fake-post-params
                  for key = (lol-web/wizards::safe-coerce-keyword k)
                  when key collect key
                  when key collect v)))
      (declare (ignore form-data))
      (let ((after (length (apropos-list "" :keyword))))
        (is (= baseline after)
            "keyword pool grew from ~D to ~D"
            baseline after)))))

(test regression-wizard-action-allowlist-rejects-attacker
  "wizard-action is allowlisted to (:next :back :complete); anything else
   returns NIL."
  (is (eq :next     (lol-web/wizards::safe-coerce-keyword
                     "next"     :allowed '(:next :back :complete))))
  (is (eq :back     (lol-web/wizards::safe-coerce-keyword
                     "back"     :allowed '(:next :back :complete))))
  (is (eq :complete (lol-web/wizards::safe-coerce-keyword
                     "complete" :allowed '(:next :back :complete))))
  (is (null (lol-web/wizards::safe-coerce-keyword
             "attacker-action-foo" :allowed '(:next :back :complete)))))

;;; ============================================================================
;;; Step-index assertion, all-prior-passed flag, auto-spawn refusal, CSRF wrap
;;; ============================================================================

(defun %register-three-step-probe (&optional (name 'three-step-probe))
  "Register a 3-step wizard whose on-complete returns :ran-on-complete.
   Validators all pass — the only thing under test is step ordering."
  (lol-web/wizards::register-wizard
   name
   (list :steps
         (list (list :name :a :title "A"
                     :form (lambda (d) (declare (ignore d)) "")
                     :validate (lambda (d) (declare (ignore d)) nil))
               (list :name :b :title "B"
                     :form (lambda (d) (declare (ignore d)) "")
                     :validate (lambda (d) (declare (ignore d)) nil))
               (list :name :c :title "C"
                     :form (lambda (d) (declare (ignore d)) "")
                     :validate (lambda (d) (declare (ignore d)) nil)))
         :on-complete (lambda (data) (declare (ignore data)) :ran-on-complete))))

(test regression-wizard-complete-from-step-zero-refused
  "wizard-action=complete from step 0 must be :forbidden — never invoke
   on-complete with whatever step 0 happens to hold."
  (%register-three-step-probe 'regression-skip-probe)
  (with-fake-session ((%make-fake-session))
    (let* ((wstate (lol-web/wizards::start-wizard 'regression-skip-probe))
           (wid (funcall wstate :id)))
      (unwind-protect
           (multiple-value-bind (status reason)
               (lol-web/wizards::process-wizard-submission
                'regression-skip-probe wid :complete nil)
             (is (eq :forbidden status)
                 ":complete from step 0 must be :forbidden, got ~S" status)
             (is (search "final step" reason)
                 "reason must mention final step, got ~S" reason))
        (lol-web/wizards::remove-wizard-session 'regression-skip-probe wid)))))

(test regression-wizard-auto-spawn-refuses-complete-and-back
  "process-wizard-submission with no session ID must refuse :complete
   and :back. Only :next may spawn a fresh wizard."
  (%register-three-step-probe 'regression-autospawn-probe)
  (with-fake-session ((%make-fake-session))
    (multiple-value-bind (status _r)
        (lol-web/wizards::process-wizard-submission
         'regression-autospawn-probe nil :complete nil)
      (declare (ignore _r))
      (is (eq :forbidden status)
          ":complete with no session must be :forbidden, got ~S" status))
    (multiple-value-bind (status _r)
        (lol-web/wizards::process-wizard-submission
         'regression-autospawn-probe nil :back nil)
      (declare (ignore _r))
      (is (eq :forbidden status)
          ":back with no session must be :forbidden, got ~S" status))
    ;; :next still spawns; clean up immediately.
    (multiple-value-bind (status wstate)
        (lol-web/wizards::process-wizard-submission
         'regression-autospawn-probe nil :next nil)
      (is (eq :continue status)
          ":next with no session must auto-spawn, got ~S" status)
      (when wstate
        (lol-web/wizards::remove-wizard-session
         'regression-autospawn-probe (funcall wstate :id))))))

(test regression-wizard-complete-requires-every-prior-step-passed
  "Walking to the last step via :goto-step (bypassing per-step :next
   validators) must not pass the :complete gate. The all-prior-passed
   check refuses completion even when current-step is correct."
  (%register-three-step-probe 'regression-jump-probe)
  (with-fake-session ((%make-fake-session))
    (let* ((wstate (lol-web/wizards::start-wizard 'regression-jump-probe))
           (wid (funcall wstate :id)))
      (unwind-protect
           (progn
             ;; Jump straight to the last step — passed-steps stays NIL.
             (funcall wstate :goto-step 2)
             (multiple-value-bind (status reason)
                 (lol-web/wizards::process-wizard-submission
                  'regression-jump-probe wid :complete nil)
               (is (eq :forbidden status)
                   ":complete at last step with no prior :next passes must be :forbidden")
               (is (search "prior step" reason)
                   "reason must mention prior step gate, got ~S" reason)))
        (lol-web/wizards::remove-wizard-session 'regression-jump-probe wid)))))

(test regression-wizard-complete-happy-path-via-next-passes
  "After legitimate :next walks every prior step, :complete fires
   on-complete and returns its result."
  (%register-three-step-probe 'regression-happy-probe)
  (with-fake-session ((%make-fake-session))
    (multiple-value-bind (_s wstate)
        (lol-web/wizards::process-wizard-submission
         'regression-happy-probe nil :next nil)
      (declare (ignore _s))
      (let ((wid (funcall wstate :id)))
        (unwind-protect
             (progn
               (lol-web/wizards::process-wizard-submission
                'regression-happy-probe wid :next nil)
               (multiple-value-bind (status result)
                   (lol-web/wizards::process-wizard-submission
                    'regression-happy-probe wid :complete nil)
                 (is (eq :complete status)
                     "happy-path :complete must return :complete, got ~S" status)
                 (is (eq :ran-on-complete result)
                     "on-complete's return value must surface, got ~S" result)))
          ;; happy-path remove-wizard-session is part of :complete; nothing to clean up.
          )))))

(test regression-defwizard-post-route-wraps-in-csrf-validation
  "The defwizard macro must auto-wrap the POST handler in
   with-csrf-validation. We assert against the macro expansion so the
   contract is enforced at compile time, not only at request time."
  (let ((expansion (macroexpand-1
                    '(lol-web/wizards:defwizard csrf-shape ()
                       :steps ((:name :a :title "A"
                                :form (lambda (d) (declare (ignore d)) ""))
                               (:name :b :title "B"
                                :form (lambda (d) (declare (ignore d)) "")))
                       :on-complete (lambda (data) (declare (ignore data)) :ran)))))
    (is (search "WITH-CSRF-VALIDATION" (princ-to-string expansion))
        "defwizard POST expansion must contain WITH-CSRF-VALIDATION")))

(test regression-wizard-per-ip-cap-honoured-on-auto-spawn
  "When client-ip is set and the per-IP wizard cap is exhausted,
   auto-spawn returns :forbidden rather than allocating yet another
   session. Cap is *wizard-sessions-per-ip-cap* (default 10)."
  (%register-three-step-probe 'regression-cap-probe)
  (let ((env-with-ip (list :remote-addr "10.0.0.1"
                           :lack.session (%make-fake-session)))
        (allocated '()))
    (let ((lol-web/server:*env* env-with-ip)
          (lol-web/server:*trusted-proxies* nil))
      (unwind-protect
           (let ((cap lol-web/wizards::*wizard-sessions-per-ip-cap*))
             ;; Allocate up to the cap.
             (loop repeat cap do
                   (multiple-value-bind (status wstate)
                       (lol-web/wizards::process-wizard-submission
                        'regression-cap-probe nil :next nil)
                     (declare (ignore status))
                     (push (funcall wstate :id) allocated)))
             ;; The (cap+1)th try must be :forbidden.
             (multiple-value-bind (status reason)
                 (lol-web/wizards::process-wizard-submission
                  'regression-cap-probe nil :next nil)
               (is (eq :forbidden status)
                   "request past per-IP cap must be :forbidden, got ~S" status)
               (is (search "rate limit" reason)
                   "reason must mention rate limit, got ~S" reason)))
        (dolist (id allocated)
          (lol-web/wizards::remove-wizard-session 'regression-cap-probe id))
        (lol-web/server:clear-rate-limit-store)))))
