;;;; HTTP-level end-to-end tests for :lol-web/wizards.
;;;;
;;;; Drives make-app + lack/test:request. The /__e2e/wizards/* probes
;;;; call process-wizard-submission / start-wizard directly and surface
;;;; the status keyword as the body, so wizard-semantics tests do not
;;;; mint a CSRF token per request.

(in-package :lol-web/wizards/test)
(in-suite :lol-web/wizards/test)

;;; ============================================================================
;;; Test wizard + probe routes
;;; ============================================================================

(defwizard __e2e-checkout ()
  :steps ((:name :step1 :title "Step 1"
           :form (lambda (data) (declare (ignore data)) ""))
          (:name :step2 :title "Step 2"
           :form (lambda (data) (declare (ignore data)) ""))
          (:name :step3 :title "Final"
           :form (lambda (data) (declare (ignore data)) "")))
  :on-complete (lambda (data) (declare (ignore data)) "done"))

(lol-web/server:defroute "/__e2e/wizards/start-via-next" (:method :post :secure nil)
  (multiple-value-bind (status result)
      (process-wizard-submission '__e2e-checkout nil :next nil)
    (declare (ignore result))
    (format nil "~A" status)))

(lol-web/server:defroute "/__e2e/wizards/submit-action" (:method :post :secure nil)
  (let* ((sid (lol-web/server:post-param "wizard-session-id"))
         (action-str (lol-web/server:post-param "wizard-action"))
         (action (cond ((string= action-str "next") :next)
                       ((string= action-str "back") :back)
                       ((string= action-str "complete") :complete)
                       (t :next))))
    (multiple-value-bind (status result)
        (process-wizard-submission '__e2e-checkout sid action nil)
      (declare (ignore result))
      (format nil "~A" status))))

(lol-web/server:defroute "/__e2e/wizards/spawn-and-capture-sid" (:method :post :secure nil)
  (let ((state (start-wizard '__e2e-checkout)))
    (funcall state :id)))

;;; ============================================================================
;;; CSRF gate on the auto-generated wizard route
;;; ============================================================================

(test e2e-wizard-csrf-without-token-rejected
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (multiple-value-bind (body status)
        (lack/test:request "/wizard/__e2e-checkout"
                           :method :post
                           :content '(("wizard-action" . "next")))
      (declare (ignore body))
      (is (= 403 status)
          "wizard POST without csrf-token must return 403, got ~D" status))))

;;; ============================================================================
;;; Auto-spawn refusal
;;; ============================================================================

(test e2e-wizard-auto-spawn-refuses-back-without-session
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/wizards/submit-action"
                                   :method :post
                                   :content '(("wizard-session-id" . "wizard-9999-bogus")
                                              ("wizard-action" . "back")))))
      (is (string= "FORBIDDEN" body)
          "auto-spawn-on-back must be FORBIDDEN, got ~S" body))))

(test e2e-wizard-auto-spawn-refuses-complete-without-session
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/wizards/submit-action"
                                   :method :post
                                   :content '(("wizard-session-id" . "wizard-9999-bogus")
                                              ("wizard-action" . "complete")))))
      (is (string= "FORBIDDEN" body)
          "auto-spawn-on-complete must be FORBIDDEN, got ~S" body))))

;;; ============================================================================
;;; Skip-to-complete refusal
;;; ============================================================================

(test e2e-wizard-skip-to-complete-refused
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((jar (cl-cookie:make-cookie-jar)))
      (let ((sid (lack/test:request "/__e2e/wizards/spawn-and-capture-sid"
                                    :method :post
                                    :cookie-jar jar)))
        (is (plusp (length sid))
            "spawn must yield a non-empty session id, got ~S" sid)
        (let ((body (lack/test:request "/__e2e/wizards/submit-action"
                                       :method :post
                                       :cookie-jar jar
                                       :content `(("wizard-session-id" . ,sid)
                                                  ("wizard-action" . "complete")))))
          (is (string= "FORBIDDEN" body)
              "skip-to-complete at step 0 must be FORBIDDEN, got ~S" body))))))

;;; ============================================================================
;;; Per-IP cap
;;; ============================================================================

(test e2e-wizard-per-ip-cap-denies
  ;; clear-rate-limit-store before and after so a leaked bucket does not
  ;; pollute sibling suites in the same image.
  (lol-web/server:clear-rate-limit-store :wizard-sessions-per-ip)
  (unwind-protect
       (let ((lol-web/wizards::*wizard-sessions-per-ip-cap* 1))
         (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                         :use-static nil
                                                         :use-accesslog nil)
           (let ((first (lack/test:request "/__e2e/wizards/start-via-next"
                                           :method :post)))
             (is (string= "CONTINUE" first)
                 "first spawn must succeed under cap, got ~S" first))
           (let ((second (lack/test:request "/__e2e/wizards/start-via-next"
                                            :method :post)))
             (is (string= "FORBIDDEN" second)
                 "second spawn over the cap must be FORBIDDEN, got ~S" second))))
    (lol-web/server:clear-rate-limit-store :wizard-sessions-per-ip)))

;;; ============================================================================
;;; Owner-token: session-id substitution
;;; ============================================================================

(test e2e-wizard-owner-token-substitution-refused
  (lol-web/server:clear-rate-limit-store :wizard-sessions-per-ip)
  (unwind-protect
       (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                       :use-static nil
                                                       :use-accesslog nil)
         (let ((jar-a (cl-cookie:make-cookie-jar))
               (jar-b (cl-cookie:make-cookie-jar)))
           (let ((sid (lack/test:request "/__e2e/wizards/spawn-and-capture-sid"
                                         :method :post
                                         :cookie-jar jar-a)))
             (is (plusp (length sid))
                 "user A spawn must yield a session id, got ~S" sid)
             (let ((body (lack/test:request "/__e2e/wizards/submit-action"
                                            :method :post
                                            :cookie-jar jar-b
                                            :content `(("wizard-session-id" . ,sid)
                                                       ("wizard-action" . "next")))))
               (is (string= "FORBIDDEN" body)
                   "user B cannot submit on user A's session id, got ~S" body)))))
    (lol-web/server:clear-rate-limit-store :wizard-sessions-per-ip)))
