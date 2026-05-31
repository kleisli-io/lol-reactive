;;;; HTTP-level end-to-end tests for :lol-web/fullstack.
;;;;
;;;; Drives make-app + lack/test:request against /api/dispatch with an
;;;; application/json body. The behavioural assertions target the
;;;; component-API ownership gate and the CSPRNG entropy of generated
;;;; component IDs.

(in-package :lol-web/fullstack/test)
(in-suite :lol-web/fullstack/test)

;;; ============================================================================
;;; Test component
;;; ============================================================================

(defcomponent e2e-counter ((count 0))
  (:render ()
    (format nil "<span>~D</span>" count))
  (:dispatch (action &rest args)
    (declare (ignore args))
    (case action
      (:inc (incf count))
      (:dec (decf count)))))

(defun %e2e-make-counter-with-binding (binding)
  "Build a counter, re-register it under its id with PRINCIPAL-BINDING.
   Returns the component's id."
  (let ((c (e2e-counter)))
    (register-component (funcall c :id) c :principal-binding binding)
    (funcall c :id)))

(defun %e2e-dispatch-json (component-id action)
  (format nil "{\"component-id\":~S,\"action\":~S,\"args\":[]}"
          component-id (string-downcase (symbol-name action))))

(defun %e2e-json-headers ()
  '(("content-type" . "application/json")))

;;; ============================================================================
;;; Cross-owner refusal
;;; ============================================================================

(test e2e-fullstack-cross-owner-refused
  (let ((cid (%e2e-make-counter-with-binding :owner-alice)))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil :use-static nil :use-accesslog nil
                                 :auth (list :authenticated-p (lambda () t)
                                             :current-principal (lambda () :owner-bob)))
      (multiple-value-bind (body status)
          (lack/test:request "/api/dispatch"
                             :method :post
                             :content (%e2e-dispatch-json cid :inc)
                             :headers (%e2e-json-headers))
        (is (= 200 status)
            "cross-owner refusal returns 200 with refusal JSON, got ~D" status)
        (is (search "Not available" body)
            "response body must carry the neutral unavailable shape, got ~S" body)))))

(test e2e-fullstack-same-owner-dispatch-allowed
  (let ((cid (%e2e-make-counter-with-binding :owner-alice)))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil :use-static nil :use-accesslog nil
                                 :auth (list :authenticated-p (lambda () t)
                                             :current-principal (lambda () :owner-alice)))
      (multiple-value-bind (body status)
          (lack/test:request "/api/dispatch"
                             :method :post
                             :content (%e2e-dispatch-json cid :inc)
                             :headers (%e2e-json-headers))
        (is (= 200 status))
        (is (search "\"success\":true" body)
            "matching owner must yield success envelope, got ~S" body)
        (is (null (search "Forbidden" body))
            "matching owner must not receive Forbidden, got ~S" body)))))

(test e2e-fullstack-unauthenticated-cross-owner-refused
  (let ((cid (%e2e-make-counter-with-binding nil)))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil :use-static nil :use-accesslog nil)
      (multiple-value-bind (body status)
          (lack/test:request "/api/dispatch"
                             :method :post
                             :content (%e2e-dispatch-json cid :inc)
                             :headers (%e2e-json-headers))
        (declare (ignore body))
        (is (= 401 status)
            "unauthenticated /api/dispatch must yield 401, got ~D" status)))))

;;; ============================================================================
;;; CSPRNG ID
;;; ============================================================================

(test e2e-fullstack-component-ids-not-enumerable
  (let ((ids (loop repeat 100
                   collect (generate-component-id 'probe))))
    (is (= 100 (length (remove-duplicates ids :test #'string=)))
        "no duplicates expected across 100 sampled ids")
    (let* ((sample (first ids))
           (dash (position #\- sample :from-end t)))
      (is (and dash (= 32 (- (length sample) (1+ dash))))
          "id suffix must be 32 hex chars, got ~S" sample)
      (is (every (lambda (c)
                   (or (and (char>= c #\0) (char<= c #\9))
                       (and (char>= c #\a) (char<= c #\f))))
                 (subseq sample (1+ dash)))
          "id suffix must be lowercase hex, got ~S" sample))))

;;; ============================================================================
;;; HMAC-signed hydration envelope on /api/set-state
;;; ============================================================================

(defun %e2e-hydration-key (label)
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for c across label
          for i from 0 below 32
          do (setf (aref key i) (char-code c)))
    key))

(defun %e2e-set-state-json (component-id signed)
  "Serialise a /api/set-state body with COMPONENT-ID and the SIGNED
   envelope (an alist with :payload + :tag) flattened into outer JSON."
  (lol-web/server:encode-json-string
    `((:component-id . ,component-id)
      (:signed . ,signed))))

(test e2e-fullstack-set-state-accepts-signed-envelope
  (let* ((key (%e2e-hydration-key "primary"))
         (cid (%e2e-make-counter-with-binding :owner-alice))
         (signed (sign-hydration-state
                  '((:key . "count") (:value . 7))
                  key)))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil :use-static nil :use-accesslog nil
                                 :hydration-secret-key key
                                 :auth (list :authenticated-p (lambda () t)
                                             :current-principal (lambda () :owner-alice)))
      (multiple-value-bind (body status)
          (lack/test:request "/api/set-state"
                             :method :post
                             :content (%e2e-set-state-json cid signed)
                             :headers (%e2e-json-headers))
        (is (= 200 status) "valid signed set-state must return 200, got ~D" status)
        (is (search "\"success\":true" body)
            "valid envelope must yield success, got ~S" body)))))

(test e2e-fullstack-set-state-refuses-tampered-tag
  (let* ((key (%e2e-hydration-key "primary"))
         (cid (%e2e-make-counter-with-binding :owner-alice))
         (signed (sign-hydration-state
                  '((:key . "count") (:value . 7))
                  key))
         (orig-tag (cdr (assoc :tag signed)))
         (tampered (list (assoc :payload signed)
                         (cons :tag
                               (concatenate 'string
                                            (string (if (char= (char orig-tag 0) #\0)
                                                        #\1 #\0))
                                            (subseq orig-tag 1))))))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil :use-static nil :use-accesslog nil
                                 :hydration-secret-key key
                                 :auth (list :authenticated-p (lambda () t)
                                             :current-principal (lambda () :owner-alice)))
      (multiple-value-bind (body status)
          (lack/test:request "/api/set-state"
                             :method :post
                             :content (%e2e-set-state-json cid tampered)
                             :headers (%e2e-json-headers))
        (is (= 200 status))
        (is (search "Signed envelope failed verification" body)
            "tampered envelope must yield refusal body, got ~S" body)
        (is (null (search "\"success\":true" body))
            "tampered envelope must NOT yield success, got ~S" body)))))

(test e2e-fullstack-set-state-refuses-when-no-key-configured
  (let* ((cid (%e2e-make-counter-with-binding :owner-alice))
         (signed (sign-hydration-state '((:key . "count") (:value . 7))
                                       (%e2e-hydration-key "primary"))))
    (lack/test:testing-app
        (lol-web/server:make-app :use-csrf nil :use-static nil :use-accesslog nil
                                 :auth (list :authenticated-p (lambda () t)
                                             :current-principal (lambda () :owner-alice)))
      (multiple-value-bind (body status)
          (lack/test:request "/api/set-state"
                             :method :post
                             :content (%e2e-set-state-json cid signed)
                             :headers (%e2e-json-headers))
        (is (= 200 status))
        (is (search "Hydration secret-key not configured" body)
            "unconfigured app must refuse with :NO-KEY message, got ~S" body)))))
