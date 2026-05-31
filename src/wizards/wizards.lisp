;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/WIZARDS; Base: 10 -*-
;;;; wizards/wizards.lisp - Continuation-Based Multi-Step Wizards
;;;;
;;;; PURPOSE:
;;;;   Write multi-step UI flows as linear code using session-based state.
;;;;   Each step suspends execution until user submits, enabling natural
;;;;   sequential code for complex workflows.
;;;;
;;;; KEY MACROS:
;;;;   DEFWIZARD - Define a multi-step wizard
;;;;   WIZARD-STEP - Single step that suspends until user input
;;;;
;;;; PATTERNS USED:
;;;;   - dlambda for wizard state machine (message-passing)
;;;;   - defmacro! for gensym hygiene
;;;;   - Session-based state storage (Hunchentoot)
;;;;   - aif for conditional handling

(in-package :lol-web/wizards)

;;; ============================================================================
;;; LOCAL UTILITIES
;;; ============================================================================

(defmacro awhen (test &body body)
  "Anaphoric when: if TEST is non-nil, bind IT to the result and execute BODY."
  `(let ((let-over-lambda:it ,test))
     (when let-over-lambda:it
       ,@body)))

;;; ============================================================================
;;; WIZARD REGISTRY
;;; ============================================================================

(defvar *wizards* (make-hash-table :test 'eq)
  "Registry of defined wizard specifications.")

(defvar *wizards-lock*
  (bordeaux-threads:make-recursive-lock "lol-web/wizards wizards registry")
  "Guards *wizards*. Recursive so DEFWIZARD's expansion may chain
   register-wizard with helper writes without releasing.")

(defvar *wizard-sessions* (make-hash-table :test 'equal)
  "Active wizard sessions: (wizard-name . session-id) -> wizard-state.
   Namespacing by wizard-name keeps a session-id valid against the
   wizard that allocated it; cross-wizard substitutions miss the
   lookup rather than authorizing a sibling wizard's submission.")

(defvar *wizard-sessions-lock*
  (bordeaux-threads:make-recursive-lock "lol-web/wizards sessions registry")
  "Guards *wizard-sessions*.")

(defparameter *wizard-sessions-per-ip-cap* 10
  "Max concurrent wizard-session starts per client-ip inside the
   *wizard-sessions-per-ip-window*. The cap rides the shared
   check-rate-limit registry under the :wizard-sessions-per-ip
   namespace; a flood from one IP can't evict legitimate entries
   from other rate-limit namespaces.")

(defparameter *wizard-sessions-per-ip-window* 60
  "Seconds of window for the per-IP wizard-session cap.")

(defun %wizard-rate-limit-allows-p ()
  "Check the per-IP wizard-session cap. Returns T when client-ip is
   absent (e.g., direct REPL or test fixture without a request env) so
   non-HTTP callers are not blocked; otherwise consults check-rate-limit
   in the :wizard-sessions-per-ip namespace."
  (let ((ip (lol-web/server:client-ip)))
    (or (null ip)
        (lol-web/server:check-rate-limit
         ip
         :namespace :wizard-sessions-per-ip
         :max-requests *wizard-sessions-per-ip-cap*
         :window-seconds *wizard-sessions-per-ip-window*))))

(defun register-wizard (name spec)
  "Register a wizard specification under NAME."
  (bordeaux-threads:with-recursive-lock-held (*wizards-lock*)
    (setf (gethash name *wizards*) spec)))

(defun %register-wizard-handler-metadata (name route-path internal)
  "Write GET+POST metadata entries for ROUTE-PATH into
   lol-web/extractors:*handler-metadata*, mirroring DEFHANDLER's shape.
   The :extractors list is NIL because wizard routes read form data via
   POST-PARAM directly, not through the extractor protocol. The OpenAPI
   emitter still consults :options for the :internal exclusion."
  (let ((options (list :method :get :internal internal)))
    (bordeaux-threads:with-recursive-lock-held
        (lol-web/extractors::*handler-metadata-lock*)
      (setf (gethash (cons :get route-path) lol-web/extractors:*handler-metadata*)
            (list :name name :method :get :path route-path
                  :extractors nil
                  :options options))
      (setf (gethash (cons :post route-path) lol-web/extractors:*handler-metadata*)
            (list :name name :method :post :path route-path
                  :extractors nil
                  :options (list :method :post :internal internal))))))

(defun get-wizard-spec (name)
  "Retrieve the spec registered for NAME, or NIL."
  (bordeaux-threads:with-recursive-lock-held (*wizards-lock*)
    (gethash name *wizards*)))

(defun list-wizards ()
  "List all registered wizard names."
  (bordeaux-threads:with-recursive-lock-held (*wizards-lock*)
    (let (wizards)
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k wizards))
               *wizards*)
      (nreverse wizards))))

;;; ============================================================================
;;; WIZARD SESSION STATE
;;; ============================================================================

(defun generate-wizard-session-id ()
  "Generate a unique session ID for a wizard instance. The high-entropy
   token comes from the OS CSPRNG (via :lol-web/server's csrf
   generator) so a session id can't be guessed by an attacker."
  (format nil "wizard-~A-~A"
          (get-universal-time)
          (lol-web/server:generate-csrf-token)))

(defun generate-owner-token ()
  "Generate a high-entropy bearer token used to bind a wizard session to
   the user that started it. Stored both on the wizard state and in
   that user's Lack session under (wizard-owner-key SESSION-ID); the
   POST handler refuses any submission whose session does not produce
   the same token, blocking session-id-substitution attacks."
  (lol-web/server:generate-csrf-token))

(defun wizard-owner-key (session-id)
  "Hash-table key under which a wizard's owner-token lives in the user's
   Lack session. Per-wizard so a single browser may run several wizards
   concurrently."
  (format nil "wizard-owner:~A" session-id))

(defun make-wizard-state (wizard-name steps &key owner-token)
  "Create initial wizard state structure using dlambda pattern.
   Returns a closure that responds to messages.

   OWNER-TOKEN binds this wizard to the user that started it; supplied
   by start-wizard from generate-owner-token. Tests may pass nil to opt
   out of binding."
  (let ((session-id (generate-wizard-session-id))
        (created-at (get-universal-time))
        (current-step 0)
        (step-data (make-hash-table :test 'equal))
        (passed-steps (make-array (length steps) :initial-element nil))
        (completed nil)
        (cancelled nil)
        (state-lock (bordeaux-threads:make-recursive-lock
                     "lol-web/wizards state")))
    (let ((machine
    (dlambda
      (:id () session-id)
      (:created-at () created-at)
      (:wizard-name () wizard-name)
      (:owner-token () owner-token)
      ;; The per-instance lock serialises every message; callers that need a
      ;; read-then-mutate sequence to be atomic (process-wizard-submission's
      ;; validate -> mark-passed -> advance) hold it across the whole compound
      ;; so a concurrent same-session POST cannot race the step-skip gate.
      (:lock () state-lock)
      (:current-step () current-step)
      (:total-steps () (length steps))
      (:step-name () (aif (nth current-step steps)
                          (getf let-over-lambda:it :name)
                          nil))
      (:step-title () (aif (nth current-step steps)
                           (getf let-over-lambda:it :title)
                           nil))
      (:completed-p () completed)
      (:cancelled-p () cancelled)
      (:passed-steps () (copy-seq passed-steps))

      ;; Mark step N as having passed its validator.
      (:mark-step-passed (n)
       (when (and (>= n 0) (< n (length steps)))
         (setf (aref passed-steps n) t)))

      ;; T when every prior step (strictly before N) has been marked passed.
      (:prior-steps-passed-p (n)
       (let ((ok t))
         (dotimes (i n ok)
           (unless (aref passed-steps i) (setf ok nil)))))

      ;; Get data for a specific step
      (:get-step-data (step-name)
       (gethash step-name step-data))

      ;; Set data for current step
      (:set-step-data (data)
       (awhen (nth current-step steps)
         (setf (gethash (getf let-over-lambda:it :name) step-data) data)))

      ;; Get all collected data
      (:all-data ()
       (let (result)
         (maphash (lambda (k v) (push (cons k v) result)) step-data)
         (nreverse result)))

      ;; Navigation
      (:can-go-back-p () (> current-step 0))
      (:can-go-forward-p () (< current-step (1- (length steps))))

      (:next-step ()
       (when (< current-step (1- (length steps)))
         (incf current-step)
         current-step))

      (:prev-step ()
       (when (> current-step 0)
         (decf current-step)
         current-step))

      (:goto-step (n)
       (when (and (>= n 0) (< n (length steps)))
         (setf current-step n)
         current-step))

      ;; Completion
      (:complete ()
       (setf completed t))

      (:cancel ()
       (setf cancelled t))

      ;; Introspection
      (:inspect ()
       (list :session-id session-id
             :created-at created-at
             :wizard-name wizard-name
             :current-step current-step
             :total-steps (length steps)
             :step-name (aif (nth current-step steps)
                             (getf let-over-lambda:it :name)
                             nil)
             :completed completed
             :cancelled cancelled
             :data-keys (let (keys)
                          (maphash (lambda (k v) (declare (ignore v)) (push k keys))
                                   step-data)
                          keys))))))
      (lambda (&rest args)
        (bordeaux-threads:with-recursive-lock-held (state-lock)
          (apply machine args))))))

(defun %wizard-session-key (wizard-name session-id)
  "Compose the namespaced *wizard-sessions* lookup key.  Pairing
   wizard-name with session-id keeps a wizard-A session-id from hitting
   when looked up under a wizard-B route."
  (cons wizard-name session-id))

(defmacro with-wizard-state ((state &rest slots) &body body)
  "Bind each SLOT keyword from STATE to a like-named variable in the
   calling package for BODY.
     (with-wizard-state (st :id :wizard-name :current-step)
       (format t \"~A on step ~A of ~A~%\" id current-step wizard-name))"
  (let ((state-var (gensym "STATE-")))
    `(let ((,state-var ,state))
       (let ,(mapcar (lambda (slot)
                       (let ((var (intern (symbol-name slot))))
                         `(,var (funcall ,state-var ,slot))))
                     slots)
         ,@body))))

(defun get-wizard-session (wizard-name session-id)
  "Fetch the wizard state for (WIZARD-NAME . SESSION-ID), or NIL."
  (bordeaux-threads:with-recursive-lock-held (*wizard-sessions-lock*)
    (gethash (%wizard-session-key wizard-name session-id)
             *wizard-sessions*)))

(defun store-wizard-session (wizard-state)
  "Store WIZARD-STATE under (wizard-name . session-id)."
  (bordeaux-threads:with-recursive-lock-held (*wizard-sessions-lock*)
    (setf (gethash (%wizard-session-key
                    (funcall wizard-state :wizard-name)
                    (funcall wizard-state :id))
                   *wizard-sessions*)
          wizard-state)))

(defun remove-wizard-session (wizard-name session-id)
  "Remove the session for (WIZARD-NAME . SESSION-ID)."
  (bordeaux-threads:with-recursive-lock-held (*wizard-sessions-lock*)
    (remhash (%wizard-session-key wizard-name session-id)
             *wizard-sessions*)))

(defun cleanup-stale-sessions (&optional (max-age-seconds 3600))
  "Remove wizard sessions older than MAX-AGE-SECONDS (default 1 hour).
   Snapshots keys under lock, classifies stale entries on the snapshot
   outside the lock, then removes under lock.  Avoids re-entering the
   hash table inside a maphash callback while another thread mutates."
  (let* ((cutoff   (- (get-universal-time) max-age-seconds))
         (snapshot (bordeaux-threads:with-recursive-lock-held
                       (*wizard-sessions-lock*)
                     (let ((keys nil))
                       (maphash (lambda (key state)
                                  (declare (ignore state))
                                  (push key keys))
                                *wizard-sessions*)
                       keys)))
         (stale    (loop for key in snapshot
                         for state = (bordeaux-threads:with-recursive-lock-held
                                         (*wizard-sessions-lock*)
                                       (gethash key *wizard-sessions*))
                         for ts = (and state (funcall state :created-at))
                         when (and ts (< ts cutoff))
                           collect key)))
    (bordeaux-threads:with-recursive-lock-held (*wizard-sessions-lock*)
      (dolist (key stale)
        (remhash key *wizard-sessions*)))
    (length stale)))

;;; ============================================================================
;;; STEP RENDERING
;;; ============================================================================

(defun %render-step-form (steps current-step session-id can-back prev-data
                          show-progress extra-classes)
  "Render the wizard step body for placement inside a <form>.  SESSION-ID
   may be NIL on the initial-GET path; the absent hidden field steers the
   first POST through process-wizard-submission's auto-spawn branch."
  (let* ((step-spec (nth current-step steps))
         (step-title (getf step-spec :title))
         (step-form-fn (getf step-spec :form))
         (is-last (= current-step (1- (length steps)))))
    (htm-str
      (:div :class (classes "wizard" extra-classes)
        (when show-progress
          (cl-who:htm
            (:div :class (classes "wizard-progress" "mb-6")
              (:div :class (classes "flex" "justify-between" "mb-2")
                (dotimes (i (length steps))
                  (let* ((step-info (nth i steps))
                         (is-current (= i current-step))
                         (is-done (< i current-step)))
                    (cl-who:htm
                      (:div :class (classes "wizard-step-indicator" "text-center" "flex-1"
                                            (when is-current "text-primary" "font-bold")
                                            (when is-done "text-success"))
                        (cl-who:esc (getf step-info :title (format nil "Step ~A" (1+ i))))))))))))

        (when step-title
          (cl-who:htm
            (:h2 :class (classes "text-xl" "font-bold" "mb-4")
              (cl-who:esc step-title))))

        ;; CSRF token sits inside the form so the POST handler's
        ;; with-csrf-validation passes; the wrapping <form> is built by
        ;; defwizard's expansion or by manual callers.
        (cl-who:str (lol-web/server:csrf-token-input))

        (when session-id
          (cl-who:htm
            (:input :type "hidden" :name "wizard-session-id"
                    :value session-id)))

        (cl-who:str (aif step-form-fn
                         (funcall let-over-lambda:it prev-data)
                         ""))

        (:div :class (classes "wizard-nav" "flex" "justify-between" "mt-6" "pt-4" "border-t" "border-muted")
          (if can-back
              (cl-who:htm
                (:button :type "submit" :name "wizard-action" :value "back"
                         :class (classes "px-4" "py-2" "border" "border-muted" "rounded-md"
                                         "hover:bg-surface-alt")
                  "Back"))
              (cl-who:htm
                (:span)))
          (:button :type "submit" :name "wizard-action"
                   :value (if is-last "complete" "next")
                   :class (classes "px-4" "py-2" "bg-primary" "text-surface" "rounded-md"
                                   "hover:brightness-90")
            (cl-who:esc (if is-last "Complete" "Next"))))))))

(defun render-wizard-step (wizard-state &key (show-progress t) (extra-classes ""))
  "Render the current wizard step as HTML for placement inside a <form>."
  (with-wizard-state (wizard-state :wizard-name :current-step :id
                                   :can-go-back-p :step-name)
    (let* ((spec (get-wizard-spec wizard-name))
           (steps (getf spec :steps))
           (prev-data (funcall wizard-state :get-step-data step-name)))
      (%render-step-form steps current-step id can-go-back-p prev-data
                         show-progress extra-classes))))

(defun render-wizard-initial-step (wizard-name &key (show-progress t)
                                                    (extra-classes ""))
  "Render the first step of WIZARD-NAME without allocating session state.
   The form carries no hidden session-id; the POST auto-spawn branch in
   process-wizard-submission allocates wizard state on the first :next.
   Use this from the GET handler so a refresh or scrape does not leak
   orphaned sessions into the registry."
  (let* ((spec (get-wizard-spec wizard-name))
         (steps (getf spec :steps)))
    (unless spec
      (error "Wizard ~A not found" wizard-name))
    (%render-step-form steps 0 nil nil nil show-progress extra-classes)))

(defun render-wizard-complete (wizard-state result)
  "Render wizard completion page."
  (let ((wizard-name (funcall wizard-state :wizard-name)))
    (htm-str
      (:div :class (classes "wizard-complete" "text-center" "p-6")
        (:h2 :class (classes "text-xl" "font-bold" "mb-4" "text-success")
          "Complete!")
        (:p :class (classes "text-muted" "mb-4")
          "Your submission has been processed.")
        (when result
          (cl-who:htm
            (:div :class (classes "wizard-result" "p-4" "bg-surface-alt" "rounded-md")
              (cl-who:esc (princ-to-string result)))))))))

;;; ============================================================================
;;; WIZARD PROCESSING
;;; ============================================================================

(defun process-wizard-submission (wizard-name session-id action form-data)
  "Process a wizard form submission.
   ACTION: :next, :back, or :complete
   FORM-DATA: Plist of form field values
   Returns: (values :continue wizard-state) on advance or back,
            (values :complete result) on the final step,
            (values :error errors-or-403) on validation or hijack failure,
            (values :forbidden reason) on auto-spawn refusal, rate-limit
            denial, owner-token mismatch, or skip-to-complete attempt."
  (let* ((wizard-state (aif session-id
                            (get-wizard-session wizard-name let-over-lambda:it)
                            nil)))

    ;; Defense-in-depth on top of the namespaced session-key: refuse any
    ;; state whose own :wizard-name disagrees with the route's wizard.
    (when (and wizard-state
               (not (eq wizard-name (funcall wizard-state :wizard-name))))
      (return-from process-wizard-submission
        (values :forbidden "session does not belong to this wizard")))

    ;; Auto-spawn only on :next. :back / :complete require an existing
    ;; session — completion from an unknown session-id is the skip-to-end
    ;; bypass; back from no session has no meaning.
    (unless wizard-state
      (unless (eq action :next)
        (return-from process-wizard-submission
          (values :forbidden "wizard action requires an existing session")))
      (unless (%wizard-rate-limit-allows-p)
        (return-from process-wizard-submission
          (values :forbidden "wizard rate limit exceeded")))
      (let ((spec (get-wizard-spec wizard-name)))
        (unless spec
          (error "Wizard ~A not found" wizard-name))
        (setf wizard-state
              (make-wizard-state wizard-name (getf spec :steps)
                                 :owner-token (generate-owner-token)))
        (lol-web/server:session-set
         (wizard-owner-key (funcall wizard-state :id))
         (funcall wizard-state :owner-token))
        (store-wizard-session wizard-state)))

    ;; Reject submissions whose Lack session does not own this wizard.
    ;; Bypassed only when the wizard was created without an owner-token
    ;; (e.g., test fixtures opting out of session binding).
    (let ((expected (funcall wizard-state :owner-token))
          (presented (lol-web/server:session-get
                      (wizard-owner-key (funcall wizard-state :id)))))
      (when (and expected
                 (not (and presented
                           (lol-web/server:constant-time-string=
                            expected presented))))
        (return-from process-wizard-submission
          (values :forbidden "wizard session-id does not belong to this user"))))

    ;; Hold the per-instance lock across the whole save -> validate ->
    ;; mark-passed -> advance/complete compound so a concurrent same-session
    ;; POST cannot interleave between the step-skip gate and the advance.
    (bordeaux-threads:with-recursive-lock-held ((funcall wizard-state :lock))
      ;; Save current step data (excluding wizard control fields)
      (let ((clean-data (loop for (k v) on form-data by #'cddr
                              unless (member k '(:wizard-session-id :wizard-action))
                                collect k and collect v)))
        (funcall wizard-state :set-step-data clean-data))

      ;; Process action
      (case action
        (:back
         (funcall wizard-state :prev-step)
         (values :continue wizard-state))

        (:next
         (let* ((spec (get-wizard-spec wizard-name))
                (steps (getf spec :steps))
                (current-step (funcall wizard-state :current-step))
                (step-spec (nth current-step steps))
                (validator (getf step-spec :validate)))

           ;; Validate if validator exists
           (when validator
             (let ((errors (funcall validator (funcall wizard-state :get-step-data
                                                       (getf step-spec :name)))))
               (when errors
                 (return-from process-wizard-submission
                   (values :error errors)))))

           ;; Step validated (or had no validator). Record passage before
           ;; advancing so :complete can require every prior validator to
           ;; have run successfully.
           (funcall wizard-state :mark-step-passed current-step)
           (funcall wizard-state :next-step)
           (values :continue wizard-state)))

        (:complete
         (let* ((spec (get-wizard-spec wizard-name))
                (steps (getf spec :steps))
                (current-step (funcall wizard-state :current-step))
                (on-complete (getf spec :on-complete)))

           ;; :complete must arrive at the final step — never an earlier one.
           ;; Without this assertion, an attacker submits wizard-action=complete
           ;; from step 0 and runs on-complete with whatever step 0 happens to
           ;; hold, bypassing every intermediate validator.
           (unless (= current-step (1- (length steps)))
             (return-from process-wizard-submission
               (values :forbidden "complete arrived before final step")))

           ;; Every prior step must have passed its validator via :next.
           (unless (funcall wizard-state :prior-steps-passed-p current-step)
             (return-from process-wizard-submission
               (values :forbidden "complete requires every prior step to have passed")))

           ;; Validate the final step.
           (let* ((step-spec (nth current-step steps))
                  (validator (getf step-spec :validate)))
             (when validator
               (let ((errors (funcall validator (funcall wizard-state :get-step-data
                                                         (getf step-spec :name)))))
                 (when errors
                   (return-from process-wizard-submission
                     (values :error errors))))))

           (funcall wizard-state :mark-step-passed current-step)
           (funcall wizard-state :complete)
           (let* ((all-data (funcall wizard-state :all-data))
                  (result (when on-complete
                            (funcall on-complete all-data))))
             (remove-wizard-session wizard-name (funcall wizard-state :id))
             (values :complete result))))

        (t
         (values :continue wizard-state))))))

(defun start-wizard (wizard-name)
  "Start a new wizard session bound to the current user's Lack session.
   The wizard's owner-token is also written to that session under
   wizard-owner-key so process-wizard-submission can verify the
   submitter is the same user. Returns wizard state (a dlambda closure),
   or signals wizard-rate-limit-exceeded when the client exhausts the
   per-IP cap."
  (let* ((spec (get-wizard-spec wizard-name)))
    (unless spec
      (error "Wizard ~A not found. Did you call DEFWIZARD?" wizard-name))
    (unless (%wizard-rate-limit-allows-p)
      (error 'lol-web/server:http-forbidden
             :body "wizard rate limit exceeded"))
    (let* ((token (generate-owner-token))
           (wizard-state (make-wizard-state wizard-name (getf spec :steps)
                                            :owner-token token)))
      (lol-web/server:session-set
       (wizard-owner-key (funcall wizard-state :id))
       token)
      (store-wizard-session wizard-state)
      wizard-state)))

;;; ============================================================================
;;; DEFWIZARD MACRO
;;; ============================================================================

(defmacro defwizard (name () &key steps on-complete (internal t))
  "Define a multi-step wizard.

   NAME: Wizard identifier
   STEPS: List of step specifications:
     (:name STEP-NAME :title \"Step Title\"
      :form (lambda (prev-data) ...) ; Returns HTML string
      :validate (lambda (data) ...)) ; Returns NIL or error list
   ON-COMPLETE: Handler receiving all collected data as alist
   INTERNAL: Marks the auto-generated GET+POST routes :internal in
     lol-web/extractors:*handler-metadata*. lol-web/openapi:emit-openapi-json
     excludes :internal routes from the published surface — wizards rarely
     belong in a consumer-facing API spec, so the default is T. Pass
     :internal nil to include the wizard endpoints.

   Creates:
   - (start-wizard 'NAME) - Begin wizard, returns state
   - (render-wizard-step state) - Render current step
   - (process-wizard-submission 'NAME session-id action data) - Process form

   Example:
     (defwizard checkout ()
       :steps ((:name :address :title \"Shipping Address\"
                :form (lambda (data) (address-form-html data))
                :validate #'validate-address)
               (:name :payment :title \"Payment Method\"
                :form (lambda (data) (payment-form-html data))
                :validate #'validate-payment)
               (:name :confirm :title \"Confirm Order\"
                :form (lambda (data) (confirmation-html data))))
       :on-complete (lambda (data) (process-order data)))"

  ;; Validate step structure at compile time
  (dolist (step steps)
    (unless (getf step :name)
      (error "Each wizard step must have a :name"))
    (unless (getf step :form)
      (error "Each wizard step must have a :form function")))

  (let ((route-path (format nil "/wizard/~A" (string-downcase name))))
    `(progn
       ;; Register wizard specification
       (register-wizard ',name
                        (list :steps (list ,@(mapcar (lambda (step)
                                                       `(list :name ,(getf step :name)
                                                              :title ,(getf step :title)
                                                              :form ,(getf step :form)
                                                              :validate ,(getf step :validate)))
                                                     steps))
                              :on-complete ,on-complete))

       ;; Surface the wizard's GET+POST routes in *handler-metadata* so the
       ;; OpenAPI emitter sees them and the :internal flag can take effect.
       ;; Extractors are NIL because defwizard reads form data directly via
       ;; post-param rather than the extractor protocol.
       (%register-wizard-handler-metadata ',name ,route-path ,internal)

       ;; GET renders the first step without allocating state; the POST
       ;; auto-spawn branch in process-wizard-submission creates the
       ;; wizard-state on the first :next.
       (defroute ,route-path (:method :get)
         (htm-str
           (:form :method "POST" :action ,route-path
             (cl-who:str (render-wizard-initial-step ',name)))))

       (defroute ,route-path (:method :post)
         (lol-web/server:with-csrf-validation
           (let* ((session-id (post-param "wizard-session-id"))
                  (action (or (safe-coerce-keyword
                               (or (post-param "wizard-action") "next")
                               :allowed '(:next :back :complete))
                              :next))
                  (form-data (loop for (k . v) in (post-params)
                                   for key = (safe-coerce-keyword k)
                                   when key collect key
                                   when key collect v)))
             (multiple-value-bind (status result)
                 (process-wizard-submission ',name session-id action form-data)
               (case status
                 (:continue
                  (htm-str
                    (:form :method "POST" :action ,route-path
                      (cl-who:str (render-wizard-step result)))))
                 (:complete
                  (render-wizard-complete result result))
                 (:error
                  ;; Re-render the same step's form below the error
                  ;; banner so the user retains the Back button and the
                  ;; session-id field needed to correct and resubmit.
                  (let ((wizard-state (get-wizard-session ',name session-id)))
                    (htm-str
                      (:form :method "POST" :action ,route-path
                        (:div :class "wizard-errors"
                          (:ul
                            (dolist (err result)
                              (cl-who:htm (:li (cl-who:esc err))))))
                        (when wizard-state
                          (cl-who:str (render-wizard-step wizard-state)))))))
                 (:forbidden
                  (error 'lol-web/server:http-forbidden :body result)))))))

       ',name)))

;;; ============================================================================
;;; WIZARD STEP HELPERS
;;; ============================================================================

(defun wizard-text-field (name &key label placeholder value required)
  "Generate a text input field for wizard forms."
  (let ((field-id (format nil "wizard-~A" (string-downcase name)))
        (field-name (string-downcase name)))
    (htm-str
      (:div :class (classes "mb-4")
        (when label
          (cl-who:htm
            (:label :for field-id :class (classes "block" "mb-2" "font-medium")
              (cl-who:esc label)
              (when required
                (cl-who:htm (:span :class "text-error" "*"))))))
        (:input :type "text"
                :id field-id
                :name field-name
                :class (classes "w-full" "p-2" "border" "border-muted" "rounded-md")
                :placeholder (or placeholder "")
                :value (or value "")
                :required (when required "required"))))))

(defun wizard-select-field (name options &key label value required)
  "Generate a select dropdown for wizard forms.
   OPTIONS: List of (value . label) pairs or just values"
  (let ((field-id (format nil "wizard-~A" (string-downcase name)))
        (field-name (string-downcase name)))
    (htm-str
      (:div :class (classes "mb-4")
        (when label
          (cl-who:htm
            (:label :for field-id :class (classes "block" "mb-2" "font-medium")
              (cl-who:esc label)
              (when required
                (cl-who:htm (:span :class "text-error" "*"))))))
        (:select :id field-id
                 :name field-name
                 :class (classes "w-full" "p-2" "border" "border-muted" "rounded-md")
                 :required (when required "required")
          (dolist (opt options)
            (let* ((opt-value (if (consp opt) (car opt) opt))
                   (opt-label (if (consp opt) (cdr opt) opt))
                   (selected (equal (princ-to-string opt-value)
                                    (princ-to-string value))))
              (cl-who:htm
                (:option :value (princ-to-string opt-value)
                         :selected (when selected "selected")
                  (cl-who:esc (princ-to-string opt-label)))))))))))

(defun wizard-radio-group (name options &key label value required)
  "Generate a radio button group for wizard forms."
  (let ((field-name (string-downcase name)))
    (htm-str
      (:div :class (classes "mb-4")
        (when label
          (cl-who:htm
            (:p :class (classes "mb-2" "font-medium")
              (cl-who:esc label)
              (when required
                (cl-who:htm (:span :class "text-error" "*"))))))
        (:div :class (classes "space-y-2")
          (dolist (opt options)
            (let* ((opt-value (if (consp opt) (car opt) opt))
                   (opt-label (if (consp opt) (cdr opt) opt))
                   (opt-id (format nil "wizard-~A-~A" field-name opt-value))
                   (checked (equal (princ-to-string opt-value)
                                   (princ-to-string value))))
              (cl-who:htm
                (:label :class (classes "flex" "items-center" "gap-2" "cursor-pointer")
                  (:input :type "radio"
                          :id opt-id
                          :name field-name
                          :value (cl-who:esc (princ-to-string opt-value))
                          :checked (when checked "checked")
                          :required (when required "required"))
                  (cl-who:esc (princ-to-string opt-label)))))))))))

;;; ============================================================================
;;; WIZARD INTROSPECTION
;;; ============================================================================

(defun inspect-wizard (name)
  "Return introspection data for a wizard."
  (let ((spec (get-wizard-spec name)))
    (when spec
      (list :name name
            :steps (mapcar (lambda (s)
                             (list :name (getf s :name)
                                   :title (getf s :title)
                                   :has-validator (not (null (getf s :validate)))))
                           (getf spec :steps))
            :has-completion-handler (not (null (getf spec :on-complete)))))))

(defun list-active-wizard-sessions ()
  "Return inspection plists for every active wizard session."
  (bordeaux-threads:with-recursive-lock-held (*wizard-sessions-lock*)
    (let (sessions)
      (maphash (lambda (key state)
                 (declare (ignore key))
                 (push (funcall state :inspect) sessions))
               *wizard-sessions*)
      (nreverse sessions))))
