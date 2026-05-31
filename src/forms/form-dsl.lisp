;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/FORMS; Base: 10 -*-
;;;; forms/form-dsl.lisp - Type-Safe Form DSL
;;;;
;;;; PURPOSE:
;;;;   Define type-safe forms with validation, CSRF protection, and client-side JS.
;;;;
;;;; KEY MACRO:
;;;;   DEFFORM - Define a form with field specifications and handlers
;;;;
;;;; GENERATED OUTPUT:
;;;;   - Server-side validation function
;;;;   - HTML form rendering function
;;;;   - Client-side Parenscript validation
;;;;   - Form registry for introspection

(in-package :lol-web/forms)

;;; ============================================================================
;;; FORM REGISTRY
;;; ============================================================================

(defvar *forms* (make-hash-table :test 'eq)
  "Registry of defined forms.")

(defparameter *form-pattern-max-length* 256
  "Reject DEFFORM :PATTERN strings longer than this. Unbounded regex
   strings are a cheap DoS vector — 256 is well above any legitimate
   field-validation pattern.")

(defparameter *form-pattern-compile-timeout-seconds* 0.1
  "Wallclock cap on CL-PPCRE:CREATE-SCANNER calls for DEFFORM :PATTERN
   compiles. Catastrophic-backtracking regex authors get bounded CPU;
   legitimate patterns compile in microseconds.")

(defparameter *form-pattern-cache-max-entries* 1024
  "Element-count cap on *FORM-PATTERN-SCANNER-CACHE*. DEFFORM-declared
   patterns are few, but hand-crafted specs passed to REGISTER-FORM at
   runtime bypass the macro and can cycle distinct pattern strings; the
   bound turns unbounded cache growth into LRU eviction.")

(defvar *form-pattern-scanner-cache*
  (lol-web/core:make-bounded-cache :max-entries *form-pattern-cache-max-entries*
                                   :test 'equal)
  "Memoization cache mapping pattern-string -> compiled CL-PPCRE scanner.
   Populated lazily by %GET-FORM-PATTERN-SCANNER. A bounded LRU cache so a
   stream of distinct runtime-registered patterns evicts rather than grows
   without limit.")

(defparameter *form-email-max-length* 254
  "Server-side cap for :EMAIL fields before regex validation.")

(defparameter *form-url-max-length* 2048
  "Server-side cap for :URL fields before regex validation.")

(defun %compile-form-pattern-bounded (pattern)
  "Compile PATTERN to a CL-PPCRE scanner, refusing strings beyond
   *FORM-PATTERN-MAX-LENGTH* and aborting compilations beyond
   *FORM-PATTERN-COMPILE-TIMEOUT-SECONDS*. Signals on length, timeout,
   or malformed-regex; the DEFFORM macro re-signals at the call site so
   the misconfiguration surfaces at form-definition time, not at the
   first request that hits the bad field."
  (unless (stringp pattern)
    (error "form-dsl :pattern must be a string; got ~S" pattern))
  (when (> (length pattern) *form-pattern-max-length*)
    (error "form-dsl :pattern length ~D exceeds *form-pattern-max-length* ~D"
           (length pattern) *form-pattern-max-length*))
  ;; Off SBCL there is no interruptible compile timer — refuse rather than run
  ;; cl-ppcre:create-scanner unbounded on a catastrophic-backtracking pattern.
  #-sbcl
  (error "form-dsl :pattern ~S compile cannot be wallclock-bounded off SBCL; ~
          refusing" pattern)
  #+sbcl
  (handler-case
      (sb-ext:with-timeout *form-pattern-compile-timeout-seconds*
        (cl-ppcre:create-scanner pattern))
    (sb-ext:timeout ()
      (error "form-dsl :pattern ~S compile exceeded ~A seconds"
             pattern *form-pattern-compile-timeout-seconds*))
    (error (e)
      (error "form-dsl :pattern ~S is not a valid regex: ~A" pattern e))))

(defun %get-form-pattern-scanner (pattern)
  "Return the cached CL-PPCRE scanner for PATTERN, compiling under the
   bounded primitive on first use. The cache is keyed by pattern string
   and bounded by *FORM-PATTERN-CACHE-MAX-ENTRIES*; DEFFORM macro-expansion
   ensures the cache only ever holds bounded-length, bounded-cost scanners."
  (multiple-value-bind (scanner present-p)
      (lol-web/core:bounded-cache-get *form-pattern-scanner-cache* pattern)
    (if present-p
        scanner
        (lol-web/core:bounded-cache-set *form-pattern-scanner-cache* pattern
                                        (%compile-form-pattern-bounded pattern)))))

(define-condition unsafe-form-field-name (error)
  ((name :initarg :name :reader unsafe-form-field-name-name))
  (:report (lambda (c stream)
             (format stream "Unsafe form field name ~S: must be an ASCII letter followed by ASCII letters, digits, `-`, or `_`."
                     (unsafe-form-field-name-name c)))))

(defun %safe-form-field-name-p (name)
  "True when NAME prints as a conservative field-name token: an ASCII
   letter followed by ASCII letters, digits, `-`, or `_`. The name flows
   into an HTML attribute, a CSS/JS `[name='...']` selector, and an interned
   keyword, so a quote, bracket, or whitespace must never reach it."
  (flet ((alpha (c) (or (char<= #\a c #\z) (char<= #\A c #\Z)))
         (alnum (c) (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9))))
    (let ((s (string name)))
      (and (plusp (length s))
           (alpha (char s 0))
           (every (lambda (c) (or (alnum c) (char= c #\-) (char= c #\_))) s)))))

(defun register-form (name spec)
  "Register a form specification for later rendering and validation.
   Every field name is checked against the safe token class so it cannot
   break out of the attribute / selector / keyword sinks it reaches."
  (dolist (field-spec (getf spec :fields))
    (let ((field-name (car field-spec)))
      (unless (%safe-form-field-name-p field-name)
        (error 'unsafe-form-field-name :name field-name))))
  (setf (gethash name *forms*) (copy-tree spec)))

(defun get-form-spec (name)
  "Retrieve a registered form specification."
  (let ((spec (gethash name *forms*)))
    (when spec (copy-tree spec))))

(defun list-forms ()
  "List all registered forms."
  (let (forms)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k forms))
             *forms*)
    (nreverse forms)))

;;; ============================================================================
;;; FIELD TYPES AND INPUT GENERATION
;;; ============================================================================

(defparameter *field-type-to-input*
  '((:string . "text")
    (:text . "textarea")
    (:email . "email")
    (:password . "password")
    (:number . "number")
    (:integer . "number")
    (:tel . "tel")
    (:url . "url")
    (:date . "date")
    (:time . "time")
    (:datetime . "datetime-local")
    (:checkbox . "checkbox")
    (:hidden . "hidden")
    (:file . "file")
    (:color . "color")
    (:range . "range"))
  "Mapping from field type keywords to HTML input types.")

(defun field-type-to-html-input (field-type)
  "Convert field type keyword to HTML input type string."
  (or (cdr (assoc field-type *field-type-to-input*))
      "text"))

(defun generate-input-element (field-type input-type field-id field-name input-class
                                value required placeholder min-val max-val minlength maxlength)
  "Generate the input or textarea element HTML.
   Uses html-attrs helper for clean conditional attribute handling.

   Note: Uses format+html-attrs rather than htm-str because cl-who doesn't
   support runtime conditional attributes (,@ splicing only works at macro
   expansion time). This pattern maintains proper escaping via html-attrs."
  (if (eq field-type :text)
      ;; Textarea for :text type
      (format nil "<textarea~A>~A</textarea>"
              (html-attrs "id" field-id
                          "name" field-name
                          "class" input-class
                          "required" (when required t)
                          "minlength" minlength
                          "maxlength" maxlength)
              (escape-html (if value (princ-to-string value) "")))
      ;; Regular input (self-closing)
      (format nil "<input~A/>"
              (html-attrs "type" input-type
                          "id" field-id
                          "name" field-name
                          "class" input-class
                          "value" value
                          "required" (when required t)
                          "placeholder" placeholder
                          "min" (when (member field-type '(:number :integer :range)) min-val)
                          "max" (when (member field-type '(:number :integer :range)) max-val)
                          "minlength" minlength
                          "maxlength" maxlength))))

(defun generate-input-html (field-spec &key (value nil) (errors nil))
  "Generate HTML for a single form field using Tailwind classes.
   FIELD-SPEC: (name :type TYPE :min MIN :max MAX :required BOOL :placeholder STR :label STR)
   VALUE: Current value for the field
   ERRORS: List of validation errors for this field"
  (let* ((name (car field-spec))
         (plist (cdr field-spec))
         (field-type (getf plist :type :string))
         (input-type (field-type-to-html-input field-type))
         (required (getf plist :required))
         (min-val (getf plist :min))
         (max-val (getf plist :max))
         (minlength (getf plist :minlength (when (member field-type '(:string :text :password))
                                             min-val)))
         (maxlength (getf plist :maxlength (when (member field-type '(:string :text :password))
                                             max-val)))
         (placeholder (getf plist :placeholder))
         (label (getf plist :label (string-capitalize (substitute #\Space #\- (string-downcase name)))))
         (field-id (format nil "field-~A" (string-downcase name)))
         (field-name (string-downcase name))
         (has-errors (and errors (listp errors)))
         ;; Tailwind classes
         (container-class (classes "mb-4" (when has-errors "has-errors")))
         (label-class (classes "block" "mb-2" "font-medium" "text-text"))
         (input-class (classes "w-full" "p-2" "border" "rounded-md" "text-text" "bg-surface"
                               (if has-errors "border-error" "border-muted")
                               "focus:outline-none" "focus:border-primary" "focus:ring-2" "focus:ring-primary/20"))
         (error-class (classes "block" "text-error" "text-sm" "mt-1"))
         (required-class "text-error"))

    (htm-str
      (:div :class container-class
        ;; Label
        (:label :for field-id :class label-class
          (cl-who:esc label)
          (when required
            (cl-who:htm " " (:span :class required-class "*"))))
        ;; Input element (via helper)
        (cl-who:str (generate-input-element field-type input-type field-id field-name input-class
                                            value required placeholder min-val max-val minlength maxlength))
        ;; Error messages
        (when has-errors
          (dolist (err errors)
            (cl-who:htm (:span :class error-class (cl-who:esc err)))))))))

;;; ============================================================================
;;; SERVER-SIDE VALIDATION
;;; ============================================================================

(defun %parse-form-number (value integerp)
  "Parse VALUE (a string) as a form number. When INTEGERP, accept only a
   whole integer with no trailing junk. Otherwise accept an integer or
   decimal/exponent literal. Returns the number, or NIL when VALUE is not a
   valid numeric literal — unlike PARSE-INTEGER with :junk-allowed t, this
   never silently truncates `\"3.9\"` to 3 or accepts `\"12abc\"` as 12, so
   the server agrees with the client's numeric validity."
  (if integerp
      (ignore-errors (parse-integer value :junk-allowed nil))
      (when (lol-web/escape:%scan-bounded
             "^[+-]?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$"
             value)
        (let ((*read-eval* nil)
              (*read-default-float-format* 'double-float))
          (let ((n (ignore-errors (read-from-string value))))
            (and (realp n) n))))))

(defun validate-field (field-spec value)
  "Validate a single field value against its specification.
   Returns NIL if valid, or a list of error messages."
  (let* ((name (car field-spec))
         (plist (cdr field-spec))
         (field-type (getf plist :type :string))
         (required (getf plist :required))
         (min-val (getf plist :min))
         (max-val (getf plist :max))
         (pattern (getf plist :pattern))
         (custom-validator (getf plist :validate))
         (errors nil))

    ;; Required check
    (when (and required (or (null value) (equal value "")))
      (push (format nil "~A is required" (string-capitalize (string-downcase name))) errors))

    ;; Only validate non-empty values further
    (when (and value (not (equal value "")))
      ;; Type-specific validation
      (case field-type
        (:email
         (cond
           ((> (length value) *form-email-max-length*)
            (push (format nil "Must be at most ~D characters"
                          *form-email-max-length*)
                  errors))
           ((not (lol-web/escape:%scan-bounded "^[^@]+@[^@]+\\.[^@]+$" value))
            (push "Invalid email address" errors))))

        (:url
         (cond
           ((> (length value) *form-url-max-length*)
            (push (format nil "Must be at most ~D characters"
                          *form-url-max-length*)
                  errors))
           ((not (lol-web/escape:%scan-bounded "^https?://" value))
            (push "Invalid URL (must start with http:// or https://)" errors))))

        ((:number :integer)
         (let ((num (%parse-form-number value (eq field-type :integer))))
           (cond
             ((null num)
              (push "Must be a number" errors))
             ((and min-val (< num min-val))
              (push (format nil "Must be at least ~A" min-val) errors))
             ((and max-val (> num max-val))
              (push (format nil "Must be at most ~A" max-val) errors)))))

        ((:string :text :password)
         (let ((len (length value)))
           (when (and min-val (< len min-val))
             (push (format nil "Must be at least ~A characters" min-val) errors))
           (when (and max-val (> len max-val))
             (push (format nil "Must be at most ~A characters" max-val) errors)))))

      ;; Pattern validation against the cached scanner. The pattern string
      ;; was already length-and-compile-bounded by DEFFORM's macro-expansion
      ;; (or by %GET-FORM-PATTERN-SCANNER's bounded compile on first hit
      ;; for hand-crafted specs that bypass the macro).
      (when (and pattern (stringp value))
        (unless (lol-web/escape:%scan-bounded (%get-form-pattern-scanner pattern) value)
          (push "Invalid format" errors)))

      ;; Custom validator
      (when custom-validator
        (let ((custom-result (funcall custom-validator value)))
          (when custom-result
            (if (stringp custom-result)
                (push custom-result errors)
                (push "Invalid value" errors))))))

    (nreverse errors)))

(defun validate-form-data (form-name data)
  "Validate form data against form specification.
   DATA: Plist of field-name -> value
   Returns (values valid-p errors-alist)"
  (let* ((spec (get-form-spec form-name))
         (fields (getf spec :fields))
         (all-errors nil)
         (valid t))
    (dolist (field-spec fields)
      (let* ((name (car field-spec))
             (value (getf data (intern (string-upcase name) :keyword)))
             (field-errors (validate-field field-spec value)))
        (when field-errors
          (setf valid nil)
          (push (cons name field-errors) all-errors))))
    (values valid (nreverse all-errors))))

(defun %form-csrf-valid-p (data)
  "Return T when form submission DATA satisfies the current session CSRF gate.
   Programmatic calls outside a request/session context stay valid."
  (let ((session (and lol-web/server:*env*
                      (getf lol-web/server:*env* :lack.session))))
    (or (not session)
        (validate-csrf-token (getf data :csrf-token)))))

;;; ============================================================================
;;; CLIENT-SIDE VALIDATION (PARENSCRIPT)
;;; ============================================================================

(defun generate-field-validation-js (field-spec)
  "Generate Parenscript validation for a single field."
  (let* ((name (car field-spec))
         (plist (cdr field-spec))
         (field-type (getf plist :type :string))
         (required (getf plist :required))
         (min-val (getf plist :min))
         (max-val (getf plist :max))
         (field-name (string-downcase name))
         (checks nil))

    ;; Required check
    (when required
      (push `(when (or (null value) (equal value ""))
               (push ,(format nil "~A is required" (string-capitalize field-name)) errors))
            checks))

    ;; Length checks for strings
    (when (and min-val (member field-type '(:string :text :password)))
      (push `(when (and value (< (ps:@ value length) ,min-val))
               (push ,(format nil "Must be at least ~A characters" min-val) errors))
            checks))

    (when (and max-val (member field-type '(:string :text :password)))
      (push `(when (and value (> (ps:@ value length) ,max-val))
               (push ,(format nil "Must be at most ~A characters" max-val) errors))
            checks))

    ;; Numeric checks
    (when (member field-type '(:number :integer))
      (push `(when (and value (not (equal value "")) (is-na-n (parse-float value)))
               (push "Must be a number" errors))
            checks)
      (when min-val
        (push `(when (and value (not (is-na-n (parse-float value)))
                          (< (parse-float value) ,min-val))
                 (push ,(format nil "Must be at least ~A" min-val) errors))
              checks))
      (when max-val
        (push `(when (and value (not (is-na-n (parse-float value)))
                          (> (parse-float value) ,max-val))
                 (push ,(format nil "Must be at most ~A" max-val) errors))
              checks)))

    ;; Email check
    (when (eq field-type :email)
      (push `(when (and value (not (equal value ""))
                        (not (ps:chain (ps:new (-Reg-Exp "^[^@]+@[^@]+\\.[^@]+$")) (test value))))
                 (push "Invalid email address" errors))
            checks))

    `(lambda (value)
       (let ((errors (list)))
         ,@(nreverse checks)
         errors))))

(defun generate-form-validation-js (form-name)
  "Generate complete client-side validation script for a form."
  (let* ((spec (get-form-spec form-name))
         (fields (getf spec :fields))
         (form-id (format nil "form-~A" (string-downcase form-name))))
    (parenscript:ps*
     `(progn
        (defvar ,(symb "*" form-name "-VALIDATORS*")
          (ps:create
           ,@(mapcan (lambda (field-spec)
                       (list (intern (string-downcase (car field-spec)) :keyword)
                             (generate-field-validation-js field-spec)))
                     fields)))

        ((ps:@ document add-event-listener) "DOMContentLoaded"
         (lambda ()
           (let ((form ((ps:@ document get-element-by-id) ,form-id)))
             (when form
               ;; Validate on submit
               ((ps:@ form add-event-listener) "submit"
                (lambda (e)
                  (let ((valid t)
                        (validators ,(symb "*" form-name "-VALIDATORS*")))
                    ;; Validate each field
                    ,@(mapcar (lambda (field-spec)
                                (let* ((name (car field-spec))
                                       (field-name (string-downcase name))
                                       (field-key (intern field-name :keyword)))
                                  `(let* ((input ((ps:@ form query-selector) ,(format nil "[name='~A']" field-name)))
                                          (value (if input (ps:@ input value) ""))
                                          (errors ((ps:@ validators ,field-key) value))
                                          (container ((ps:@ input closest) ".form-field")))
                                     (when (and errors (> (ps:@ errors length) 0))
                                       (setf valid nil)
                                       (when container
                                         ((ps:@ container class-list add) "has-errors")
                                         ;; Remove old error messages
                                         (let ((old-errors ((ps:@ container query-selector-all) ".field-error")))
                                           ((ps:@ old-errors for-each) (lambda (el) ((ps:@ el remove)))))
                                         ;; Add new error messages
                                         ((ps:@ errors for-each)
                                          (lambda (msg)
                                            (let ((err-el ((ps:@ document create-element) "span")))
                                              (setf (ps:@ err-el class-name) "field-error")
                                              (setf (ps:@ err-el text-content) msg)
                                              ((ps:@ container append-child) err-el)))))))))
                              fields)
                    (unless valid
                      ((ps:@ e prevent-default))))))
               ;; Clear errors on input
               ((ps:@ form add-event-listener) "input"
                (lambda (e)
                  (let ((container ((ps:@ (ps:@ e target) closest) ".form-field")))
                    (when container
                      ((ps:@ container class-list remove) "has-errors")
                      (let ((errors ((ps:@ container query-selector-all) ".field-error")))
                        ((ps:@ errors for-each) (lambda (el) ((ps:@ el remove))))))))
                t)))))))))

;;; ============================================================================
;;; FORM RENDERING
;;; ============================================================================

(defun render-form-content (fields values errors include-csrf method actions-class button-class submit-text)
  "Render the inner content of a form (CSRF, fields, submit button).
   Helper for render-form."
  (htm-str
    ;; CSRF token
    (when (and include-csrf (string-equal method "POST"))
      (cl-who:str (csrf-token-input)))
    ;; Fields
    (dolist (field-spec fields)
      (let* ((name (car field-spec))
             (value (getf values (intern (string-upcase name) :keyword)))
             (field-errors (cdr (assoc name errors :test #'string-equal))))
        (cl-who:str (generate-input-html field-spec :value value :errors field-errors))))
    ;; Submit button
    (:div :class actions-class
      (:button :type "submit" :class button-class
        (cl-who:esc submit-text)))))

(defun render-form (form-name &key (action nil) (method "POST") (values nil) (errors nil)
                                   (submit-text "Submit") (include-csrf t) (extra-classes ""))
  "Render a registered form as HTML with Tailwind classes.
   ACTION: Form action URL (default: current URL)
   METHOD: HTTP method (default: POST)
   VALUES: Plist of field values to pre-fill
   ERRORS: Alist of (field-name . error-list) from validation
   SUBMIT-TEXT: Text for submit button
   INCLUDE-CSRF: Include CSRF token hidden field
   EXTRA-CLASSES: Additional Tailwind classes for form element"
  (let* ((spec (get-form-spec form-name))
         (fields (getf spec :fields))
         (form-id (format nil "form-~A" (string-downcase form-name)))
         ;; multipart/form-data is required for browsers to transmit
         ;; file-upload bodies; without it the file input submits only
         ;; the filename string.
         (has-file-field (some (lambda (f) (eq (getf (cdr f) :type) :file)) fields))
         (enctype (when has-file-field "multipart/form-data"))
         ;; Tailwind classes
         (form-class (classes "max-w-md" extra-classes))
         (actions-class "mt-6")
         (button-class (classes "px-4" "py-2" "bg-primary" "text-surface" "rounded-md"
                                "cursor-pointer" "hover:brightness-90")))
    (unless spec
      (error "Form ~A not found. Did you call DEFFORM?" form-name))

    (concatenate 'string
      ;; Form tag with conditional action attribute (uses html-attrs for consistency)
      (format nil "<form~A>"
              (html-attrs "id" form-id
                          "class" form-class
                          "action" action
                          "method" method
                          "enctype" enctype))
      ;; Form content via htm-str
      (render-form-content fields values errors include-csrf method actions-class button-class submit-text)
      "</form>"
      ;; Validation script
      (htm-str
        (:script (cl-who:str (generate-form-validation-js form-name)))))))

;;; ============================================================================
;;; DEFFORM MACRO
;;; ============================================================================

(defmacro defform (name () &key fields on-submit on-error)
  "Define a type-safe form with validation.

   NAME: Form identifier
   FIELDS: List of field specifications:
     (field-name :type TYPE :min MIN :max MAX :required BOOL
                 :placeholder STR :label STR :pattern REGEX :validate FN)
   ON-SUBMIT: Handler receiving validated field values as keyword arguments
   ON-ERROR: Handler receiving validation errors alist

   Supported field types:
     :string :text :email :password :number :integer
     :tel :url :date :time :datetime :checkbox :hidden :file :color :range

   Creates:
   - (render-form 'NAME ...) - Render form HTML
   - (validate-form-data 'NAME data) - Server-side validation
   - (process-NAME-submission request) - Handle form submission

   Example:
     (defform user-registration ()
       :fields ((username :type :string :min 3 :max 20 :required t)
                (email :type :email :required t)
                (password :type :password :min 8)
                (age :type :integer :min 18 :max 120))
       :on-submit (register-user :username username :email email
                                 :password password :age age)
       :on-error (show-validation-errors errors))"
  (dolist (field-spec fields)
    (let ((pattern (getf (cdr field-spec) :pattern)))
      (when pattern
        ;; Validate at macro-expand: a too-long or pathological pattern
        ;; signals here rather than at the first request that hits the
        ;; field. The compiled scanner is discarded — VALIDATE-FIELD
        ;; recomputes (and caches) it via %GET-FORM-PATTERN-SCANNER, so
        ;; the cached scanner uses the application's runtime parameters
        ;; rather than whatever value of *FORM-PATTERN-MAX-LENGTH* was
        ;; bound during macro-expansion.
        (%compile-form-pattern-bounded pattern))))
  (let* ((field-names (mapcar #'car fields))
         (process-fn-name (symb "PROCESS-" name "-SUBMISSION"))
         (g-data (gensym "REQUEST-DATA-"))
         (g-valid (gensym "VALID-"))
         (g-errors (gensym "ERRORS-")))
    (flet ((field-let-bindings (data-var)
             (mapcar (lambda (fname)
                       `(,fname (getf ,data-var
                                      ,(intern (string-upcase fname) :keyword))))
                     field-names)))
      `(progn
         ;; Register form spec. :on-submit / :on-error are compiled to closures
         ;; at registration so the registry holds executable code, not source
         ;; forms — and the bookkeeping bindings (gensymed below) can't be
         ;; shadowed by a field named `errors` or `request-data`.
         (register-form ',name
                        (list :fields ',fields
                              :on-submit
                              (lambda (,g-data)
                                (let* ,(field-let-bindings g-data)
                                  ,on-submit))
                              :on-error
                              ,(when on-error
                                 `(lambda (errors) ,on-error))))

         ;; Submission handler delegates to the registered closures.
         (defun ,process-fn-name (,g-data)
           "Process form submission with validation.
            Returns: result of :on-submit (when valid) or :on-error (when invalid),
            falling back to the errors alist when no :on-error was supplied."
           (unless (%form-csrf-valid-p ,g-data)
             (error 'http-forbidden :body "Invalid or missing CSRF token"))
           (multiple-value-bind (,g-valid ,g-errors)
               (validate-form-data ',name ,g-data)
             (if ,g-valid
                 (funcall (getf (get-form-spec ',name) :on-submit) ,g-data)
                 ,(if on-error
                      `(funcall (getf (get-form-spec ',name) :on-error) ,g-errors)
                      g-errors))))

         ',name))))

;;; ============================================================================
;;; FORM STYLES
;;; ============================================================================

(defun form-styles-css ()
  "OPTIONAL: CSS for projects NOT using Tailwind.
   The default render-form uses Tailwind classes. This function provides
   fallback CSS for the .has-errors class used by client-side validation.
   Only needed if you're not using Tailwind CDN."
  (flet ((p (s) (make-safe-css-payload-string s)))
    (concatenate 'string
      (css-section (p "Form Container")
        (p (css-rule ".lol-form"
                     `(("max-width" . "500px")))))
      (format nil "~%")
      (css-section (p "Form Fields")
        (p (css-rule ".form-field"
                     `(("margin-bottom" . ,(css-var "spacing-4")))))
        (p (css-rule ".form-field label"
                     `(("display" . "block")
                       ("margin-bottom" . ,(css-var "spacing-2"))
                       ("font-weight" . "500")
                       ("color" . ,(css-var "color-text")))))
        (p (css-rule ".form-field input, .form-field textarea"
                     `(("width" . "100%")
                       ("padding" . ,(css-var "spacing-2"))
                       ("border" . ,(format nil "1px solid ~A" (css-var "color-muted")))
                       ("border-radius" . ,(css-var "radius-md"))
                       ("font-size" . "1rem")
                       ("background" . ,(css-var "color-surface"))
                       ("color" . ,(css-var "color-text")))))
        (p (css-rule ".form-field input:focus, .form-field textarea:focus"
                     `(("outline" . "none")
                       ("border-color" . ,(css-var "color-primary"))
                       ("box-shadow" . ,(format nil "0 0 0 2px color-mix(in srgb, ~A 20%, transparent)"
                                                (css-var "color-primary")))))))
      (format nil "~%")
      (css-section (p "Form Validation States")
        (p (css-rule ".form-field.has-errors input, .form-field.has-errors textarea"
                     `(("border-color" . ,(css-var "color-error")))))
        (p (css-rule ".form-field .required"
                     `(("color" . ,(css-var "color-error")))))
        (p (css-rule ".form-field .field-error"
                     `(("display" . "block")
                       ("color" . ,(css-var "color-error"))
                       ("font-size" . "0.875rem")
                       ("margin-top" . ,(css-var "spacing-1"))))))
      (format nil "~%")
      (css-section (p "Form Actions")
        (p (css-rule ".form-actions"
                     `(("margin-top" . ,(css-var "spacing-6")))))
        (p (css-rule ".btn"
                     `(("padding" . ,(format nil "~A ~A" (css-var "spacing-2") (css-var "spacing-4")))
                       ("border" . "none")
                       ("border-radius" . ,(css-var "radius-md"))
                       ("cursor" . "pointer")
                       ("font-size" . "1rem"))))
        (p (css-rule ".btn-primary"
                     `(("background" . ,(css-var "color-primary"))
                       ("color" . ,(css-var "color-surface")))))
        (p (css-rule ".btn-primary:hover"
                     `(("filter" . "brightness(0.9)"))))))))

;;; ============================================================================
;;; FORM INTROSPECTION
;;; ============================================================================

(defun inspect-form (name)
  "Return introspection data for a form."
  (let ((spec (get-form-spec name)))
    (when spec
      (list :name name
            :fields (mapcar (lambda (f)
                              (list :name (car f)
                                    :type (getf (cdr f) :type :string)
                                    :required (getf (cdr f) :required)))
                            (getf spec :fields))
            :has-submit-handler (not (null (getf spec :on-submit)))
            :has-error-handler (not (null (getf spec :on-error)))))))
