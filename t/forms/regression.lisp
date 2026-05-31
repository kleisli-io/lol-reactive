;;;; Regression test for forms/form-dsl.lisp Parenscript symbol-concat typos.
;;;;
;;;; (ps:@ elremove) emits elremove() instead of el.remove();
;;;; (ps:@ eprevent-default) emits epreventDefault() instead of e.preventDefault();
;;;; (ps:@ etarget) emits etarget instead of e.target.
;;;; Every form with validation errors threw a JS ReferenceError on submit
;;;; (or on input clearing) before the fix.

(in-package :lol-web/forms/test)
(in-suite :lol-web/forms/test)

(test regression-form-dsl-no-symbol-concat-typos
  "Generated form validation JS uses property access, not symbol concat"
  (lol-web/forms::defform regression-form-probe ()
    :fields ((username :type :string :min 3 :required t)))
  (let ((js (lol-web/forms::generate-form-validation-js 'regression-form-probe)))
    ;; Typo forms must not appear
    (is (null (search "elremove" js))
        "elremove() typo present — should be el.remove()")
    (is (null (search "epreventDefault" js))
        "epreventDefault() typo present — should be e.preventDefault()")
    (is (null (search ".etarget" js))
        ".etarget property access typo present — should be .target on e")
    ;; Correct forms must appear
    (is (search "el.remove" js)
        "Expected el.remove() in generated JS for error-element removal")
    (is (search "e.preventDefault" js)
        "Expected e.preventDefault() in generated JS for invalid-form submit")
    (is (search "e.target" js)
        "Expected e.target in generated JS for input-clear listener")))

(test regression-render-form-emits-multipart-enctype-when-file-field-present
  "A form with any :file field renders enctype=\"multipart/form-data\"; a form
   with no :file field omits the attribute. Browsers ignore the file input's
   contents under the default urlencoded enctype, so the attribute is the
   only mechanism that makes file uploads reach the server."
  (lol-web/forms::defform regression-form-with-upload ()
    :fields ((avatar :type :file :required t)
             (caption :type :string)))
  (lol-web/forms::defform regression-form-text-only ()
    :fields ((username :type :string :required t)))
  (let ((with-file (lol-web/forms::render-form 'regression-form-with-upload))
        (without-file (lol-web/forms::render-form 'regression-form-text-only)))
    (is (search "enctype=\"multipart/form-data\"" with-file)
        "form with :file field must include enctype=\"multipart/form-data\"")
    (is (null (search "enctype=" without-file))
        "form without :file fields must not emit an enctype attribute")))

;;; ============================================================================
;;; defform hygiene — field names cannot shadow bookkeeping
;;; ============================================================================

(test regression-defform-gensym-no-shadow
  "A form whose field is named ERRORS does not break the macro's
   internal `errors` bookkeeping: the validation-failure path still
   returns the per-field errors alist rather than erroring on a
   shadowed binding. Covered by defining such a form, submitting empty
   data (so the required field fails), and inspecting the result."
  (lol-web/forms::defform regression-form-errors-field ()
    :fields ((errors :type :string :required t)))
  (let ((result (process-regression-form-errors-field-submission '())))
    (is (listp result) "process-fn must return the errors alist, not error out")
    (is (not (null (assoc 'errors result)))
        "errors alist must mention the ERRORS field")))

(test regression-defform-request-data-field-does-not-shadow-parameter
  "A field named REQUEST-DATA does not shadow the macro's request-data
   parameter. The macro gensyms its parameter, so validate-form-data
   still sees the full submission."
  (lol-web/forms::defform regression-form-request-data-field ()
    :fields ((request-data :type :string :required t)))
  (let ((result (process-regression-form-request-data-field-submission
                 (list :request-data "x"))))
    ;; Required satisfied so process-fn falls through to :on-submit (nil here),
    ;; which evaluates to NIL — but crucially, no error.
    (is (null result) "valid submission must not error")))

(test regression-on-submit-compiled-at-registration
  "DEFFORM stores :on-submit as a callable closure in the registry, not
   as a source list. inspect-form's predicate still works (non-NIL =
   handler exists), and direct funcall on the registry entry returns
   the user body's value."
  (lol-web/forms::defform regression-form-compiled ()
    :fields ((name :type :string))
    :on-submit (list :got name))
  (let ((on-submit
          (getf (lol-web/forms::get-form-spec 'regression-form-compiled)
                :on-submit)))
    (is (functionp on-submit)
        ":on-submit registry entry must be a function, was ~S"
        (type-of on-submit))
    (is (equal '(:got "alice")
               (funcall on-submit (list :name "alice")))
        "calling the registered closure must rebind the user's name reference")))

(test regression-get-form-spec-returns-copy
  "Mutating an introspected form spec does not rewrite the registry."
  (lol-web/forms::defform regression-form-spec-copy ()
    :fields ((name :type :string))
    :on-submit (list :got name))
  (let ((spec (lol-web/forms::get-form-spec 'regression-form-spec-copy)))
    (setf (getf spec :on-submit) (lambda (data) (declare (ignore data)) :mutated)))
  (let ((on-submit
          (getf (lol-web/forms::get-form-spec 'regression-form-spec-copy)
                :on-submit)))
    (is (equal '(:got "alice")
               (funcall on-submit (list :name "alice"))))))

;;; ============================================================================
;;; defform :pattern — bounded length + compile, cached scanner
;;; ============================================================================

(test regression-defform-pattern-bounded-at-macroexpand
  "A DEFFORM whose :PATTERN exceeds *FORM-PATTERN-MAX-LENGTH* signals at
   macro-expansion time, surfacing the misconfiguration at the form
   definition site instead of at the first request that hits the field."
  (let ((lol-web/forms:*form-pattern-max-length* 16))
    (signals error
      (macroexpand-1
       '(lol-web/forms::defform regression-form-overlong-pattern ()
         :fields ((slug :type :string
                        :pattern "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))))

(test regression-defform-pattern-scanner-cached
  "VALIDATE-FIELD must reuse the cached scanner across calls — same
   string identity returns EQ scanner instances out of
   %GET-FORM-PATTERN-SCANNER. Without caching, every request recompiles."
  (let ((pat "^[a-z]+$"))
    ;; Clear cache for the probe pattern so we measure a fresh insert.
    (lol-web/core:bounded-cache-remove lol-web/forms::*form-pattern-scanner-cache* pat)
    (let ((first  (lol-web/forms::%get-form-pattern-scanner pat))
          (second (lol-web/forms::%get-form-pattern-scanner pat)))
      (is (eq first second)
          "second %get-form-pattern-scanner call must return the same scanner"))))

(test regression-form-scanner-cache-bounded
  "*FORM-PATTERN-SCANNER-CACHE* is a bounded LRU cache: feeding more
   distinct patterns than its cap leaves the entry count at the cap rather
   than growing without limit — a stream of runtime-registered patterns
   cannot OOM the image."
  (let ((lol-web/forms::*form-pattern-scanner-cache*
          (lol-web/core:make-bounded-cache :max-entries 4 :test 'equal)))
    (dotimes (i 20)
      (lol-web/forms::%get-form-pattern-scanner (format nil "^a~D$" i)))
    (is (<= (lol-web/core:bounded-cache-count
             lol-web/forms::*form-pattern-scanner-cache*)
            4)
        "cache must stay at or below its 4-entry cap after 20 distinct inserts")))

(test regression-defform-pattern-accept-and-reject
  "End-to-end: a DEFFORM with a small :PATTERN validates legitimate input
   and rejects bad input. The scanner cache must not affect correctness."
  (lol-web/forms::defform regression-form-pattern-slug ()
    :fields ((slug :type :string :pattern "^[a-z]+$" :required t)))
  (multiple-value-bind (valid errors)
      (lol-web/forms::validate-form-data 'regression-form-pattern-slug
                                         (list :slug "abcd"))
    (is (eq t valid))
    (is (null errors)))
  (multiple-value-bind (valid errors)
      (lol-web/forms::validate-form-data 'regression-form-pattern-slug
                                         (list :slug "AB-12"))
    (is (null valid))
    (is (consp errors))
    (is (search "Invalid format" (princ-to-string errors)))))

;;; ============================================================================
;;; field bounds and generated CSRF gate
;;; ============================================================================

(test regression-form-email-and-url-length-caps
  "Email and URL fields reject overlong values before regex validation."
  (lol-web/forms::defform regression-form-length-caps ()
    :fields ((email :type :email)
             (site :type :url)))
  (let ((long-email (concatenate 'string
                                 (make-string lol-web/forms:*form-email-max-length*
                                              :initial-element #\a)
                                 "@x.test"))
        (long-url (concatenate 'string
                               "https://"
                               (make-string lol-web/forms:*form-url-max-length*
                                            :initial-element #\a))))
    (multiple-value-bind (valid errors)
        (lol-web/forms::validate-form-data
         'regression-form-length-caps
         (list :email long-email :site "https://example.com"))
      (is (null valid))
      (is (search "at most" (princ-to-string (cdr (assoc 'email errors))))))
    (multiple-value-bind (valid errors)
        (lol-web/forms::validate-form-data
         'regression-form-length-caps
         (list :email "ada@example.com" :site long-url))
      (is (null valid))
      (is (search "at most" (princ-to-string (cdr (assoc 'site errors))))))))

(test regression-defform-process-submission-enforces-csrf-in-session
  "Generated process-* functions require csrf-token when a session exists."
  (lol-web/forms::defform regression-form-csrf-auto ()
    :fields ((name :type :string :required t))
    :on-submit (list :ok name))
  (let ((session (make-hash-table :test 'equal)))
    (setf (gethash "csrf-token" session) "csrf-regression-token")
    (let ((lol-web/server:*env* (list :lack.session session)))
      (signals lol-web/server:http-forbidden
        (process-regression-form-csrf-auto-submission
         (list :name "Ada")))
      (is (equal '(:ok "Ada")
                 (process-regression-form-csrf-auto-submission
                  (list :name "Ada"
                        :csrf-token "csrf-regression-token")))))))

;;; ============================================================================
;;; register-form — field-name token gate
;;; ============================================================================

(test regression-register-form-rejects-unsafe-field-name
  "register-form refuses a field name outside the conservative token class:
   a name carrying a quote, bracket, space, or leading digit could break out
   of the HTML attribute, the [name='...'] selector, or the interned keyword
   it reaches. A safe name registers without signalling."
  (signals lol-web/forms::unsafe-form-field-name
    (lol-web/forms::register-form
     'regression-form-unsafe-name
     '(:fields (("x' onfocus='alert(1)" :type :string)))))
  (dolist (bad '("a b" "a]" "a[" "a'" "a\"b" "a<b" "1abc" ""))
    (is (not (lol-web/forms::%safe-form-field-name-p bad))
        "field name ~S must be rejected by the token gate" bad))
  (dolist (good '("name" "user-id" "field_2" "Email"))
    (is (lol-web/forms::%safe-form-field-name-p good)
        "field name ~S must pass the token gate" good))
  (is (not (null (lol-web/forms::register-form
                  'regression-form-safe-name
                  '(:fields ((username :type :string))))))
      "a safe field name must register without signalling"))
