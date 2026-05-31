;;;; HTML escape macros and the SAFE-HTML-STRING trust tag.
;;;;
;;;; `escape-html` lives in :lol-web/escape; the macros here wrap it for
;;;; convenient use inside cl-who templates, where `cl-who:str` is the
;;;; insertion verb and the caller wants the value escaped before write.
;;;;
;;;; SAFE-HTML-STRING is the type-level trust contract for sinks that
;;;; accept HTML payloads (html-page, ws/sse broadcasts, optimistic UI).
;;;; A SAFE-HTML-STRING is content the producer asserts is safe to emit
;;;; verbatim — already escaped, sanitized, or generated from trusted
;;;; source. Sinks consult the type: tagged values bypass escape; bare
;;;; strings get escape-html applied.

(in-package :lol-web/html)

(defmacro safe-str (expr)
  "Output the escaped value of EXPR inside a cl-who template.

   Example (cl-who context):
     (:p (safe-str user-input))"
  `(cl-who:str (escape-html ,expr)))

(defmacro safe-fmt (control-string &rest args)
  "Format ARGS with CONTROL-STRING and emit the escaped result inside a
   cl-who template.

   Example:
     (safe-fmt \"Hello, ~A!\" username)"
  (unless (stringp control-string)
    (error "safe-fmt control string must be a literal string, got ~S"
           control-string))
  `(cl-who:str (escape-html (format nil ,control-string ,@args))))

(defstruct (safe-html-string
            (:constructor %make-safe-html-string)
            (:predicate safe-html-string-p)
            (:copier nil))
  "Trust marker for HTML/CSS/JS payloads. Wrapping a string in this type
   asserts the content is safe to emit verbatim into an HTML document."
  (value "" :type string :read-only t))

(defun make-safe-html-string (value)
  "Tag VALUE as safe to emit verbatim. Idempotent: returns VALUE unchanged
   when it is already a SAFE-HTML-STRING. Strings become a fresh tagged
   wrapper; other types are coerced via PRINC-TO-STRING.

   The caller is responsible for the safety claim — escape-html the
   content first if it carries user input, or generate it from a trusted
   source (parenscript, css generators, server-issued tokens)."
  (cond
    ((safe-html-string-p value) value)
    ((stringp value) (%make-safe-html-string :value value))
    (t (%make-safe-html-string :value (princ-to-string value)))))

(defun coerce-html-emit (value)
  "Coerce VALUE to a string for cl-who:str at an HTML-payload position.
   - SAFE-HTML-STRING → unwrap value, emit verbatim
   - NIL              → empty string
   - String           → escape-html
   - Other            → princ-to-string then escape-html

   Untyped input is treated as untrusted: it gets escaped. Use
   MAKE-SAFE-HTML-STRING at the producer to opt content into raw emit."
  (cond
    ((null value) "")
    ((safe-html-string-p value) (safe-html-string-value value))
    ((stringp value) (escape-html value))
    (t (escape-html (princ-to-string value)))))
