;;;; CSS-payload trust type and identifier predicate.
;;;;
;;;; SAFE-CSS-PAYLOAD-STRING is the type-level claim that a string is a
;;;; producer-asserted-safe CSS chunk — body of a `@keyframes`, `@media`,
;;;; section block, or variable definition. Sinks that emit raw CSS
;;;; payload at the `<style>` boundary `check-type` against this struct
;;;; so untyped strings cannot reach the output position.
;;;;
;;;; The trust type carries no escaping. Producers earn the tag by
;;;; building the value from validated parts (css-rule, escape-css-value,
;;;; safe-css-selector-p) or from a constant literal. There is no
;;;; coerce-shim at the sink: raw-string input is a type-error, not a
;;;; silently-escaped emit.

(in-package :lol-web/css)

(defstruct (safe-css-payload-string
            (:constructor %make-safe-css-payload-string)
            (:predicate safe-css-payload-string-p)
            (:copier nil))
  "Producer's claim that VALUE is a CSS payload safe to emit verbatim
   inside a `<style>` element, a `@keyframes` block, or any other CSS
   container. Idempotency lives in MAKE-SAFE-CSS-PAYLOAD-STRING."
  (value "" :type string :read-only t))

(defun make-safe-css-payload-string (value)
  "Tag VALUE as a safe CSS payload. Idempotent: returns VALUE when it is
   already a SAFE-CSS-PAYLOAD-STRING. VALUE must be a string; other
   types signal a type-error rather than coerce silently."
  (cond
    ((safe-css-payload-string-p value) value)
    ((stringp value) (%make-safe-css-payload-string :value value))
    (t (error 'type-error :datum value :expected-type 'string))))

(defun %ascii-letter-p (c)
  "True for an ASCII letter only. CL's ALPHA-CHAR-P is locale-wide and
   returns T for non-ASCII letters (é, Cyrillic А, ...)."
  (or (char<= #\a c #\z) (char<= #\A c #\Z)))

(defun %ascii-digit-p (c)
  "True for an ASCII decimal digit only. CL's DIGIT-CHAR-P returns a
   weight for Unicode digits such as Arabic-Indic ٦."
  (char<= #\0 c #\9))

(defun safe-css-ident-p (ident)
  "Return T if IDENT is a non-empty string that looks like a CSS
   identifier — ASCII letter / digit / `-` / `_`, leading character
   non-digit. Rejects whitespace, `:`, `;`, `{`, `}`, `<`, `>`, `/`,
   `*`, `\\`, quote characters, and every non-ASCII byte. Suitable for
   property names and custom-property names."
  (and (stringp ident)
       (plusp (length ident))
       (let ((first (char ident 0)))
         (or (%ascii-letter-p first)
             (char= first #\-)
             (char= first #\_)))
       (every (lambda (c)
                (or (%ascii-letter-p c)
                    (%ascii-digit-p c)
                    (char= c #\-)
                    (char= c #\_)))
              ident)))

(define-condition unsafe-css-ident (error)
  ((ident :initarg :ident :reader unsafe-css-ident-ident))
  (:report (lambda (c stream)
             (format stream "Unsafe CSS identifier: ~S (letters, digits, `-`, `_` only; non-digit lead)"
                     (unsafe-css-ident-ident c)))))

(defun escape-css-ident (ident)
  "Coerce IDENT to a string and rewrite every character outside the
   SAFE-CSS-IDENT-P alphabet as the CSS hex escape `\\XX `. Useful for
   passing legacy-source identifiers through a generator without
   exposing the call site to a SAFE-CSS-IDENT-P type-error.

   The result satisfies SAFE-CSS-IDENT-P only when the trailing space
   is consumed by the immediate next syntactic boundary; callers that
   need a typed-safe ident must validate the source instead of escaping."
  (let* ((str (if (stringp ident) ident (princ-to-string ident)))
         (out (make-string-output-stream)))
    (loop for c across str
          for first = t then nil
          do (cond
               ((or (%ascii-letter-p c)
                    (char= c #\-)
                    (char= c #\_)
                    (and (not first) (%ascii-digit-p c)))
                (write-char c out))
               (t
                (format out "\\~X " (char-code c)))))
    (get-output-stream-string out)))
