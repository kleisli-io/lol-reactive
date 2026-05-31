;;;; JS-string-literal trust type and string escaper.
;;;;
;;;; SAFE-JS-STRING-LITERAL is the type-level claim that VALUE is a
;;;; JavaScript string literal safe to splice into a JS source position
;;;; without further escaping — the outer quotes are part of the value.
;;;; Producers earn the tag by running their input through
;;;; ESCAPE-JS-STRING (which handles every character that can break out
;;;; of a single- or double-quoted literal, plus the U+2028 / U+2029
;;;; line-separator pair which terminate JS string literals even though
;;;; they are not literal newlines).
;;;;
;;;; Sinks that splice JS literals (js-value, hx-dispatch, hx-bind) take
;;;; only SAFE-JS-STRING-LITERAL inputs. Raw-string input is a
;;;; type-error: there is no coerce-shim at the sink.

(in-package :lol-web/parenscript)

(defstruct (safe-js-string-literal
            (:constructor %make-safe-js-string-literal)
            (:predicate safe-js-string-literal-p)
            (:copier nil))
  "Producer's claim that VALUE is a complete JS string literal — the
   surrounding quote characters are part of VALUE — safe to splice into
   a JS source position. Idempotency lives in MAKE-SAFE-JS-STRING-LITERAL."
  (value "''" :type string :read-only t))

(defun escape-js-string (raw)
  "Escape RAW for inclusion inside a single-quoted JS string literal.
   Rewrites `\\`, `'`, `\"`, `\\n`, `\\r`, `<`, U+2028, U+2029. Backslash
   is rewritten first so the introducer of every other escape does not
   get double-escaped. The result is the literal body; callers add the
   surrounding quotes."
  (let* ((str (if (stringp raw) raw (princ-to-string raw)))
         (out (make-string-output-stream)))
    (loop for c across str do
          (case c
            (#\\        (write-string "\\\\" out))
            (#\'        (write-string "\\'" out))
            (#\"        (write-string "\\\"" out))
            (#\Newline  (write-string "\\n" out))
            (#\Return   (write-string "\\r" out))
            (#\<        (write-string "\\u003C" out))
            (t          (cond
                          ((eql (char-code c) #x2028) (write-string "\\u2028" out))
                          ((eql (char-code c) #x2029) (write-string "\\u2029" out))
                          (t (write-char c out))))))
    (get-output-stream-string out)))

(defun make-safe-js-string-literal (raw)
  "Tag RAW as a single-quoted JS string literal. Idempotent: returns RAW
   when it is already a SAFE-JS-STRING-LITERAL. Strings are escaped via
   ESCAPE-JS-STRING and wrapped in single quotes; other types signal a
   type-error rather than coerce silently."
  (cond
    ((safe-js-string-literal-p raw) raw)
    ((stringp raw)
     (%make-safe-js-string-literal
      :value (concatenate 'string "'" (escape-js-string raw) "'")))
    (t (error 'type-error :datum raw :expected-type 'string))))
