;;;; css/tailwind.lisp - Tailwind CSS class generation helpers
;;;;
;;;; PURPOSE:
;;;;   Utilities for generating Tailwind CSS classes from design tokens.
;;;;   Provides clean DSL for combining classes and token-aware generators.
;;;;
;;;; USAGE:
;;;;   (classes "p-4" "bg-black" nil "text-white")  ; => "p-4 bg-black text-white"
;;;;   (tw-color "bg" :primary)                      ; => "bg-primary"
;;;;   (tw-spacing "p" :4)                           ; => "p-4"

(in-package :lol-web/css)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; String Utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(defun null-or-empty-p (x)
  "Return T if X is nil or an empty string."
  (or (null x)
      (and (stringp x) (string= x ""))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Class Composition
;;; ─────────────────────────────────────────────────────────────────────────────

(defun classes (&rest class-strings)
  "Combine class strings, filtering nil/empty values.
   Flattens nested lists for convenient conditional composition.

   Examples:
   (classes \"p-4\" nil \"bg-black\")        ; => \"p-4 bg-black\"
   (classes \"base\" (when cond \"extra\"))  ; => \"base extra\" or \"base\"
   (classes (list \"a\" \"b\") \"c\")         ; => \"a b c\""
  (format nil "~{~A~^ ~}"
          (remove-if #'null-or-empty-p
                     (alexandria:flatten class-strings))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Token-Based Class Generators
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tw-color (prefix key)
  "Generate Tailwind color class from token key.
   PREFIX: Tailwind prefix (\"bg\", \"text\", \"border\", etc.)
   KEY: Color token keyword

   Examples:
   (tw-color \"bg\" :primary)     ; => \"bg-primary\"
   (tw-color \"text\" :muted)     ; => \"text-muted\"
   (tw-color \"border\" :error)   ; => \"border-error\""
  (format nil "~A-~A" prefix (string-downcase (symbol-name key))))

(defun tw-spacing (prefix key)
  "Generate Tailwind spacing class from token key.
   PREFIX: Tailwind prefix (\"p\", \"m\", \"gap\", \"px\", \"py\", etc.)
   KEY: Spacing token keyword (numeric)

   Examples:
   (tw-spacing \"p\" :4)   ; => \"p-4\"
   (tw-spacing \"mx\" :8)  ; => \"mx-8\"
   (tw-spacing \"gap\" :6) ; => \"gap-6\""
  (format nil "~A-~A" prefix (string-downcase (symbol-name key))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Convenience Generators
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tw-bg (key)
  "Generate background color class. (tw-bg :primary) => \"bg-primary\""
  (tw-color "bg" key))

(defun tw-text (key)
  "Generate text color class. (tw-text :muted) => \"text-muted\""
  (tw-color "text" key))

(defun tw-border (key)
  "Generate border color class. (tw-border :error) => \"border-error\""
  (tw-color "border" key))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Arbitrary Value Classes (for token values)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tw-arbitrary (prefix value)
  "Generate Tailwind arbitrary value class.
   PREFIX: Tailwind prefix
   VALUE: Literal CSS value

   Strips characters that could break out of the `prefix-[value]`
   arbitrary-value syntax, the surrounding double-quoted `class` attribute,
   or an unquoted attribute sink: whitespace, `]`, quotes, angle brackets,
   plus `{ } ; = \\` and backtick. The first set is load-bearing in the
   current double-quoted sink; the second is defense-in-depth so the same
   token is also inert if a future caller emits it into an unquoted
   attribute (where space/`=`/backtick would otherwise start a new
   attribute). Legitimate CSS-value characters — parens, commas, `#`, `%`,
   `.` — survive.

   Examples:
   (tw-arbitrary \"bg\" \"#FF0000\")    ; => \"bg-[#FF0000]\"
   (tw-arbitrary \"w\" \"clamp(1rem, 5vw, 3rem)\") ; => \"w-[clamp(1rem,5vw,3rem)]\""
  (format nil "~A-[~A]" prefix
          (remove-if (lambda (c)
                       (or (char= c #\Space)
                           (char= c #\])
                           (char= c #\")
                           (char= c #\')
                           (char= c #\<)
                           (char= c #\>)
                           (char= c #\{)
                           (char= c #\})
                           (char= c #\;)
                           (char= c #\=)
                           (char= c #\\)
                           (char= c #\`)
                           (char= c #\Newline)
                           (char= c #\Return)
                           (char= c #\Tab)))
                     value)))

(defun tw-bg-value (key)
  "Generate background class with token value.
   (tw-bg-value :primary) => \"bg-[#00FF41]\""
  (tw-arbitrary "bg" (get-color key)))

(defun tw-text-value (key)
  "Generate text class with token value.
   (tw-text-value :muted) => \"text-[#9EB3C8]\""
  (tw-arbitrary "text" (get-color key)))

(defun tw-border-value (key)
  "Generate border class with token value.
   (tw-border-value :error) => \"border-[#FF3333]\""
  (tw-arbitrary "border" (get-color key)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tailwind Configuration Generation (Parenscript)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *tailwind-config-max-tokens* 256
  "Cap on the number of color tokens TAILWIND-CONFIG will emit.
   A caller-controlled COLORS alist longer than this is refused at
   the boundary, bounding the work the parenscript emitter performs
   and the size of the resulting JS literal sent to the client.")

(define-condition tailwind-config-token-invalid (error)
  ((key :initarg :key :reader tailwind-config-token-invalid-key))
  (:report
   (lambda (c stream)
     (format stream "TAILWIND-CONFIG color token key ~S is not a keyword; ~
                     pass an alist of (keyword . string) pairs."
             (tailwind-config-token-invalid-key c))))
  (:documentation
   "Signalled when TAILWIND-CONFIG sees a color-token key that is not a
    keyword. Refusing coercion at this boundary keeps the keyword pool
    bounded by the application's declared color tokens."))

(define-condition tailwind-config-too-many-tokens (error)
  ((count :initarg :count :reader tailwind-config-too-many-tokens-count)
   (limit :initarg :limit :reader tailwind-config-too-many-tokens-limit))
  (:report
   (lambda (c stream)
     (format stream "TAILWIND-CONFIG received ~D color tokens; cap is ~D."
             (tailwind-config-too-many-tokens-count c)
             (tailwind-config-too-many-tokens-limit c)))))

(defun tailwind-config (&key (colors *colors*) (typography *typography*))
  "Generate Tailwind CDN configuration script via Parenscript.
   Extends Tailwind's theme with current design tokens.
   NO hardcoded values - all from token system.

   COLORS keys must be keywords — TAILWIND-CONFIG no longer interns
   downcased variants, so the keyword pool stays bounded by what the
   application declared in *COLORS* (or whatever alist the caller
   supplies). A non-keyword key signals TAILWIND-CONFIG-TOKEN-INVALID;
   a COLORS alist longer than *TAILWIND-CONFIG-MAX-TOKENS* signals
   TAILWIND-CONFIG-TOO-MANY-TOKENS."
  (when (> (length colors) *tailwind-config-max-tokens*)
    (error 'tailwind-config-too-many-tokens
           :count (length colors)
           :limit *tailwind-config-max-tokens*))
  (let ((color-pairs
          (mapcan (lambda (pair)
                    (let ((key (car pair)))
                      (unless (keywordp key)
                        (error 'tailwind-config-token-invalid :key key))
                      (let ((name (string-downcase (symbol-name key))))
                        (unless (safe-css-ident-p name)
                          (error 'unsafe-css-ident :ident name)))
                      (list key (cdr pair))))
                  colors))
        (font-family (or (cdr (assoc :family typography))
                         (error "Typography token :family is required. Set *typography* before calling tailwind-config."))))
    (parenscript:ps*
      `(setf tailwind.config
             (ps:create
              :theme (ps:create
                      :extend (ps:create
                               :colors (ps:create ,@color-pairs)
                               :font-family (ps:create
                                             :sans (array ,font-family)))))))))
