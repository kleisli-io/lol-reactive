;;;; css/generation.lisp - CSS generation utilities
;;;;
;;;; PURPOSE:
;;;;   Low-level CSS generation functions for creating rules, sections,
;;;;   and keyframes from Lisp data structures.
;;;;
;;;; These functions work standalone or with css-modules:
;;;;
;;;;   ;; Standalone usage
;;;;   (css-rule ".btn" '(("padding" . "1rem")))
;;;;   => ".btn { padding: 1rem; }"
;;;;
;;;;   ;; With css-module
;;;;   (let ((module (make-css-module :buttons)))
;;;;     (funcall module :add-rule ".btn" '(("padding" . "1rem")))
;;;;     (funcall module :render))

(in-package :lol-web/css)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Output-emission safety predicates and escape
;;; ─────────────────────────────────────────────────────────────────────────────

(defun safe-css-selector-p (selector)
  "Return T if SELECTOR is a non-empty string free of `;`, `{`, `}`,
   `<`, and the literal sequence `*/`.

   `;`, `{`, `}` can close the current rule and open a sibling one.
   `<` enables a `</style>` break-out when the rule is emitted inside a
   `<style>` element — rejecting it is sufficient because the close-tag
   sequence starts with `<`. `*/` closes a C-style CSS comment; the
   browser strips comments before parsing, so an attacker-supplied
   selector that smuggles `*/` can hide additional rules between the
   block-start and block-end markers.

   `>` is legitimate as the child combinator (`body > .x`) and is left
   permitted. CSS selectors also legitimately contain `.`, `#`, `:`,
   `[`, `]`, `~`, `+`, `*`, whitespace, and quoted attribute values —
   none of those are dangerous in the selector position itself once
   the listed characters are excluded."
  (and (stringp selector)
       (plusp (length selector))
       (not (find-if (lambda (c)
                       (or (char= c #\;)
                           (char= c #\{)
                           (char= c #\})
                           (char= c #\<)))
                     selector))
       (not (search "*/" selector))))

(define-condition unsafe-css-selector (error)
  ((selector :initarg :selector :reader unsafe-css-selector-selector))
  (:report (lambda (c stream)
             (format stream "Unsafe CSS selector: ~S (forbidden chars: `;`, `{`, `}`, `<`, `*/`)"
                     (unsafe-css-selector-selector c)))))

(defun safe-css-value-p (value)
  "Return T if VALUE is a string with no unescaped `;`, `}`, or `<`.
   These characters can close the current declaration/rule or open a
   tag if the CSS is embedded in `<style>`; reject before emit.

   The escape function `escape-css-value` rewrites these characters into
   their CSS-safe forms — after escape, the result satisfies this
   predicate."
  (and (stringp value)
       (not (find-if (lambda (c)
                       (or (char= c #\;)
                           (char= c #\})
                           (char= c #\<)
                           (char= c #\>)))
                     value))))

(defun escape-css-value (value)
  "Coerce VALUE to a string and CSS-escape `\\`, `;`, `}`, `<`, `>`, and
   newline/carriage-return.

   Each dangerous character becomes its hex-escape form (`\\3B `, etc.) —
   the trailing space is a CSS escape terminator. Backslash is escaped first
   to avoid double-escaping the introducer. Newlines are escaped so an
   attacker value cannot inject a line break that some downstream tooling
   treats as a statement boundary.

   Quotes (`\"`/`'`) and parentheses (`(`/`)`) are deliberately NOT escaped:
   css-rule and generate-css-variables route EVERY value through here,
   including framework-generated values that legitimately need them — the
   default `font-family` is `\"JetBrains Mono\", monospace` (quoted) and
   token values are routinely var()/rgb()/calc()/url(). Hex-escaping those
   characters would corrupt every legitimate value. Closing the
   content:/url()/font-family: string-context vectors needs a CSS-value trust
   type that separates generated-trusted from untrusted values, not a blunt
   escape here; the rule/tag-breakout characters (`;}<>`) are already escaped.

   RESIDUAL EXPOSURE (verdict-vs-trust contract): because `\"`, `'`, `(`, `)`
   pass through, this function is NOT sufficient to neutralise an
   *untrusted* value spliced into a URL-accepting or string-accepting
   property. A value reaching `background: url(<here>)` or
   `content: \"<here>\"` can stay inside the function/string context and
   exfiltrate via a remote `url()` fetch — escape-css-value closes the
   declaration/rule/`<style>`-tag boundary, NOT the value-internal
   url()/string context. Callers MUST NOT route attacker-controlled values
   into url()/content/font-family properties through this function alone;
   restrict such properties to framework-generated values, or gate them
   behind a dedicated CSS-value trust type."
  (let* ((str (if (stringp value) value (princ-to-string value)))
         (out (make-string-output-stream)))
    (loop for c across str do
          (case c
            (#\\       (write-string "\\5C " out))
            (#\;       (write-string "\\3B " out))
            (#\}       (write-string "\\7D " out))
            (#\<       (write-string "\\3C " out))
            (#\>       (write-string "\\3E " out))
            (#\Newline (write-string "\\A " out))
            (#\Return  (write-string "\\D " out))
            (t         (write-char c out))))
    (get-output-stream-string out)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Rule Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-rule (selector properties)
  "Generate a CSS rule from selector and property alist.

   SELECTOR: CSS selector string (e.g., \".btn\", \"#header\", \"body\").
   Must satisfy `safe-css-selector-p` — characters `;`, `{`, `}` are
   rejected so a hostile selector cannot close the current rule and
   inject a sibling one.

   PROPERTIES: Alist of (property . value) pairs. Keys may be strings or
   symbols (including keywords); symbol keys are downcased to their
   `symbol-name` so `:opacity` renders as `opacity`, not `OPACITY`.
   Values pass through `escape-css-value`, which rewrites `;`, `}`,
   `<`, `>`, and `\\` into their hex-escape form so an attacker-supplied
   value cannot close the declaration or open an HTML tag.

   Example:
   (css-rule \".btn\" '((\"padding\" . \"1rem\") (\"margin\" . \"0\")))
   => \".btn { padding: 1rem; margin: 0; }\""
  (unless (safe-css-selector-p selector)
    (error 'unsafe-css-selector :selector selector))
  (format nil "~A { ~{~A: ~A;~^ ~} }"
          selector
          (mapcan (lambda (pair)
                    (let* ((k (car pair))
                           (name (typecase k
                                   (string k)
                                   (symbol (string-downcase (symbol-name k)))
                                   (t (princ-to-string k)))))
                      (unless (safe-css-ident-p name)
                        (error 'unsafe-css-ident :ident name))
                      (list name (escape-css-value (cdr pair)))))
                  properties)))

(defun css-rules (selector &rest property-pairs)
  "Generate CSS rule with inline property pairs.

   SELECTOR: CSS selector string
   PROPERTY-PAIRS: Alternating property names and values

   Example:
   (css-rules \".btn\" \"padding\" \"1rem\" \"margin\" \"0\")
   => \".btn { padding: 1rem; margin: 0; }\""
  (css-rule selector
            (loop for (prop val) on property-pairs by #'cddr
                  collect (cons prop val))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Sections
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-section (name &rest rules)
  "Group CSS rules under a named section comment.

   NAME: SAFE-CSS-PAYLOAD-STRING — the comment text. Required as a typed
   payload so an attacker-supplied name cannot smuggle `*/` and close
   the comment.
   RULES: list of SAFE-CSS-PAYLOAD-STRING — each is a CSS rule the
   producer asserts is safe to emit. Wrap CSS-RULE output in
   MAKE-SAFE-CSS-PAYLOAD-STRING at the call site.

   Example:
   (css-section (make-safe-css-payload-string \"Buttons\")
     (make-safe-css-payload-string
      (css-rule \".btn\" '((\"padding\" . \"1rem\")))))
   => \"/* --- Buttons --- */
       .btn { padding: 1rem; }\""
  (check-type name safe-css-payload-string)
  (dolist (r rules) (check-type r safe-css-payload-string))
  (format nil "/* --- ~A --- */~%~{~A~^~%~}"
          (safe-css-payload-string-value name)
          (mapcar #'safe-css-payload-string-value rules)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Keyframes
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-keyframes (name &rest frames)
  "Generate CSS @keyframes animation.

   NAME: Animation name (CSS identifier). Must pass SAFE-CSS-IDENT-P.
   FRAMES: List of (percentage . properties-alist) pairs. Each frame's
   percentage flows through CSS-RULE which validates it as a selector.

   Example:
   (css-keyframes \"fade-in\"
     '(\"0%\" . ((\"opacity\" . \"0\")))
     '(\"100%\" . ((\"opacity\" . \"1\"))))
   => \"@keyframes fade-in { 0% { opacity: 0; } 100% { opacity: 1; } }\""
  (unless (safe-css-ident-p name)
    (error 'unsafe-css-ident :ident name))
  (format nil "@keyframes ~A { ~{~A~^ ~} }"
          name
          (mapcar (lambda (frame)
                    (css-rule (car frame) (cdr frame)))
                  frames)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Media Queries
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-media (query &rest rules)
  "Generate CSS @media query block.

   QUERY: SAFE-CSS-PAYLOAD-STRING — the media query body
   (e.g., \"(min-width: 768px)\"). Required typed at entry.
   RULES: list of SAFE-CSS-PAYLOAD-STRING — CSS rules to include.
   Wrap CSS-RULE output in MAKE-SAFE-CSS-PAYLOAD-STRING at the call site.

   Example:
   (css-media (make-safe-css-payload-string \"(min-width: 768px)\")
     (make-safe-css-payload-string
      (css-rule \".container\" '((\"max-width\" . \"1200px\")))))
   => \"@media (min-width: 768px) { .container { max-width: 1200px; } }\""
  (check-type query safe-css-payload-string)
  (dolist (r rules) (check-type r safe-css-payload-string))
  (format nil "@media ~A { ~{~A~^ ~} }"
          (safe-css-payload-string-value query)
          (mapcar #'safe-css-payload-string-value rules)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Variables
;;; ─────────────────────────────────────────────────────────────────────────────

(defun css-var (name)
  "Reference a CSS custom property (variable).

   NAME: variable name (without -- prefix). Must pass SAFE-CSS-IDENT-P.
   Returns: \"var(--<name>)\" string.

   Example:
   (css-var \"primary\") => \"var(--primary)\""
  (unless (safe-css-ident-p name)
    (error 'unsafe-css-ident :ident name))
  (format nil "var(--~A)" name))

(defun css-var-definition (name value)
  "Generate a CSS custom property definition.

   NAME: variable name (without -- prefix). Must pass SAFE-CSS-IDENT-P.
   VALUE: SAFE-CSS-PAYLOAD-STRING — the variable value as a producer-
   asserted-safe CSS payload.

   Example:
   (css-var-definition \"primary\"
                       (make-safe-css-payload-string \"#00FF41\"))
   => \"--primary: #00FF41\""
  (unless (safe-css-ident-p name)
    (error 'unsafe-css-ident :ident name))
  (check-type value safe-css-payload-string)
  (format nil "--~A: ~A" name (safe-css-payload-string-value value)))
