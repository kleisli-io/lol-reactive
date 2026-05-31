(in-package :lol-web/css/test)
(in-suite :lol-web/css/test)

;;; ============================================================================
;;; css-rule — symbol/keyword keys downcase to lowercase property names
;;; ============================================================================

(test regression-css-rule-keyword-keys-downcase
  "Keyword keys produce lowercase CSS property names. Unguarded `~A`
   formats `:opacity` as `OPACITY`, which browsers silently ignore."
  (let ((rule (css-rule ".x" '((:opacity . "0.7")
                               (:transition . "opacity 0.2s")))))
    (is (search "opacity: 0.7" rule)
        "keyword :opacity must render as `opacity`, not `OPACITY`")
    (is (search "transition: opacity 0.2s" rule)
        "keyword :transition must render as `transition`")
    (is (not (search "OPACITY" rule))
        "no uppercase property name should appear")))

(test regression-css-rule-symbol-keys-downcase
  "Non-keyword symbol keys also downcase. Reader uppercases symbol names
   by default, so unguarded `~A` would emit uppercase property names for
   any symbol key, not just keywords."
  (let ((rule (css-rule ".y" `((,(intern "MARGIN") . "1rem")))))
    (is (search "margin: 1rem" rule))
    (is (not (search "MARGIN: " rule)))))

(test regression-css-rule-string-keys-passthrough
  "String keys are passed through unchanged — including mixed-case
   strings, which must not be downcased."
  (let ((rule (css-rule ".z" '(("padding" . "0.5rem")
                               ("Border-Radius" . "4px")))))
    (is (search "padding: 0.5rem" rule))
    (is (search "Border-Radius: 4px" rule)
        "mixed-case string keys are preserved verbatim")))

(test regression-css-rule-mixed-keys
  "An alist mixing string, symbol, and keyword keys renders all of them
   correctly in a single rule."
  (let ((rule (css-rule ".mix" `(("padding" . "1rem")
                                 (:margin . "0")
                                 (,(intern "COLOR") . "black")))))
    (is (search "padding: 1rem" rule))
    (is (search "margin: 0" rule))
    (is (search "color: black" rule))))

;;; ============================================================================
;;; css-rules — alternating-pair convenience wrapper
;;; ============================================================================

(test regression-css-rules-keyword-keys-downcase
  "css-rules builds an alist from alternating pairs and delegates to
   css-rule, so the keyword-key normalisation applies transparently."
  (let ((rule (css-rules ".indicator" :opacity "0.7" :display "none")))
    (is (search "opacity: 0.7" rule))
    (is (search "display: none" rule))
    (is (not (search "OPACITY" rule)))
    (is (not (search "DISPLAY" rule)))))

;;; ============================================================================
;;; css-keyframes — composes css-rule
;;; ============================================================================

(test regression-css-keyframes-keyword-keys-downcase
  "css-keyframes renders each frame via css-rule, so keyword keys inside
   a frame's property alist are downcased the same way."
  (let ((kf (css-keyframes "fade"
              '("0%"   . ((:opacity . "0")))
              '("100%" . ((:opacity . "1"))))))
    (is (search "0% { opacity: 0;" kf))
    (is (search "100% { opacity: 1;" kf))
    (is (not (search "OPACITY" kf)))))

(test regression-css-keyframes-shape
  "css-keyframes emits a complete @keyframes block with each frame's
   selector wrapped in braces."
  (let ((kf (css-keyframes "spin"
              '("from" . (("transform" . "rotate(0deg)")))
              '("to"   . (("transform" . "rotate(360deg)"))))))
    (is (search "@keyframes spin {" kf))
    (is (search "from { transform: rotate(0deg); }" kf))
    (is (search "to { transform: rotate(360deg); }" kf))))

;;; ============================================================================
;;; safe-css-selector-p / safe-css-value-p / escape-css-value
;;; ============================================================================

(test regression-css-selector-rejects-curly
  "css-rule with an unsafe selector signals UNSAFE-CSS-SELECTOR.
   `;`, `{`, `}` would close the current rule and inject a sibling one."
  (signals unsafe-css-selector
    (css-rule ".x } body { background: red" '(("padding" . "0"))))
  (signals unsafe-css-selector
    (css-rule "; injected" '(("padding" . "0"))))
  (signals unsafe-css-selector
    (css-rule "{ open" '(("padding" . "0")))))

(test regression-safe-css-selector-p-shapes
  "Predicate accepts well-formed selectors and rejects dangerous chars."
  (is (safe-css-selector-p ".btn"))
  (is (safe-css-selector-p "#main"))
  (is (safe-css-selector-p "body > .x[data-foo=\"bar\"]"))
  (is (safe-css-selector-p "@media (min-width: 768px)"))
  (is (not (safe-css-selector-p "")))
  (is (not (safe-css-selector-p ".x; injected")))
  (is (not (safe-css-selector-p ".x { injected")))
  (is (not (safe-css-selector-p ".x } injected"))))

(test regression-css-value-escapes-backslash
  "escape-css-value rewrites `\\`, `;`, `}`, `<`, `>` into hex-escape
   form. The hex sequence is followed by a space — the CSS escape
   terminator — so adjoining characters never accidentally extend the
   escape sequence."
  (is (string= (escape-css-value "a;b") "a\\3B b"))
  (is (string= (escape-css-value "a}b") "a\\7D b"))
  (is (string= (escape-css-value "a\\b") "a\\5C b"))
  (is (string= (escape-css-value "a<b") "a\\3C b"))
  (is (string= (escape-css-value "a>b") "a\\3E b"))
  (is (string= (escape-css-value "plain") "plain")))

(test regression-css-rule-escapes-values
  "css-rule routes values through escape-css-value; attacker-controlled
   `;` cannot close the declaration to inject a sibling property.
   The escape neutralises the boundary character — the substring after
   `;` survives but as inert text within the preceding value, not as a
   new declaration."
  (let ((rule (css-rule ".x" '(("padding" . "1rem; background: red")))))
    (is (null (search "; background:" rule))
        "no un-escaped `; background:' boundary survives in output")
    (is (search "\\3B " rule)
        "the `;` separator must appear as the hex-escape form")))

;;; ============================================================================
;;; safe-css-selector-p — `<style>` break-out and `*/` comment-break classes
;;; ============================================================================

(test regression-safe-css-selector-rejects-style-break-out
  "safe-css-selector-p must reject `<`. Without that, an attacker-
   controlled selector emitted inside `<style>` can close the tag
   with `</style>` and pivot to HTML content position. `>` is left
   permitted because the child combinator (`body > .x`) is legitimate
   CSS — the break-out requires the leading `<`, not the closing `>`."
  (is (not (safe-css-selector-p ".x</style><script>")))
  (is (not (safe-css-selector-p ".x<svg>")))
  (is (safe-css-selector-p "body > .x") "child combinator must still pass")
  (signals unsafe-css-selector
    (css-rule "</style><script>alert(1)</script>" '(("color" . "red")))))

(test regression-safe-css-selector-rejects-comment-close
  "safe-css-selector-p must reject the literal `*/` sequence. CSS
   comments are stripped before parsing, so an attacker who closes one
   mid-selector can hide additional rules between the boundaries."
  (is (not (safe-css-selector-p ".x */ body { background: red")))
  (signals unsafe-css-selector
    (css-rule ".x */ body" '(("color" . "red")))))

;;; ============================================================================
;;; safe-css-ident-p / css-rule property-name gate
;;; ============================================================================

(test regression-safe-css-ident-p-shapes
  "safe-css-ident-p accepts CSS identifiers (letter / digit / `-` /
   `_`, leading non-digit) and rejects whitespace, punctuation, and
   the empty string."
  (is (safe-css-ident-p "color"))
  (is (safe-css-ident-p "border-radius"))
  (is (safe-css-ident-p "-webkit-mask"))
  (is (safe-css-ident-p "_private"))
  (is (not (safe-css-ident-p "")))
  (is (not (safe-css-ident-p "color: red")))
  (is (not (safe-css-ident-p "color;injected")))
  (is (not (safe-css-ident-p "1color")) "leading digit must be rejected"))

(test regression-css-rule-rejects-unsafe-property-name
  "css-rule routes property names through safe-css-ident-p so an
   attacker-controlled property name cannot smuggle CSS syntax."
  (signals unsafe-css-ident
    (css-rule ".x" '(("color: red; background" . "red"))))
  (signals unsafe-css-ident
    (css-rule ".x" '(("padding}injected" . "0"))))
  (signals unsafe-css-ident
    (css-rule ".x" '(("" . "0")))))

;;; ============================================================================
;;; safe-css-payload-string — type gate at the @-rule and section boundaries
;;; ============================================================================

(test regression-make-safe-css-payload-string-refuses-non-string
  "Constructor refuses non-string input (numbers, symbols, lists).
   Producer-asserted trust must start from a real string."
  (signals type-error (make-safe-css-payload-string 42))
  (signals type-error (make-safe-css-payload-string :foo))
  (signals type-error (make-safe-css-payload-string '(rule))))

(test regression-make-safe-css-payload-string-idempotent
  "Constructor is idempotent: re-tagging an already-typed value
   returns the same instance, so callers can freely upgrade strings
   without thinking about layering."
  (let* ((p1 (make-safe-css-payload-string ".x { color: red; }"))
         (p2 (make-safe-css-payload-string p1)))
    (is (eq p1 p2))))

(test regression-css-keyframes-refuses-unsafe-name
  "css-keyframes refuses any animation name that fails safe-css-ident-p."
  (signals unsafe-css-ident
    (css-keyframes "fade } body { background: red"
                   '("0%" . (("opacity" . "0")))))
  (signals unsafe-css-ident
    (css-keyframes "" '("0%" . (("opacity" . "0"))))))

(test regression-css-media-refuses-raw-string
  "css-media refuses a raw string for the query argument — must be
   tagged safe-css-payload-string."
  (signals type-error
    (css-media "(min-width: 768px)"
               (make-safe-css-payload-string ".x { color: red; }"))))

(test regression-css-media-refuses-raw-rule
  "css-media refuses a raw string in the rules-list."
  (signals type-error
    (css-media (make-safe-css-payload-string "(min-width: 768px)")
               ".x { color: red; }")))

(test regression-css-media-accepts-tagged-payload
  "css-media composes a typed query and typed rules into a single block."
  (let ((out (css-media (make-safe-css-payload-string "(min-width: 768px)")
                        (make-safe-css-payload-string ".x { color: red; }"))))
    (is (search "@media (min-width: 768px)" out))
    (is (search ".x { color: red; }" out))))

(test regression-css-section-refuses-raw-string
  "css-section refuses raw strings for the name AND the rules."
  (signals type-error
    (css-section "Buttons"
                 (make-safe-css-payload-string ".btn { padding: 1rem; }")))
  (signals type-error
    (css-section (make-safe-css-payload-string "Buttons")
                 ".btn { padding: 1rem; }")))

(test regression-css-section-rejects-comment-break-in-name
  "Wrapping in make-safe-css-payload-string is a producer claim; if the
   producer feeds `*/`, the type-tag carries it through. The discipline
   is at the producer, but consumers should know what slips: a name
   containing `*/` closes the surrounding comment. Test confirms the
   text appears verbatim in the comment, so producers are warned."
  (let ((out (css-section
              (make-safe-css-payload-string "Inert text")
              (make-safe-css-payload-string ".x { color: red; }"))))
    (is (search "/* --- Inert text ---" out)
        "header text must appear inside the comment delimiters")))

(test regression-css-var-refuses-unsafe-name
  "css-var refuses anything that fails safe-css-ident-p."
  (signals unsafe-css-ident (css-var "primary; injected"))
  (signals unsafe-css-ident (css-var "")))

(test regression-css-var-definition-refuses-raw-value
  "css-var-definition requires the value as a tagged payload; raw
   strings signal TYPE-ERROR."
  (signals type-error (css-var-definition "primary" "#00FF41")))

(test regression-css-var-definition-accepts-tagged-value
  "css-var-definition accepts a tagged payload value and emits
   `--<name>: <value>`."
  (let ((out (css-var-definition "primary"
                                 (make-safe-css-payload-string "#00FF41"))))
    (is (string= "--primary: #00FF41" out))))

;;; ============================================================================
;;; escape-css-ident — fallback for callers that can't earn safe-css-ident-p
;;; ============================================================================

(test regression-escape-css-ident-hex-encodes-unsafe-chars
  "escape-css-ident rewrites whitespace, punctuation, and non-ASCII
   into the CSS hex-escape form, so a hostile source identifier cannot
   smuggle a `;` or `}` boundary."
  (let ((out (escape-css-ident "a;b}c")))
    (is (search "\\3B " out))
    (is (search "\\7D " out))))

(test regression-escape-css-value-escapes-newline
  "escape-css-value hex-escapes newline and carriage-return so an
   attacker value cannot inject a line break a downstream tool might
   read as a statement boundary. Quotes/parens are deliberately left
   intact — every token value (incl. quoted font-family and var()/rgb())
   routes through here, so escaping them would corrupt legitimate CSS."
  (is (string= "a\\A b" (escape-css-value (format nil "a~Cb" #\Newline))))
  (is (string= "a\\D b" (escape-css-value (format nil "a~Cb" #\Return))))
  (is (string= "a\"b" (escape-css-value "a\"b")))
  (is (string= "(a)" (escape-css-value "(a)"))))

(test regression-escape-css-value-url-context-residual-documented
  "Pins the documented A-M01 residual: escape-css-value closes the
   declaration/rule/<style>-tag boundary (`;}<>` escaped) but NOT the
   value-internal url()/string context. `\"`, `'`, `(`, `)` pass through by
   design (every framework value routes through here), so this function
   alone is INSUFFICIENT for an untrusted value bound for a url()/content/
   font-family property — callers must restrict such properties to
   framework-generated values or a dedicated CSS-value trust type."
  ;; rule/tag-breakout characters ARE neutralised
  (is (search "\\3B " (escape-css-value "a;b")))
  (is (search "\\3C " (escape-css-value "a<b")))
  (is (search "\\7D " (escape-css-value "a}b")))
  ;; value-context characters deliberately pass through (the residual)
  (is (string= "url(http://evil/?leak)" (escape-css-value "url(http://evil/?leak)"))
      "url() functional notation must survive verbatim — the residual exposure")
  (is (find #\" (escape-css-value "say \"hi\""))
      "double quote passes through unescaped by design")
  (is (find #\' (escape-css-value "it's"))
      "single quote passes through unescaped by design"))
