;;;; Regression tests for js-value (src/client/parenscript.lisp).
;;;;
;;;; NIL is both null and symbol in CL, so the typecase that produces JS
;;;; literals must dispatch on null before symbol — otherwise NIL stringifies
;;;; to "nil" instead of "null", and downstream JSON.parse / setState calls
;;;; explode at runtime.

(in-package :lol-web/parenscript/test)
(in-suite :lol-web/parenscript/test)

(test regression-js-value-nil-is-null
  "js-value converts nil to the JS literal `null`, not `nil`."
  (let ((v (lol-web/parenscript::js-value nil)))
    (is (safe-js-string-literal-p v))
    (is (string= "null" (safe-js-string-literal-value v)))))

(test regression-js-value-symbol
  "js-value tags non-nil symbols with their downcased name."
  (let ((source (safe-js-string-literal-value
                 (lol-web/parenscript::js-value 'foo))))
    (is (search "foo" source :test #'char-equal))))

(test regression-js-value-numbers
  "js-value tags numbers with their printed form."
  (is (string= "42"   (safe-js-string-literal-value
                       (lol-web/parenscript::js-value 42))))
  (is (string= "3.14" (safe-js-string-literal-value
                       (lol-web/parenscript::js-value 3.14)))))

(test regression-js-value-strings
  "js-value wraps strings in single quotes."
  (let ((source (safe-js-string-literal-value
                 (lol-web/parenscript::js-value "hello"))))
    (is (char= #\' (char source 0)))
    (is (search "hello" source))))

(test regression-generate-ws-client-derives-protocol
  "generate-ws-client uses window.location.protocol so https pages get wss://"
  (let ((js (lol-web/parenscript::generate-ws-client "abc")))
    (is (stringp js))
    (is (search "window.location.protocol" js)
        "missing protocol switch — hardcoded ws:// breaks https pages")
    (is (search "'wss://'" js) "missing wss:// branch")
    (is (search "'ws://'"  js) "missing ws:// branch")))

(test regression-component-client-script-on-mount-call
  "component-client-script accesses on-mount with string key and invokes it"
  (let ((js (lol-web/parenscript::component-client-script "abc")))
    (is (stringp js))
    (is (search "['on-mount']()" js)
        "on-mount must be accessed as ['on-mount'] and invoked with ()")
    (is (null (search ".at('on-mount')" js))
        ".at('on-mount') is the broken (ps:chain ... (ps:@ :on-mount)) form")))

;;; ============================================================================
;;; jsonify — JS-literal coercion at the parenscript boundary
;;;
;;; Anything parenscript would translate as JS code (symbols → identifiers,
;;; conses → function calls) must either be coerced to a JS-string-literal
;;; equivalent or signal at the helper. No caller-controlled value may reach
;;; JS code position.
;;; ============================================================================

(test regression-jsonify-passes-strings-through
  "Strings reach parenscript unchanged and emit as JS string literals."
  (is (string= "hello" (jsonify "hello")))
  (is (string= "" (jsonify ""))))

(test regression-jsonify-passes-numbers-through
  "Numbers reach parenscript unchanged so they emit as JS number literals."
  (is (= 42 (jsonify 42)))
  (is (= 0 (jsonify 0)))
  (is (= -3.14 (jsonify -3.14))))

(test regression-jsonify-passes-booleans-through
  "T and NIL reach parenscript as themselves and emit as true/null."
  (is (eq t (jsonify t)))
  (is (eq nil (jsonify nil))))

(test regression-jsonify-coerces-symbol-to-downcased-string
  "Symbols become their downcased name so parenscript emits a string literal."
  (is (string= "fetch-action" (jsonify 'fetch-action)))
  (is (string= "value" (jsonify 'value))))

(test regression-jsonify-signals-on-cons
  "Cons cells in arg position are the RCE vector; helper must signal."
  (signals error (jsonify '(alert "x")))
  (signals error (jsonify (list 1 2 3)))
  (signals error (jsonify '(funcall window.location.replace "//evil"))))

(test regression-jsonify-signals-on-hash-table
  "Non-atomic types other than cons also signal, with the type named."
  (signals error (jsonify (make-hash-table)))
  (signals error (jsonify #(1 2 3))))

;;; ============================================================================
;;; on-click / on-change / on-submit — every dynamic arg routes through jsonify
;;; ============================================================================

(test regression-on-click-rejects-cons-action
  "Cons in the action position would emit JS function-call code; refuse it."
  (signals error (on-click "ctl" '(alert "x"))))

(test regression-on-click-rejects-cons-arg
  "Cons in &rest args would emit JS function-call code; refuse it."
  (signals error (on-click "ctl" "click" '(alert "x"))))

(test regression-on-click-symbol-arg-emits-string-literal
  "Symbol &rest arg becomes a quoted JS string, never a bare identifier."
  (let ((js (on-click "ctl" "click" 'mallory)))
    (is (search "'mallory'" js)
        "symbol arg must round-trip as a JS string literal")
    (is (null (search ", mallory)" js))
        "symbol arg must not appear as a bare JS identifier")))

(test regression-on-click-string-and-number-args-emit-as-literals
  "Strings emit as JS strings, numbers as JS numbers — both are safe."
  (let ((js (on-click "ctl" "click" "alice" 42 t nil)))
    (is (search "'alice'" js))
    (is (search "42" js))
    (is (search "true" js))
    (is (search "null" js))))

(test regression-on-click-rejects-non-string-component-id
  "COMPONENT-ID must be a string; numbers/symbols/lists all signal."
  (signals type-error (on-click :sym "click"))
  (signals type-error (on-click 42 "click"))
  (signals type-error (on-click '(x) "click")))

(test regression-on-change-rejects-cons-state-key
  "Cons in the state-key position would emit JS function-call code; refuse it."
  (signals error (on-change "ctl" '(arbitrary-call))))

(test regression-on-change-symbol-key-emits-string-literal
  "Symbol state-key becomes a quoted JS string."
  (let ((js (on-change "ctl" 'value)))
    (is (search "'value'" js))
    (is (null (search ", value," js))
        "symbol state-key must not appear as a bare JS identifier")))

(test regression-on-submit-rejects-cons-action
  "Cons in the action position would emit JS function-call code; refuse it."
  (signals error (on-submit "ctl" '(window.location.replace "//evil"))))

(test regression-on-submit-symbol-action-emits-string-literal
  "Symbol action becomes a quoted JS string."
  (let ((js (on-submit "ctl" 'register)))
    (is (search "'register'" js))
    (is (search "preventDefault" js)
        "preventDefault prefix must still be emitted")))

;;; ============================================================================
;;; safe-js-string-literal — escape-js-string covers quote / line-separator
;;; break-out classes, type-gate at hx-dispatch / hx-bind refuses raw input
;;; ============================================================================

(test regression-escape-js-string-escapes-single-quote-and-line-separator
  "escape-js-string rewrites single-quote (close-literal), U+2028 (LINE
   SEPARATOR — a JS string-terminator that does not look like a newline),
   and U+2029 (PARAGRAPH SEPARATOR — same). Without these, a literal U+2028
   inside an attacker-controlled string flips a quoted JS literal into
   code position even though the source looks like a normal string."
  (let ((esc (escape-js-string (format nil "x'~Cy~Cz" (code-char #x2028)
                                       (code-char #x2029)))))
    (is (search "\\'" esc) "single quote must be backslash-escaped")
    (is (search "\\u2028" esc) "U+2028 must be unicode-escaped")
    (is (search "\\u2029" esc) "U+2029 must be unicode-escaped")))

(test regression-js-value-escapes-single-quote-and-line-separator
  "js-value of an injection-shaped string returns the escaped JS literal,
   not the raw injection. The leading `'` (which would close the literal
   and pivot to code position) is rewritten to `\\'`; the rest of the
   payload survives as inert string content inside the still-open
   literal."
  (let ((source (safe-js-string-literal-value
                 (lol-web/parenscript::js-value "'); alert(1); //"))))
    ;; Outer wrapping single quotes are present.
    (is (char= #\' (char source 0)))
    (is (char= #\' (char source (1- (length source)))))
    ;; The first inner character is `\\` — the leading `'` was escaped.
    (is (char= #\\ (char source 1)))
    (is (char= #\' (char source 2)))
    ;; Sanity: the unescaped close-quote sequence `';` does NOT appear.
    (is (null (search "';" source))
        "an un-escaped single-quote+semicolon would close the literal")))

(test regression-js-value-refuses-cons
  "js-value of a cons signals TYPE-ERROR — RCE-shaped inputs cannot
   reach JS source position."
  (signals type-error (lol-web/parenscript::js-value '(alert "x"))))

(test regression-hx-dispatch-refuses-raw-string
  "hx-dispatch refuses raw strings: each input must be tagged via
   make-safe-js-string-literal / js-value at the producer."
  (signals type-error (hx-dispatch "comp-1" "submit"))
  (signals type-error
    (hx-dispatch (make-safe-js-string-literal "comp-1") "submit"))
  (signals type-error
    (hx-dispatch (make-safe-js-string-literal "comp-1")
                 (make-safe-js-string-literal "submit")
                 :name "alice")))

(test regression-hx-dispatch-accepts-tagged-inputs
  "When every input is tagged, hx-dispatch emits data-dispatch /
   data-action / data-arg-NAME with HTML-attribute-escaped values."
  (let ((html (hx-dispatch
               (make-safe-js-string-literal "comp-1")
               (make-safe-js-string-literal "submit")
               :name (make-safe-js-string-literal "alice"))))
    (is (search "data-dispatch=" html))
    (is (search "data-action=" html))
    (is (search "data-arg-NAME=" html))
    (is (search "alice" html))))

(test regression-hx-bind-refuses-raw-string
  "hx-bind refuses raw strings on either argument."
  (signals type-error (hx-bind "comp-1" "value"))
  (signals type-error (hx-bind (make-safe-js-string-literal "comp-1") "value"))
  (signals type-error (hx-bind "comp-1" (make-safe-js-string-literal "value"))))

(test regression-hx-bind-accepts-tagged-inputs
  "When both arguments are tagged, hx-bind emits data-bind / data-state."
  (let ((html (hx-bind (make-safe-js-string-literal "comp-1")
                       (make-safe-js-string-literal "value"))))
    (is (search "data-bind=" html))
    (is (search "data-state=" html))))

(test regression-make-safe-js-string-literal-refuses-non-string
  "The constructor refuses non-string input rather than coerce silently.
   The trust contract is producer-asserted; coerce-shims invite escape-
   hatches at the sink."
  (signals type-error (make-safe-js-string-literal 42))
  (signals type-error (make-safe-js-string-literal '(:foo))))

(test regression-hx-dispatch-key-validated
  "hx-dispatch confines each arg KEY to letters/digits/-/_ before emitting
   it in data-arg-<key> attribute-NAME position, where escape-attribute
   cannot protect — a key bearing a space or quote signals instead of
   breaking out into a second attribute."
  (let ((cid (make-safe-js-string-literal "comp"))
        (act (make-safe-js-string-literal "act"))
        (val (make-safe-js-string-literal "v")))
    (let ((s (hx-dispatch cid act :count val)))
      (is (search "data-arg-" s))
      (is (search "data-dispatch=" s)))
    (signals error
      (hx-dispatch cid act (intern "x\" onx=y" :keyword) val))
    (signals error
      (hx-dispatch cid act (intern "bad key" :keyword) val))))
