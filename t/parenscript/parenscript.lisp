(in-package :lol-web/parenscript/test)
(in-suite :lol-web/parenscript/test)

;;; ============================================================================
;;; js-value — internal helper, accessed via package qualifier
;;; ============================================================================

(test js-value-nil
  "js-value tags nil as the JS literal `null`."
  (let ((v (lol-web/parenscript::js-value nil)))
    (is (safe-js-string-literal-p v))
    (is (string= "null" (safe-js-string-literal-value v)))))

(test js-value-numbers
  "js-value tags numbers as their printed JS form."
  (is (string= "42"  (safe-js-string-literal-value
                      (lol-web/parenscript::js-value 42))))
  (is (string= "0"   (safe-js-string-literal-value
                      (lol-web/parenscript::js-value 0))))
  (is (string= "-10" (safe-js-string-literal-value
                      (lol-web/parenscript::js-value -10)))))

(test js-value-strings
  "js-value tags strings as single-quoted JS string literals."
  (let* ((v (lol-web/parenscript::js-value "hello"))
         (source (safe-js-string-literal-value v)))
    (is (safe-js-string-literal-p v))
    (is (> (length source) (length "hello")))
    (is (search "hello" source))))

(test js-value-symbols
  "js-value tags symbols as their downcased name in JS literal form."
  (let ((source (safe-js-string-literal-value
                 (lol-web/parenscript::js-value 'my-symbol))))
    (is (search "my-symbol" source :test #'char-equal))))

;;; ============================================================================
;;; Event-handler JS generation
;;; ============================================================================

(test on-click-symbol-action-emits-js-string-literal
  "Symbol action becomes a JS string literal, never a JS identifier."
  (let ((result (on-click "test-comp" 'fetch-action)))
    (is (stringp result))
    (is (search "dispatch" result))
    (is (search "'fetch-action'" result)
        "symbol action must be emitted as quoted JS string, not bare identifier")))

(test on-change-symbol-key-emits-js-string-literal
  "Symbol state-key becomes a JS string literal, never a JS identifier."
  (let ((result (on-change "test-comp" 'value)))
    (is (stringp result))
    (is (search "setState" result :test #'char-equal))
    (is (search "'value'" result)
        "symbol state-key must be emitted as quoted JS string, not bare identifier")))

