;;;; LOL-REACTIVE Test Suite - Parenscript Utilities
;;;;
;;;; Tests for Parenscript helpers and JS generation.

(in-package :lol-reactive.tests)
(in-suite :parenscript)

;;; ============================================================================
;;; JS-VALUE TESTS
;;; ============================================================================

(test js-value-nil
  "js-value converts nil to null"
  (is (string= "null" (lol-reactive::js-value nil))))

(test js-value-numbers
  "js-value handles integers and floats"
  (is (string= "42" (lol-reactive::js-value 42)))
  (is (string= "0" (lol-reactive::js-value 0)))
  (is (string= "-10" (lol-reactive::js-value -10))))

(test js-value-strings
  "js-value wraps strings in quotes"
  (let ((result (lol-reactive::js-value "hello")))
    (is (stringp result))
    (is (> (length result) (length "hello")))
    (is (search "hello" result))))

(test js-value-symbols
  "js-value converts symbols to lowercase strings"
  (let ((result (lol-reactive::js-value 'my-symbol)))
    (is (stringp result))
    (is (search "my-symbol" result :test #'char-equal))))

;;; ============================================================================
;;; EVENT HANDLER TESTS
;;; ============================================================================

(test on-click-generates-js
  "on-click generates dispatch JavaScript"
  ;; on-click takes (component-id action &rest args)
  (let ((result (lol-reactive:on-click "test-comp" '(alert "clicked"))))
    (is (stringp result))
    (is (search "dispatch" result :test #'char-equal))))

(test on-change-generates-js
  "on-change generates setState JavaScript"
  ;; on-change takes (component-id state-key)
  (let ((result (lol-reactive:on-change "test-comp" 'value)))
    (is (stringp result))
    (is (search "setState" result :test #'char-equal))))

;;; ============================================================================
;;; REACTIVE RUNTIME TESTS
;;; ============================================================================

(test reactive-runtime-js-exists
  "reactive-runtime-js function exists"
  (is (fboundp 'lol-reactive:reactive-runtime-js)))

(test reactive-runtime-js-generates-code
  "reactive-runtime-js generates JavaScript"
  (let ((js (lol-reactive:reactive-runtime-js)))
    (is (stringp js))
    (is (> (length js) 100))))
