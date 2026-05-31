(in-package :lol-web/optimization/test)
(in-suite :lol-web/optimization/test)

;;; ============================================================================
;;; with-reactive-bindings: no macro-time eval of the controller
;;; ============================================================================

(test regression-with-reactive-bindings-takes-explicit-names
  "with-reactive-bindings macro-expands to a let that holds the
   controller, never invoking it at macro-expansion time. Macro-time
   evaluation of a runtime variable would only succeed for literal
   forms and would mask symbol-resolution failures."
  (let ((expansion (macroexpand-1
                    '(lol-web/optimization:with-reactive-bindings (some-var x y)
                      (+ x y)))))
    (is (consp expansion)
        "with-reactive-bindings must expand to a form")
    (is (eq (car expansion) 'let)
        "expansion wraps the controller in a let")))

(test regression-with-reactive-bindings-runtime-fetches-named-bindings
  "Runtime: with-reactive-bindings fetches each named binding through the
   controller's :get message and binds it locally for the body."
  (let* ((calls nil)
         (controller (lambda (msg &rest args)
                       (push (cons msg args) calls)
                       (case msg
                         (:get (case (first args)
                                 (x 10)
                                 (y 32)))
                         (t nil)))))
    (let ((sum (lol-web/optimization:with-reactive-bindings (controller x y)
                 (+ x y))))
      (is (= sum 42)
          "with-reactive-bindings binds named values from controller :get")
      (is (member '(:get x) calls :test #'equal)
          "controller was queried for x")
      (is (member '(:get y) calls :test #'equal)
          "controller was queried for y"))))

;;; ============================================================================
;;; CSS prefix-matching: bare prefixes must not validate as classes
;;; ============================================================================

(test regression-css-prefix-bare-not-valid
  "A registered prefix like \"p-\" must not itself validate as a CSS
   class — bare prefixes are not real Tailwind class names. Validation
   accepts a class only when it extends a prefix with at least one
   trailing character."
  (let ((lol-web/optimization:*registered-css-classes* (make-hash-table :test 'equal))
        (lol-web/optimization:*registered-css-prefixes* nil))
    (lol-web/optimization:register-tailwind-classes)
    (signals warning
             (lol-web/optimization:validate-css-class "p-"))
    (signals warning
             (lol-web/optimization:validate-css-class "text-"))))

(defun %warnings-emitted (thunk)
  "Collect every WARNING signalled while THUNK runs, returning the list
   of condition objects. Lets tests assert on the absence of warnings
   without confusing fiveam's IS macro about nil vs no-warning."
  (let ((warnings nil))
    (handler-bind ((warning (lambda (c)
                              (push c warnings)
                              (muffle-warning c))))
      (funcall thunk))
    (nreverse warnings)))

(test regression-css-prefix-extends-prefix-validates
  "A class that extends a registered prefix (p-4, text-red-500,
   hover:bg-blue-500) validates without warning. Tailwind class names
   compose freely after the prefix, so prefix support is required to
   avoid noise on every utility class."
  (let ((lol-web/optimization:*registered-css-classes* (make-hash-table :test 'equal))
        (lol-web/optimization:*registered-css-prefixes* nil))
    (lol-web/optimization:register-tailwind-classes)
    (let ((warnings (%warnings-emitted
                     (lambda ()
                       (lol-web/optimization:validate-css-class "p-4")
                       (lol-web/optimization:validate-css-class "text-red-500")
                       (lol-web/optimization:validate-css-class "hover:bg-blue-500")))))
      (is (null warnings)
          "extending classes p-4, text-red-500, hover:bg-blue-500 must not warn"))))

(test regression-css-static-utility-validates
  "Static utility classes (flex, grid, block, inline, hidden) live in
   the exact-match registry and validate without involving the prefix
   table."
  (let ((lol-web/optimization:*registered-css-classes* (make-hash-table :test 'equal))
        (lol-web/optimization:*registered-css-prefixes* nil))
    (lol-web/optimization:register-tailwind-classes)
    (let ((warnings (%warnings-emitted
                     (lambda ()
                       (lol-web/optimization:validate-css-class "flex")
                       (lol-web/optimization:validate-css-class "block")))))
      (is (null warnings)
          "static utilities flex/block must not warn"))))

;;; ============================================================================
;;; analyze-dependencies: smoke-level structural check
;;; ============================================================================

(test smoke-analyze-dependencies-detects-direct-references
  "analyze-dependencies returns a hash table mapping each binding to the
   subset of other binding names it references in its value form."
  (let ((g (lol-web/optimization:analyze-dependencies
            '((count 0)
              (doubled (* count 2))
              (message (format nil "~A ~A" count doubled))))))
    (is (hash-table-p g))
    (is (null (gethash 'count g))
        "root binding has no dependencies")
    (is (equal '(count) (gethash 'doubled g))
        "doubled depends on count")
    (let ((deps (gethash 'message g)))
      (is (and (member 'count deps) (member 'doubled deps))
          "message depends on count and doubled"))))

;;; ============================================================================
;;; defvalidated-template: cl-who:str of unescaped input warns
;;; ============================================================================

(test regression-template-lint-flags-str-of-user-input
  "defvalidated-template walks its body at macro-expansion time and
   warns when (cl-who:str X) wraps a form that is neither a literal
   string nor a call to a recognised escape/unwrap wrapper. The bare
   (cl-who:str user-data) shape inlines user-data into HTML without
   escaping — an XSS vector."
  (let ((warnings (%warnings-emitted
                   (lambda ()
                     (macroexpand-1
                      '(lol-web/optimization:defvalidated-template
                           %lint-str-unsafe-tpl (user-data)
                         (:div (cl-who:str user-data))))))))
    (is (some (lambda (c)
                (search "cl-who:str" (princ-to-string c)))
              warnings)
        "macro-expansion must warn about cl-who:str of unescaped input")))

(test regression-template-lint-allows-str-of-escaped-input
  "(cl-who:str (cl-who:esc X)) — explicitly escaped — does not trigger
   the lint. The wrapper list also accepts escape-html, safe-str,
   coerce-html-emit, and safe-html-string-value."
  (let ((warnings (%warnings-emitted
                   (lambda ()
                     (macroexpand-1
                      '(lol-web/optimization:defvalidated-template
                           %lint-str-safe-tpl (user-data)
                         (:div (cl-who:str (cl-who:esc user-data)))))))))
    (is (notany (lambda (c)
                  (search "cl-who:str" (princ-to-string c)))
                warnings)
        "escaped cl-who:str must not warn about unsafe emission")))

;;; ============================================================================
;;; lint-hx-on-not-literal: dynamic event-handler payloads are RCE surface
;;; ============================================================================

(test regression-hx-on-lint-flags-dynamic-value
  "An :hx-on-* attribute whose value is anything other than a literal
   string is reported as a warning. Dynamic computation of the inline
   handler body escapes static review; the lint refuses it."
  (let ((warnings (lol-web/optimization:lint-hx-on-not-literal
                   '(:button :hx-on-click (format nil "alert(~A)" user-input)
                     "ok"))))
    (is (= 1 (length warnings))
        "one non-literal hx-on-click value must produce exactly one warning")
    (is (search "hx-on-click" (first warnings)))
    (is (search "format" (first warnings)))))

(test regression-hx-on-lint-passes-literal-string
  "Literal-string values for :hx-on-* are accepted — the contract is
   that the inline handler body is visible at template-load time."
  (let ((warnings (lol-web/optimization:lint-hx-on-not-literal
                   '(:button :hx-on-click "alert('ok')" "ok"))))
    (is (null warnings)
        "literal-string :hx-on-click must not produce a warning")))

(test regression-hx-on-lint-walks-nested-elements
  "A non-literal hx-on-* hidden under a nested element still triggers
   the lint — the walker descends into children, not just the top form."
  (let ((warnings (lol-web/optimization:lint-hx-on-not-literal
                   '(:form :class "outer"
                     (:button :hx-on-click foo "submit")))))
    (is (= 1 (length warnings)))))
