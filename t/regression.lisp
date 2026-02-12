;;;; LOL-REACTIVE Test Suite - Regression Tests
;;;;
;;;; Regression tests for bugs fixed in specific commits.
;;;; These tests ensure fixed bugs don't reappear.
;;;;
;;;; Commit d99f073 (2026-01-23):
;;;; - html-sexp-attrs only returned first attribute pair
;;;; - js-value returned 'nil' instead of 'null' for nil
;;;; - oob-swap created duplicate IDs when content had target ID
;;;; - find-tag-end failed with > inside quoted attributes

(in-package :lol-reactive.tests)
(in-suite :regression)

;;; ============================================================================
;;; d99f073: html-sexp-attrs plist iteration bug
;;; Fixed in: rendering/dom-diff.lisp:22
;;; Problem: Was iterating with (loop for item in ...) instead of
;;;          (loop for (key val) on ... by #'cddr)
;;; ============================================================================

(test regression-html-sexp-attrs-all-pairs
  "html-sexp-attrs returns ALL attribute pairs, not just first"
  (let ((sexp '(:div :id "test" :class "foo" :data-x "bar")))
    (let ((attrs (lol-reactive::html-sexp-attrs sexp)))
      ;; Should have 3 key-value pairs = 6 elements
      (is (= 6 (length attrs)))
      (is (eq :id (first attrs)))
      (is (string= "test" (second attrs)))
      (is (eq :class (third attrs)))
      (is (string= "foo" (fourth attrs)))
      (is (eq :data-x (fifth attrs)))
      (is (string= "bar" (sixth attrs))))))

(test regression-html-sexp-attrs-single-pair
  "html-sexp-attrs works correctly with single attribute"
  (let ((sexp '(:span :class "highlight" "content")))
    (let ((attrs (lol-reactive::html-sexp-attrs sexp)))
      (is (= 2 (length attrs)))
      (is (eq :class (first attrs)))
      (is (string= "highlight" (second attrs))))))

(test regression-html-sexp-attrs-no-attrs
  "html-sexp-attrs returns empty list when no attributes"
  (let ((sexp '(:p "just text")))
    (let ((attrs (lol-reactive::html-sexp-attrs sexp)))
      (is (null attrs)))))

;;; ============================================================================
;;; d99f073: js-value nil handling bug
;;; Fixed in: parenscript-utils.lisp:64
;;; Problem: NIL is both null and symbol in CL; specific types must
;;;          come before general types in typecase
;;; ============================================================================

(test regression-js-value-nil-is-null
  "js-value converts nil to 'null', not 'nil'"
  (is (string= "null" (lol-reactive::js-value nil))))

(test regression-js-value-symbol
  "js-value still works correctly for non-nil symbols"
  (let ((result (lol-reactive::js-value 'foo)))
    (is (stringp result))
    (is (search "foo" result :test #'char-equal))))

(test regression-js-value-numbers
  "js-value handles numbers correctly"
  (is (string= "42" (lol-reactive::js-value 42)))
  (is (string= "3.14" (lol-reactive::js-value 3.14))))

(test regression-js-value-strings
  "js-value wraps strings in quotes"
  (let ((result (lol-reactive::js-value "hello")))
    (is (char= #\' (char result 0)))
    (is (search "hello" result))))

;;; ============================================================================
;;; d99f073: oob-swap duplicate ID bug
;;; Fixed in: htmx/runtime.lisp:342
;;; Problem: When content already had the target ID, wrapping created
;;;          duplicate IDs like <div id="x"><div id="x">...</div></div>
;;; ============================================================================

(test regression-oob-swap-no-duplicate-id
  "oob-swap with outerHTML doesn't wrap when content has target ID"
  (let ((html "<div id=\"target\">content</div>"))
    (let ((result (lol-reactive:oob-swap "target" html :swap "outerHTML")))
      ;; Should inject attribute, not wrap in another div
      ;; Check there's no duplicate div pattern
      (is (null (search "<div id=\"target\"><div id=\"target\"" result)))
      ;; Should have hx-swap-oob attribute injected
      (is (search "hx-swap-oob" result)))))

(test regression-oob-swap-wrap-when-no-id
  "oob-swap still wraps when content lacks target ID"
  (let ((html "<span>some content</span>"))
    (let ((result (lol-reactive:oob-swap "my-target" html :swap "innerHTML")))
      ;; Should wrap with target ID
      (is (search "id=\"my-target\"" result))
      (is (search "hx-swap-oob" result)))))

(test regression-oob-swap-innerhtml-always-wraps
  "oob-swap innerHTML strategy always wraps"
  (let ((html "<div id=\"target\">content</div>"))
    (let ((result (lol-reactive:oob-swap "target" html :swap "innerHTML")))
      ;; innerHTML strategy wraps even if content has ID
      (is (search "hx-swap-oob" result)))))

;;; ============================================================================
;;; d99f073: find-tag-end quoted > bug
;;; Fixed in: htmx/runtime.lisp:289
;;; Problem: Simple (position #\>) fails when attributes contain >
;;;          like value="a > b". Must track quote state.
;;; ============================================================================

(test regression-find-tag-end-simple
  "find-tag-end works with simple tags"
  (is (= 4 (lol-reactive::find-tag-end "<div>")))
  (is (= 17 (lol-reactive::find-tag-end "<div class=\"test\">x"))))

(test regression-find-tag-end-quoted-gt
  "find-tag-end handles > inside quoted attributes"
  ;; This was the actual bug case
  (let ((html "<input value=\"a > b\" />"))
    (is (= 22 (lol-reactive::find-tag-end html)))))

(test regression-find-tag-end-multiple-quoted-gt
  "find-tag-end handles multiple > in quotes"
  (let ((html "<div title=\"x > y > z\" class=\"a > b\">content"))
    (let ((pos (lol-reactive::find-tag-end html)))
      ;; Should find the > after the last quote, not the ones inside
      (is (numberp pos))
      (is (char= #\> (char html pos))))))

(test regression-find-tag-end-self-closing
  "find-tag-end works with self-closing tags"
  (is (= 4 (lol-reactive::find-tag-end "<br/>")))
  (is (= 5 (lol-reactive::find-tag-end "<br />")))
  (is (= 30 (lol-reactive::find-tag-end "<input type=\"text\" value=\">\" />"))))

;;; ============================================================================
;;; d99f073: inject-oob-attribute self-closing tag bug
;;; Related fix: Insert attribute before / not before > for self-closing
;;; ============================================================================

(test regression-inject-oob-self-closing
  "inject-oob-attribute correctly handles self-closing tags"
  (let ((html "<input type=\"text\" />"))
    (let ((result (lol-reactive::inject-oob-attribute html "outerHTML")))
      ;; Attribute should be injected before the closing
      (is (search "hx-swap-oob=\"outerHTML\"" result))
      ;; Should end with />
      (is (search "/>" result)))))

(test regression-inject-oob-regular-tag
  "inject-oob-attribute works with regular tags"
  (let ((html "<div class=\"test\">content</div>"))
    (let ((result (lol-reactive::inject-oob-attribute html "true")))
      (is (search "hx-swap-oob=\"true\"" result)))))

;;; ============================================================================
;;; d99f073: content-starts-with-id-p quote-aware
;;; Related fix: Must use find-tag-end to handle > in quotes
;;; ============================================================================

(test regression-content-starts-with-id-basic
  "content-starts-with-id-p detects ID in simple element"
  (is (lol-reactive::content-starts-with-id-p
        "<div id=\"target\">content</div>" "target"))
  (is (not (lol-reactive::content-starts-with-id-p
             "<div id=\"other\">content</div>" "target"))))

(test regression-content-starts-with-id-quoted-gt
  "content-starts-with-id-p handles > in quoted attributes"
  ;; ID detection should work even with > in other attributes
  (is (lol-reactive::content-starts-with-id-p
        "<div title=\"x > y\" id=\"target\">content</div>" "target"))
  (is (not (lol-reactive::content-starts-with-id-p
             "<div title=\"id=target\">content</div>" "target"))))
