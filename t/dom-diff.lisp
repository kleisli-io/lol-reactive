;;;; LOL-REACTIVE Test Suite - DOM Diffing
;;;;
;;;; Tests for diff-html-sexp, html-sexp-* helpers, and patch generation.

(in-package :lol-reactive.tests)
(in-suite :dom-diff)

;;; ============================================================================
;;; HTML-SEXP-* HELPER TESTS
;;; ============================================================================

(test html-sexp-tag-extracts-tag
  "html-sexp-tag extracts tag name from sexp"
  (is (eq :div (lol-reactive::html-sexp-tag '(:div :id "x" "content"))))
  (is (eq :span (lol-reactive::html-sexp-tag '(:span "text"))))
  (is (eq :p (lol-reactive::html-sexp-tag '(:p)))))

(test html-sexp-tag-nil-for-non-list
  "html-sexp-tag returns NIL for non-list"
  (is (null (lol-reactive::html-sexp-tag "just a string")))
  (is (null (lol-reactive::html-sexp-tag nil))))

(test html-sexp-children-extracts-children
  "html-sexp-children extracts child elements"
  (let ((sexp '(:ul :class "list"
                (:li "First")
                (:li "Second"))))
    (let ((children (lol-reactive::html-sexp-children sexp)))
      (is (= 2 (length children)))
      (is (equal '(:li "First") (first children)))
      (is (equal '(:li "Second") (second children))))))

(test html-sexp-children-text-child
  "html-sexp-children handles text-only children"
  (let ((sexp '(:p :class "text" "Just some text")))
    (let ((children (lol-reactive::html-sexp-children sexp)))
      (is (= 1 (length children)))
      (is (string= "Just some text" (first children))))))

;;; ============================================================================
;;; DIFF-HTML-SEXP TESTS
;;; ============================================================================

(test diff-identical-returns-nil
  "Diffing identical trees returns no patches"
  (let ((sexp '(:div :id "x" "content")))
    (let ((patches (lol-reactive::diff-html-sexp sexp sexp)))
      (is (null patches)))))

(test diff-text-change
  "Diffing text change generates :UPDATE-CHILD patch with :UPDATE-TEXT"
  (let ((old '(:p "old text"))
        (new '(:p "new text")))
    (let ((patches (lol-reactive::diff-html-sexp old new)))
      (is (not (null patches)))
      ;; Implementation uses :UPDATE-CHILD containing :UPDATE-TEXT
      (is (member :update-child patches :key #'first)))))

(test diff-attr-change
  "Diffing attribute change generates :UPDATE-ATTR patch"
  (let ((old '(:div :class "old"))
        (new '(:div :class "new")))
    (let ((patches (lol-reactive::diff-html-sexp old new)))
      (is (not (null patches)))
      ;; Implementation uses :UPDATE-ATTR
      (is (member :update-attr patches :key #'first)))))

(test diff-add-child
  "Diffing with added child generates :INSERT-CHILD patch"
  (let ((old '(:ul (:li "First")))
        (new '(:ul (:li "First") (:li "Second"))))
    (let ((patches (lol-reactive::diff-html-sexp old new)))
      (is (not (null patches)))
      ;; Implementation uses :INSERT-CHILD
      (is (member :insert-child patches :key #'first)))))

(test diff-remove-child
  "Diffing with removed child generates :REMOVE-CHILD patch"
  (let ((old '(:ul (:li "First") (:li "Second")))
        (new '(:ul (:li "First"))))
    (let ((patches (lol-reactive::diff-html-sexp old new)))
      (is (not (null patches)))
      ;; Implementation uses :REMOVE-CHILD
      (is (member :remove-child patches :key #'first)))))

;;; ============================================================================
;;; SEXP-TO-HTML TESTS
;;; ============================================================================

(test sexp-to-html-basic
  "sexp-to-html converts simple elements"
  (let ((html (lol-reactive::sexp-to-html '(:div "content"))))
    (is (stringp html))
    (is (search "<div>" html))
    (is (search "content" html))
    (is (search "</div>" html))))

(test sexp-to-html-with-attrs
  "sexp-to-html includes attributes"
  (let ((html (lol-reactive::sexp-to-html '(:a :href "/test" "link"))))
    (is (search "href" html))
    (is (search "/test" html))))
