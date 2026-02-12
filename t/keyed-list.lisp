;;;; LOL-REACTIVE Test Suite - Keyed Lists
;;;;
;;;; Tests for keyed list reconciliation and efficient rendering.

(in-package :lol-reactive.tests)
(in-suite :keyed-list)

;;; ============================================================================
;;; RECONCILE-LIST TESTS
;;; ============================================================================

(test reconcile-list-identical
  "Reconciling identical lists produces no operations"
  (let ((items '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c"))))
    (let ((ops (lol-reactive::reconcile-list
                 items items
                 :key (lambda (x) (getf x :id)))))
      (is (null ops)))))

(test reconcile-list-addition
  "Reconciling with new item produces insert operation"
  (let ((old '((:id 1 :name "a") (:id 2 :name "b")))
        (new '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c"))))
    (let ((ops (lol-reactive::reconcile-list
                 old new
                 :key (lambda (x) (getf x :id)))))
      (is (not (null ops)))
      (is (member :insert ops :key #'first)))))

(test reconcile-list-removal
  "Reconciling with removed item produces remove operation"
  (let ((old '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c")))
        (new '((:id 1 :name "a") (:id 3 :name "c"))))
    (let ((ops (lol-reactive::reconcile-list
                 old new
                 :key (lambda (x) (getf x :id)))))
      (is (not (null ops)))
      (is (member :remove ops :key #'first)))))

(test reconcile-list-reorder
  "Reconciling reordered items with same elements produces no ops"
  ;; Current implementation treats reorders as identical (same keys present)
  ;; Move optimization would be a future enhancement
  (let ((old '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c")))
        (new '((:id 3 :name "c") (:id 1 :name "a") (:id 2 :name "b"))))
    (let ((ops (lol-reactive::reconcile-list
                 old new
                 :key (lambda (x) (getf x :id)))))
      ;; No operations because same keys are present
      (is (null ops)))))

;;; ============================================================================
;;; KEYED-RENDER TESTS
;;; ============================================================================

(test keyed-render-exists
  "keyed-render function exists"
  (is (fboundp 'lol-reactive::keyed-render)))

;;; ============================================================================
;;; FOR-EACH MACRO TESTS
;;; ============================================================================

(test for-each-macro-exists
  "for-each macro exists"
  (is (macro-function 'lol-reactive::for-each)))
