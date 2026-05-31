(in-package :lol-web/rendering/test)
(in-suite :lol-web/rendering/test)

(defun %rendered-lists-count ()
  (lol-web/core:bounded-cache-count lol-web/rendering::*rendered-lists*))

(test reconcile-list-identical
  "Reconciling identical lists produces no operations"
  (let ((items '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c"))))
    (let ((ops (lol-web/rendering::reconcile-list
                 items items
                 :key (lambda (x) (getf x :id)))))
      (is (null ops)))))

(test reconcile-list-addition
  "Reconciling with new item produces insert operation"
  (let ((old '((:id 1 :name "a") (:id 2 :name "b")))
        (new '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c"))))
    (let ((ops (lol-web/rendering::reconcile-list
                 old new
                 :key (lambda (x) (getf x :id)))))
      (is (not (null ops)))
      (is (member :insert ops :key #'first)))))

(test reconcile-list-removal
  "Reconciling with removed item produces remove operation"
  (let ((old '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c")))
        (new '((:id 1 :name "a") (:id 3 :name "c"))))
    (let ((ops (lol-web/rendering::reconcile-list
                 old new
                 :key (lambda (x) (getf x :id)))))
      (is (not (null ops)))
      (is (member :remove ops :key #'first)))))

(test reconcile-list-reorder
  "Reordered items with same keys at different positions emit remove+insert pairs"
  (let ((old '((:id 1 :name "a") (:id 2 :name "b") (:id 3 :name "c")))
        (new '((:id 3 :name "c") (:id 1 :name "a") (:id 2 :name "b"))))
    (let* ((ops (lol-web/rendering::reconcile-list
                  old new
                  :key (lambda (x) (getf x :id))))
           (n-remove (count :remove ops :key #'first))
           (n-insert (count :insert ops :key #'first)))
      (is (not (null ops))
          "reorder must produce ops — silently treating it as identical leaves the DOM stale")
      (is (= 3 n-remove) "every moved key needs a :remove (got ~D)" n-remove)
      (is (= 3 n-insert) "every moved key needs an :insert (got ~D)" n-insert)
      (is (every (lambda (op) (eq :remove (first op)))
                 (subseq ops 0 n-remove))
          "removes must come before inserts so the client can apply them in stream order"))))

(test keyed-render-exists
  "keyed-render function exists"
  (is (fboundp 'lol-web/rendering::keyed-render)))

(test for-each-macro-exists
  "for-each macro exists"
  (is (macro-function 'lol-web/rendering::for-each)))

;;; ============================================================================
;;; :cache kwarg — per-call cache isolation
;;; ============================================================================

(test keyed-render-cache-kwarg-isolates-state
  "Two keyed-render calls with disjoint :cache hash-tables do not see each
   other's prior renders; the image-global *rendered-lists* is untouched."
  (let ((cache-a (make-hash-table :test 'equal))
        (cache-b (make-hash-table :test 'equal))
        (before  (%rendered-lists-count)))
    ;; First render in A populates A only.
    (lol-web/rendering::keyed-render "list-x" '((:k 1)) (lambda (i) (getf i :k))
                                     (lambda (i) (format nil "~A" (getf i :k)))
                                     :cache cache-a)
    (is (= 1 (hash-table-count cache-a)))
    (is (= 0 (hash-table-count cache-b)))
    ;; Render in B under same list-id — B has no prior, so ops are NIL.
    (multiple-value-bind (html ops)
        (lol-web/rendering::keyed-render
          "list-x" '((:k 99)) (lambda (i) (getf i :k))
          (lambda (i) (format nil "~A" (getf i :k)))
          :cache cache-b)
      (declare (ignore html))
      (is (null ops)
          "B had no prior cache entry, so no diff ops emitted"))
    (is (= before (%rendered-lists-count))
        "image-global *rendered-lists* untouched")))

(test clear-list-cache-honours-cache-kwarg
  "clear-list-cache with :cache clears the provided table, not *rendered-lists*."
  (let ((cache (make-hash-table :test 'equal))
        (before (%rendered-lists-count)))
    (setf (gethash "lid" cache) '((:k . "v")))
    (lol-web/rendering::clear-list-cache "lid" cache)
    (is (= 0 (hash-table-count cache)))
    (setf (gethash "lid" cache) '((:k . "v")))
    (lol-web/rendering::clear-list-cache nil cache)
    (is (= 0 (hash-table-count cache))
        "no list-id + :cache argument clrhashes only that cache")
    (is (= before (%rendered-lists-count))
        "image-global *rendered-lists* still untouched")))

(test regression-default-rendered-lists-cache-is-bounded
  "*rendered-lists* is a bounded cache, and keyed-render accepts bounded caches."
  (is (lol-web/core:bounded-cache-p lol-web/rendering::*rendered-lists*))
  (let ((cache (lol-web/core:make-bounded-cache :max-entries 2 :test 'equal)))
    (dolist (list-id '("a" "b" "c"))
      (lol-web/rendering::keyed-render
       list-id
       (list (list :k list-id))
       (lambda (i) (getf i :k))
       (lambda (i) (getf i :k))
       :cache cache))
    (is (= 2 (lol-web/core:bounded-cache-count cache)))
    (multiple-value-bind (_ present-p)
        (lol-web/core:bounded-cache-get cache "a")
      (declare (ignore _))
      (is (null present-p)
          "oldest entry must be evicted when max-entries is exceeded"))
    (is (not (null (lol-web/rendering::inspect-list-cache cache))))))
