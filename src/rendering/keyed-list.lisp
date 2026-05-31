;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/RENDERING; Base: 10 -*-
;;;; Keyed List Rendering
;;;; Efficient list updates with key-based reconciliation

(in-package :lol-web/rendering)

;;; ============================================================================
;;; FOR-EACH - Keyed List Rendering Macro
;;; ============================================================================

(defmacro! for-each ((item-var collection &key (key '#'identity) (test '#'equal)) &body body)
  "Render a collection with keyed reconciliation.

   KEY: Function to extract unique key from each item (default: identity)
   TEST: Equality test for keys (default: equal)

   Returns list of (key . rendered-html) pairs for reconciliation.

   Example:
   (for-each (task tasks :key #'task-id)
     (htm-str (:li :data-key (task-id task)
               (task-title task))))"
  `(let* ((,g!items ,collection)
          (,g!key-fn ,key)
          (,g!results nil))
     (dolist (,item-var ,g!items)
       (let ((,g!item-key (funcall ,g!key-fn ,item-var)))
         (push (cons ,g!item-key (progn ,@body)) ,g!results)))
     (nreverse ,g!results)))

;;; ============================================================================
;;; RECONCILE-LIST - Compute Minimal Diff Operations
;;; ============================================================================

(defun reconcile-list (old-items new-items &key (key #'identity) (test #'equal))
  "Compute diff between old and new item lists keyed by KEY.

   OLD-ITEMS: List of items before update
   NEW-ITEMS: List of items after update
   KEY: Function to extract unique key from each item
   TEST: Equality test for item content (used to detect updates at same position)

   Returns list of operations:
     (:insert position item)  - Insert item at position
     (:remove position key)   - Remove item at position
     (:update position item)  - Update item at position (same key, same position,
                                different content)

   A moved key (present in both lists but at a different index) is emitted as
   (:remove old-pos key) followed by (:insert new-pos new-item) — the diff is
   not a true minimal edit script; the client is expected to apply removes
   before inserts. Returns removes (ascending old-pos) before inserts/updates."
  (let* ((old-keys (mapcar key old-items))
         (new-keys (mapcar key new-items))
         (old-map (make-hash-table :test test))
         (new-map (make-hash-table :test test))
         (ops nil))

    ;; Build position maps
    (loop for item in old-items
          for i from 0
          do (setf (gethash (funcall key item) old-map) (cons i item)))
    (loop for item in new-items
          for i from 0
          do (setf (gethash (funcall key item) new-map) (cons i item)))

    ;; Removes for keys that disappeared from new
    (loop for k in old-keys
          for i from 0
          unless (gethash k new-map)
          do (push `(:remove ,i ,k) ops))

    ;; For each new key: insert if absent in old, remove+insert if moved,
    ;; update if same position with different content
    (loop for k in new-keys
          for new-pos from 0
          for new-item in new-items
          do (let ((old-entry (gethash k old-map)))
               (cond
                 ((null old-entry)
                  (push `(:insert ,new-pos ,new-item) ops))
                 ((/= (car old-entry) new-pos)
                  (push `(:remove ,(car old-entry) ,k) ops)
                  (push `(:insert ,new-pos ,new-item) ops))
                 ((not (funcall test (cdr old-entry) new-item))
                  (push `(:update ,new-pos ,new-item) ops)))))

    ;; Return operations in order (removes first, then inserts/updates)
    (let ((removes (remove-if-not (lambda (op) (eq (car op) :remove)) ops))
          (others (remove-if (lambda (op) (eq (car op) :remove)) ops)))
      (append (nreverse removes) (nreverse others)))))

;;; ============================================================================
;;; KEYED RENDER - Render with Key Tracking
;;; ============================================================================

(defvar *rendered-lists* (make-bounded-cache :max-entries 1024 :test 'equal)
  "Default bounded cache of rendered keyed lists for diffing. Image-global;
   pass :CACHE to KEYED-RENDER / CLEAR-LIST-CACHE / INSPECT-LIST-CACHE to use
   a caller-owned cache for per-app or per-test isolation.")

(defun %cache-get (cache key)
  (etypecase cache
    (hash-table (gethash key cache))
    (bounded-cache (bounded-cache-get cache key))))

(defun %cache-set (cache key value)
  (etypecase cache
    (hash-table (setf (gethash key cache) value))
    (bounded-cache (bounded-cache-set cache key value))))

(defun %cache-remove (cache key)
  (etypecase cache
    (hash-table (remhash key cache))
    (bounded-cache (bounded-cache-remove cache key))))

(defun %cache-clear (cache)
  (etypecase cache
    (hash-table (clrhash cache))
    (bounded-cache (bounded-cache-clear cache))))

(defun %cache-map (fn cache)
  (etypecase cache
    (hash-table (maphash fn cache))
    (bounded-cache
     (dolist (key (bounded-cache-keys cache))
       (multiple-value-bind (value present-p) (bounded-cache-get cache key)
         (when present-p
           (funcall fn key value)))))))

(defun keyed-render (list-id items key-fn render-fn
                     &key (cache *rendered-lists*))
  "Render items with key tracking and return diff operations.

   LIST-ID: Unique identifier for this list
   ITEMS: List of items to render
   KEY-FN: Function to extract key from item
   RENDER-FN: Function to render item to HTML string
   CACHE: hash-table or bounded-cache for prior-render lookup; defaults to
   *RENDERED-LISTS*.

   Returns (values html-string operations) where operations
   can be applied incrementally on the client."
  (let* ((old-rendered (%cache-get cache list-id))
         (new-rendered (mapcar (lambda (item)
                                 (cons (funcall key-fn item)
                                       (funcall render-fn item)))
                               items))
         (ops (when old-rendered
                (reconcile-rendered-list old-rendered new-rendered))))
    (%cache-set cache list-id new-rendered)
    (values (format nil "~{~A~}" (mapcar #'cdr new-rendered))
            ops)))

(defun reconcile-rendered-list (old-rendered new-rendered)
  "Reconcile two lists of (key . html) pairs.
   Returns operations for incremental DOM updates."
  (let ((old-keys (mapcar #'car old-rendered))
        (new-keys (mapcar #'car new-rendered))
        (old-map (make-hash-table :test 'equal))
        (new-map (make-hash-table :test 'equal))
        (ops nil))

    ;; Build maps: key -> (position . html)
    (loop for (k . html) in old-rendered
          for i from 0
          do (setf (gethash k old-map) (cons i html)))
    (loop for (k . html) in new-rendered
          for i from 0
          do (setf (gethash k new-map) (cons i html)))

    ;; Find removes
    (loop for k in old-keys
          for i from 0
          unless (gethash k new-map)
          do (push `(:remove ,i ,k) ops))

    ;; Find inserts and content updates
    (loop for k in new-keys
          for new-pos from 0
          for new-html in (mapcar #'cdr new-rendered)
          do (let ((old-entry (gethash k old-map)))
               (cond
                 ((null old-entry)
                  (push `(:insert ,new-pos ,k ,new-html) ops))
                 ((not (string= (cdr old-entry) new-html))
                  (push `(:update ,new-pos ,k ,new-html) ops)))))

    (nreverse ops)))

;;; ============================================================================
;;; CLEAR-LIST-CACHE
;;; ============================================================================

(defun clear-list-cache (&optional list-id (cache *rendered-lists*))
  "Clear rendered-list entries from CACHE (defaults to *RENDERED-LISTS*).
   If LIST-ID provided, clear only that entry; otherwise CLRHASH CACHE."
  (if list-id
      (%cache-remove cache list-id)
      (%cache-clear cache)))

;;; ============================================================================
;;; INSPECTION
;;; ============================================================================

(defun inspect-list-cache (&optional (cache *rendered-lists*))
  "Return info about cached rendered lists in CACHE."
  (let ((lists nil))
    (%cache-map (lambda (id rendered)
                  (push (list :id id
                              :count (length rendered)
                              :keys (mapcar #'car rendered))
                        lists))
                cache)
    lists))
