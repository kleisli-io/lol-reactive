;;;; bounded-cache.lisp - small bounded LRU cache primitive

(in-package :lol-web/core)

(defun %cache-test-function (test)
  (ecase test
    (eq #'eq)
    (eql #'eql)
    (equal #'equal)
    (equalp #'equalp)))

(defstruct (bounded-cache
            (:constructor %make-bounded-cache)
            (:predicate bounded-cache-p)
            (:copier nil))
  (table (make-hash-table :test 'equal) :read-only t)
  (order nil)
  (max-entries 1024 :type (integer 1 *) :read-only t)
  (eviction :lru :read-only t)
  (test #'equal :type function :read-only t)
  (lock (bordeaux-threads:make-recursive-lock "lol-web/core bounded cache")
        :read-only t))

(defun make-bounded-cache (&key (max-entries 1024) (test 'equal) (eviction :lru))
  "Create a bounded cache. EVICTION is :LRU or :FIFO."
  (unless (and (integerp max-entries) (plusp max-entries))
    (error "make-bounded-cache: MAX-ENTRIES must be a positive integer, got ~S"
           max-entries))
  (unless (member eviction '(:lru :fifo))
    (error "make-bounded-cache: EVICTION must be :LRU or :FIFO, got ~S"
           eviction))
  (%make-bounded-cache
   :table (make-hash-table :test test)
   :max-entries max-entries
   :eviction eviction
   :test (%cache-test-function test)))

(defun %bounded-cache-touch (cache key)
  (setf (bounded-cache-order cache)
        (cons key (remove key (bounded-cache-order cache)
                          :test (bounded-cache-test cache)))))

(defun %bounded-cache-evict-overflow (cache)
  (loop while (> (hash-table-count (bounded-cache-table cache))
                 (bounded-cache-max-entries cache))
        for oldest = (car (last (bounded-cache-order cache)))
        do (remhash oldest (bounded-cache-table cache))
           (setf (bounded-cache-order cache)
                 (butlast (bounded-cache-order cache)))))

(defun bounded-cache-count (cache)
  "Return the number of entries in CACHE."
  (bordeaux-threads:with-recursive-lock-held ((bounded-cache-lock cache))
    (hash-table-count (bounded-cache-table cache))))

(defun bounded-cache-get (cache key &optional default)
  "Return (values VALUE PRESENT-P) for KEY."
  (bordeaux-threads:with-recursive-lock-held ((bounded-cache-lock cache))
    (multiple-value-bind (value present-p)
        (gethash key (bounded-cache-table cache))
      (when (and present-p (eq (bounded-cache-eviction cache) :lru))
        (%bounded-cache-touch cache key))
      (values (if present-p value default) present-p))))

(defun bounded-cache-set (cache key value)
  "Store VALUE under KEY and evict the oldest entry when CACHE is full."
  (bordeaux-threads:with-recursive-lock-held ((bounded-cache-lock cache))
    (multiple-value-bind (old present-p)
        (gethash key (bounded-cache-table cache))
      (declare (ignore old))
      (setf (gethash key (bounded-cache-table cache)) value)
      (cond
        (present-p
         (when (eq (bounded-cache-eviction cache) :lru)
           (%bounded-cache-touch cache key)))
        (t
         (setf (bounded-cache-order cache)
               (cons key (bounded-cache-order cache))))))
    (%bounded-cache-evict-overflow cache)
    value))

(defun bounded-cache-remove (cache key)
  "Remove KEY from CACHE. Returns T when it was present."
  (bordeaux-threads:with-recursive-lock-held ((bounded-cache-lock cache))
    (let ((present-p (nth-value 1 (gethash key (bounded-cache-table cache)))))
      (remhash key (bounded-cache-table cache))
      (setf (bounded-cache-order cache)
            (remove key (bounded-cache-order cache)
                    :test (bounded-cache-test cache)))
      present-p)))

(defun bounded-cache-clear (cache)
  "Remove every entry from CACHE."
  (bordeaux-threads:with-recursive-lock-held ((bounded-cache-lock cache))
    (clrhash (bounded-cache-table cache))
    (setf (bounded-cache-order cache) nil)
    cache))

(defun bounded-cache-keys (cache)
  "Return CACHE keys from newest to oldest."
  (bordeaux-threads:with-recursive-lock-held ((bounded-cache-lock cache))
    (copy-list (bounded-cache-order cache))))
