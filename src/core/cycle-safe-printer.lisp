;;;; cycle-safe-printer.lisp - bounded, cycle-safe serialization for diagnostic surfaces

(in-package :lol-web/core)

(defparameter *cycle-safe-print-level* 16
  "*PRINT-LEVEL* bound inside WITH-CYCLE-SAFE-PRINTER. Caps nesting depth of
   any ~S/~A rendered on a diagnostic surface so a deep object graph cannot
   blow the printer.")

(defparameter *cycle-safe-print-length* 256
  "*PRINT-LENGTH* bound inside WITH-CYCLE-SAFE-PRINTER. Caps the element count
   the printer emits per level so a long sequence cannot flood the surface.")

(defmacro with-cycle-safe-printer (&body body)
  "Run BODY with printer settings that tolerate cyclic object graphs and bound
   the rendered depth/breadth (*PRINT-LEVEL* / *PRINT-LENGTH*) so an adversarial
   value cannot produce an unbounded rendering."
  `(let ((*print-circle* t)
         (*print-readably* nil)
         (*print-level* *cycle-safe-print-level*)
         (*print-length* *cycle-safe-print-length*))
     ,@body))

(defparameter *serialize-max-depth* 16
  "Default depth cap for BOUNDED-SERIALIZE; structure below it collapses to
   *SERIALIZE-TRUNCATION-MARKER*.")

(defparameter *serialize-max-nodes* 10000
  "Default total-node cap for BOUNDED-SERIALIZE across the whole walk.")

(defparameter *serialize-max-string-length* 4096
  "Default per-string length cap for BOUNDED-SERIALIZE; longer strings are
   truncated with *SERIALIZE-TRUNCATION-MARKER* appended.")

(defparameter *serialize-truncation-marker* "#<truncated>"
  "Sentinel string BOUNDED-SERIALIZE substitutes when a depth, node, or string
   cap is hit, or when a cycle is re-encountered.")

(defun bounded-serialize (value &key (max-depth *serialize-max-depth*)
                                     (max-nodes *serialize-max-nodes*)
                                     (max-string-length *serialize-max-string-length*))
  "Walk VALUE into a JSON-serializable tree bounded in depth, total node count,
   and per-string length. NIL becomes :NULL, T stays T, numbers pass through,
   strings are length-capped, keywords/symbols stringify, conses become lists,
   hash-tables become alists, and any other atom is rendered with a cycle-safe
   ~S. Cyclic references and anything past a cap collapse to
   *SERIALIZE-TRUNCATION-MARKER*, so an adversarial value can neither exhaust
   memory nor loop forever through this serializer."
  (let ((nodes 0)
        (seen (make-hash-table :test 'eq)))
    (labels ((over-budget-p ()
               (incf nodes)
               (> nodes max-nodes))
             (cap-string (s)
               (if (> (length s) max-string-length)
                   (concatenate 'string (subseq s 0 max-string-length)
                                *serialize-truncation-marker*)
                   s))
             (walk-list (list depth)
               (cond
                 ((gethash list seen) *serialize-truncation-marker*)
                 (t
                  (let ((out nil)
                        (tail list))
                    (loop
                      (cond
                        ((null tail) (return (nreverse out)))
                        ((and (consp tail) (gethash tail seen))
                         (push *serialize-truncation-marker* out)
                         (return (nreverse out)))
                        ((over-budget-p)
                         (push *serialize-truncation-marker* out)
                         (return (nreverse out)))
                        ((consp tail)
                         (setf (gethash tail seen) t)
                         (push (walk (car tail) (1+ depth)) out)
                         (setf tail (cdr tail)))
                        (t
                         (push (walk tail (1+ depth)) out)
                         (return (nreverse out)))))))))
             (walk-hash (ht depth)
               (cond
                 ((gethash ht seen) *serialize-truncation-marker*)
                 (t
                  (setf (gethash ht seen) t)
                  (let ((out nil))
                    (block per-entry
                      (maphash (lambda (k v)
                                 (when (over-budget-p)
                                   (push (cons *serialize-truncation-marker*
                                               *serialize-truncation-marker*)
                                         out)
                                   (return-from per-entry))
                                 (push (cons (format nil "~a" k)
                                             (walk v (1+ depth)))
                                       out))
                               ht))
                    (nreverse out)))))
             (walk (v depth)
               (cond
                 ((> depth max-depth) *serialize-truncation-marker*)
                 ((over-budget-p) *serialize-truncation-marker*)
                 (t
                  (typecase v
                    (null :null)
                    ((eql t) t)
                    (number v)
                    (string (cap-string v))
                    (keyword (format nil "~a" v))
                    (symbol (format nil "~a" v))
                    (cons (walk-list v depth))
                    (hash-table (walk-hash v depth))
                    (t (cap-string (with-cycle-safe-printer (format nil "~s" v)))))))))
      (walk value 0))))
