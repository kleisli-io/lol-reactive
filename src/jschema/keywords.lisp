;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/JSCHEMA; Base: 10 -*-
;;;; Per-keyword PARSE / CHECK functions. Each REGISTER-KEYWORD call attaches
;;;; one keyword's logic to the parser's and validator's dispatch tables.

(in-package :lol-web/jschema)

;;; ============================================================================
;;; SHARED: jzon shape predicates
;;; ============================================================================

(defun %json-null-p (v) (eq v 'null))
(defun %json-true-p (v) (eq v t))
(defun %json-false-p (v) (eq v nil))
(defun %json-bool-p (v) (or (%json-true-p v) (%json-false-p v)))
(defun %json-string-p (v) (stringp v))
(defun %json-integer-p (v) (integerp v))
(defun %json-number-p (v) (and (numberp v) (not (complexp v))))
(defun %json-array-p (v) (and (vectorp v) (not (stringp v))))
(defun %json-object-p (v) (hash-table-p v))

(defun %json-integer-valued-p (v)
  "JSON Schema `integer` matches by value: an integer, or a real with zero
   fractional part (5.0, 10/2). jzon yields only finite reals, so TRUNCATE is
   always defined here."
  (and (%json-number-p v) (zerop (nth-value 1 (truncate v)))))

(defun %json-type-tag (v)
  "Return the JSON Schema type tag for a jzon-shaped value, as a string."
  (cond
    ((%json-null-p v) "null")
    ((%json-bool-p v) "boolean")
    ((%json-integer-p v) "integer")
    ((%json-number-p v) "number")
    ((%json-string-p v) "string")
    ((%json-array-p v) "array")
    ((%json-object-p v) "object")
    (t "unknown")))

(defparameter *json-compare-max-depth* 256
  "Independent recursion bound for %JSON-EQUAL and %JSON-WRITE-CANONICAL.
   Their stack safety must not depend on whichever parser produced the value:
   the request-body path caps nesting at *json-body-max-depth*, but VALIDATE
   may be handed a value from any source.")

(define-condition json-structure-too-deep (error)
  ((depth :initarg :depth :reader json-structure-too-deep-depth))
  (:report (lambda (c stream)
             (format stream "JSON value nesting exceeds ~D; refusing to recurse further."
                     (json-structure-too-deep-depth c)))))

(defun %json-equal (a b &optional (depth 0))
  "Structural equality used by 'const' and 'enum' (and 'uniqueItems').
   Hash-tables compare by keys + recursive value equality; vectors compare
   length-then-pointwise. Recursion is bounded by *json-compare-max-depth*."
  (when (> depth *json-compare-max-depth*)
    (error 'json-structure-too-deep :depth *json-compare-max-depth*))
  (cond
    ((and (numberp a) (numberp b)) (= a b))
    ((and (stringp a) (stringp b)) (string= a b))
    ((and (%json-bool-p a) (%json-bool-p b)) (eq a b))
    ((and (%json-null-p a) (%json-null-p b)) t)
    ((and (vectorp a) (vectorp b))
     (and (= (length a) (length b))
          (loop for i below (length a)
                always (%json-equal (aref a i) (aref b i) (1+ depth)))))
    ((and (hash-table-p a) (hash-table-p b))
     (and (= (hash-table-count a) (hash-table-count b))
          (loop for k being the hash-keys of a using (hash-value va)
                always (multiple-value-bind (vb present-p) (gethash k b)
                         (and present-p (%json-equal va vb (1+ depth)))))))
    (t nil)))

;;; ============================================================================
;;; PATTERN COMPILATION BOUNDS
;;;
;;; cl-ppcre:create-scanner on attacker-controlled regex is two DoS vectors:
;;; an arbitrarily long pattern string, and a catastrophic-backtracking
;;; pattern whose compile phase runs unbounded. Cap both at parse time —
;;; matching at request time reuses the cached scanner.
;;; ============================================================================

(defparameter *pattern-max-length* 256
  "Reject `pattern` / `patternProperties` keys longer than this at parse time.
   Unbounded regex strings are a cheap DoS vector — 256 is comfortably above
   any legitimate JSON Schema pattern we have observed.")

(defparameter *pattern-compile-timeout-seconds* 0.1
  "Wallclock cap on cl-ppcre:create-scanner. Catastrophic-backtracking regex
   authors get bounded CPU; legitimate patterns compile in microseconds.")

(defun %compile-pattern-bounded (pattern)
  "Compile PATTERN to a cl-ppcre scanner, refusing strings beyond
   *pattern-max-length* and aborting compilations beyond
   *pattern-compile-timeout-seconds*. Raises INVALID-SCHEMA on either
   refusal or on cl-ppcre's own malformed-regex error."
  (when (> (length pattern) *pattern-max-length*)
    (raise-invalid-schema
     "pattern length ~D exceeds *pattern-max-length* ~D"
     (length pattern) *pattern-max-length*))
  ;; Off SBCL there is no interruptible compile timer — refuse rather than
  ;; run cl-ppcre:create-scanner unbounded on an attacker-controlled pattern.
  #-sbcl
  (raise-invalid-schema
   "pattern compilation cannot be wallclock-bounded off SBCL; refusing ~S"
   pattern)
  #+sbcl
  (handler-case
      (sb-ext:with-timeout *pattern-compile-timeout-seconds*
        (cl-ppcre:create-scanner pattern))
    (sb-ext:timeout ()
      (raise-invalid-schema
       "pattern compilation exceeded ~A seconds"
       *pattern-compile-timeout-seconds*))
    (error () (raise-invalid-schema "pattern ~S is not a valid regex" pattern))))

(defparameter *pattern-match-timeout-seconds* 0.1
  "Wallclock bound on a single patternProperties / additionalProperties coverage
   match over an attacker-controlled key. A match that exceeds it is
   inconclusive — neither a definite match nor a definite miss — so callers
   reject the key rather than silently skip the constraint it might impose.")

(defun %pattern-match (scanner key)
  "Match SCANNER against KEY under a wallclock bound. Two values: MATCHED-P and
   INCONCLUSIVE-P. A match aborted at *pattern-match-timeout-seconds* returns
   (NIL T), so a catastrophic-backtracking pattern can neither hang a worker nor
   let a key escape a constraint it might be subject to. On non-SBCL there is no
   interruptible match timer, so the match is NOT run — it reports inconclusive
   (NIL T) and the caller rejects the key, never running unbounded."
  #+sbcl
  (handler-case
      (sb-ext:with-timeout *pattern-match-timeout-seconds*
        (values (and (cl-ppcre:scan scanner key) t) nil))
    (sb-ext:timeout () (values nil t)))
  #-sbcl
  (values nil t))

;;; ============================================================================
;;; ANNOTATION-ONLY KEYWORDS — parse cleanly, don't check
;;; ============================================================================

(dolist (k '("title" "description" "default" "examples" "deprecated"
             "readOnly" "writeOnly" "format"))
  (register-keyword k :parser #'identity))

;;; ============================================================================
;;; type
;;; ============================================================================

(defparameter +valid-type-tags+
  '("null" "boolean" "integer" "number" "string" "array" "object"))

(defun %parse-type (val)
  (cond
    ((stringp val)
     (unless (find val +valid-type-tags+ :test #'string=)
       (raise-invalid-schema "Invalid type ~S" val))
     (list val))
    ((vectorp val)
     (let ((tags (coerce val 'list)))
       (unless (every #'stringp tags)
         (raise-invalid-schema "type array must contain only strings"))
       (dolist (t1 tags)
         (unless (find t1 +valid-type-tags+ :test #'string=)
           (raise-invalid-schema "Invalid type ~S" t1)))
       tags))
    (t (raise-invalid-schema "type must be a string or array of strings"))))

(defun %check-type (allowed value ctx schema)
  (declare (ignore schema))
  ;; integer is a refinement of number: a number-tagged value satisfies
  ;; "number", and any integer-valued real (5, 5.0) satisfies "integer".
  (let ((tag (%json-type-tag value)))
    (unless (or (find tag allowed :test #'string=)
                (and (member tag '("integer" "number") :test #'string=)
                     (find "number" allowed :test #'string=))
                (and (find "integer" allowed :test #'string=)
                     (%json-integer-valued-p value)))
      (push-error ctx (format nil "Expected type ~A, got ~A"
                              (format nil "~{~A~^/~}" allowed) tag)))))

(register-keyword "type" :parser #'%parse-type :checker #'%check-type)

;;; ============================================================================
;;; const
;;; ============================================================================

(defun %check-const (parsed value ctx schema)
  (declare (ignore schema))
  (unless (%json-equal value parsed)
    (push-error ctx (format nil "Value does not equal const ~S" parsed))))

(register-keyword "const" :parser #'identity :checker #'%check-const)

;;; ============================================================================
;;; enum
;;; ============================================================================

(defun %parse-enum (val)
  (unless (and (vectorp val) (plusp (length val)))
    (raise-invalid-schema "enum must be a non-empty array"))
  (coerce val 'list))

(defun %check-enum (parsed value ctx schema)
  (declare (ignore schema))
  (unless (some (lambda (e) (%json-equal value e)) parsed)
    (push-error ctx (format nil "Value not in enum (~D options)"
                            (length parsed)))))

(register-keyword "enum" :parser #'%parse-enum :checker #'%check-enum)

;;; ============================================================================
;;; required
;;; ============================================================================

(defun %parse-required (val)
  (unless (vectorp val)
    (raise-invalid-schema "required must be an array"))
  (let ((names (coerce val 'list)))
    (unless (every #'stringp names)
      (raise-invalid-schema "required must contain only strings"))
    names))

(defun %check-required (names value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (dolist (name names)
      (unless (multiple-value-bind (stored-value present) (gethash name value)
                (declare (ignore stored-value))
                present)
        (push-error ctx (format nil "Missing required property ~S" name))))))

(register-keyword "required" :parser #'%parse-required :checker #'%check-required)

;;; ============================================================================
;;; properties / patternProperties / additionalProperties / propertyNames /
;;; unevaluatedProperties
;;; ============================================================================

(defun %parse-properties (val)
  (unless (hash-table-p val)
    (raise-invalid-schema "properties must be a JSON object"))
  (let ((acc '()))
    (loop for k being the hash-keys of val using (hash-value v)
          do (with-pointer-extension
                 ((concatenate 'string "/" (%escape-pointer-segment k)))
               (push (cons k (%make-schema v nil)) acc)))
    (nreverse acc)))

(defun %check-properties (parsed value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (dolist (entry parsed)
      (multiple-value-bind (sub present-p) (gethash (car entry) value)
        (when present-p
          (with-pointer (ctx (concatenate 'string "/"
                                          (%escape-pointer-segment (car entry))))
            (%check-schema (cdr entry) sub ctx))
          (setf (gethash (car entry) (eval-ctx-evaluated-props ctx)) t))))))

(register-keyword "properties"
                  :parser #'%parse-properties
                  :checker #'%check-properties
                  :child-schemas
                  (lambda (parsed)
                    (mapcar (lambda (entry)
                              (cons (concatenate 'string "/properties/"
                                                 (%escape-pointer-segment
                                                  (car entry)))
                                    (cdr entry)))
                            parsed)))

(defun %parse-pattern-properties (val)
  (unless (hash-table-p val)
    (raise-invalid-schema "patternProperties must be a JSON object"))
  (let ((acc '()))
    (loop for k being the hash-keys of val using (hash-value v)
          do (let ((scanner (%compile-pattern-bounded k)))
               (with-pointer-extension
                   ((concatenate 'string "/" (%escape-pointer-segment k)))
                 (push (list k scanner (%make-schema v nil)) acc))))
    (nreverse acc)))

(defun %check-pattern-properties (parsed value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (loop for k being the hash-keys of value using (hash-value v)
          do (dolist (triple parsed)
               (multiple-value-bind (matched inconclusive)
                   (%pattern-match (second triple) k)
                 (cond
                   ;; An inconclusive match cannot prove the key is outside this
                   ;; pattern, so skipping its sub-schema would let the key escape
                   ;; a constraint it may be subject to: reject instead.
                   (inconclusive
                    (with-pointer (ctx (concatenate 'string "/"
                                                    (%escape-pointer-segment k)))
                      (push-error ctx "patternProperties match did not complete; rejecting key"))
                    (setf (gethash k (eval-ctx-evaluated-props ctx)) t))
                   (matched
                    (with-pointer (ctx (concatenate 'string "/"
                                                    (%escape-pointer-segment k)))
                      (%check-schema (third triple) v ctx))
                    (setf (gethash k (eval-ctx-evaluated-props ctx)) t))))))))

(register-keyword "patternProperties"
                  :parser #'%parse-pattern-properties
                  :checker #'%check-pattern-properties
                  :child-schemas
                  (lambda (parsed)
                    (mapcar (lambda (triple)
                              (cons (concatenate 'string "/patternProperties/"
                                                 (%escape-pointer-segment
                                                  (first triple)))
                                    (third triple)))
                            parsed)))

(defun %parse-additional-properties (val)
  (%make-schema val nil))

(defun %same-schema-property-covered-p (key schema)
  "Whether KEY is covered by this schema's own properties/patternProperties.
   Two values: COVERED-P and INCONCLUSIVE-P. A patternProperties match that does
   not complete reports inconclusive, so the caller fails closed rather than
   guessing the key covered or uncovered."
  (let ((keywords (json-schema-keywords schema)))
    (if (assoc key (cdr (assoc "properties" keywords :test #'string=))
              :test #'string=)
        (values t nil)
        (dolist (triple (cdr (assoc "patternProperties" keywords :test #'string=))
                        (values nil nil))
          (multiple-value-bind (matched inconclusive)
              (%pattern-match (second triple) key)
            (cond (inconclusive (return (values nil t)))
                  (matched (return (values t nil)))))))))

(defun %check-additional-properties (sub value ctx schema)
  (when (%json-object-p value)
    (loop for k being the hash-keys of value using (hash-value v)
          do (multiple-value-bind (covered inconclusive)
                 (%same-schema-property-covered-p k schema)
               (cond
                 ;; Coverage could not be decided: exempting the key would let it
                 ;; bypass additionalProperties, so reject.
                 (inconclusive
                  (with-pointer (ctx (concatenate 'string "/"
                                                  (%escape-pointer-segment k)))
                    (push-error ctx "additionalProperties coverage match did not complete; rejecting key"))
                  (setf (gethash k (eval-ctx-evaluated-props ctx)) t))
                 ((not covered)
                  (with-pointer (ctx (concatenate 'string "/"
                                                  (%escape-pointer-segment k)))
                    (%check-schema sub v ctx))
                  (setf (gethash k (eval-ctx-evaluated-props ctx)) t)))))))

(register-keyword "additionalProperties"
                  :parser #'%parse-additional-properties
                  :checker #'%check-additional-properties
                  :child-schemas
                  (lambda (parsed) (list (cons "/additionalProperties" parsed))))

(defun %parse-property-names (val)
  (%make-schema val nil))

(defun %check-property-names (sub value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (loop for k being the hash-keys of value
          do (with-pointer (ctx (concatenate 'string "/"
                                             (%escape-pointer-segment k)))
               (%check-schema sub k ctx)))))

(register-keyword "propertyNames"
                  :parser #'%parse-property-names
                  :checker #'%check-property-names
                  :child-schemas
                  (lambda (parsed) (list (cons "/propertyNames" parsed))))

(defun %parse-unevaluated-properties (val)
  (%make-schema val nil))

(defun %check-unevaluated-properties (sub value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (let ((covered (eval-ctx-evaluated-props ctx)))
      (loop for k being the hash-keys of value using (hash-value v)
            do (unless (gethash k covered)
                 (with-pointer (ctx (concatenate 'string "/"
                                                 (%escape-pointer-segment k)))
                   (%check-schema sub v ctx))
                 (setf (gethash k covered) t))))))

(register-keyword "unevaluatedProperties"
                  :parser #'%parse-unevaluated-properties
                  :checker #'%check-unevaluated-properties
                  :child-schemas
                  (lambda (parsed) (list (cons "/unevaluatedProperties" parsed))))

;;; ============================================================================
;;; minProperties / maxProperties
;;; ============================================================================

(defun %parse-non-negative-integer (val keyword)
  (unless (and (integerp val) (>= val 0))
    (raise-invalid-schema "~A must be a non-negative integer" keyword))
  val)

(defun %check-min-properties (n value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (when (< (hash-table-count value) n)
      (push-error ctx (format nil "Object has fewer than ~D properties" n)))))

(defun %check-max-properties (n value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (when (> (hash-table-count value) n)
      (push-error ctx (format nil "Object has more than ~D properties" n)))))

(register-keyword "minProperties"
                  :parser (lambda (v) (%parse-non-negative-integer v "minProperties"))
                  :checker #'%check-min-properties)
(register-keyword "maxProperties"
                  :parser (lambda (v) (%parse-non-negative-integer v "maxProperties"))
                  :checker #'%check-max-properties)

;;; ============================================================================
;;; items / prefixItems / contains / minItems / maxItems / uniqueItems
;;; ============================================================================

(defun %parse-items (val)
  (%make-schema val nil))

(defun %check-items (sub value ctx schema)
  (when (%json-array-p value)
    (let* ((prefix-items (cdr (assoc "prefixItems"
                                     (json-schema-keywords schema)
                                     :test #'string=)))
           (start (length prefix-items)))
      (loop for i from start below (length value)
            for item = (aref value i)
            do (with-pointer (ctx (format nil "/~D" i))
                 (%check-schema sub item ctx))
            do (pushnew i (eval-ctx-evaluated-items ctx))))))

(register-keyword "items"
                  :parser #'%parse-items
                  :checker #'%check-items
                  :child-schemas
                  (lambda (parsed) (list (cons "/items" parsed))))

(defun %parse-prefix-items (val)
  (unless (vectorp val)
    (raise-invalid-schema "prefixItems must be an array"))
  (let ((acc '()))
    (loop for i from 0
          for v across val
          do (with-pointer-extension ((format nil "/~D" i))
               (push (%make-schema v nil) acc)))
    (nreverse acc)))

(defun %check-prefix-items (subs value ctx schema)
  (declare (ignore schema))
  (when (%json-array-p value)
    (loop for i below (min (length subs) (length value))
          for sub in subs
          for item = (aref value i)
          do (with-pointer (ctx (format nil "/~D" i))
               (%check-schema sub item ctx))
          do (pushnew i (eval-ctx-evaluated-items ctx)))))

(register-keyword "prefixItems"
                  :parser #'%parse-prefix-items
                  :checker #'%check-prefix-items
                  :child-schemas
                  (lambda (parsed)
                    (loop for s in parsed
                          for i from 0
                          collect (cons (format nil "/prefixItems/~D" i) s))))

(defun %check-min-items (n value ctx schema)
  (declare (ignore schema))
  (when (%json-array-p value)
    (when (< (length value) n)
      (push-error ctx (format nil "Array has fewer than ~D items" n)))))

(defun %check-max-items (n value ctx schema)
  (declare (ignore schema))
  (when (%json-array-p value)
    (when (> (length value) n)
      (push-error ctx (format nil "Array has more than ~D items" n)))))

(register-keyword "minItems"
                  :parser (lambda (v) (%parse-non-negative-integer v "minItems"))
                  :checker #'%check-min-items)
(register-keyword "maxItems"
                  :parser (lambda (v) (%parse-non-negative-integer v "maxItems"))
                  :checker #'%check-max-items)

(defparameter *unique-items-hash-set-threshold* 100
  "Array length above which %CHECK-UNIQUE-ITEMS switches from O(n^2) pairwise
   comparison to an O(n) hash-set keyed by canonical JSON serialization. At
   100 items the pairwise variant still finishes in microseconds; past that
   the quadratic cost grows fast enough that the constant-factor cost of
   canonicalisation pays for itself.")

(defun %json-canonical-key (v)
  "Deterministic string encoding of V usable as an EQUAL hash-table key. Mirrors
   %JSON-EQUAL's semantics: objects serialise with keys sorted lexicographically
   so two equal hash-tables produce identical strings regardless of insertion
   order."
  (with-output-to-string (s)
    (%json-write-canonical v s)))

(defun %json-write-canonical (v stream &optional (depth 0))
  (when (> depth *json-compare-max-depth*)
    (error 'json-structure-too-deep :depth *json-compare-max-depth*))
  (cond
    ((%json-null-p v) (write-string "null" stream))
    ((eq v t) (write-string "true" stream))
    ((eq v nil) (write-string "false" stream))
    ((stringp v) (prin1 v stream))
    ((integerp v) (princ v stream))
    ((numberp v) (princ v stream))
    ((vectorp v)
     (write-char #\[ stream)
     (loop for i below (length v)
           do (when (plusp i) (write-char #\, stream))
              (%json-write-canonical (aref v i) stream (1+ depth)))
     (write-char #\] stream))
    ((hash-table-p v)
     (let ((keys (sort (loop for k being the hash-keys of v collect k)
                       #'string<)))
       (write-char #\{ stream)
       (loop for k in keys
             for first = t then nil
             do (unless first (write-char #\, stream))
                (prin1 k stream)
                (write-char #\: stream)
                (%json-write-canonical (gethash k v) stream (1+ depth)))
       (write-char #\} stream)))
    (t (prin1 v stream))))

(defun %check-unique-items (val value ctx schema)
  (when (and val (%json-array-p value))
    (let* ((n (length value))
           (max-items (cdr (assoc "maxItems"
                                  (json-schema-keywords schema)
                                  :test #'string=))))
      (cond
        ;; maxItems will already complain — don't spend O(n^2) on an array
        ;; the validator is about to reject anyway.
        ((and max-items (> n max-items)))
        ((<= n *unique-items-hash-set-threshold*)
         (loop for i below n
               do (loop for j from (1+ i) below n
                        when (%json-equal (aref value i) (aref value j))
                          do (push-error
                              ctx (format nil "Duplicate items at indices ~D and ~D"
                                          i j)))))
        (t
         (let ((seen (make-hash-table :test 'equal)))
           (loop for i below n
                 for k = (%json-canonical-key (aref value i))
                 do (multiple-value-bind (first-index present-p) (gethash k seen)
                      (if present-p
                          (push-error
                           ctx (format nil "Duplicate items at indices ~D and ~D"
                                       first-index i))
                          (setf (gethash k seen) i))))))))))

(register-keyword "uniqueItems"
                  :parser #'identity
                  :checker #'%check-unique-items)

;;; ============================================================================
;;; minLength / maxLength / pattern
;;; ============================================================================

(defun %check-min-length (n value ctx schema)
  (declare (ignore schema))
  (when (%json-string-p value)
    (when (< (length value) n)
      (push-error ctx (format nil "String shorter than ~D characters" n)))))

(defun %check-max-length (n value ctx schema)
  (declare (ignore schema))
  (when (%json-string-p value)
    (when (> (length value) n)
      (push-error ctx (format nil "String longer than ~D characters" n)))))

(register-keyword "minLength"
                  :parser (lambda (v) (%parse-non-negative-integer v "minLength"))
                  :checker #'%check-min-length)
(register-keyword "maxLength"
                  :parser (lambda (v) (%parse-non-negative-integer v "maxLength"))
                  :checker #'%check-max-length)

(defun %parse-pattern (val)
  (unless (stringp val)
    (raise-invalid-schema "pattern must be a string"))
  (%compile-pattern-bounded val))

(defun %check-pattern (scanner value ctx schema)
  (declare (ignore schema))
  (when (%json-string-p value)
    (unless (lol-web/escape:%scan-bounded scanner value)
      (push-error ctx "String does not match pattern"))))

(register-keyword "pattern" :parser #'%parse-pattern :checker #'%check-pattern)

;;; ============================================================================
;;; minimum / maximum / exclusiveMinimum / exclusiveMaximum / multipleOf
;;; ============================================================================

(defun %check-minimum (n value ctx schema)
  (declare (ignore schema))
  (when (%json-number-p value)
    (when (< value n)
      (push-error ctx (format nil "Value less than minimum ~A" n)))))

(defun %check-maximum (n value ctx schema)
  (declare (ignore schema))
  (when (%json-number-p value)
    (when (> value n)
      (push-error ctx (format nil "Value greater than maximum ~A" n)))))

(defun %check-exclusive-minimum (n value ctx schema)
  (declare (ignore schema))
  (when (%json-number-p value)
    (when (<= value n)
      (push-error ctx (format nil "Value not greater than exclusive minimum ~A" n)))))

(defun %check-exclusive-maximum (n value ctx schema)
  (declare (ignore schema))
  (when (%json-number-p value)
    (when (>= value n)
      (push-error ctx (format nil "Value not less than exclusive maximum ~A" n)))))

(defun %check-multiple-of (n value ctx schema)
  (declare (ignore schema))
  ;; Exact rational arithmetic: float division can overflow to a non-finite
  ;; result (e.g. 1e307 / 0.01) and escape VALIDATE's invalid-json contract as
  ;; an arithmetic-error. The parser guarantees n > 0 and jzon yields only
  ;; finite reals, so RATIONAL is always defined and the division never traps.
  (when (%json-number-p value)
    (let ((q (/ (rational value) (rational n))))
      (unless (integerp q)
        (push-error ctx (format nil "Value is not a multiple of ~A" n))))))

(dolist (k '(("minimum" . %check-minimum)
             ("maximum" . %check-maximum)
             ("exclusiveMinimum" . %check-exclusive-minimum)
             ("exclusiveMaximum" . %check-exclusive-maximum)))
  ;; Fresh per-iteration binding so each parser closure captures its own
  ;; keyword name; a closure over the loop variable would report the final
  ;; (or NIL) name for every keyword.
  (let ((name (car k))
        (checker (cdr k)))
    (register-keyword name
                      :parser (lambda (v)
                                (unless (%json-number-p v)
                                  (raise-invalid-schema "~A must be a number" name))
                                v)
                      :checker (symbol-function checker))))

(register-keyword "multipleOf"
                  :parser (lambda (v)
                            (unless (and (%json-number-p v) (plusp v))
                              (raise-invalid-schema
                               "multipleOf must be a number > 0"))
                            v)
                  :checker #'%check-multiple-of)

;;; ============================================================================
;;; allOf / anyOf / oneOf / not
;;; ============================================================================

(defun %parse-schema-array (val keyword)
  (unless (vectorp val)
    (raise-invalid-schema "~A must be an array of schemas" keyword))
  (let ((acc '()))
    (loop for i from 0
          for v across val
          do (with-pointer-extension ((format nil "/~D" i))
               (push (%make-schema v nil) acc)))
    (nreverse acc)))

(defun %check-all-of (subs value ctx schema)
  (declare (ignore schema))
  (dolist (s subs)
    (%check-schema s value ctx)))

(defun %check-branch (sub value ctx saved-props saved-items)
  "Validate SUB against VALUE in an isolated fork; return (values passed-p
   evaluated-prop-keys evaluated-items aborted-p). PASSED-P means the branch
   definitely validated — null errors AND not aborted — so a fork that merely
   refused to descend (depth/stack cap) is never read as a clean match; ABORTED-P
   lets anyOf/oneOf fail closed on an inconclusive branch. SAVED-PROPS /
   SAVED-ITEMS seed the fork's annotation baseline so an in-branch unevaluated*
   keyword sees what the surrounding schema already evaluated. When the ctx
   carries a combinator memo — installed only for documents with no unevaluated*
   keyword, where nothing reads that baseline — the (sub . value) result is
   shared across the call, collapsing a recursive union's exponential re-work to
   linear. The memoised branch validates from an empty baseline, equivalent
   precisely because the baseline is dead in that case."
  (let ((memo (eval-ctx-combinator-memo ctx)))
    (flet ((isolated (props items)
             (let ((scratch (if props
                                (fork-eval-ctx
                                 ctx
                                 :evaluated-props (alexandria:copy-hash-table props)
                                 :evaluated-items (copy-list items))
                                (fork-eval-ctx ctx))))
               (%check-schema sub value scratch)
               (list (and (null (eval-ctx-errors scratch))
                          (not (eval-ctx-aborted scratch)))
                     (loop for k being the hash-keys of (eval-ctx-evaluated-props scratch)
                           collect k)
                     (eval-ctx-evaluated-items scratch)
                     (eval-ctx-aborted scratch)))))
      (let ((result
              (if memo
                  (let ((per-sub (or (gethash sub memo)
                                     (setf (gethash sub memo)
                                           (make-hash-table :test 'eq)))))
                    (multiple-value-bind (cached present) (gethash value per-sub)
                      (if present
                          cached
                          (setf (gethash value per-sub) (isolated nil nil)))))
                  (isolated saved-props saved-items))))
        (values (first result) (second result) (third result) (fourth result))))))

(defun %check-any-of (subs value ctx schema)
  (declare (ignore schema))
  ;; Pass if any branch definitely validates; surface a generic failure if all
  ;; fail (per-branch errors are not propagated, to keep diagnostics terse). A
  ;; branch that only aborted (depth/stack cap) is inconclusive, not a miss: if
  ;; no branch cleanly passes, an aborted one makes the whole keyword fail closed.
  (let ((any-passed nil)
        (any-aborted nil)
        (saved-props (alexandria:copy-hash-table (eval-ctx-evaluated-props ctx)))
        (saved-items (copy-list (eval-ctx-evaluated-items ctx))))
    (dolist (s subs)
      (multiple-value-bind (passed props items aborted)
          (%check-branch s value ctx saved-props saved-items)
        (when aborted (setf any-aborted t))
        (when passed
          (setf any-passed t)
          ;; Merge the passing branch's annotations into the parent context.
          (dolist (k props) (setf (gethash k (eval-ctx-evaluated-props ctx)) t))
          (setf (eval-ctx-evaluated-items ctx)
                (union (eval-ctx-evaluated-items ctx) items)))))
    (cond
      (any-passed)
      (any-aborted
       (push-error ctx "Could not evaluate an `anyOf` branch (depth/stack cap); rejecting")
       (setf (eval-ctx-aborted ctx) t))
      (t
       (push-error ctx "Value matched no anyOf branches")))))

(defun %check-one-of (subs value ctx schema)
  (declare (ignore schema))
  (let ((passed 0)
        (any-aborted nil)
        (passed-props nil)
        (passed-items nil)
        (saved-props (alexandria:copy-hash-table (eval-ctx-evaluated-props ctx)))
        (saved-items (copy-list (eval-ctx-evaluated-items ctx))))
    (dolist (s subs)
      (multiple-value-bind (ok props items aborted)
          (%check-branch s value ctx saved-props saved-items)
        (when aborted (setf any-aborted t))
        (when ok
          (incf passed)
          (setf passed-props props
                passed-items items))))
    (cond
      ;; An aborted branch makes the exact-count untrustworthy — the value might
      ;; match it — so "exactly one" can no longer be certified. Fail closed.
      (any-aborted
       (push-error ctx "Could not evaluate a `oneOf` branch (depth/stack cap); rejecting")
       (setf (eval-ctx-aborted ctx) t))
      ((zerop passed)
       (push-error ctx "Value matched no oneOf branches"))
      ((> passed 1)
       (push-error ctx (format nil "Value matched ~D oneOf branches; expected 1"
                               passed)))
      (t
       (dolist (k passed-props) (setf (gethash k (eval-ctx-evaluated-props ctx)) t))
       (setf (eval-ctx-evaluated-items ctx)
             (union (eval-ctx-evaluated-items ctx) passed-items))))))

(defun %check-not (sub value ctx schema)
  (declare (ignore schema))
  (let ((scratch (fork-eval-ctx ctx)))
    (%check-schema sub value scratch)
    (cond
      ;; An aborted fork is inconclusive, not a clean failure: reject rather than
      ;; read the refusal-to-descend as "the subschema did not match", which
      ;; would let `not` pass a value it cannot vouch for.
      ((eval-ctx-aborted scratch)
       (push-error ctx "Could not evaluate `not` subschema (depth/stack cap); rejecting")
       (setf (eval-ctx-aborted ctx) t))
      ((null (eval-ctx-errors scratch))
       (push-error ctx "Value matched a `not` branch")))))

(dolist (entry '(("allOf" . %check-all-of)
                 ("anyOf" . %check-any-of)
                 ("oneOf" . %check-one-of)))
  (register-keyword (car entry)
                    :parser (let ((kw (car entry)))
                              (lambda (v) (%parse-schema-array v kw)))
                    :checker (symbol-function (cdr entry))
                    :child-schemas
                    (let ((kw (car entry)))
                      (lambda (parsed)
                        (loop for s in parsed
                              for i from 0
                              collect (cons (format nil "/~A/~D" kw i) s))))))

(register-keyword "not"
                  :parser (lambda (v) (%make-schema v nil))
                  :checker #'%check-not
                  :child-schemas (lambda (p) (list (cons "/not" p))))

;;; ============================================================================
;;; if / then / else
;;; ============================================================================
;;; if/then/else are stored as separate keywords, so their dispatch order is
;;; whatever the keyword alist yields — unspecified. then/else therefore do not
;;; rely on `if` having run: each resolves the branch decision through
;;; %if-branch-matches-p, which evaluates the sibling `if` lazily and caches the
;;; (value . match) so it runs at most once per value. The value-keyed cache
;;; recomputes when the same `if` is re-applied to a different value (array
;;; items, $ref recursion).

(defun %if-branch-matches-p (if-form value ctx)
  "Resolve the sibling `if` subschema IF-FORM against VALUE to a three-valued
   verdict: T (matched), NIL (cleanly did not match), or :ABORTED (the fork
   refused to descend at the depth/stack cap, so the branch is undecidable).
   Forks a scratch context so the condition's own errors never leak, caches the
   (value . verdict) on the ctx, and on a match merges the condition's evaluated
   annotations into the parent — as any applicator that passes contributes its
   annotations. An :ABORTED verdict also marks the parent ctx aborted so the
   undecidable `if` fails closed (then is not dropped, else is not applied)."
  (let ((cache (eval-ctx-if-branch-cache ctx)))
    (multiple-value-bind (cell present) (gethash if-form cache)
      (if (and present (eq (car cell) value))
          (cdr cell)
          (let ((scratch (fork-eval-ctx ctx)))
            (%check-schema if-form value scratch)
            (let ((verdict (cond ((eval-ctx-aborted scratch) :aborted)
                                 ((eval-ctx-errors scratch) nil)
                                 (t t))))
              (setf (gethash if-form cache) (cons value verdict))
              (when (eq verdict :aborted)
                (setf (eval-ctx-aborted ctx) t))
              (when (eq verdict t)
                (loop for k being the hash-keys of (eval-ctx-evaluated-props scratch)
                      do (setf (gethash k (eval-ctx-evaluated-props ctx)) t)))
              verdict))))))

(defun %check-if (sub value ctx schema)
  (declare (ignore schema))
  ;; Populate the cache and merge annotations eagerly when `if` is dispatched;
  ;; then/else recompute lazily if either runs first.
  (%if-branch-matches-p sub value ctx)
  (values))

(defun %check-then (sub value ctx schema)
  (let ((if-form (cdr (assoc "if" (json-schema-keywords schema)
                              :test #'string=))))
    ;; Only a definite match runs `then`; an :aborted verdict leaves it alone
    ;; (the abort already failed the value closed) rather than silently dropping it.
    (when (and if-form (eq (%if-branch-matches-p if-form value ctx) t))
      (%check-schema sub value ctx))))

(defun %check-else (sub value ctx schema)
  (let ((if-form (cdr (assoc "if" (json-schema-keywords schema)
                              :test #'string=))))
    ;; Only a definite non-match runs `else`; :aborted is undecidable, not a miss.
    (when (and if-form (null (%if-branch-matches-p if-form value ctx)))
      (%check-schema sub value ctx))))

(register-keyword "if"
                  :parser (lambda (v) (%make-schema v nil))
                  :checker #'%check-if
                  :child-schemas (lambda (p) (list (cons "/if" p))))
(register-keyword "then"
                  :parser (lambda (v) (%make-schema v nil))
                  :checker #'%check-then
                  :child-schemas (lambda (p) (list (cons "/then" p))))
(register-keyword "else"
                  :parser (lambda (v) (%make-schema v nil))
                  :checker #'%check-else
                  :child-schemas (lambda (p) (list (cons "/else" p))))

;;; ============================================================================
;;; dependentSchemas / dependentRequired
;;; ============================================================================

(defun %parse-dependent-schemas (val)
  (unless (hash-table-p val)
    (raise-invalid-schema "dependentSchemas must be a JSON object"))
  (let ((acc '()))
    (loop for k being the hash-keys of val using (hash-value v)
          do (with-pointer-extension
                 ((concatenate 'string "/" (%escape-pointer-segment k)))
               (push (cons k (%make-schema v nil)) acc)))
    (nreverse acc)))

(defun %check-dependent-schemas (parsed value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (dolist (entry parsed)
      (multiple-value-bind (dependent-value present-p) (gethash (car entry) value)
        (declare (ignore dependent-value))
        (when present-p
          (%check-schema (cdr entry) value ctx))))))

(register-keyword "dependentSchemas"
                  :parser #'%parse-dependent-schemas
                  :checker #'%check-dependent-schemas
                  :child-schemas
                  (lambda (parsed)
                    (mapcar (lambda (e)
                              (cons (concatenate 'string "/dependentSchemas/"
                                                 (%escape-pointer-segment
                                                  (car e)))
                                    (cdr e)))
                            parsed)))

(defun %parse-dependent-required (val)
  (unless (hash-table-p val)
    (raise-invalid-schema "dependentRequired must be a JSON object"))
  (let ((acc '()))
    (loop for k being the hash-keys of val using (hash-value v)
          do (unless (vectorp v)
               (raise-invalid-schema
                "dependentRequired entry ~S must be an array" k))
          do (push (cons k (coerce v 'list)) acc))
    (nreverse acc)))

(defun %check-dependent-required (parsed value ctx schema)
  (declare (ignore schema))
  (when (%json-object-p value)
    (dolist (entry parsed)
      (multiple-value-bind (dependent-value present-p) (gethash (car entry) value)
        (declare (ignore dependent-value))
        (when present-p
          (dolist (req (cdr entry))
            (multiple-value-bind (required-value req-present) (gethash req value)
              (declare (ignore required-value))
              (unless req-present
                (push-error ctx
                            (format nil "Property ~S requires ~S to also be present"
                                    (car entry) req))))))))))

(register-keyword "dependentRequired"
                  :parser #'%parse-dependent-required
                  :checker #'%check-dependent-required)

;;; ============================================================================
;;; $ref / $dynamicRef
;;; ============================================================================
;;; Stored as a parsed-marker so the validator dispatches to %CHECK-REF.
;;; Resolution happens lazily at validate time (lets parse complete even when
;;; the target schema is forward-defined inside the same document).

(defstruct ref-marker uri kind) ; KIND is :REF or :DYNAMIC

(defun %parse-ref (val)
  (unless (stringp val)
    (raise-invalid-schema "$ref must be a string"))
  (make-ref-marker :uri val :kind :ref))

(defun %parse-dynamic-ref (val)
  (unless (stringp val)
    (raise-invalid-schema "$dynamicRef must be a string"))
  (make-ref-marker :uri val :kind :dynamic))

(defun %resolve-ref (marker ctx)
  "Resolve MARKER against CTX's root self-registry. Returns the JSON-SCHEMA
   referent or NIL when unresolvable. For $dynamicRef, walks the dynamic-scope
   stack looking for a matching $dynamicAnchor."
  (let* ((uri (ref-marker-uri marker))
         (root (eval-ctx-root ctx))
         (self (json-schema-self-registry root)))
    (cond
      ;; Same-document JSON Pointer: "#/foo/bar"
      ((and (plusp (length uri))
            (char= (char uri 0) #\#)
            (or (= (length uri) 1)
                (char= (char uri 1) #\/)))
       (gethash (subseq uri 1) self))
      ;; Same-document anchor / dynamic-anchor: "#meta"
      ((and (plusp (length uri))
            (char= (char uri 0) #\#))
       (let ((name (subseq uri 1)))
         (or (when (eq (ref-marker-kind marker) :dynamic)
               ;; Dynamic resolution: search the scope stack outermost-first.
               (loop for scope in (reverse (eval-ctx-dynamic-scope ctx))
                     for found = (gethash (concatenate 'string "$dyn:" name)
                                          scope)
                     when found return found))
             (gethash name self)
             ;; Fall through: anchor lookup may match a $dynamicAnchor entry.
             (gethash (concatenate 'string "$dyn:" name) self))))
      ;; Cross-document URI lookup (registry).
      (t (get-schema uri)))))

(defun %check-ref (marker value ctx schema)
  "Resolve MARKER and validate VALUE against the target. Cycle-safe: if the
   same (target . value) pair is already on the live validation stack, push
   one error and stop — re-entering would loop forever. Legitimate
   recursive schemas (target re-applied against a NESTED value with
   different identity) still descend freely."
  (declare (ignore schema))
  (let ((target (%resolve-ref marker ctx)))
    (cond
      ((null target)
       (unless (eval-ctx-ignore-unresolvable-refs ctx)
         (push-error ctx (format nil "Unresolvable ~A: ~S"
                                 (ref-marker-kind marker)
                                 (ref-marker-uri marker)))))
      ((loop for frame in (eval-ctx-seen-refs ctx)
             thereis (and (eq (car frame) target)
                          (eq (cdr frame) value)))
       (push-error ctx
                   (format nil "$ref cycle: ~A re-enters the same target against the same value (~S)"
                           (ref-marker-kind marker)
                           (ref-marker-uri marker))))
      (t
       (push (cons target value) (eval-ctx-seen-refs ctx))
       (unwind-protect (%check-schema target value ctx)
         (pop (eval-ctx-seen-refs ctx)))))))

(register-keyword "$ref" :parser #'%parse-ref :checker #'%check-ref)
(register-keyword "$dynamicRef"
                  :parser #'%parse-dynamic-ref
                  :checker #'%check-ref)
