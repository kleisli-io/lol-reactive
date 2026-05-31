;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/JSCHEMA; Base: 10 -*-
;;;; VALIDATE — apply a parsed JSON-SCHEMA to a jzon-shaped value.
;;;;
;;;; Validation is a single recursive walk over the schema's keyword alist.
;;;; Each keyword's checker pushes INVALID-JSON-VALUE conditions onto the
;;;; current evaluation context's error list and may extend the
;;;; evaluated-properties / evaluated-items annotation sets that
;;;; unevaluatedProperties / unevaluatedItems consult.

(in-package :lol-web/jschema)

;;; ============================================================================
;;; EVALUATION CONTEXT
;;; ============================================================================

(defparameter *max-validation-depth* 256
  "Recursion bound for %check-schema; every frame counts (value nesting and
   in-place applicators allOf/anyOf/oneOf/if/$ref alike). Exceeding it marks the
   context aborted and refuses to descend, so not/if/anyOf/oneOf fail closed
   rather than read the truncation as a clean non-match. Every native descent
   passes through %check-schema with an O(1) applicator chain between frames, so
   this finite cap bounds the native control stack by construction. The 256
   default sits within a default thread's stack; a caller rebinding it deeper
   must keep the value within its own stack budget.")

(defstruct eval-ctx
  "Per-validation-call mutable state. ROOT is the top-level JSON-SCHEMA.
   ERRORS accumulates INVALID-JSON-VALUE instances. POINTER is the current
   value-side JSON Pointer. EVALUATED-PROPS / EVALUATED-ITEMS are filled by
   applicators so unevaluated* keywords know what's already covered.
   IGNORE-UNRESOLVABLE-REFS mirrors cl-jschema's option of the same name.
   DEPTH tracks the current %check-schema recursion depth (capped by
   *max-validation-depth*). SEEN-REFS is the per-call active-ref stack
   ($ref cycle detector — each entry is (target-schema . value) on the
   live validation path). IF-BRANCH-CACHE maps each `if` parsed-schema
   instance to a (value . match) cell: then/else compute the branch lazily and
   order-independently, the cell caches the decision so it runs at most once per
   value, and the stored value identity makes a re-applied `if` (array items,
   $ref recursion) recompute instead of reusing a stale branch. It is
   per-context, so each scratch fork decides its own branches in isolation.
   COMBINATOR-MEMO, when non-NIL, memoizes isolated (sub-schema . value) branch
   validations shared across the call, so a recursive union schema costs O(n)
   rather than exponential work; it is installed only when no unevaluated*
   keyword can read the annotation baseline, which keeps the memoized result a
   pure function of (sub-schema, value). ABORTED is set when the depth cap
   refuses to descend; the branch-isolating applicators read it to fail closed
   rather than mistake the refusal for a clean non-match."
  root
  (errors nil)
  (pointer "")
  (evaluated-props (make-hash-table :test 'equal))
  (evaluated-items '())
  (ignore-unresolvable-refs nil)
  (dynamic-scope nil)          ; stack of root self-registries for $dynamicRef
  (depth 0 :type fixnum)
  (seen-refs nil)
  (if-branch-cache (make-hash-table :test 'eq))
  (combinator-memo nil)
  (aborted nil))

(defmacro with-pointer ((ctx suffix) &body body)
  "Run BODY with CTX's pointer extended by SUFFIX (already including its
   leading slash). Restores the prior pointer on exit."
  (alexandria:with-gensyms (g-saved g-ctx g-suffix)
    `(let* ((,g-ctx ,ctx)
            (,g-suffix ,suffix)
            (,g-saved (eval-ctx-pointer ,g-ctx)))
       (unwind-protect
            (progn
              (setf (eval-ctx-pointer ,g-ctx)
                    (concatenate 'string ,g-saved ,g-suffix))
              ,@body)
         (setf (eval-ctx-pointer ,g-ctx) ,g-saved)))))

(defun push-error (ctx message)
  "Record an INVALID-JSON-VALUE on the eval context."
  (push (make-condition 'invalid-json-value
                        :error-message message
                        :json-pointer (eval-ctx-pointer ctx))
        (eval-ctx-errors ctx)))

(defun fork-eval-ctx (ctx &key (evaluated-props nil props-supplied)
                               (evaluated-items nil items-supplied))
  "Scratch eval-ctx for validating a branch sub-schema (anyOf / oneOf / if /
   not) in isolation. Threads every field that must survive the fork — ROOT,
   POINTER, IGNORE-UNRESOLVABLE-REFS, DYNAMIC-SCOPE, the live DEPTH counter,
   and an independent copy of SEEN-REFS — so the recursion-depth cap and the
   $ref cycle detector keep applying inside the branch. COMBINATOR-MEMO threads
   by shared reference, so a (sub-schema . value) result computed in one branch
   is reused by every other. ERRORS start empty so the caller can test whether
   the branch validated on its own. EVALUATED-PROPS / EVALUATED-ITEMS default to
   empty; a caller threading an annotation baseline into the branch passes it
   explicitly."
  (make-eval-ctx
   :root (eval-ctx-root ctx)
   :pointer (eval-ctx-pointer ctx)
   :ignore-unresolvable-refs (eval-ctx-ignore-unresolvable-refs ctx)
   :dynamic-scope (eval-ctx-dynamic-scope ctx)
   :depth (eval-ctx-depth ctx)
   :seen-refs (copy-list (eval-ctx-seen-refs ctx))
   :combinator-memo (eval-ctx-combinator-memo ctx)
   :evaluated-props (if props-supplied
                        evaluated-props
                        (make-hash-table :test 'equal))
   :evaluated-items (if items-supplied evaluated-items '())))

;;; ============================================================================
;;; ENTRYPOINT
;;; ============================================================================

(defgeneric validate (schema value &key &allow-other-keys)
  (:documentation
   "Validate VALUE against SCHEMA. Returns T on success or signals INVALID-JSON
    with all collected errors on failure. VALUE must be in jzon shape (hash-
    tables for objects, vectors for arrays)."))

(defun %document-reads-annotation-baseline-p (root)
  "True when any schema in ROOT's document carries unevaluatedProperties or
   unevaluatedItems — the only keywords whose result depends on the annotation
   baseline a branch fork inherits. The scan covers the document's own schemas
   (every descended schema is registered in the root self-registry); a boolean
   or registry-less root has no subschemas to read it."
  (let ((registry (json-schema-self-registry root)))
    (and registry
         (loop for s being the hash-values of registry
               thereis (and (json-schema-p s)
                            (or (assoc "unevaluatedProperties"
                                       (json-schema-keywords s) :test #'string=)
                                (assoc "unevaluatedItems"
                                       (json-schema-keywords s) :test #'string=)))))))

(defmethod validate ((schema json-schema) value &key ignore-unresolvable-refs)
  (let ((ctx (make-eval-ctx
              :root schema
              :ignore-unresolvable-refs ignore-unresolvable-refs
              :dynamic-scope (when (json-schema-self-registry schema)
                               (list (json-schema-self-registry schema)))
              ;; The (sub-schema . value) branch memo is sound only when no
              ;; keyword reads the annotation baseline; otherwise a branch result
              ;; is not a pure function of its inputs, so leave it off.
              :combinator-memo (unless (%document-reads-annotation-baseline-p schema)
                                 (make-hash-table :test 'eq)))))
    (%check-schema schema value ctx)
    (when (and (eval-ctx-aborted ctx) (null (eval-ctx-errors ctx)))
      (push-error ctx "Validation aborted before completing (depth cap); rejecting."))
    (when (eval-ctx-errors ctx)
      (error 'invalid-json :errors (nreverse (eval-ctx-errors ctx))))
    t))

;;; ============================================================================
;;; CORE: %CHECK-SCHEMA
;;; ============================================================================

(defun %check-schema (schema value ctx)
  "Apply SCHEMA to VALUE, mutating CTX. Boolean schemas short-circuit:
   true → no-op; false → one error. Recursion is bounded by
   *max-validation-depth*: exceeding it marks CTX aborted, pushes one error, and
   refuses to descend, so an enclosing not/if/anyOf/oneOf fails closed on the
   truncation. Every descent passes through here, so the cap also bounds native
   control-stack use."
  (when (>= (eval-ctx-depth ctx) *max-validation-depth*)
    (setf (eval-ctx-aborted ctx) t)
    (push-error ctx
                (format nil "Validation depth exceeds *max-validation-depth* (~D); refusing to descend further."
                        *max-validation-depth*))
    (return-from %check-schema (values)))
  (incf (eval-ctx-depth ctx))
  (unwind-protect
       (progn
         (case (json-schema-bool schema)
           (:true (return-from %check-schema (values)))
           (:false
            (push-error ctx "Schema is `false` — value cannot validate.")
            (return-from %check-schema (values))))
         (dolist (entry (json-schema-keywords schema))
           (let ((checker (gethash (car entry) *keyword-checkers*)))
             (when checker
               (funcall checker (cdr entry) value ctx schema))))
         (values))
    (decf (eval-ctx-depth ctx))))
