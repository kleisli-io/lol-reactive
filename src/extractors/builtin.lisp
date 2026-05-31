;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/EXTRACTORS; Base: 10 -*-
;;;; Built-in RESOLVE-EXTRACTOR methods for :path :query :body :header :json-body.
;;;; Each method reads from *env* / *path-params* (already populated by
;;;; LOL-WEB/SERVER:BUILD-CLACK-ENV / FIND-MATCHING-ROUTE), applies the
;;;; required-p / default / coercion policy, returns the resolved value.

(in-package :lol-web/extractors)

;;; ============================================================================
;;; POLICY HELPER
;;; ============================================================================

(defun %apply-spec-policy (spec raw)
  "Common required-p / default / coercion handling. RAW is what the
   kind-specific resolver pulled from the request (a string for string-sourced
   extractors, an arbitrary Lisp value for :json-body, etc.).

   - Missing-and-required → MISSING-EXTRACTOR-INPUT (422).
   - Missing-and-optional with DEFAULT → call the default thunk (no coercion).
   - Missing-and-optional without DEFAULT → NIL.
   - Present and TYPE = T → return as-is.
   - Present otherwise → %COERCE-VALUE (per-type coercers handle both string
     and already-typed inputs defensively).

   Note: NIL is treated as missing here, even though :json-body could in
   principle yield NIL for a literal JSON null. v0.1.0 trade-off: the
   simpler missing/present model wins; users who need to distinguish
   absent-from-body from explicit-null can wrap with a custom resolver."
  (cond
    ((null raw)
     (cond
       ((extractor-spec-required-p spec)
        ;; No :body — the generic default avoids naming the extractor on the
        ;; wire; the condition REPORT carries kind+name for server logs.
        (error 'missing-extractor-input
               :extractor-name (extractor-spec-name spec)
               :extractor-kind (extractor-spec-kind spec)))
       ((extractor-spec-default spec)
        (funcall (extractor-spec-default spec)))
       (t nil)))
    ((eq (extractor-spec-type spec) t)
     raw)
    (t
     (%coerce-value raw (extractor-spec-type spec) spec))))

(defun %default-source (spec)
  "Default lookup key: source-string slot, else downcased symbol-name of NAME."
  (or (extractor-spec-source-string spec)
      (string-downcase (symbol-name (extractor-spec-name spec)))))

;;; ============================================================================
;;; CUSTOM RESOLVER DISPATCH
;;; ============================================================================
;;;
;;; If the spec carries a CUSTOM-RESOLVER symbol, we route to it ahead of the
;;; kind-based dispatch. Implemented as an :around method on (kind t) so it
;;; intercepts before any user-added methods on specific kinds.

(defmethod resolve-extractor :around ((kind t) spec env path-params)
  (let ((resolver (extractor-spec-custom-resolver spec)))
    (if resolver
        (funcall (fdefinition resolver) spec env path-params)
        (call-next-method))))

;;; ============================================================================
;;; :path
;;; ============================================================================

(defmethod resolve-extractor ((kind (eql :path)) spec env path-params)
  (declare (ignore env))
  (let* ((source (%default-source spec))
         (raw (cdr (assoc source path-params :test #'string=))))
    (%apply-spec-policy spec raw)))

;;; ============================================================================
;;; :query
;;; ============================================================================

(defmethod resolve-extractor ((kind (eql :query)) spec env path-params)
  (declare (ignore path-params))
  (let* ((source (%default-source spec))
         (params (getf env :query-parameters))
         (raw (cdr (assoc source params :test #'string=))))
    (%apply-spec-policy spec raw)))

;;; ============================================================================
;;; :body — form-encoded POST parameter
;;; ============================================================================

(defmethod resolve-extractor ((kind (eql :body)) spec env path-params)
  (declare (ignore path-params))
  (let* ((source (%default-source spec))
         (params (getf env :body-parameters))
         (raw (cdr (assoc source params :test #'string=))))
    (%apply-spec-policy spec raw)))

;;; ============================================================================
;;; :header
;;; ============================================================================

(defmethod resolve-extractor ((kind (eql :header)) spec env path-params)
  (declare (ignore path-params))
  ;; Headers in :env are stored under :headers as a hash-table keyed by
  ;; lowercased name. The default-source is already lowercased; if the user
  ;; supplies :source \"X-API-Key\" we lowercase that too for the lookup.
  (let* ((source (string-downcase (%default-source spec)))
         (headers (getf env :headers))
         (raw (and headers (gethash source headers))))
    (%apply-spec-policy spec raw)))

;;; ============================================================================
;;; :json-body
;;; ============================================================================

(defmethod resolve-extractor ((kind (eql :json-body)) spec env path-params)
  (declare (ignore path-params))
  ;; Dispatches through PARSE-REQUEST-JSON which memoizes via :lol/cached-body-json.
  ;; The :env binding here is what RESOLVE-EXTRACTOR's caller passes; PARSE-REQUEST-JSON
  ;; reads the dynamic *env* (which the macro expansion set up), so we let-bind
  ;; *env* to the passed env to keep the API consistent across resolvers.
  (let* ((lol-web/server:*env* env)
         (raw (lol-web/server:parse-request-json)))
    (%apply-spec-policy spec raw)))
