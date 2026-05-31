;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/OPENAPI; Base: 10 -*-
;;;; Walk *HANDLER-METADATA*, group by path, build OpenAPI 3.1 alist.
;;;; Serialise via LOL-WEB/SERVER:ENCODE-JSON-STRING.
;;;;
;;;; Output shape mirrors the OpenAPI 3.1 Document object: openapi /
;;;; info / paths. Each PathItem has one OperationObject per HTTP
;;;; method registered for that path. Operations carry parameters
;;;; (one per :path/:query/:header extractor), an optional
;;;; requestBody (one per :body / :json-body extractor), and a
;;;; default 200 response.

(in-package :lol-web/openapi)

;;; ============================================================================
;;; EXTRACTOR → PARAMETER OBJECT
;;; ============================================================================

(defun %extractor-spec-to-parameter (spec)
  "Build an OpenAPI 3.1 ParameterObject alist from a parameter-bearing
   EXTRACTOR-SPEC (kind in :path / :query / :header / :cookie). Path
   parameters always emit `required: true` per OpenAPI's hard
   constraint; other locations emit `required: true` only when the
   extractor was declared :REQUIRED T. Falsy `required` is omitted
   (defaults to false in OpenAPI). The schema field is omitted when
   the extractor's :TYPE is T (boolean-schema true is implied)."
  (let* ((kind (extractor-spec-kind spec))
         (location (kind-to-openapi-location kind))
         (param-name (or (extractor-spec-source-string spec)
                         (string-downcase
                          (symbol-name (extractor-spec-name spec)))))
         (path-param-p (eq kind :path))
         (required (if path-param-p
                       t
                       (extractor-spec-required-p spec)))
         (schema (lisp-type-to-openapi-schema (extractor-spec-type spec)))
         (cells (list (cons "name" param-name)
                      (cons "in"   location))))
    (when required
      (setf cells (append cells (list (cons "required" t)))))
    (append cells (list (cons "schema" schema)))))

;;; ============================================================================
;;; BODY EXTRACTORS → REQUEST BODY OBJECT
;;; ============================================================================

(defun %body-extractor-content-type (kind)
  "Map a body-bearing extractor kind to its OpenAPI media-type key."
  (case kind
    (:json-body "application/json")
    (:body      "application/x-www-form-urlencoded")))

(defun %body-extractors-to-request-body (body-extractors)
  "Build an OpenAPI RequestBodyObject alist from the body-bearing
   extractors of one operation. v0.1.0 emits one media-type entry per
   distinct body extractor; if multiple extractors share the same
   media-type their schemas are not merged (the first wins). REQUIRED
   tracks any one body extractor being required."
  (let* ((seen (make-hash-table :test 'equal))
         (any-required nil)
         (content-cells '()))
    (dolist (spec body-extractors)
      (let ((ct (%body-extractor-content-type (extractor-spec-kind spec))))
        (when (extractor-spec-required-p spec)
          (setf any-required t))
        (unless (gethash ct seen)
          (setf (gethash ct seen) t)
          (let ((schema (lisp-type-to-openapi-schema (extractor-spec-type spec))))
            (setf content-cells
                  (append content-cells
                          (list (cons ct (list (cons "schema" schema))))))))))
    (let ((cells (list (cons "content" content-cells))))
      (when any-required
        (setf cells (append cells (list (cons "required" t)))))
      cells)))

;;; ============================================================================
;;; OPERATION OBJECT
;;; ============================================================================

(defun %partition-extractors (extractors)
  "Split EXTRACTORS into (PARAMETER-EXTRACTORS BODY-EXTRACTORS) by
   KIND-TO-OPENAPI-LOCATION dispatch. Custom-resolver extractors with
   no recognisable KIND are dropped — they cannot be emitted without
   user-supplied schema integration."
  (let ((params '()) (bodies '()))
    (dolist (spec extractors)
      (let ((kind (extractor-spec-kind spec)))
        (cond
          ((kind-to-openapi-location kind) (push spec params))
          ((or (eq kind :body) (eq kind :json-body)) (push spec bodies)))))
    (values (nreverse params) (nreverse bodies))))

(defun %symbol-to-display (symbol)
  "Render a handler-name SYMBOL as the lowercase string used for both
   the operation summary and the operationId."
  (string-downcase (symbol-name symbol)))

(defun %meta-security-requirement (meta security-scheme-names)
  "Translate META's :OPTIONS :AUTH annotation into an OpenAPI 3.1
   SecurityRequirementObject list, or NIL when no security applies.

   :AUTH semantics —
   - NIL / absent           → NIL (operation emits no `security` key).
   - T                      → require every name in SECURITY-SCHEME-NAMES.
   - list of strings        → require exactly those scheme names (each with
                              empty scopes — sufficient for cookieAuth-style
                              session gates; per-scope work is out of scope).

   Returns the requirement value as an alist-of-alists ready to be emitted
   as a JSON array of objects."
  (let ((auth (getf (getf meta :options) :auth)))
    (cond
      ((null auth) nil)
      ((null security-scheme-names) nil)
      ((eq auth t)
       (list (mapcar (lambda (name) (cons name '())) security-scheme-names)))
      ((and (listp auth) (every #'stringp auth))
       (list (mapcar (lambda (name) (cons name '())) auth)))
      (t (error "defhandler :auth must be T or a list of scheme-name strings, got ~S" auth)))))

(defun %build-operation-object (meta security-scheme-names)
  "Assemble an OpenAPI OperationObject alist for one METADATA plist
   entry from *HANDLER-METADATA*. The operation always carries a
   default 200 response (description `OK`) so that consumers without
   declared response types still emit a spec-valid responses map —
   the OpenAPI 3.1 schema requires `responses` to be a non-empty
   object.

   SECURITY-SCHEME-NAMES is the list of scheme names declared on the
   emitted document via :SECURITY-SCHEMES; used to expand :AUTH T."
  (multiple-value-bind (param-extractors body-extractors)
      (%partition-extractors (getf meta :extractors))
    (let* ((display-name (%symbol-to-display (getf meta :name)))
           (parameters (mapcar #'%extractor-spec-to-parameter param-extractors))
           (request-body (when body-extractors
                           (%body-extractors-to-request-body body-extractors)))
           (security (%meta-security-requirement meta security-scheme-names))
           (cells (list (cons "summary" display-name)
                        (cons "operationId" display-name))))
      (when parameters
        (setf cells (append cells (list (cons "parameters" parameters)))))
      (when request-body
        (setf cells (append cells (list (cons "requestBody" request-body)))))
      (when security
        (setf cells (append cells (list (cons "security" security)))))
      (append cells
              (list (cons "responses"
                          (list (cons "200"
                                      (list (cons "description" "OK"))))))))))

;;; ============================================================================
;;; PATH ITEM
;;; ============================================================================

(defun %method-keyword-to-string (method)
  "Convert a method keyword (e.g. :GET) to its OpenAPI path-item key
   (e.g. `get`). All HTTP methods OpenAPI 3.1 supports are lowercase
   in the spec — the symbol-name downcase suffices."
  (string-downcase (symbol-name method)))

(defun %build-path-item (metas security-scheme-names)
  "Build a PathItemObject alist from all METAS (handler-metadata plists)
   that share an OpenAPI path. One entry per HTTP method, value is the
   method's OperationObject. METAS need not be ordered; the resulting
   alist preserves the input order for stability across builds against
   the same registration sequence."
  (loop for meta in metas
        collect (cons (%method-keyword-to-string (getf meta :method))
                      (%build-operation-object meta security-scheme-names))))

(defun %meta-internal-p (meta)
  "T when META carries `:internal T` in its :OPTIONS plist. Internal routes
   (e.g. those generated by DEFWIZARD) are excluded from the emitted spec
   so the published surface stays curated."
  (and (getf (getf meta :options) :internal) t))

;;; ============================================================================
;;; PATHS — GROUP BY OPENAPI-PATH STRING
;;; ============================================================================

(defun %group-handler-metadata-by-openapi-path ()
  "Snapshot *HANDLER-METADATA* under the registry lock and group its
   plist values by their OpenAPI-translated path. Returns a hash-table
   keyed by OpenAPI path string, value a list of metadata plists in
   registration order (push-then-nreverse)."
  (let ((groups (make-hash-table :test 'equal)))
    ;; Architectural :: reach into :lol-web/extractors. The lock symbol is
    ;; deliberately unexported — taking the snapshot under the same lock
    ;; defhandler registrations take is the only way to avoid torn reads
    ;; under concurrent registration. Exporting the lock would invite
    ;; external misuse, so the cross-package reach is intentional.
    (bordeaux-threads:with-recursive-lock-held
        (lol-web/extractors::*handler-metadata-lock*)
      (maphash
       (lambda (key meta)
         (declare (ignore key))
         (let ((openapi-path (%lisp-path-to-openapi-path (getf meta :path))))
           (push meta (gethash openapi-path groups))))
       *handler-metadata*))
    (maphash (lambda (k v) (setf (gethash k groups) (nreverse v))) groups)
    groups))

(defun %build-paths-object (&key only-paths security-scheme-names)
  "Build the OpenAPI PathsObject alist from *HANDLER-METADATA*.
   When ONLY-PATHS is non-NIL, restrict the output to the given list
   of OpenAPI-translated paths (string equality). The default is to
   emit every registered path.

   Metas marked :INTERNAL T (e.g. wizard routes) are dropped; a path whose
   every meta is internal contributes no entry."
  (let ((groups (%group-handler-metadata-by-openapi-path))
        (paths-cells '()))
    (maphash
     (lambda (openapi-path metas)
       (let ((public-metas (remove-if #'%meta-internal-p metas)))
         (when (and public-metas
                    (or (null only-paths)
                        (member openapi-path only-paths :test #'string=)))
           (push (cons openapi-path
                       (%build-path-item public-metas security-scheme-names))
                 paths-cells))))
     groups)
    paths-cells))

;;; ============================================================================
;;; INFO OBJECT
;;; ============================================================================

(defun %build-info-object (title version description)
  "Build the OpenAPI InfoObject alist. TITLE and VERSION are required
   by the spec; DESCRIPTION is optional and omitted when NIL."
  (let ((cells (list (cons "title" title)
                     (cons "version" version))))
    (when description
      (setf cells (append cells (list (cons "description" description)))))
    cells))

;;; ============================================================================
;;; PUBLIC ENTRYPOINTS
;;; ============================================================================

(defun build-openapi-spec (&key (title "lol-web API")
                                (version "0.0.0")
                                description
                                only-paths
                                security-schemes)
  "Walk LOL-WEB/EXTRACTORS:*HANDLER-METADATA* and return an OpenAPI 3.1
   document as a Lisp alist suitable for LOL-WEB/SERVER:ENCODE-JSON-STRING.

   :TITLE / :VERSION populate the InfoObject (defaults are placeholder
   strings; production callers should pass real values from their
   project's version.sexp / readme). :DESCRIPTION is added to InfoObject
   when supplied. :ONLY-PATHS, when non-NIL, restricts output to the
   given list of OpenAPI-translated paths — useful for emitting
   per-subsystem specs.

   :SECURITY-SCHEMES is an alist ((SCHEME-NAME . SCHEME-OBJECT) ...) emitted
   under components.securitySchemes. Operations whose meta carries
   :OPTIONS :AUTH propagate a `security` requirement referencing those
   scheme names. When :SECURITY-SCHEMES is NIL the document omits
   `components` and no operation emits `security`.

   The returned alist is immutable structurally — re-call to refresh."
  (let* ((scheme-names (mapcar #'car security-schemes))
         (cells (list (cons "openapi" "3.1.0")
                      (cons "info" (%build-info-object title version description))
                      (cons "paths" (%build-paths-object
                                     :only-paths only-paths
                                     :security-scheme-names scheme-names)))))
    (when security-schemes
      (setf cells
            (append cells
                    (list (cons "components"
                                (list (cons "securitySchemes"
                                            security-schemes)))))))
    cells))

(defun emit-openapi-json (&key (title "lol-web API")
                               (version "0.0.0")
                               description
                               only-paths
                               security-schemes)
  "Build the OpenAPI 3.1 spec via BUILD-OPENAPI-SPEC and serialise to
   JSON via LOL-WEB/SERVER:ENCODE-JSON-STRING. Returns the JSON string."
  (encode-json-string
   (build-openapi-spec :title title
                       :version version
                       :description description
                       :only-paths only-paths
                       :security-schemes security-schemes)))
