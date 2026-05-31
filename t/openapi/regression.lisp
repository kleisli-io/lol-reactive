;;;; Regression tests for :lol-web/openapi.
;;;;
;;;; Covers path-template translation, lisp-type-to-openapi-schema mapping,
;;;; kind-to-openapi-location dispatch, parameter object emission, request
;;;; body emission for :body / :json-body, full operation/path-item/document
;;;; assembly via BUILD-OPENAPI-SPEC, JSON-string serialisation via
;;;; EMIT-OPENAPI-JSON, and the conformance gate validating an emitted spec
;;;; against the upstream OpenAPI 3.1 base schema via :lol-web/jschema.

(in-package :lol-web/openapi/test)
(in-suite :lol-web/openapi/test)

;;; ============================================================================
;;; SCHEMA-PATH SLOT
;;;
;;; ASDF:SYSTEM-RELATIVE-PATHNAME is unavailable under buildLisp (each source
;;; is pulled into the Nix store individually, with no system-relative root).
;;; The buildLisp wrapper rewrites this defparameter at compile time to bind
;;; *OPENAPI-3.1-SCHEMA-PATH* to the literal Nix-store path of the bundled
;;; OpenAPI 3.1 schema fixture. Under interactive REPL load the value stays
;;; NIL and the conformance gate test below short-circuits.
;;; ============================================================================

(defparameter *openapi-3.1-schema-path* nil
  "Nix-store path of the bundled OpenAPI 3.1 schema fixture, set by the
   buildLisp test wrapper. NIL during interactive REPL use; the
   schema-validation gate test degrades to a no-op when unbound.")

;;; ============================================================================
;;; FIXTURES
;;;
;;; Each fixture defhandler lands in *handler-metadata* on file load. Tests
;;; scope BUILD-OPENAPI-SPEC via :ONLY-PATHS so unrelated registered routes
;;; (from other sub-systems loaded into the same image) cannot bleed in.
;;; ============================================================================

(defhandler %fixture-get-thing "/openapi-fixtures/things/:id"
    (:method :get :content-type "application/json")
    ((id :path :type integer)
     (limit :query :type integer :required nil :default 20)
     (verbose :query :type boolean :required nil)
     (api-key :header :source "X-API-Key" :type string))
  (encode-json-string (list (cons :id id))))

(defhandler %fixture-create-thing "/openapi-fixtures/things"
    (:method :post :content-type "application/json")
    ((body :json-body :type t))
  (encode-json-string body))

(defhandler %fixture-form-post "/openapi-fixtures/forms"
    (:method :post :content-type "application/json")
    ((body :body :type t :required nil))
  (encode-json-string (list (cons :got (or body "(none)")))))

(defhandler %fixture-list-things "/openapi-fixtures/things"
    (:method :get :content-type "application/json")
    ()
  "[]")

;;; ============================================================================
;;; HELPERS
;;; ============================================================================

(defun %assoc-string (key alist)
  (cdr (assoc key alist :test #'string=)))

(defun %fixture-spec ()
  (build-openapi-spec
   :title "Fixture API" :version "0.1.0"
   :only-paths '("/openapi-fixtures/things/{id}"
                 "/openapi-fixtures/things"
                 "/openapi-fixtures/forms")))

;;; ============================================================================
;;; PATH TEMPLATE TRANSLATION
;;; ============================================================================

(test path-template-rewrites-colon-segments
  "/users/:id → /users/{id}; multiple parameters; non-parameter segments
   pass through unchanged."
  (is (string= "/users/{id}"
               (lol-web/openapi::%lisp-path-to-openapi-path "/users/:id")))
  (is (string= "/users/{id}/posts/{post-id}"
               (lol-web/openapi::%lisp-path-to-openapi-path
                "/users/:id/posts/:post-id")))
  (is (string= "/static/path"
               (lol-web/openapi::%lisp-path-to-openapi-path "/static/path"))))

;;; ============================================================================
;;; lisp-type-to-openapi-schema
;;; ============================================================================

(test lisp-type-to-openapi-schema-built-ins
  "Built-in coercion types map to JSON Schema fragments; T means
   'any value valid' (encoder serialises this to JSON true)."
  (is (eq t (lisp-type-to-openapi-schema t)))
  (is (equal '(("type" . "integer")) (lisp-type-to-openapi-schema 'integer)))
  (is (equal '(("type" . "string"))  (lisp-type-to-openapi-schema 'string)))
  (is (equal '(("type" . "boolean")) (lisp-type-to-openapi-schema 'boolean)))
  (is (equal '(("type" . "string"))  (lisp-type-to-openapi-schema 'keyword)))
  (is (equal '(("type" . "string"))  (lisp-type-to-openapi-schema 'symbol))))

(test lisp-type-to-openapi-schema-unknown-falls-back-to-true
  "Unknown types fall back to T so unfamiliar coercions emit a permissive
   schema rather than crashing the emitter."
  (is (eq t (lisp-type-to-openapi-schema 'some-unknown-type)))
  (is (eq t (lisp-type-to-openapi-schema 'list))))

;;; ============================================================================
;;; kind-to-openapi-location
;;; ============================================================================

(test kind-to-openapi-location-parameters
  ":path / :query / :header / :cookie are parameter locations and emit
   their string forms."
  (is (string= "path"   (kind-to-openapi-location :path)))
  (is (string= "query"  (kind-to-openapi-location :query)))
  (is (string= "header" (kind-to-openapi-location :header)))
  (is (string= "cookie" (kind-to-openapi-location :cookie))))

(test kind-to-openapi-location-bodies-are-not-parameters
  ":body and :json-body are request-body kinds, not parameters; the NIL
   return signals BUILD-OPENAPI-SPEC to route them into requestBody."
  (is (null (kind-to-openapi-location :body)))
  (is (null (kind-to-openapi-location :json-body))))

(test kind-to-openapi-location-unknown-kind-returns-nil
  "Custom user-defined kinds default to NIL — they cannot be emitted
   without user-supplied schema integration."
  (is (null (kind-to-openapi-location :totally-custom))))

;;; ============================================================================
;;; OPERATION OBJECT — PARAMETERS
;;; ============================================================================

(test get-operation-emits-parameter-per-extractor-kind
  "GET /things/{id}'s operation has four parameters (path id, query
   limit, query verbose, header X-API-Key) — one per parameter
   extractor, ordered as in the lambda list."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things/{id}" paths))
         (op (%assoc-string "get" path))
         (params (%assoc-string "parameters" op)))
    (is (= 4 (length params)))
    (is (equal '("id" "limit" "verbose" "X-API-Key")
               (mapcar (lambda (p) (%assoc-string "name" p)) params)))
    (is (equal '("path" "query" "query" "header")
               (mapcar (lambda (p) (%assoc-string "in" p)) params)))))

(test path-parameter-required-is-always-true
  "OpenAPI requires path parameters to declare required: true regardless
   of the extractor's :REQUIRED-P; the emitter forces this."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things/{id}" paths))
         (op (%assoc-string "get" path))
         (params (%assoc-string "parameters" op))
         (id-param (find "id" params :key (lambda (p) (%assoc-string "name" p))
                         :test #'string=)))
    (is (eq t (%assoc-string "required" id-param)))))

(test optional-query-parameter-omits-required-key
  "Query parameter with :REQUIRED NIL has no `required` key in the
   parameter object (OpenAPI's default is false; omitting is canonical)."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things/{id}" paths))
         (op (%assoc-string "get" path))
         (params (%assoc-string "parameters" op))
         (limit (find "limit" params :key (lambda (p) (%assoc-string "name" p))
                      :test #'string=)))
    (is (null (assoc "required" limit :test #'string=)))))

(test required-header-parameter-emits-required-true
  "Required (:REQUIRED T or default) non-path parameter emits required: true."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things/{id}" paths))
         (op (%assoc-string "get" path))
         (params (%assoc-string "parameters" op))
         (header (find "X-API-Key" params
                       :key (lambda (p) (%assoc-string "name" p))
                       :test #'string=)))
    (is (eq t (%assoc-string "required" header)))))

(test header-parameter-uses-source-string-as-name
  "When :SOURCE is supplied (e.g. for headers like X-API-Key whose
   conventional spelling differs from the symbol name), the OpenAPI
   parameter name is the source string, not the symbol's downcased
   name."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things/{id}" paths))
         (op (%assoc-string "get" path))
         (params (%assoc-string "parameters" op))
         (names (mapcar (lambda (p) (%assoc-string "name" p)) params)))
    (is (member "X-API-Key" names :test #'string=))
    (is (not (member "api-key" names :test #'string=)))))

(test parameter-schema-reflects-extractor-type
  "Each parameter's `schema` matches LISP-TYPE-TO-OPENAPI-SCHEMA on the
   extractor's :TYPE: integer / boolean / string."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things/{id}" paths))
         (op (%assoc-string "get" path))
         (params (%assoc-string "parameters" op)))
    (flet ((schema-of (n)
             (%assoc-string "schema"
                            (find n params
                                  :key (lambda (p) (%assoc-string "name" p))
                                  :test #'string=))))
      (is (equal '(("type" . "integer")) (schema-of "id")))
      (is (equal '(("type" . "integer")) (schema-of "limit")))
      (is (equal '(("type" . "boolean")) (schema-of "verbose")))
      (is (equal '(("type" . "string"))  (schema-of "X-API-Key"))))))

;;; ============================================================================
;;; OPERATION OBJECT — REQUEST BODY
;;; ============================================================================

(test json-body-extractor-emits-application-json-request-body
  "POST /things with a :json-body extractor emits requestBody with a
   single application/json content entry; required: true follows the
   extractor's :REQUIRED-P."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things" paths))
         (op (%assoc-string "post" path))
         (rb (%assoc-string "requestBody" op))
         (content (%assoc-string "content" rb))
         (json-entry (%assoc-string "application/json" content)))
    (is (not (null rb)))
    (is (= 1 (length content)))
    (is (not (null json-entry)))
    (is (eq t (%assoc-string "required" rb)))))

(test body-extractor-emits-form-urlencoded-request-body
  "POST /forms with a :body extractor emits requestBody with a single
   application/x-www-form-urlencoded content entry. :REQUIRED NIL on
   the extractor omits the requestBody-level required key."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/forms" paths))
         (op (%assoc-string "post" path))
         (rb (%assoc-string "requestBody" op))
         (content (%assoc-string "content" rb))
         (form-entry (%assoc-string "application/x-www-form-urlencoded" content)))
    (is (not (null rb)))
    (is (= 1 (length content)))
    (is (not (null form-entry)))
    (is (null (assoc "required" rb :test #'string=)))))

(test operation-without-extractors-omits-parameters-and-request-body
  "An empty extractor list emits an operation with no `parameters` and
   no `requestBody` keys — only summary, operationId, and responses."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things" paths))
         (op (%assoc-string "get" path)))
    (is (null (assoc "parameters" op :test #'string=)))
    (is (null (assoc "requestBody" op :test #'string=)))
    (is (not (null (%assoc-string "summary" op))))
    (is (not (null (%assoc-string "operationId" op))))
    (is (not (null (%assoc-string "responses" op))))))

;;; ============================================================================
;;; PATH ITEM — METHOD GROUPING
;;; ============================================================================

(test path-item-groups-multiple-methods
  "Two handlers sharing the same path produce one path-item with one
   operation per method (here: GET and POST on /things)."
  (let* ((spec (%fixture-spec))
         (paths (%assoc-string "paths" spec))
         (path (%assoc-string "/openapi-fixtures/things" paths))
         (methods (mapcar #'car path)))
    (is (= 2 (length methods)))
    (is (member "get"  methods :test #'string=))
    (is (member "post" methods :test #'string=))))

;;; ============================================================================
;;; DOCUMENT — TOP-LEVEL FIELDS
;;; ============================================================================

(test document-emits-required-top-level-fields
  "An OpenAPI 3.1 document needs openapi/info/paths. Info needs
   title/version. Description is optional and omitted when not
   supplied."
  (let* ((spec (build-openapi-spec :title "T" :version "1.0.0"
                                   :only-paths '("/openapi-fixtures/things")))
         (info (%assoc-string "info" spec)))
    (is (string= "3.1.0" (%assoc-string "openapi" spec)))
    (is (string= "T"     (%assoc-string "title" info)))
    (is (string= "1.0.0" (%assoc-string "version" info)))
    (is (null (assoc "description" info :test #'string=)))
    (is (not (null (%assoc-string "paths" spec))))))

(test document-includes-description-when-supplied
  "InfoObject carries description when build is called with :DESCRIPTION."
  (let* ((spec (build-openapi-spec :title "T" :version "1.0.0"
                                   :description "Hello"
                                   :only-paths '("/openapi-fixtures/things")))
         (info (%assoc-string "info" spec)))
    (is (string= "Hello" (%assoc-string "description" info)))))

(test only-paths-restricts-emission
  ":ONLY-PATHS scopes the emitted PathsObject to the given OpenAPI paths."
  (let* ((spec (build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/things/{id}")))
         (paths (%assoc-string "paths" spec))
         (path-keys (mapcar #'car paths)))
    (is (= 1 (length path-keys)))
    (is (string= "/openapi-fixtures/things/{id}" (first path-keys)))))

;;; ============================================================================
;;; JSON SERIALISATION
;;; ============================================================================

(test emit-openapi-json-round-trips-via-jzon
  "EMIT-OPENAPI-JSON returns a string that COM.INUOE.JZON:PARSE accepts;
   the parsed shape exposes the same top-level fields as the alist build."
  (let* ((json (emit-openapi-json
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/things/{id}")))
         (parsed (com.inuoe.jzon:parse json)))
    (is (stringp json))
    (is (hash-table-p parsed))
    (is (string= "3.1.0" (gethash "openapi" parsed)))
    (let ((info (gethash "info" parsed))
          (paths (gethash "paths" parsed)))
      (is (string= "T" (gethash "title" info)))
      (is (string= "1.0.0" (gethash "version" info)))
      (is (not (null (gethash "/openapi-fixtures/things/{id}" paths)))))))

(test emit-openapi-json-preserves-camel-case-operation-id
  "operationId is camelCase per OpenAPI convention; the encoder must not
   downcase it (string keys preserve case in the hash-table key path)."
  (let* ((json (emit-openapi-json
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/things/{id}")))
         (parsed (com.inuoe.jzon:parse json))
         (paths (gethash "paths" parsed))
         (path (gethash "/openapi-fixtures/things/{id}" paths))
         (op (gethash "get" path)))
    (is (gethash "operationId" op))
    (is (null (gethash "operationid" op)))))

;;; ============================================================================
;;; CONFORMANCE — VALIDATE EMITTED SPEC AGAINST OPENAPI 3.1 SCHEMA
;;; ============================================================================

(defun %validation-errors (schema-source emitted-json)
  "Parse SCHEMA-SOURCE as a JSON Schema, parse EMITTED-JSON as a JSON
   value, validate the value against the schema, and return either NIL
   (validates) or a list of (json-pointer error-message) for the
   surfaced errors."
  (let ((schema (lol-web/jschema:parse schema-source))
        (value (com.inuoe.jzon:parse emitted-json)))
    (handler-case (progn (lol-web/jschema:validate schema value) nil)
      (lol-web/jschema:invalid-json (c)
        (loop for e in (lol-web/jschema:invalid-json-errors c)
              collect (list (lol-web/jschema:invalid-json-value-json-pointer e)
                            (lol-web/jschema:invalid-json-value-error-message e)))))))

(test emitted-spec-validates-against-openapi-3.1-schema
  "EMIT-OPENAPI-JSON output for the fixture handlers (path/query/header
   parameters, :json-body, :body, multi-method path) validates against
   the upstream OpenAPI 3.1 base schema parsed via lol-web/jschema:parse.

   Skipped when *openapi-3.1-schema-path* is NIL — the buildLisp wrapper
   binds this to a Nix-store path; under interactive REPL load it stays
   unset."
  (cond
    ((null *openapi-3.1-schema-path*)
     (is (eq t t)))
    (t
     (let* ((schema-source (alexandria:read-file-into-string
                            *openapi-3.1-schema-path*))
            (emitted (emit-openapi-json
                      :title "Fixture API" :version "0.1.0"
                      :only-paths '("/openapi-fixtures/things/{id}"
                                    "/openapi-fixtures/things"
                                    "/openapi-fixtures/forms")))
            (errors (%validation-errors schema-source emitted)))
       (is (null errors)
           "Emitted spec did not validate against OpenAPI 3.1 schema.~%~
            Errors: ~{~%  ~A~}"
           errors)))))

(test malformed-spec-rejected-by-openapi-3.1-schema
  "A deliberately-malformed OpenAPI document — info missing the
   required `version` field — fails validation against the upstream
   OpenAPI 3.1 schema. Confirms the schema-validation gate actually
   distinguishes valid from invalid emissions; without this companion
   test, an always-pass validator would silently mask real bugs.

   Skipped when *openapi-3.1-schema-path* is NIL."
  (cond
    ((null *openapi-3.1-schema-path*)
     (is (eq t t)))
    (t
     (let* ((schema-source (alexandria:read-file-into-string
                            *openapi-3.1-schema-path*))
            (malformed (encode-json-string
                        (list (cons "openapi" "3.1.0")
                              (cons "info" (list (cons "title" "Bad")))
                              (cons "paths" '()))))
            (errors (%validation-errors schema-source malformed)))
       (is (not (null errors))
           "Malformed spec missing info.version was unexpectedly accepted")))))

;;; ============================================================================
;;; FIXTURES — :auth + :internal annotations
;;; ============================================================================

(defhandler %fixture-secured-endpoint "/openapi-fixtures/secured"
    (:method :get :content-type "application/json" :auth t)
    ()
  "{}")

(defhandler %fixture-explicit-scheme-endpoint "/openapi-fixtures/explicit-scheme"
    (:method :get :content-type "application/json" :auth ("bearerAuth"))
    ()
  "{}")

(defhandler %fixture-internal-endpoint "/openapi-fixtures/internal"
    (:method :get :content-type "application/json" :internal t)
    ()
  "{}")

;;; ============================================================================
;;; OPERATION OBJECT — SECURITY FROM HANDLER METADATA
;;; ============================================================================

(test regression-openapi-emits-security-from-handler-metadata
  ":AUTH T on a defhandler propagates through *handler-metadata* and the
   emitter attaches a `security` requirement listing every scheme name
   declared on the document via :SECURITY-SCHEMES."
  (let* ((spec (build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/secured")
                :security-schemes
                '(("cookieAuth"
                   ("type" . "apiKey")
                   ("in" . "cookie")
                   ("name" . "session")))))
         (paths (cdr (assoc "paths" spec :test #'string=)))
         (path (cdr (assoc "/openapi-fixtures/secured" paths :test #'string=)))
         (op (cdr (assoc "get" path :test #'string=)))
         (security (cdr (assoc "security" op :test #'string=))))
    (is (not (null security)) "secured op must emit `security`")
    (is (= 1 (length security)))
    (is (equal '((("cookieAuth"))) security)
        "security should list cookieAuth with empty scope list")))

(test regression-openapi-emits-explicit-scheme-list-from-auth
  ":AUTH (\"bearerAuth\") on a defhandler emits exactly that requirement —
   the list value bypasses the default-all-schemes expansion."
  (let* ((spec (build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/explicit-scheme")
                :security-schemes
                '(("cookieAuth" ("type" . "apiKey"))
                  ("bearerAuth" ("type" . "http") ("scheme" . "bearer")))))
         (paths (cdr (assoc "paths" spec :test #'string=)))
         (path (cdr (assoc "/openapi-fixtures/explicit-scheme" paths
                            :test #'string=)))
         (op (cdr (assoc "get" path :test #'string=)))
         (security (cdr (assoc "security" op :test #'string=))))
    (is (equal '((("bearerAuth"))) security)
        "explicit-scheme op must reference bearerAuth only, not cookieAuth")))

(test regression-openapi-emits-components-security-schemes
  "When :SECURITY-SCHEMES is supplied, the document grows a
   components.securitySchemes mapping populated from that alist."
  (let* ((spec (build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/secured")
                :security-schemes
                '(("cookieAuth"
                   ("type" . "apiKey")
                   ("in" . "cookie")
                   ("name" . "session")))))
         (components (cdr (assoc "components" spec :test #'string=)))
         (schemes (cdr (assoc "securitySchemes" components :test #'string=))))
    (is (not (null schemes)))
    (is (not (null (assoc "cookieAuth" schemes :test #'string=)))
        "cookieAuth scheme must appear under components.securitySchemes")))

(test regression-openapi-omits-components-when-no-security-schemes
  "Default emission (no :SECURITY-SCHEMES) emits no `components` and no
   per-op `security`, preserving the v0 surface for callers that have
   not opted in."
  (let* ((spec (build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/secured")))
         (paths (cdr (assoc "paths" spec :test #'string=)))
         (path (cdr (assoc "/openapi-fixtures/secured" paths :test #'string=)))
         (op (cdr (assoc "get" path :test #'string=))))
    (is (null (assoc "components" spec :test #'string=))
        "components must be absent when no security-schemes supplied")
    (is (null (assoc "security" op :test #'string=))
        "operation security must be absent when no schemes to reference")))

;;; ============================================================================
;;; PATHS — :INTERNAL EXCLUSION
;;; ============================================================================

(test regression-openapi-excludes-internal-routes
  "A handler whose :OPTIONS carries :INTERNAL T drops out of the emitted
   paths object even when explicitly listed in :ONLY-PATHS. The filter
   is unconditional — :internal is a publish-time decision, not a
   request-time gate."
  (let* ((spec (build-openapi-spec
                :title "T" :version "1.0.0"
                :only-paths '("/openapi-fixtures/internal"
                              "/openapi-fixtures/secured")))
         (paths (cdr (assoc "paths" spec :test #'string=))))
    (is (null (assoc "/openapi-fixtures/internal" paths :test #'string=))
        "internal route must NOT appear in emitted paths")
    (is (not (null (assoc "/openapi-fixtures/secured" paths :test #'string=)))
        "non-internal route must still appear when listed")))
