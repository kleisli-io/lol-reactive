;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/EXTRACTORS; Base: 10 -*-
;;;; Extractor protocol surface: extractor-spec struct, condition hierarchy,
;;;; resolve-extractor generic, *handler-metadata* registry.

(in-package :lol-web/extractors)

;;; ============================================================================
;;; EXTRACTOR-SPEC
;;; ============================================================================

(defstruct extractor-spec
  "One extractor declaration from a defhandler lambda list.

   NAME — symbol bound in the handler body. When SOURCE-STRING is NIL, drives
     the default lookup key as (string-downcase (symbol-name name)).
   KIND — keyword used as the eql-method specializer for RESOLVE-EXTRACTOR.
     Built-ins: :path :query :body :header :json-body. User-extensible.
   TYPE — symbol naming the target Lisp type for coercion. T means no coercion
     (raw string for string-sourced extractors, raw value for JSON-sourced).
     Built-in coercions: integer, boolean, keyword, symbol.
   REQUIRED-P — when T, missing input signals MISSING-EXTRACTOR-INPUT (422).
     When NIL and DEFAULT is NIL, missing input resolves to NIL.
   DEFAULT — function of zero args, or NIL. When non-NIL and the source is
     missing, the function is called and its value used (no coercion). The
     macro lifts the user-supplied default form into a thunk so the form is
     evaluated at request time, not at registration time.
   SOURCE-STRING — string overriding the default lookup key. Required for
     header names that don't match the symbol name (e.g. \"X-API-Key\" for
     a parameter bound to API-KEY).
   CUSTOM-RESOLVER — symbol naming a function that takes (SPEC ENV PATH-PARAMS)
     and returns the resolved value, or NIL. When non-NIL, the resolver
     function is invoked instead of dispatching on KIND. Lets one-off extractors
     bypass the CLOS protocol when a method-add would be heavyweight."
  (name nil)
  (kind nil)
  (type t)
  (required-p t)
  (default nil :type (or null function))
  (source-string nil :type (or null string))
  (custom-resolver nil))

;;; ============================================================================
;;; CONDITIONS
;;; ============================================================================

(define-condition extractor-error (lol-web/server:http-error)
  ((extractor-name
    :initarg :extractor-name
    :reader extractor-error-name
    :initform nil)
   (extractor-kind
    :initarg :extractor-kind
    :reader extractor-error-kind
    :initform nil))
  (:documentation
   "Parent of all extractor-related errors. Inherits HTTP-ERROR so
    LOL-WEB/SERVER:WITH-ERROR-HANDLING translates it via HTTP-ERROR-STATUS.
    EXTRACTOR-ERROR-NAME / -KIND identify the failing extractor for callers
    that want to log or branch on which spec failed."))

(define-condition missing-extractor-input (extractor-error)
  ()
  ;; BODY is the wire message returned to the (unauthenticated) client; it
  ;; stays generic so the 4xx response neither echoes input nor names the
  ;; failing extractor. The REPORT carries the diagnostic for server logs.
  (:default-initargs :status 422
                     :body "A required parameter was missing.")
  (:report (lambda (c s)
             (format s "Missing required ~(~A~) extractor ~S."
                     (extractor-error-kind c)
                     (extractor-error-name c))))
  (:documentation
   "Signalled when REQUIRED-P is T and the extractor's source returned NIL.
    Translates to a 422 response."))

(define-condition extractor-coercion-error (extractor-error)
  ((raw-value
    :initarg :raw-value
    :initform nil
    :reader extractor-coercion-error-raw-value)
   (target-type
    :initarg :target-type
    :initform nil
    :reader extractor-coercion-error-target-type))
  ;; BODY is generic on purpose: reflecting RAW-VALUE into a text/plain 400
  ;; turns the endpoint into an input-echo / extractor-name+type oracle for
  ;; an unauthenticated client. The full diagnostic lives in REPORT (logs).
  (:default-initargs :status 400
                     :body "A parameter value could not be processed.")
  (:report (lambda (c s)
             (format s "Cannot coerce ~S to ~A for ~A extractor ~S."
                     (extractor-coercion-error-raw-value c)
                     (extractor-coercion-error-target-type c)
                     (extractor-error-kind c)
                     (extractor-error-name c))))
  (:documentation
   "Signalled when the raw extracted value cannot be coerced to TARGET-TYPE.
    Translates to a 400 response."))

(define-condition extractor-not-registered (extractor-error)
  ()
  (:default-initargs :status 500)
  (:documentation
   "Signalled by the default RESOLVE-EXTRACTOR method when no eql-method
    matches the spec's KIND. Translates to a 500 response if it surfaces at
    request time. The pre-server-start sentinel hook (added in 7.3) walks
    *HANDLER-METADATA* and signals this at startup so the server refuses
    to come up rather than serving requests with broken extractor wiring."))

;;; ============================================================================
;;; RESOLVE-EXTRACTOR PROTOCOL
;;; ============================================================================

(defgeneric resolve-extractor (kind spec env path-params)
  (:documentation
   "Return the value bound to SPEC's NAME in the handler body, or signal
    a typed condition if the input is missing/uncoerceable.

    KIND is the keyword from SPEC's :KIND slot (the eql-method specializer).
    Built-in methods specialize on (eql :path) (eql :query) (eql :body)
    (eql :header) (eql :json-body). Users extend by adding methods.

    Errors:
    - missing required input → MISSING-EXTRACTOR-INPUT (422)
    - coercion failure       → EXTRACTOR-COERCION-ERROR (400)

    The default method signals EXTRACTOR-NOT-REGISTERED (500). This is the
    backstop — by construction, the sentinel pre-server-start hook ensures
    no defhandler-registered KIND lacks a method before requests can hit."))

(defmethod resolve-extractor ((kind t) spec env path-params)
  (declare (ignore env path-params))
  (error 'extractor-not-registered
         :extractor-name (extractor-spec-name spec)
         :extractor-kind kind
         :body (format nil "No RESOLVE-EXTRACTOR method registered for kind ~S (extractor ~S)."
                       kind (extractor-spec-name spec))))

;;; ============================================================================
;;; HANDLER METADATA REGISTRY
;;; ============================================================================

(defvar *handler-metadata* (make-hash-table :test 'equal)
  "Handler metadata registry mapping (cons METHOD PATH) to a metadata plist.
   Same key shape as LOL-WEB/SERVER:*ROUTES*. Populated by DEFHANDLER at
   registration time; consumed by the OpenAPI emitter at server start.

   Plist keys:
     :NAME       — symbol from the DEFHANDLER form
     :METHOD     — :GET / :POST / :PUT / :DELETE / ...
     :PATH       — string with :param segments for path params
     :EXTRACTORS — list of EXTRACTOR-SPEC, ordered as in the lambda list
     :OPTIONS    — plist passing DEFROUTE options through (:CONTENT-TYPE, :SECURE)")

(defvar *handler-metadata-lock*
  (bordeaux-threads:make-recursive-lock "lol-web/extractors handler-metadata")
  "Guards *HANDLER-METADATA*. Recursive so a DEFHANDLER expansion can chain
   multiple writes without releasing.")

(defun handler-metadata (method path)
  "Read the metadata plist registered for METHOD/PATH. Returns NIL if no
   DEFHANDLER registered the route. Safe under concurrent registration."
  (bordeaux-threads:with-recursive-lock-held (*handler-metadata-lock*)
    (gethash (cons method path) *handler-metadata*)))
