;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/EXTRACTORS; Base: 10 -*-
;;;; The DEFHANDLER macro. Parses the extractor lambda list, expands to a
;;;; metadata-registration form plus a delegated DEFROUTE call whose body
;;;; let-binds the resolved extractors before invoking the user body.

(in-package :lol-web/extractors)

;;; ============================================================================
;;; PARSER
;;; ============================================================================

(defun %parse-extractor-spec (form)
  "Parse one extractor spec from a defhandler lambda list.
   Returns a plist with keys :name :kind :type :required :default-form
   :source :resolver. The default-form is the *unevaluated* user form;
   the macro lifts it into a thunk at expansion time."
  (unless (and (listp form) (>= (length form) 2))
    (error "defhandler extractor spec must be (NAME KIND . OPTIONS), got: ~S" form))
  (destructuring-bind (name kind &rest options)
      form
    (unless (and (symbolp name) name)
      (error "defhandler extractor name must be a non-NIL symbol, got: ~S" name))
    (unless (keywordp kind)
      (error "defhandler extractor kind must be a keyword, got: ~S" kind))
    (let ((parsed (list :name name :kind kind
                        :type t :required t
                        :default-form nil
                        :source nil :resolver nil)))
      (loop for (opt val . rest) on options by #'cddr
            do (case opt
                 (:type     (setf (getf parsed :type) val))
                 (:required (setf (getf parsed :required) val))
                 (:default  (setf (getf parsed :default-form) val
                                  (getf parsed :required) nil))
                 (:source   (setf (getf parsed :source) val))
                 (:resolver (setf (getf parsed :resolver) val))
                 (t (error "Unknown defhandler extractor option ~S in spec ~S"
                           opt form)))
            unless rest do (loop-finish))
      parsed)))

(defun %spec-construction-form (parsed)
  "Build a MAKE-EXTRACTOR-SPEC call form from a parsed plist. Lifts the
   user's default form into a thunk so it is evaluated at request time."
  `(make-extractor-spec
    :name      ',(getf parsed :name)
    :kind      ,(getf parsed :kind)
    :type      ',(getf parsed :type)
    :required-p ,(getf parsed :required)
    ,@(when (getf parsed :default-form)
        `(:default (lambda () ,(getf parsed :default-form))))
    ,@(when (getf parsed :source)
        `(:source-string ,(getf parsed :source)))
    ,@(when (getf parsed :resolver)
        `(:custom-resolver ',(getf parsed :resolver)))))

(defun %let-binding-form (parsed spec-var)
  "Build one (NAME (resolve-extractor ... SPEC-VAR ...)) form for the
   handler body's LET*. SPEC-VAR is a gensym already bound to the spec
   instance so the LET* can call RESOLVE-EXTRACTOR with the same instance
   that landed in *HANDLER-METADATA*."
  `(,(getf parsed :name)
    (resolve-extractor ,(getf parsed :kind)
                       ,spec-var
                       lol-web/server:*env*
                       lol-web/server:*path-params*)))

;;; ============================================================================
;;; DEFHANDLER
;;; ============================================================================

(defmacro! defhandler (name o!path defroute-options extractor-specs &body body)
  "Define a handler that declares its inputs in its lambda list. Each entry
   in EXTRACTOR-SPECS is (PARAM-NAME KIND &key TYPE REQUIRED DEFAULT SOURCE
   RESOLVER); the framework resolves each declared input from the request
   before calling the body, with the resolved values bound to the names in
   the body's lexical scope.

   Options (forwarded verbatim to LOL-WEB/SERVER:DEFROUTE):
     :METHOD       — :GET / :POST / ... (default :GET in DEFROUTE)
     :CONTENT-TYPE — response content type (default \"text/html\" in DEFROUTE)
     :SECURE       — wrap body in WITH-SECURITY (default T in DEFROUTE)

   Extractor option semantics:
     :TYPE      — target Lisp type (T = no coercion; INTEGER, BOOLEAN, KEYWORD,
                  SYMBOL are built in). Default T.
     :REQUIRED  — when NIL, missing input falls through to :DEFAULT or NIL.
                  Default T (omitting :DEFAULT makes the extractor required).
                  Supplying :DEFAULT implies :REQUIRED NIL.
     :DEFAULT   — form evaluated at request time when the source is missing
                  and :REQUIRED is NIL. Not coerced.
     :SOURCE    — string overriding the lookup key. Required for header names
                  that differ from the symbol name (e.g. :SOURCE \"X-API-Key\").
     :RESOLVER  — symbol naming a function (SPEC ENV PATH-PARAMS) → value.
                  Bypasses CLOS dispatch on KIND when present.

   Example:
     (defhandler search-posts \"/posts\"
         (:method :get :content-type \"application/json\")
         ((q       :query :type string)
          (limit   :query :type integer :required nil :default 20)
          (api-key :header :source \"X-API-Key\" :type string))
       (encode-json-string (search-posts :q q :limit limit :auth api-key)))

   Once-only safety:
     PATH and the extracted :METHOD form are each evaluated exactly once at
     registration time, even though both appear at multiple sites in the
     expansion (metadata key, metadata plist, DEFROUTE call, return values).

   Expansion:
     1. Registers metadata in *HANDLER-METADATA* under (cons METHOD PATH).
     2. Delegates to LOL-WEB/SERVER:DEFROUTE for the per-request handler.
        The DEFROUTE body is a LET* binding each extractor's name to its
        resolved value, then the user body. Coercion / missing-input errors
        signal extractor conditions which WITH-ERROR-HANDLING (already inside
        DEFROUTE's expansion) translates to the appropriate HTTP status."
  (let* ((parsed-specs (mapcar #'%parse-extractor-spec extractor-specs))
         (spec-vars (loop repeat (length parsed-specs)
                          collect (gensym "EXTRACTOR-SPEC-")))
         (spec-construction-forms (mapcar #'%spec-construction-form parsed-specs))
         (method-form (getf defroute-options :method :get))
         ;; Pass-through options excluding :method (which we route through
         ;; the once-only g!method binding to keep evaluation count = 1).
         ;; :auth and :internal are metadata-only annotations consumed by the
         ;; OpenAPI emitter — defroute does not accept them, so filter them out
         ;; of the forwarded list while keeping them in the metadata :options.
         (other-options (loop for (k v . rest) on defroute-options by #'cddr
                              unless (eq k :method)
                              collect k and collect v))
         (defroute-options-filtered (loop for (k v . rest) on other-options by #'cddr
                                          unless (member k '(:auth :internal))
                                          collect k and collect v)))
    `(let ((,g!method ,method-form))
       (bordeaux-threads:with-recursive-lock-held (*handler-metadata-lock*)
         (setf (gethash (cons ,g!method ,g!path) *handler-metadata*)
               (list :name ',name
                     :method ,g!method
                     :path ,g!path
                     :extractors (list ,@spec-construction-forms)
                     ;; Options are stored as data (the OpenAPI emitter reads
                     ;; :auth / :internal as literals). Quote the static tail so
                     ;; values like (\"bearerAuth\") aren't evaluated as function
                     ;; calls; :method threads through the once-only g!method
                     ;; binding to preserve its single-evaluation contract.
                     :options (list* :method ,g!method ',other-options))))
       (lol-web/server:defroute ,g!path (:method ,g!method ,@defroute-options-filtered)
         (let* (,@(loop for parsed in parsed-specs
                        for var in spec-vars
                        for ctor in spec-construction-forms
                        collect `(,var ,ctor)
                        collect (%let-binding-form parsed var)))
           ,@body))
       (values ,g!path ,g!method ',name))))
