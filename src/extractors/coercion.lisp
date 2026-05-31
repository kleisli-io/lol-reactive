;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/EXTRACTORS; Base: 10 -*-
;;;; String→typed-value coercion. Failures signal EXTRACTOR-COERCION-ERROR
;;;; (status 400) so WITH-ERROR-HANDLING translates them to a Bad Request
;;;; response with a useful message naming the failing extractor.

(in-package :lol-web/extractors)

(defun %coerce-value (raw target-type spec)
  "Coerce RAW (a string) to TARGET-TYPE. Returns the coerced value or
   signals EXTRACTOR-COERCION-ERROR. SPEC is passed through to the
   condition so the response message can name the extractor."
  (case target-type
    ((t) raw)
    ((string) raw)
    ((integer) (%coerce-integer raw spec))
    ((boolean) (%coerce-boolean raw spec))
    ((keyword) (%coerce-keyword raw spec))
    ((symbol) (%coerce-symbol raw spec))
    (t
     (error 'extractor-coercion-error
            :extractor-name (extractor-spec-name spec)
            :extractor-kind (extractor-spec-kind spec)
            :raw-value raw
            :target-type target-type))))

(defun %coerce-integer (raw spec)
  "Parse RAW as an integer. Accepts already-INTEGER values (e.g. from
   :json-body with a numeric field) and returns them unchanged. Strings
   are parsed via PARSE-INTEGER with surrounding-whitespace tolerance.
   Anything else signals EXTRACTOR-COERCION-ERROR."
  (cond
    ((integerp raw) raw)
    ((stringp raw)
     (handler-case
         (let ((trimmed (string-trim '(#\Space #\Tab) raw)))
           (when (zerop (length trimmed))
             (%signal-coercion raw 'integer spec))
           (parse-integer trimmed))
       (parse-error () (%signal-coercion raw 'integer spec))
       (type-error () (%signal-coercion raw 'integer spec))))
    (t (%signal-coercion raw 'integer spec))))

(defun %coerce-boolean (raw spec)
  "Coerce RAW to a boolean. Accepts CL booleans (T / NIL) directly.
   Strings are parsed for true/false/1/0/yes/no/on/off case-insensitively.
   Anything else signals EXTRACTOR-COERCION-ERROR."
  (cond
    ((eq raw t) t)
    ((null raw) nil)
    ((stringp raw)
     (let ((normalized (string-downcase (string-trim '(#\Space #\Tab) raw))))
       (cond
         ((member normalized '("true" "1" "yes" "on" "t") :test #'string=) t)
         ((member normalized '("false" "0" "no" "off" "nil" "") :test #'string=) nil)
         (t (%signal-coercion raw 'boolean spec)))))
    (t (%signal-coercion raw 'boolean spec))))

(defun %coerce-keyword (raw spec)
  "Coerce RAW to a keyword via SAFE-COERCE-KEYWORD (FIND-SYMBOL against
   :keyword). RAW must already name an interned keyword; otherwise the
   request fails as EXTRACTOR-COERCION-ERROR (400). Empty strings fail —
   RESOLVE-EXTRACTOR's required-p check catches missing input; an empty
   string reaching coercion is not a valid keyword."
  (cond
    ((keywordp raw) raw)
    ((stringp raw)
     (let ((trimmed (string-trim '(#\Space #\Tab) raw)))
       (when (zerop (length trimmed))
         (%signal-coercion raw 'keyword spec))
       (or (safe-coerce-keyword trimmed)
           (%signal-coercion raw 'keyword spec))))
    (t (%signal-coercion raw 'keyword spec))))

(defun %coerce-symbol (raw spec)
  "Coerce RAW to a symbol. A symbol RAW passes through unchanged; a string
   RAW resolves via SAFE-COERCE-KEYWORD against the keyword package, so the
   result is always a KEYWORD. Keywords satisfy SYMBOLP, so the SYMBOL type
   contract holds, but a string-typed :symbol parameter never yields a
   non-keyword symbol — callers needing that distinction must inspect the
   value, not assume the package. Resolution is FIND-SYMBOL-bounded (never
   INTERN), so hostile input cannot grow the symbol pool; the SYMBOL vs
   KEYWORD label is an OpenAPI-emission distinction, not a runtime one."
  (cond
    ((symbolp raw) raw)
    ((stringp raw)
     (let ((trimmed (string-trim '(#\Space #\Tab) raw)))
       (when (zerop (length trimmed))
         (%signal-coercion raw 'symbol spec))
       (or (safe-coerce-keyword trimmed)
           (%signal-coercion raw 'symbol spec))))
    (t (%signal-coercion raw 'symbol spec))))

(defun %signal-coercion (raw target-type spec)
  ;; No :body — the generic default keeps RAW out of the wire response; the
  ;; condition REPORT formats RAW + extractor identity for server logs.
  (error 'extractor-coercion-error
         :extractor-name (extractor-spec-name spec)
         :extractor-kind (extractor-spec-kind spec)
         :raw-value raw
         :target-type target-type))
