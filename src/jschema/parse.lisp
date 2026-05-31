;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/JSCHEMA; Base: 10 -*-
;;;; PARSE — turn a JSON Schema document (string, stream, or pre-parsed value)
;;;; into a JSON-SCHEMA struct. Builds the per-document self-registry as it
;;;; descends; resolves $id against the document's base URI; checks each
;;;; keyword's shape via the keyword-table dispatch in keywords.lisp.

(in-package :lol-web/jschema)

;;; ============================================================================
;;; KEYWORD TABLE — populated by keywords.lisp
;;; ============================================================================

(defvar *keyword-parsers* (make-hash-table :test 'equal)
  "Maps a keyword name (string, e.g. \"type\") to a parse function of one
   argument (the raw value from the schema's hash-table). The parse function
   either returns a parsed value, or signals INVALID-SCHEMA.")

(defvar *keyword-checkers* (make-hash-table :test 'equal)
  "Maps a keyword name to a check function of three arguments — the keyword's
   parsed value, the value being validated, and the validation context plist
   bound during VALIDATE. The check function pushes INVALID-JSON-VALUE
   conditions onto the context's :ERRORS slot for each failure.")

(defvar *child-schema-keywords* (make-hash-table :test 'equal)
  "Maps a keyword name to a function that, given the keyword's parsed form,
   returns the list of (json-pointer-suffix . child-schema) pairs the parser
   must register in the self-registry. Populated by keywords.lisp for
   composite keywords like properties / items / allOf.")

(defun register-keyword (name &key parser checker child-schemas)
  "Install handlers for one JSON Schema keyword."
  (when parser (setf (gethash name *keyword-parsers*) parser))
  (when checker (setf (gethash name *keyword-checkers*) checker))
  (when child-schemas
    (setf (gethash name *child-schema-keywords*) child-schemas)))

;;; ============================================================================
;;; INTERNAL PARSE STATE
;;; ============================================================================

(defvar *self-registry* nil
  "Bound during PARSE to the root schema's self-registry hash-table. Each
   descended-into schema is registered under its JSON Pointer.")

(defparameter *max-schema-depth* 256
  "Hard cap on nested schema construction during PARSE. A schema document
   whose subschema nesting exceeds this signals INVALID-SCHEMA rather than
   recursing far enough to blow the parser's call stack. Rebind via let
   in callers ingesting legitimately deeper documents.")

(defvar *parse-schema-depth* 0
  "Bound during PARSE; incremented by %make-schema on every recursive
   descent into a child schema, checked against *max-schema-depth*.")

(defun %tracked-pointer-push (pointer-suffix)
  "Append POINTER-SUFFIX (a string already prefixed with '/') to the current
   tracked pointer. Returns the new full pointer."
  (concatenate 'string *parse-json-pointer* pointer-suffix))

(defmacro with-pointer-extension ((suffix) &body body)
  "Run BODY with *PARSE-JSON-POINTER* extended by SUFFIX (no trailing slash;
   SUFFIX must already include its leading slash, e.g. \"/properties/foo\")."
  `(let ((*parse-json-pointer* (%tracked-pointer-push ,suffix)))
     ,@body))

;;; ============================================================================
;;; INPUT NORMALIZATION
;;; ============================================================================

(defparameter *schema-json-max-depth* 128
  "Cap on object/array nesting depth when decoding a JSON Schema *document*
   with jzon. The schema-construction guard *max-schema-depth* only fires
   after jzon has already materialised the whole value, so the parse needs
   its own bound to refuse a pathologically deep document up front. Pins
   jzon's implicit default explicitly so the limit is declared at the parse
   site and tunable via let; symmetric with the request-body path's
   *json-body-max-depth*.")

(defparameter *schema-json-max-string-length* 1048576
  "Cap on individual JSON string length (characters) when decoding a JSON
   Schema document with jzon. Declared explicitly at the parse site,
   symmetric with the request-body path's *json-body-max-string-length*.")

(defun %parse-json (input allow-comments allow-trailing-comma)
  "Decode INPUT with jzon, bounding nesting depth and string length so a
   hostile schema document is refused before its whole tree is built.
   Returns the parsed JSON value."
  (handler-case
      (com.inuoe.jzon:parse input
                            :allow-comments allow-comments
                            :allow-trailing-comma allow-trailing-comma
                            :max-depth *schema-json-max-depth*
                            :max-string-length *schema-json-max-string-length*)
    (com.inuoe.jzon:json-error (e)
      (error 'unparsable-json
             :error-message (format nil "Cannot decode JSON Schema: ~A" e)
             :unparsable-json-error e))))

(defun %schema-like-p (input)
  "True if INPUT is a value already parsed by jzon (hash-table, boolean,
   or T as the JSON true sentinel handled by alternate parsers)."
  (or (eq input t)
      (eq input nil)  ; we treat NIL as the JSON 'false' shape conservatively
      (hash-table-p input)))

;;; ============================================================================
;;; ENTRYPOINT
;;; ============================================================================

(defun parse (input &key allow-comments allow-trailing-comma)
  "Parse INPUT into a JSON-SCHEMA. INPUT may be a string, stream, or value
   previously produced by COM.INUOE.JZON:PARSE. Signals INVALID-SCHEMA on
   shape errors, UNPARSABLE-JSON on undecodable input, NOT-IMPLEMENTED for
   features outside the v0.1.0 vocabulary subset."
  (let ((parsed (etypecase input
                  ((or string stream)
                   (%parse-json input allow-comments allow-trailing-comma))
                  (hash-table input)
                  (boolean input))))
    (let ((*parse-base-uri* nil)
          (*parse-json-pointer* "")
          (*self-registry* (make-hash-table :test 'equal)))
      (let ((schema (%make-schema parsed t)))
        (when (and (json-schema-id schema)
                   (json-schema-base-uri schema))
          (register-schema (puri:render-uri
                            (json-schema-base-uri schema) nil)
                           schema))
        schema))))

;;; ============================================================================
;;; CORE: %MAKE-SCHEMA
;;; ============================================================================
;;; Recursive descent. ROOT-P is T only for the top-level call from PARSE;
;;; child-schema construction recurses with ROOT-P NIL and an extended
;;; *PARSE-JSON-POINTER*.

(defun %make-schema (json root-p)
  "Build a JSON-SCHEMA from a jzon-parsed JSON value. Registers the result in
   *SELF-REGISTRY* under the current *PARSE-JSON-POINTER*. Recursion bounded
   by *max-schema-depth*."
  (when (>= *parse-schema-depth* *max-schema-depth*)
    (raise-invalid-schema
     "Schema nesting exceeds *max-schema-depth* (~D) — refusing to parse deeper"
     *max-schema-depth*))
  (let* ((*parse-schema-depth* (1+ *parse-schema-depth*))
         (schema
          (cond
            ((eq json t)
             (make-json-schema :bool :true))
            ((eq json nil)
             (make-json-schema :bool :false))
            ((not (hash-table-p json))
             (raise-invalid-schema
              "JSON Schema must be a JSON boolean or object, got ~S"
              (type-of json)))
            (t (%make-object-schema json root-p)))))
    (setf (gethash *parse-json-pointer* *self-registry*) schema)
    (when (and (json-schema-anchor schema)
               (not (zerop (length (json-schema-anchor schema)))))
      (setf (gethash (json-schema-anchor schema) *self-registry*) schema))
    (when (and (json-schema-dynamic-anchor schema)
               (not (zerop (length (json-schema-dynamic-anchor schema)))))
      (setf (gethash (concatenate 'string "$dyn:"
                                  (json-schema-dynamic-anchor schema))
                     *self-registry*)
            schema))
    schema))

(defun %make-object-schema (json root-p)
  "Build a non-boolean JSON-SCHEMA from a hash-table JSON object."
  (let* ((id-raw (gethash "$id" json))
         (schema-raw (gethash "$schema" json))
         (anchor-raw (gethash "$anchor" json))
         (dyn-anchor-raw (gethash "$dynamicAnchor" json))
         (defs-raw (gethash "$defs" json))
         (this-id-uri (when id-raw
                        (handler-case (puri:parse-uri id-raw)
                          (error () (raise-invalid-schema
                                     "$id is not a valid URI: ~S" id-raw)))))
         ;; Base URI promotes only at document root from $id; child $id values
         ;; resolve relative to the inherited *PARSE-BASE-URI*.
         (*parse-base-uri*
           (cond
             ((and root-p this-id-uri) this-id-uri)
             (t *parse-base-uri*))))
    ;; Draft 2020-12 permits $defs at any schema location (nested or root) and
    ;; $schema in $id-rooted subdocuments — both checks deliberately omitted.
    (let ((schema (make-json-schema
                   :id id-raw
                   :base-uri *parse-base-uri*
                   :schema-uri schema-raw
                   :anchor anchor-raw
                   :dynamic-anchor dyn-anchor-raw
                   :self-registry (when root-p *self-registry*))))
      (setf (json-schema-keywords schema)
            (%parse-keywords json schema))
      (when defs-raw
        (setf (json-schema-defs schema)
              (%parse-defs defs-raw)))
      schema)))

(defun %parse-keywords (json schema)
  "Walk the schema object's keys, dispatching each to its registered parser.
   Unknown keywords are silently retained as raw values — JSON Schema 2020-12
   permits unknown keywords as annotations."
  (declare (ignore schema))
  (let ((acc '()))
    (loop for key being the hash-keys of json using (hash-value val)
          do (cond
               ((or (string= key "$id")
                    (string= key "$schema")
                    (string= key "$anchor")
                    (string= key "$dynamicAnchor")
                    (string= key "$defs")
                    (string= key "$comment"))
                ;; Already absorbed into the schema struct, or annotation-only.
                nil)
               ((gethash key *keyword-parsers*)
                (let ((parsed
                        (with-pointer-extension
                            ((concatenate 'string "/"
                                          (%escape-pointer-segment key)))
                          (funcall (gethash key *keyword-parsers*) val))))
                  (push (cons key parsed) acc)))
               (t
                ;; Unknown keyword — keep raw for annotation completeness.
                (push (cons key val) acc))))
    (nreverse acc)))

(defun %parse-defs (defs-raw)
  "Parse a $defs hash-table. Each entry is itself a JSON Schema."
  (unless (hash-table-p defs-raw)
    (raise-invalid-schema "$defs must be a JSON object"))
  (let ((acc '()))
    (loop for k being the hash-keys of defs-raw using (hash-value v)
          do (with-pointer-extension
                 ((concatenate 'string "/$defs/" (%escape-pointer-segment k)))
               (push (cons k (%make-schema v nil)) acc)))
    (nreverse acc)))

(defun %escape-pointer-segment (s)
  "RFC 6901 §4 — escape '~' as '~0' and '/' as '~1'."
  (with-output-to-string (out)
    (loop for c across s
          do (case c
               (#\~ (write-string "~0" out))
               (#\/ (write-string "~1" out))
               (t (write-char c out))))))
