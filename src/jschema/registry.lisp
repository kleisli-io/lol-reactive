;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/JSCHEMA; Base: 10 -*-
;;;; The JSON-SCHEMA struct and the cross-document registry.
;;;;
;;;; *REGISTRY* (URI → JSON-SCHEMA) holds hosted $id documents; image-global
;;;; by default, let-bound per-app by make-app's :SCHEMA-REGISTRY middleware.
;;;; Each root schema also carries a self-registry (JSON Pointer → child)
;;;; for same-document $ref / $dynamicRef resolution.

(in-package :lol-web/jschema)

;;; ============================================================================
;;; JSON-SCHEMA — parsed schema instance
;;; ============================================================================

(defstruct json-schema
  "A parsed JSON Schema. Boolean schemas (true/false) carry BOOL set to
   :TRUE / :FALSE and have no other slots populated. Object schemas carry
   KEYWORDS as an alist of (NAME . PARSED-VALUE) pairs."
  (bool nil)              ; :TRUE / :FALSE / NIL when BOOL is not boolean-shaped
  (id nil)                ; resolved-against-base-URI string, or NIL
  (base-uri nil)          ; PURI:URI for this schema-resource document, or NIL
  (anchor nil)            ; "$anchor" value, or NIL
  (dynamic-anchor nil)    ; "$dynamicAnchor" value, or NIL
  (schema-uri nil)        ; "$schema" string, or NIL — only meaningful at root
  (defs nil)              ; alist of ($defs-name . child-json-schema)
  (keywords nil)          ; alist of (keyword-string . parsed-value)
  (self-registry nil)     ; hash-table json-pointer → json-schema (root only)
  (parent-self-registry nil)) ; back-link for child schemas to find sibling refs

;;; ============================================================================
;;; GLOBAL REGISTRY
;;; ============================================================================

(defvar *registry* (make-hash-table :test 'equal)
  "Cross-document schema registry. Keys are URI strings (post-resolution),
   values are JSON-SCHEMA instances. Image-global by default; let-bound
   per-app by make-app's :SCHEMA-REGISTRY middleware so two apps in one
   image can hold disjoint URI namespaces. Mutated only through
   REGISTER-SCHEMA / CLEAR-REGISTRY under *REGISTRY-LOCK*.")

(defvar *registry-lock*
  (bordeaux-threads:make-recursive-lock "lol-web/jschema registry"))

(defun clear-registry ()
  "Wipe the global cross-document registry. Useful between test runs."
  (bordeaux-threads:with-recursive-lock-held (*registry-lock*)
    (clrhash *registry*))
  (values))

(defun call-with-registry (registry-table thunk)
  "Funcall THUNK with *REGISTRY* let-bound to REGISTRY-TABLE. Keeps the
   dynamic-binding establishment inside the package that owns the special
   so cross-package callers don't have to declare it themselves."
  (let ((*registry* registry-table))
    (funcall thunk)))

(defun register-schema (uri schema)
  "Register SCHEMA under URI in the global registry. URI is a string."
  (bordeaux-threads:with-recursive-lock-held (*registry-lock*)
    (setf (gethash uri *registry*) schema)))

(defun get-schema (uri)
  "Find a schema by URI. URI may be a string or a PURI:URI. Returns the
   JSON-SCHEMA, or NIL if no schema is registered. If URI carries a fragment,
   the fragment is resolved through the schema's self-registry as a JSON
   Pointer or anchor name."
  (let* ((uri-obj (etypecase uri
                    (string (puri:parse-uri uri))
                    (puri:uri uri)))
         (fragment (puri:uri-fragment uri-obj))
         (uri-no-frag (puri:copy-uri uri-obj))
         lookup-key)
    (setf (puri:uri-fragment uri-no-frag) nil
          lookup-key (puri:render-uri uri-no-frag nil))
    (let ((root (bordeaux-threads:with-recursive-lock-held (*registry-lock*)
                  (gethash lookup-key *registry*))))
      (cond
        ((null root) nil)
        ((or (null fragment) (string= fragment ""))
         root)
        (t
         (resolve-fragment root fragment))))))

(defun resolve-fragment (root fragment)
  "Resolve FRAGMENT against ROOT's self-registry. FRAGMENT is the URI fragment
   without the leading '#'. JSON Pointer fragments start with '/'; everything
   else is treated as an anchor name."
  (let ((self (json-schema-self-registry root)))
    (when self
      (gethash fragment self))))
