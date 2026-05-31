;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/jschema — JSON Schema (draft 2020-12, OpenAPI 3.1 subset) validator
;;;;
;;;; Sufficient-subset implementation: parses and validates the JSON Schema
;;;; vocabulary keywords actually used by the OpenAPI 3.1 base schema and by
;;;; lol-web/openapi's emitted specs. Public surface mirrors cl-jschema so
;;;; consumers can swap implementations behind a package nickname if needed.
;;;;
;;;; Coverage: $id $schema $ref $defs $anchor $dynamicRef $dynamicAnchor
;;;;           $comment type enum const properties required additionalProperties
;;;;           patternProperties propertyNames unevaluatedProperties items
;;;;           prefixItems contains minItems maxItems uniqueItems minLength
;;;;           maxLength pattern minimum maximum exclusiveMinimum exclusiveMaximum
;;;;           multipleOf minProperties maxProperties allOf anyOf oneOf not
;;;;           if then else dependentSchemas dependentRequired format(annotation)
;;;;           title description default examples deprecated readOnly writeOnly
;;;;
;;;; Outside scope (signals NOT-IMPLEMENTED at parse time): contentEncoding,
;;;; contentMediaType, contentSchema, $vocabulary, dynamic-scope walking for
;;;; $dynamicRef beyond same-document anchor lookup.

(in-package :cl-user)

(defpackage :lol-web/jschema
  (:use :cl)
  (:import-from :alexandria
                :when-let
                :if-let
                :hash-table-keys)
  (:export
   ;; Entrypoints
   #:parse
   #:validate
   #:clear-registry
   #:get-schema
   #:*registry*
   #:call-with-registry
   ;; Depth + cycle guards
   #:*max-schema-depth*
   #:*max-validation-depth*
   #:*schema-json-max-depth*
   #:*schema-json-max-string-length*
   ;; Attacker-input bounds
   #:*pattern-max-length*
   #:*pattern-compile-timeout-seconds*
   #:*unique-items-hash-set-threshold*
   ;; Schema object
   #:json-schema
   #:json-schema-p
   ;; Schema-side conditions
   #:invalid-schema
   #:invalid-schema-error-message
   #:invalid-schema-base-uri
   #:invalid-schema-json-pointer
   #:unparsable-json
   #:unparsable-json-error
   #:not-implemented
   ;; Validation-side conditions
   #:invalid-json
   #:invalid-json-errors
   #:invalid-json-value
   #:invalid-json-value-error-message
   #:invalid-json-value-json-pointer))
