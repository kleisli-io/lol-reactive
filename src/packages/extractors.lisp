;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/extractors — typed extractor protocol + defhandler macro
;;;;   src/extractors/{registry,coercion,builtin,defhandler}.lisp
;;;;
;;;; The handler signature declares what it needs; the framework resolves
;;;; each extractor against the request before invoking the body. Extractors
;;;; dispatch via a CLOS generic (RESOLVE-EXTRACTOR) on the extractor KIND;
;;;; users add new kinds by adding methods.

(in-package :cl-user)

(defpackage :lol-web/extractors
  (:use :cl
        :lol-web/server)
  (:import-from :let-over-lambda
                :defmacro!
                :symb)
  ;; :type :keyword / :type :symbol coercers go through safe-coerce-keyword.
  (:import-from :lol-web/escape
                #:safe-coerce-keyword)
  (:export
   ;; registry.lisp — protocol surface
   #:extractor-spec
   #:make-extractor-spec
   #:extractor-spec-p
   #:extractor-spec-name
   #:extractor-spec-kind
   #:extractor-spec-type
   #:extractor-spec-required-p
   #:extractor-spec-default
   #:extractor-spec-source-string
   #:extractor-spec-custom-resolver
   #:resolve-extractor
   #:*handler-metadata*
   #:handler-metadata
   ;; conditions
   #:extractor-error
   #:extractor-error-name
   #:extractor-error-kind
   #:missing-extractor-input
   #:extractor-coercion-error
   #:extractor-coercion-error-raw-value
   #:extractor-coercion-error-target-type
   #:extractor-not-registered
   ;; defhandler.lisp
   #:defhandler))
