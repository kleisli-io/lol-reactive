;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/forms — form DSL (exports the bounded-regex tunables consumers
;;;;                  may want to override; DEFFORM itself is reached via the
;;;;                  umbrella :USE on the package)
;;;;   src/forms/form-dsl.lisp

(in-package :cl-user)

(defpackage :lol-web/forms
  (:use :cl :iterate
        :lol-web/escape    ; escape-attribute, escape-html, safe-url
        :lol-web/css       ; classes, css-rule, css-var, css-section
        :lol-web/html      ; htm, htm-str, safe-str
        :lol-web/server)   ; post-param, csrf-token-input
  (:import-from :let-over-lambda :symb)
  (:export
   #:*form-pattern-max-length*
   #:*form-pattern-compile-timeout-seconds*
   #:*form-email-max-length*
   #:*form-url-max-length*))
