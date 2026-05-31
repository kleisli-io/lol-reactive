;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/escape — output-emission escape helpers.
;;;;   src/escape/escape.lisp
;;;;
;;;; Escape, not sanitize: the helpers render untrusted text safe to embed
;;;; in HTML text, attribute, and URL contexts. They do not produce safe
;;;; HTML from untrusted markup.

(in-package :cl-user)

(defpackage :lol-web/escape
  (:use :cl :iterate)
  (:export
   #:escape-html
   #:escape-attribute
   #:safe-attr
   #:safe-href
   #:neutralize-script-close
   #:safe-url
   #:safe-url-allowlist
   #:safe-coerce-keyword
   #:%scan-bounded
   #:*scan-match-timeout-seconds*
   #:*hx-on-attribute-prefix*
   #:hx-on-attribute-name-p
   #:sanitize-hx-on-attrs))
