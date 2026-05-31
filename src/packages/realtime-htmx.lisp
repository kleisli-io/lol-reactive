;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/realtime-htmx — client-side JS emitters for WS/SSE/optimistic
;;;;   src/realtime/{ws-client,sse-client,optimistic}.lisp

(in-package :cl-user)

(defpackage :lol-web/realtime-htmx
  (:use :cl :iterate)
  (:import-from :lol-web/core
                #:find-component
                #:component-originals)
  (:import-from :lol-web/html
                #:safe-html-string
                #:safe-html-string-p
                #:safe-html-string-value)
  (:export
   #:ws-client-js
   #:sse-client-js
   #:optimistic-js
   #:*optimistic-originals-cap*
   #:*optimistic-originals-global-cap*
   #:optimistic-apply-payload
   #:optimistic-record-original
   #:optimistic-clear-originals))
