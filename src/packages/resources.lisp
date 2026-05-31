;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/resources — async resource management (no public API today)
;;;;   src/async/resources.lisp

(in-package :cl-user)

(defpackage :lol-web/resources
  (:use :cl :iterate
        :lol-web/html)   ; htm-str
  (:import-from :lol-web/css
                #:classes
                #:css-keyframes
                #:css-rule
                #:css-section
                #:css-var
                #:make-safe-css-payload-string)
  (:import-from :let-over-lambda :symb)
  (:export))
