;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/parenscript — Lisp-side helpers for emitting JS via parenscript
;;;;   src/client/parenscript.lisp

(in-package :cl-user)

(defpackage :lol-web/parenscript
  (:use :cl :iterate)
  (:import-from :let-over-lambda
                :aif
                :symb)
  (:export
   #:reactive-script
   #:jsonify
   #:on-click
   #:on-change
   #:on-submit
   #:hx-dispatch
   #:hx-bind
   ;; safety.lisp
   #:safe-js-string-literal
   #:safe-js-string-literal-p
   #:safe-js-string-literal-value
   #:make-safe-js-string-literal
   #:escape-js-string))
