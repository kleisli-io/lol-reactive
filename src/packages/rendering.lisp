;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/rendering — keyed-list reconciliation (no public API today)
;;;;   src/rendering/keyed-list.lisp

(in-package :cl-user)

(defpackage :lol-web/rendering
  (:use :cl :iterate
        :lol-web/core
        :lol-web/html)   ; htm-str
  (:import-from :let-over-lambda :defmacro!)
  (:export))
