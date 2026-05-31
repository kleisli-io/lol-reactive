;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/optimization — reactive analysis, template validation
;;;;   src/optimization/{reactive-analysis,template-validation}.lisp
;;;;
;;;; STATUS: alpha. The macro-time analysis (reactive-let,
;;;; defvalidated-template) emits code shapes that have not yet been
;;;; exercised by integration consumers; the template validator only
;;;; covers a subset of cl-who patterns; the API of with-reactive-bindings
;;;; will likely change once a real consumer surfaces. Suitable for
;;;; experimentation; not yet a stable public surface.

(in-package :cl-user)

(defpackage :lol-web/optimization
  (:use :cl :iterate
        :lol-web/core      ; register-component
        :lol-web/css       ; *colors*, *effects*, *spacing*, *typography*, validate-token, find-closest-match
        :lol-web/html      ; htm, htm-str, escape-html, safe-str
        :lol-web/server)   ; param
  (:import-from :let-over-lambda
                :dlambda :defmacro!
                :symb)
  (:export
   ;; reactive-analysis.lisp
   #:analyze-dependencies
   #:reactive-let
   #:with-reactive-bindings
   ;; template-validation.lisp
   #:defvalidated-template
   #:validate-css-class
   #:*registered-css-classes*
   #:*registered-css-prefixes*
   #:register-css-class
   #:register-css-prefix
   #:register-tailwind-classes
   #:lint-hx-on-not-literal))
