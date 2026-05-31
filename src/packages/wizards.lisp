;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/wizards — continuation-based wizard form flows
;;;;   src/advanced/wizards.lisp

(in-package :cl-user)

(defpackage :lol-web/wizards
  (:use :cl :iterate
        :lol-web/css       ; classes
        :lol-web/html      ; htm, htm-str
        :lol-web/server)   ; defroute, post-param
  (:import-from :let-over-lambda
                :dlambda :defmacro!
                :aif)
  ;; wizard-action + form-field keys go through safe-coerce-keyword.
  (:import-from :lol-web/escape
                #:safe-coerce-keyword)
  (:export
   #:defwizard
   #:register-wizard
   #:get-wizard-spec
   #:list-wizards
   #:inspect-wizard
   #:start-wizard
   #:get-wizard-session
   #:remove-wizard-session
   #:cleanup-stale-sessions
   #:list-active-wizard-sessions
   #:process-wizard-submission
   #:render-wizard-step
   #:render-wizard-initial-step
   #:render-wizard-complete
   #:with-wizard-state
   #:wizard-text-field
   #:wizard-select-field
   #:wizard-radio-group))
