;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/devtools — surgery panel, snapshot/restore, runtime instrumentation
;;;;   src/devtools/{surgery,surgery-js,surgery-routes}.lisp

(in-package :cl-user)

(defpackage :lol-web/devtools
  (:use :cl :iterate
        :lol-web/core      ; find-component, register-component, defcomponent
        :lol-web/html      ; htm
        :lol-web/server
        :lol-web/extractors) ; defhandler, :json-body extractor
  (:import-from :let-over-lambda
                :with-pandoric
                :symb)
  ;; surgery-update body :key goes through safe-coerce-keyword;
  ;; the panel CSRF meta tag goes through escape-attribute.
  (:import-from :lol-web/escape
                #:safe-coerce-keyword
                #:escape-attribute)
  (:export
   #:capture-snapshot
   #:restore-snapshot
   #:list-snapshots
   #:clear-snapshots
   #:component-state-tree
   #:surgery-get-state
   #:surgery-set-state
   #:surgery-dispatch
   #:xray-wrapper-html
   #:surgery-panel-html
   #:enable-surgery-mode
   #:disable-surgery-mode
   #:surgery-mode-p
   #:*surgery-enable-audit-hook*
   #:*surgery-production-env-var*
   #:surgery-middleware
   #:surgery-runtime-js
   #:surgery-css
   #:push-undo
   #:surgery-undo
   #:surgery-redo
   #:register-component-metadata
   #:public-condition-message
   #:*public-condition-accessors*))
