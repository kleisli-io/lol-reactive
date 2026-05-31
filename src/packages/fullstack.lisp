;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/fullstack — isomorphic components and component-API endpoints
;;;;   src/fullstack/{isomorphic,component-api}.lisp
;;;;
;;;; STATUS: alpha. defisomorphic-component / hydrate-all generate a
;;;; client runtime that has not yet been exercised by an integration
;;;; consumer end-to-end; the hydration handshake (server-rendered
;;;; markup + client takeover) currently only does ID lookup and
;;;; logging, no actual state rebinding. defcomponent-with-api is
;;;; further along — used by the in-tree component-api routes — but
;;;; the surface around it is still in flux. Suitable for
;;;; experimentation; not yet a stable public API.

(in-package :cl-user)

(defpackage :lol-web/fullstack
  (:use :cl :iterate
        :lol-web/core      ; defcomponent, find-component, register-component
        :lol-web/html      ; html-page, htm, htm-str
        :lol-web/server    ; encode-json-string, decode-json-string
        :lol-web/extractors) ; defhandler, :json-body extractor
  (:import-from :let-over-lambda
                :pandoriclet
                :dlambda :defmacro!
                :symb)
  ;; escape-html serializes hydration state for attribute embedding;
  ;; safe-coerce-keyword guards component-api :set-state / :action body keys.
  (:import-from :lol-web/escape
                #:escape-html
                #:safe-coerce-keyword)
  (:export
   ;; hydration.lisp
   #:sign-hydration-state
   #:verify-hydration-state
   ;; isomorphic.lisp
   #:defisomorphic-component
   #:render-isomorphic
   #:isomorphic-page
   #:hydration-runtime-js
   #:include-hydration-runtime
   #:client-action-attr
   #:serialize-state
   #:deserialize-state
   ;; component-api.lisp
   #:defcomponent-with-api
   #:register-api-component
   #:find-api-component
   #:list-api-components
   #:list-api-routes
   #:inspect-api-component
   #:generate-api-client-js
   #:api-client-script-tag
   #:*action-arities*
   #:register-action-arity
   #:action-arity))
