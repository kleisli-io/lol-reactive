;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/htmx — HTMX runtime, OOB, autocomplete, server helpers, morph
;;;;   src/htmx/{runtime,oob,autocomplete,server,morph}.lisp
;;;;   src/htmx/runtime/{config,swap,ajax,triggers}.lisp (internal helpers)

(in-package :cl-user)

(defpackage :lol-web/htmx
  (:use :cl :iterate
        :lol-web/css       ; css-rules (used by autocomplete-css)
        :lol-web/html      ; htm-str, html-page (used by htmx/server.lisp)
        :lol-web/server)   ; add-response-header, encode-json-string, defroute
  (:import-from :let-over-lambda
                :defmacro!)   ; with-htmx-response gensyms via g!-prefix
  (:import-from :lol-web/escape
                #:safe-url-allowlist)
  (:import-from :lol-web/parenscript
                #:jsonify)
  (:import-from :lol-web/crypto
                #:mint-token
                #:verify-token)
  (:export
   ;; runtime.lisp
   #:htmx-runtime-js
   #:emit-hx-attrs
   #:hx-get
   #:hx-post
   #:hx-put
   #:hx-delete
   ;; autocomplete.lisp
   #:htmx-indicator-css
   #:render-autocomplete
   #:render-autocomplete-results
   #:autocomplete-css
   ;; oob.lisp
   #:oob-swap
   #:oob-content
   #:make-oob-swap
   #:with-oob-swaps
   #:*oob-selector-allowlist*
   #:*oob-selector-denylist*
   #:*oob-signed-selector-secret*
   #:validate-oob-selector
   #:mint-oob-selector-token
   #:unsafe-oob-selector
   #:unsafe-oob-selector-selector
   #:unsafe-oob-selector-reason
   ;; server.lisp — canonical home for htmx-* request helpers
   #:htmx-request-p
   #:htmx-boosted-p
   #:htmx-history-restore-request-p
   #:htmx-target
   #:htmx-trigger
   #:htmx-trigger-name
   #:htmx-current-url
   #:htmx-prompt
   #:with-htmx-response
   #:set-htmx-trigger
   #:set-htmx-redirect
   #:set-htmx-location
   #:render-with-oob
   #:render-oob-only
   #:htmx-or-redirect
   #:htmx-or-full-page
   ;; morph.lisp
   #:*idiomorph-version*
   #:idiomorph-js-path
   #:include-idiomorph
   #:htmx-morph-extension-js
   #:htmx-runtime-with-morph-js
   #:include-htmx-with-morph
   #:hx-morph))
