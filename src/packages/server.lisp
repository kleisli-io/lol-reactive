;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/server — clack abstraction, routes, app, security, error handling
;;;;   src/server/{clack,security,http-errors,errors,app,routes}.lisp

(in-package :cl-user)

(defpackage :lol-web/server
  (:use :cl :iterate
        :lol-web/core    ; find-component, register-component, etc.
        :lol-web/html)   ; render-component, html-page, htm-str
  (:import-from :let-over-lambda
                :defmacro!
                :symb)
  ;; Re-export Lack's pluggable session store protocol verbatim so consumers
  ;; implementing Redis/Postgres/etc. backends never have to depend on the
  ;; :lack/session/store package directly. (find-class lol-web/server:store)
  ;; eq (find-class lack/session/store:store) — same class, no wrapping.
  (:import-from :lack/session/store
                #:store
                #:fetch-session
                #:store-session
                #:remove-session)
  ;; constant-time-string= lives in :lol-web/crypto; server keeps it in its
  ;; :export list so existing consumers calling lol-web/server:constant-time-string=
  ;; keep working. random-bytes-hex is used by generate-csrf-token below.
  (:import-from :lol-web/crypto
                #:constant-time-string=
                #:random-bytes-hex
                ;; sha256-hex content-addresses registered assets.
                #:sha256-hex)
  ;; JSON-body decoder uses safe-coerce-keyword (find-symbol, not intern)
  ;; so JSON keys cannot grow the keyword pool.
  (:import-from :lol-web/escape
                #:safe-coerce-keyword
                #:escape-html
                ;; assets.lisp: attribute-escape data-block ids and
                ;; </script>-neutralise embedded JSON.
                #:escape-attribute
                #:neutralize-script-close)
  (:export
   ;; clack.lisp — request/response abstraction
   #:*env*
   #:*response-headers*
   #:request-path
   #:request-method
   #:request-query-string
   #:request-header
   #:request-content-type
   #:request-content-length
   #:request-body
   #:request-body-json
   #:query-param
   #:query-params
   #:post-param
   #:post-params
   #:param
   #:parse-request-json
   #:encode-json-string
   #:decode-json-string
   #:malformed-json-body
   #:malformed-json-body-input
   #:malformed-json-body-reason
   #:+json-null+
   #:*json-body-max-depth*
   #:*json-body-max-string-length*
   #:response
   #:html-response
   #:json-response
   #:text-response
   #:redirect-response
   #:unsafe-redirect-error
   #:safe-redirect-path-p
   #:*canonical-host*
   #:error-response
   #:with-response-headers
   #:add-response-header
   #:get-response-headers
   #:http-status-text
   #:session-get
   #:session-set
   #:session-delete
   #:session-clear
   #:session-keys
   #:session-rotate
   #:streaming-session-rotate-error
   #:session-expire
   #:current-session-id
   ;; Lack session store protocol — re-exported so consumers subclass
   ;; lol-web/server:store and define methods on the re-exported generics.
   #:store
   #:fetch-session
   #:store-session
   #:remove-session
   #:csrf-token
   ;; security.lisp
   #:validate-header-value
   #:validate-origin
   #:add-security-headers
   #:add-csp-header
   #:with-security
   #:generate-csrf-token
   #:get-csrf-token
   #:validate-csrf-token
   #:csrf-token-input
   #:constant-time-string=
   #:with-csrf-validation
   #:csrf-middleware
   #:request-csrf-valid-p
   #:check-rate-limit
   #:get-client-ip
   #:client-ip
   #:*trusted-proxies*
   #:with-rate-limit
   #:*rate-limit-registry*
   #:rate-limit-entry
   #:rate-limit-entry-p
   #:make-rate-limit-entry
   #:rate-limit-entry-count
   #:rate-limit-entry-window-start
   #:rate-limit-entry-last-seen
   #:rate-limit-namespace
   #:rate-limit-namespace-p
   #:make-rate-limit-namespace
   #:rate-limit-namespace-name
   #:rate-limit-namespace-store
   #:rate-limit-namespace-lock
   #:rate-limit-namespace-max-entries
   #:rate-limit-namespace-inserts-since-eviction
   #:configure-rate-limit-namespace
   #:*rate-limit-min-evict-age*
   #:*rate-limit-eviction-every-n*
   #:*rate-limit-eviction-interval*
   #:configure-rate-limit-eviction-timer
   #:rate-limit-store-full
   #:rate-limit-store-full-namespace
   #:rate-limit-store-full-max-entries
   #:rate-limit-entry-of
   #:rate-limit-namespace-count
   #:clear-rate-limit-store
   #:current-principal
   #:current-principal-of-env
   #:session-get-of-env
   #:with-auth
   ;; http-errors.lisp
   #:http-error
   #:http-error-status
   #:http-error-body
   #:client-error
   #:server-error
   #:http-bad-request
   #:http-unauthorized
   #:http-forbidden
   #:http-not-found
   #:http-unprocessable-entity
   ;; errors.lisp
   #:*debug-mode*
   #:*debug-mode-locked-p*
   #:debug-mode-locked-error
   #:debug-mode-locked-error-value
   #:set-debug-mode
   #:lock-debug-mode
   #:*error-log-path*
   #:with-error-handling
   #:log-error
   #:render-error-page
   #:render-404-page
   #:render-500-page
   #:enable-debug-mode
   #:disable-debug-mode
   ;; app.lisp
   #:*max-request-body-bytes*
   #:request-body-too-large
   #:request-body-too-large-limit
   #:request-body-too-large-declared
   #:request-body-too-large-actual
   #:*path-params*
   #:path-param
   #:unsafe-path-segment
   #:unsafe-path-segment-segment
   #:safe-path-segment-p
   #:safe-path-segment
   #:*routes*
   #:clear-routes
   #:list-routes
   #:route-handler
   #:make-app
   #:app-middleware-order
   #:middleware-order-error
   #:middleware-order-error-order
   #:middleware-order-error-reason
   #:*server*
   #:*before-handler-hook*
   #:*before-server-start-hook*
   #:start-server
   #:stop-server
   #:defstreaming-route
   #:streaming-route-entry
   #:streaming-route-entry-p
   #:make-streaming-route-entry
   #:streaming-route-entry-body
   #:streaming-route-entry-auth
   #:streaming-route-entry-origin
   #:streaming-route-entry-bearer-token
   ;; streaming-gate.lisp
   #:streaming-gate
   ;; routes.lisp
   #:defroute
   ;; assets.lisp — content-addressed external asset serving
   #:register-asset
   #:clear-asset-registry
   #:*asset-route-prefix*
   #:asset-middleware
   #:page
   #:embed-json-data))
