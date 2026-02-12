;;;; LOL-REACTIVE Demo - Realtime Dashboard
;;;; Stress tests WebSocket, SSE, and Optimistic Update features

(defpackage :lol-reactive-demo
  (:use :cl :iterate)
  (:import-from :let-over-lambda
                :dlambda
                :aif)
  (:import-from :parenscript
                :chain
                :@
                :create)
  (:import-from :lol-reactive
                ;; CSS
                :defcss
                :css-rule
                :css-rules
                :get-color
                :get-font
                :get-spacing
                :classes
                :generate-all-component-css
                ;; HTML
                :htm
                :htm-str
                :html-page
                :escape-html
                ;; Server
                :defroute
                :defapi
                :start-server
                :stop-server
                :json-body
                :json-response
                :error-response
                :post-param
                :post-params
                :query-param
                ;; Signals
                :make-signal
                :make-computed
                :make-effect
                :make-store
                ;; HTMX
                :oob-swap
                :render-with-oob
                :with-htmx-response
                ;; Parenscript
                :on-click
                :on-change
                ;; Realtime
                :defws
                :ws-broadcast
                :ws-broadcast-html
                :ws-broadcast-json
                :ws-connection-count
                :defsse
                :sse-broadcast
                :sse-broadcast-html
                ;; Runtime JS
                :lol-reactive-runtime-js)
  (:export
   #:main
   #:start
   #:stop
   ;; Chaos mode
   #:*chaos-mode*
   #:*chaos-delay-range*
   #:*chaos-failure-rate*
   #:with-chaos
   #:chaos-delay
   #:chaos-maybe-fail))

(in-package :lol-reactive-demo)
