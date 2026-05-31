;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; :lol-web/realtime — server-side WebSocket and SSE primitives
;;;;   src/realtime/{websocket,sse}.lisp

(in-package :cl-user)

(defpackage :lol-web/realtime
  (:use :cl :iterate
        :lol-web/server)   ; encode-json-string, decode-json-string
  (:import-from :lol-web/escape
                #:escape-html
                #:sanitize-hx-on-attrs)
  (:import-from :lol-web/html
                #:safe-html-string
                #:safe-html-string-p
                #:safe-html-string-value
                #:make-safe-html-string)
  (:export
   ;; websocket.lisp
   #:*ws-connections*
   #:*ws-per-ip-counts*
   #:*ws-per-ip-conn-cap*
   #:*ws-global-conn-cap*
   #:*ws-max-frame-size*
   #:ws-cap-exceeded
   #:ws-cap-exceeded-scope
   #:ws-cap-exceeded-ip
   #:ws-connection-count
   #:ws-per-ip-count
   #:ws-channels
   #:make-ws-handler
   #:defws
   #:ws-send
   #:ws-send-text
   #:ws-send-binary
   #:ws-send-json
   #:ws-close
   #:ws-broadcast
   #:ws-broadcast-json
   #:ws-broadcast-all
   #:ws-broadcast-text
   #:ws-broadcast-safe-html
   #:ws-broadcast-oob
   #:ws-broadcast-trigger
   #:make-oob-update
   ;; sse.lisp
   #:*sse-connections*
   #:*sse-per-ip-counts*
   #:*sse-per-ip-conn-cap*
   #:*sse-global-conn-cap*
   #:*sse-max-event-bytes*
   #:*sse-default-worker-pool-size*
   #:*sse-worker-pool-reserve*
   #:sse-cap-exceeded
   #:sse-cap-exceeded-scope
   #:sse-cap-exceeded-ip
   #:sse-connection
   #:sse-connection-p
   #:make-sse-connection
   #:sse-connection-id
   #:sse-connection-stream
   #:sse-connection-channel
   #:sse-connection-ip
   #:sse-connection-created-at
   #:sse-connection-alive-p
   #:sse-connection-on-disconnect
   #:sse-connection-count
   #:sse-remove-connection
   #:sse-per-ip-count
   #:sse-channels
   #:make-sse-handler
   #:defsse
   #:format-sse-event
   #:sse-send
   #:sse-send-comment
   #:sse-ping-all
   #:sse-broadcast
   #:sse-broadcast-all
   #:sse-broadcast-text
   #:sse-broadcast-safe-html
   #:sse-broadcast-oob
   #:sse-broadcast-trigger))
