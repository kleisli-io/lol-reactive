;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; WebSocket client runtime (Parenscript)
;;;;
;;;; Client-side WebSocket connection management, reconnection with
;;;; exponential backoff, and message processing.

(in-package :lol-reactive)

;;; ============================================================================
;;; WEBSOCKET CLIENT RUNTIME (Parenscript)
;;; ============================================================================

(defun ws-client-js ()
  "Generate WebSocket client runtime via Parenscript.
   Handles connection management, reconnection, and message processing."
  (parenscript:ps
    ;; WebSocket Manager Object
    (defvar *ws-manager*
      (ps:create
       "connections" (ps:create)  ; channel -> WebSocket
       "reconnectDelay" 1000
       "maxReconnectDelay" 30000

       ;; Connect to a WebSocket channel
       "connect" (lambda (channel &optional options)
                   (let* ((protocol (if (= (ps:@ window location protocol) "https:") "wss:" "ws:"))
                          (url (+ protocol "//" (ps:@ window location host) "/ws/" channel))
                          (ws (ps:new (-Web-Socket url)))
                          (reconnect-delay (ps:@ *ws-manager* reconnect-delay))
                          ;; Support both kebab-case ('on-open') and camelCase (onOpen) keys
                          (on-message (and options (or (ps:getprop options "on-message")
                                                       (ps:@ options on-message))))
                          (on-open (and options (or (ps:getprop options "on-open")
                                                    (ps:@ options on-open))))
                          (on-close (and options (or (ps:getprop options "on-close")
                                                     (ps:@ options on-close)))))

                     ;; Store connection
                     (setf (ps:getprop (ps:@ *ws-manager* connections) channel) ws)

                     ;; Handle connection open
                     (setf (ps:@ ws onopen)
                           (lambda ()
                             (ps:chain console (log "WebSocket connected:" channel))
                             (setf reconnect-delay (ps:@ *ws-manager* reconnect-delay))
                             (when on-open
                               (funcall on-open ws))))

                     ;; Handle incoming messages
                     (setf (ps:@ ws onmessage)
                           (lambda (event)
                             (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                               ;; Process based on message type
                               (let ((msg-type (ps:@ data type)))
                                 (cond
                                   ;; HTML swap
                                   ((= msg-type "html")
                                    ((ps:@ *ws-manager* handle-html-update) data))
                                   ;; OOB updates
                                   ((= msg-type "oob")
                                    ((ps:@ *ws-manager* handle-oob-updates) data))
                                   ;; Event trigger
                                   ((= msg-type "trigger")
                                    ((ps:@ *ws-manager* handle-trigger) data))
                                   ;; Custom handler
                                   (t
                                    (when on-message
                                      (funcall on-message data ws))))))))

                     ;; Handle connection close with reconnection
                     (setf (ps:@ ws onclose)
                           (lambda (event)
                             (ps:chain console (log "WebSocket closed:" channel "- reconnecting in" reconnect-delay "ms"))
                             (when on-close
                               (funcall on-close event ws))
                             ;; Attempt reconnection with exponential backoff
                             (set-timeout
                              (lambda ()
                                ((ps:@ *ws-manager* connect) channel options))
                              reconnect-delay)
                             ;; Increase delay for next attempt (with max)
                             (setf reconnect-delay
                                   (ps:chain -math (min (* reconnect-delay 2)
                                                        (ps:@ *ws-manager* max-reconnect-delay))))))

                     ;; Handle errors
                     (setf (ps:@ ws onerror)
                           (lambda (error)
                             (ps:chain console (error "WebSocket error:" channel error))))

                     ;; Handle race condition: if connection opened before handlers were set
                     (when (and on-open (= (ps:@ ws ready-state) 1))
                       (ps:chain console (log "WebSocket already connected:" channel))
                       (funcall on-open ws))

                     ws))

       ;; Disconnect from a channel
       "disconnect" (lambda (channel)
                      (let ((ws (ps:getprop (ps:@ *ws-manager* connections) channel)))
                        (when ws
                          (ps:chain ws (close))
                          (delete (ps:getprop (ps:@ *ws-manager* connections) channel)))))

       ;; Send message to a channel
       "send" (lambda (channel data)
                (let ((ws (ps:getprop (ps:@ *ws-manager* connections) channel)))
                  (when (and ws (= (ps:@ ws ready-state) 1))
                    (ps:chain ws (send (if (stringp data)
                                           data
                                           (ps:chain -j-s-o-n (stringify data))))))))

       ;; Handle HTML update message
       "handleHtmlUpdate" (lambda (data)
                            (let ((target (ps:chain document (get-element-by-id (ps:@ data target)))))
                              (when target
                                (let ((swap (or (ps:@ data swap) "innerHTML")))
                                  ((ps:@ *htmx* swap) target (ps:@ data html) swap)
                                  ;; Re-initialize HTMX on updated content
                                  ((ps:@ *htmx* process-element) target)))))

       ;; Handle OOB updates message
       "handleOobUpdates" (lambda (data)
                            (ps:chain (ps:@ data updates) (for-each
                              (lambda (update)
                                (let ((target (ps:chain document (get-element-by-id (ps:@ update target)))))
                                  (when target
                                    (let ((swap (or (ps:@ update swap) "outerHTML")))
                                      ((ps:@ *htmx* swap) target (ps:@ update html) swap)
                                      ;; Re-process the element
                                      (let ((new-el (ps:chain document (get-element-by-id (ps:@ update target)))))
                                        (when new-el
                                          ((ps:@ *htmx* process-element) new-el))))))))))

       ;; Handle event trigger message
       "handleTrigger" (lambda (data)
                         (let ((event (ps:new (-Custom-Event (ps:@ data event)
                                                          (ps:create :detail (ps:@ data detail)
                                                                     :bubbles t)))))
                           (ps:chain document (dispatch-event event))))))))
