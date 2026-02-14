;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Server-Sent Events (SSE) client runtime (Parenscript)
;;;;
;;;; Client-side SSE connection management and message processing.

(in-package :lol-reactive)

;;; ============================================================================
;;; SSE CLIENT RUNTIME (Parenscript)
;;; ============================================================================

(defun sse-client-js ()
  "Generate Server-Sent Events client runtime via Parenscript.
   Handles EventSource connections and message processing."
  (parenscript:ps
    ;; SSE Manager Object
    (defvar *sse-manager*
      (ps:create
       "connections" (ps:create)  ; channel -> EventSource
       "reconnectDelay" 3000      ; Default retry from SSE spec

       ;; Connect to an SSE channel
       "connect" (lambda (channel &optional options)
                   (let* ((url (+ "/sse/" channel))
                          (source (ps:new (-Event-Source url)))
                          ;; Support both kebab-case ('on-open') and camelCase (onOpen) keys
                          (on-message (and options (or (ps:getprop options "on-message")
                                                       (ps:@ options on-message))))
                          (on-open (and options (or (ps:getprop options "on-open")
                                                    (ps:@ options on-open))))
                          (on-error (and options (or (ps:getprop options "on-error")
                                                     (ps:@ options on-error)))))

                     ;; Store connection
                     (setf (ps:getprop (ps:@ *sse-manager* connections) channel) source)

                     ;; Handle connection open
                     (setf (ps:@ source onopen)
                           (lambda ()
                             (ps:chain console (log "SSE connected:" channel))
                             (when on-open
                               (funcall on-open source))))

                     ;; Handle 'connected' event (server confirmation)
                     (ps:chain source (add-event-listener "connected"
                       (lambda (event)
                         (ps:chain console (log "SSE confirmed:" channel
                                                (ps:chain -j-s-o-n (parse (ps:@ event data))))))))

                     ;; Handle 'update' event (HTML updates)
                     (ps:chain source (add-event-listener "update"
                       (lambda (event)
                         (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                           ((ps:@ *sse-manager* handle-html-update) data)))))

                     ;; Handle 'oob' event (out-of-band updates)
                     (ps:chain source (add-event-listener "oob"
                       (lambda (event)
                         (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                           ((ps:@ *sse-manager* handle-oob-updates) data)))))

                     ;; Handle 'trigger' event (custom events)
                     (ps:chain source (add-event-listener "trigger"
                       (lambda (event)
                         (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                           ((ps:@ *sse-manager* handle-trigger) data)))))

                     ;; Handle generic 'message' event (fallback)
                     (setf (ps:@ source onmessage)
                           (lambda (event)
                             (when on-message
                               (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                                 (funcall on-message data source)))))

                     ;; Handle errors (EventSource auto-reconnects)
                     (setf (ps:@ source onerror)
                           (lambda (event)
                             (ps:chain console (warn "SSE error:" channel event))
                             (when on-error
                               (funcall on-error event source))))

                     ;; Handle race condition: if connection opened before handlers were set
                     (when (and on-open (= (ps:@ source ready-state) 1))
                       (ps:chain console (log "SSE already connected:" channel))
                       (funcall on-open source))

                     source))

       ;; Disconnect from a channel
       "disconnect" (lambda (channel)
                      (let ((source (ps:getprop (ps:@ *sse-manager* connections) channel)))
                        (when source
                          (ps:chain source (close))
                          (delete (ps:getprop (ps:@ *sse-manager* connections) channel)))))

       ;; Handle HTML update event
       "handleHtmlUpdate" (lambda (data)
                            (let ((target (ps:chain document (get-element-by-id (ps:@ data target)))))
                              (when target
                                (let ((swap (or (ps:@ data swap) "innerHTML")))
                                  ((ps:@ *htmx* swap) target (ps:@ data html) swap)
                                  ;; Re-initialize HTMX on updated content
                                  ((ps:@ *htmx* process-element) target)))))

       ;; Handle OOB updates event
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

       ;; Handle event trigger
       "handleTrigger" (lambda (data)
                         (let ((event (ps:new (-Custom-Event (ps:@ data event)
                                                          (ps:create :detail (ps:@ data detail)
                                                                     :bubbles t)))))
                           (ps:chain document (dispatch-event event))))))))
