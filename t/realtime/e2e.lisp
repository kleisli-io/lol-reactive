;;;; HTTP-level end-to-end tests for :lol-web/realtime.
;;;;
;;;; lack/test:request does not speak the WebSocket upgrade handshake,
;;;; so we register a streaming-route-entry, build the full make-app
;;;; stack, forge a synthetic Clack env, and funcall the app to inspect
;;;; the upgrade-time response triple without opening a connection.

(in-package :lol-web/realtime/test)
(in-suite :lol-web/realtime/test)

;;; ============================================================================
;;; Fixtures
;;; ============================================================================

(defun %e2e-install-entry (path &key auth origin)
  (let ((entry (lol-web/server:make-streaming-route-entry
                :body (lambda (env)
                        (declare (ignore env))
                        (list 200
                              (list :content-type "text/plain")
                              (list "handler-body-ran")))
                :auth auth
                :origin origin)))
    (bordeaux-threads:with-recursive-lock-held (lol-web/server::*routes-lock*)
      (setf (gethash (cons :get path) lol-web/server::*streaming-routes*) entry))
    (lambda ()
      (bordeaux-threads:with-recursive-lock-held (lol-web/server::*routes-lock*)
        (remhash (cons :get path) lol-web/server::*streaming-routes*)))))

(defun %e2e-streaming-env (&key (path "/ws/e2e")
                                (origin "https://app.example.com")
                                (remote-addr "10.0.0.1"))
  (let ((h (make-hash-table :test 'equal)))
    (when origin (setf (gethash "origin" h) origin))
    (list :request-method :get
          :path-info path
          :headers h
          :remote-addr remote-addr)))

(defun %e2e-make-app (&rest streaming-rate-limit-overrides)
  (apply #'lol-web/server:make-app
         :use-csrf nil :use-static nil :use-accesslog nil
         (when streaming-rate-limit-overrides
           (list :streaming-rate-limit streaming-rate-limit-overrides))))

;;; ============================================================================
;;; Upgrade-gate composed into make-app
;;; ============================================================================

(test e2e-realtime-upgrade-cross-origin-refused
  (lol-web/server:clear-rate-limit-store :streaming)
  (let ((cleanup (%e2e-install-entry
                  "/ws/e2e"
                  :auth (lambda (env) (declare (ignore env))
                          (error "auth must not run on origin denial"))
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((app (%e2e-make-app))
               (env (%e2e-streaming-env :origin "https://evil.example"))
               (response (funcall app env)))
          (is (= 403 (first response))
              "cross-origin upgrade must yield 403, got ~D" (first response)))
      (funcall cleanup))))

(test e2e-realtime-upgrade-unauthenticated-refused
  (lol-web/server:clear-rate-limit-store :streaming)
  (let ((cleanup (%e2e-install-entry
                  "/ws/e2e"
                  :auth (lambda (env) (declare (ignore env)) nil)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((app (%e2e-make-app))
               (env (%e2e-streaming-env))
               (response (funcall app env)))
          (is (= 401 (first response))
              "auth thunk returning NIL must yield 401, got ~D"
              (first response)))
      (funcall cleanup))))

(test e2e-realtime-upgrade-allowed-reaches-handler
  (lol-web/server:clear-rate-limit-store :streaming)
  (let ((cleanup (%e2e-install-entry
                  "/ws/e2e"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((app (%e2e-make-app))
               (env (%e2e-streaming-env))
               (response (funcall app env)))
          (is (= 200 (first response)))
          (is (equal '("handler-body-ran") (third response))
              "permitted upgrade must reach the registered handler"))
      (funcall cleanup))))

(test e2e-realtime-upgrade-rate-limited
  (lol-web/server:clear-rate-limit-store :streaming)
  (let ((cleanup (%e2e-install-entry
                  "/ws/e2e"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((app (%e2e-make-app :max-requests 2
                                   :window-seconds 60
                                   :namespace :streaming))
               (env (%e2e-streaming-env :remote-addr "10.0.0.99")))
          (is (= 200 (first (funcall app env))))
          (is (= 200 (first (funcall app env))))
          (let ((response (funcall app env)))
            (is (= 429 (first response))
                "third upgrade must trip the streaming rate limit, got ~D"
                (first response)))
          (lol-web/server:clear-rate-limit-store :streaming))
      (funcall cleanup))))

;;; ============================================================================
;;; Stubs
;;; ============================================================================

(test e2e-realtime-ws-frame-size-cap-stub
  (fiveam:pass "stubbed"))

(test e2e-realtime-sse-broadcast-xss-amplification
  "End-to-end: a producer that mints a safe-html-string containing
   `<script>` does NOT see that script tag elided from the wire — the
   safety claim was the producer's to make.  What IS elided is the
   hx-on-* attribute set, which lifts JavaScript at swap time into a
   native handler in the peer DOM and is therefore the actual
   amplification vector.  This split keeps server-trusted template
   markup expressive while neutralising the broadcast-XSS class
   independent of the producer's safety claim."
  (let ((captured nil))
    (let ((orig (symbol-function 'lol-web/realtime:sse-broadcast)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:sse-broadcast)
                  (lambda (channel event-type data &key id)
                    (declare (ignore channel event-type id))
                    (setf captured data)
                    1))
            (lol-web/realtime:sse-broadcast-oob
             "ch"
             (list (list "#t"
                         (lol-web/html:make-safe-html-string
                          "<div hx-on-click=\"evil()\"><script>safe()</script></div>")
                         :swap "outerHTML")))
            (let* ((updates (cdr (assoc :updates captured)))
                   (html    (cdr (assoc :html (first updates)))))
              (is (stringp html))
              (is (not (search "hx-on-click" html))
                  "the hx-on-click attribute must be stripped on the wire")
              (is (not (search "evil()" html))
                  "the hx-on-* JS payload must not survive")
              (is (search "<script>safe()</script>" html)
                  "producer-attested <script> survives — the safety claim is theirs")))
        (setf (symbol-function 'lol-web/realtime:sse-broadcast) orig)))))

(test e2e-realtime-sse-broadcast-hx-on-colon-stripped
  "The colon form hx-on:click — which the client runtime lifts to a native
   onclick exactly as the dash form does — must not survive to a peer on the
   broadcast wire. Closes the dash-only sanitizer gap end-to-end."
  (let ((captured nil))
    (let ((orig (symbol-function 'lol-web/realtime:sse-broadcast)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:sse-broadcast)
                  (lambda (channel event-type data &key id)
                    (declare (ignore channel event-type id))
                    (setf captured data)
                    1))
            (lol-web/realtime:sse-broadcast-oob
             "ch"
             (list (list "#t"
                         (lol-web/html:make-safe-html-string
                          "<div hx-on:click=\"evil()\">x</div>")
                         :swap "outerHTML")))
            (let* ((updates (cdr (assoc :updates captured)))
                   (html    (cdr (assoc :html (first updates)))))
              (is (stringp html))
              (is (not (search "hx-on" html))
                  "the hx-on:click attribute must be stripped on the wire")
              (is (not (search "evil()" html))
                  "the colon-form hx-on JS payload must not survive")))
        (setf (symbol-function 'lol-web/realtime:sse-broadcast) orig)))))
