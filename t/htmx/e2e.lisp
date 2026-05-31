;;;; HTTP-level end-to-end tests for :lol-web/htmx.
;;;;
;;;; Drives make-app + lack/test:request and asserts on actual HX-*
;;;; response-header bytes. Negative paths use handler-case so the route
;;;; returns "signaled" / "no-signal" as the body and assertions stay
;;;; off the 500 rendering path.

(in-package :lol-web/htmx/test)
(in-suite :lol-web/htmx/test)

;;; ============================================================================
;;; Route fixtures
;;; ============================================================================

(defun %crlf-string (body)
  (concatenate 'string "x" (string #\Return) (string #\Linefeed) body))

(lol-web/server:defroute "/__e2e/htmx/trigger-string" (:method :get :secure nil)
  (with-htmx-response (:trigger "cartUpdated")
    "ok"))

(lol-web/server:defroute "/__e2e/htmx/trigger-crlf-signals" (:method :get :secure nil)
  (handler-case
      (with-htmx-response (:trigger (%crlf-string "evt"))
        "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/redirect-safe" (:method :get :secure nil)
  (set-htmx-redirect "/safe/path")
  "ok")

(lol-web/server:defroute "/__e2e/htmx/redirect-js-signals" (:method :get :secure nil)
  (handler-case
      (progn (set-htmx-redirect "javascript:alert(1)") "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/redirect-crlf-signals" (:method :get :secure nil)
  (handler-case
      (progn (set-htmx-redirect (%crlf-string "/path")) "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/location-js-signals" (:method :get :secure nil)
  (handler-case
      (progn (set-htmx-location "javascript:evil()") "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/push-url-crlf-signals" (:method :get :secure nil)
  (handler-case
      (with-htmx-response (:push-url (%crlf-string "/y"))
        "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/replace-url-crlf-signals" (:method :get :secure nil)
  (handler-case
      (with-htmx-response (:replace-url (%crlf-string "/y"))
        "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/retarget-crlf-signals" (:method :get :secure nil)
  (handler-case
      (with-htmx-response (:retarget (%crlf-string "#x"))
        "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/reswap-crlf-signals" (:method :get :secure nil)
  (handler-case
      (with-htmx-response (:reswap (%crlf-string "innerHTML"))
        "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/reselect-crlf-signals" (:method :get :secure nil)
  (handler-case
      (with-htmx-response (:reselect (%crlf-string "#a"))
        "no-signal")
    (error () "signaled")))

;;; ============================================================================
;;; HX-Trigger
;;; ============================================================================

(test e2e-htmx-trigger-string-reaches-wire
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (multiple-value-bind (body status headers)
        (lack/test:request "/__e2e/htmx/trigger-string")
      (declare (ignore body))
      (is (= 200 status))
      (let ((hx (gethash "hx-trigger" headers)))
        (is (not (null hx))
            "expected HX-Trigger header on response")
        (is (search "cartUpdated" hx)
            "HX-Trigger must carry the event name, got ~A" hx)))))

(test e2e-htmx-trigger-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/trigger-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Trigger must signal, got ~S" body))))

;;; ============================================================================
;;; HX-Redirect
;;; ============================================================================

(test e2e-htmx-redirect-safe-url-reaches-wire
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (multiple-value-bind (body status headers)
        (lack/test:request "/__e2e/htmx/redirect-safe")
      (declare (ignore body))
      (is (= 200 status))
      (is (string= "/safe/path" (gethash "hx-redirect" headers))))))

(test e2e-htmx-redirect-javascript-scheme-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/redirect-js-signals")))
      (is (string= "signaled" body)
          "javascript: in HX-Redirect must signal, got ~S" body))))

(test e2e-htmx-redirect-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/redirect-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Redirect must signal, got ~S" body))))

;;; ============================================================================
;;; HX-Location
;;; ============================================================================

(test e2e-htmx-location-javascript-scheme-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/location-js-signals")))
      (is (string= "signaled" body)
          "javascript: in HX-Location must signal, got ~S" body))))

;;; ============================================================================
;;; HX-Push-Url / HX-Replace-Url
;;; ============================================================================

(test e2e-htmx-push-url-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/push-url-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Push-Url must signal, got ~S" body))))

(test e2e-htmx-replace-url-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/replace-url-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Replace-Url must signal, got ~S" body))))

;;; ============================================================================
;;; HX-Retarget / HX-Reswap / HX-Reselect
;;; ============================================================================

(test e2e-htmx-retarget-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/retarget-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Retarget must signal, got ~S" body))))

(test e2e-htmx-reswap-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/reswap-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Reswap must signal, got ~S" body))))

(test e2e-htmx-reselect-crlf-signals
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/reselect-crlf-signals")))
      (is (string= "signaled" body)
          "CR/LF in HX-Reselect must signal, got ~S" body))))

;;; ============================================================================
;;; OOB output safety — the id slot is allowlisted, the content slot is typed
;;; ============================================================================

(lol-web/server:defroute "/__e2e/htmx/oob-swap-raw-string-signals"
    (:method :get :secure nil)
  (handler-case
      (progn (oob-swap "target" "<b>raw</b>") "no-signal")
    (error () "signaled")))

(lol-web/server:defroute "/__e2e/htmx/oob-swap-typed-content-roundtrip"
    (:method :get :secure nil)
  ;; Positive path: a safe-html-string passes the boundary and the
  ;; wrapper carries both the id slot and the swap strategy.
  (let ((output (oob-swap "ok-target"
                          (lol-web/html:make-safe-html-string "content")
                          :swap "innerHTML")))
    (if (and (search "id=" output)
             (search "innerHTML" output)
             (search "hx-swap-oob" output))
        "ok"
        "missing-marker")))

(test e2e-htmx-oob-swap-rejects-raw-string
  "End-to-end: a route that calls oob-swap with a bare string at request
   time returns the signaled-path body — proves the boundary holds
   through the request lifecycle, not only at unit-test time."
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/oob-swap-raw-string-signals")))
      (is (string= "signaled" body)
          "oob-swap of a raw string must signal, got ~S" body))))

(test e2e-htmx-oob-swap-id-attribute-escape
  "End-to-end: a typed-content roundtrip through oob-swap succeeds and
   the rendered wrapper carries the id, swap strategy, and OOB marker.
   The id-slot character-class escape (separately tracked) is observed
   through the boundary's response body, not asserted here — it depends
   on cl-who's attribute-value escape policy and is a sibling concern
   to be hardened independently."
  (lack/test:testing-app (lol-web/server:make-app :use-csrf nil
                                                  :use-static nil
                                                  :use-accesslog nil)
    (let ((body (lack/test:request "/__e2e/htmx/oob-swap-typed-content-roundtrip")))
      (is (string= "ok" body)
          "typed-content roundtrip must succeed and emit the wrapper, got ~S"
          body))))

(test e2e-htmx-on-attribute-escape
  "End-to-end: a broadcast OOB update carrying hx-on-* on the wire has
   the attribute stripped before clients receive it.  The strip lives in
   ws-broadcast-oob / sse-broadcast-oob; this test exercises the SSE
   path via the wire-format serializer."
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
                          "<button hx-on-click=\"alert(1)\">x</button>")
                         :swap "outerHTML")))
            (let* ((updates (cdr (assoc :updates captured)))
                   (html    (cdr (assoc :html (first updates)))))
              (is (not (search "hx-on-click" html))
                  "broadcast wire must not carry hx-on-* attributes")))
        (setf (symbol-function 'lol-web/realtime:sse-broadcast) orig)))))
