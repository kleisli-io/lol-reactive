;;;; LOL-WEB streaming-gate middleware
;;;; Enforces per-entry origin + auth policy plus a shared rate-limit on
;;;; every request that matches *streaming-routes*. Streaming dispatch
;;;; happens inside route-handler ahead of with-response-headers /
;;;; with-error-handling, so the upgrade is reached before any other
;;;; middleware can deny. streaming-gate closes that gap by sitting
;;;; innermost in the make-app chain, looking up the matched
;;;; streaming-route-entry, and denying with 401 / 403 / 429 before the
;;;; handler body runs. Non-streaming requests pass through untouched.

(in-package :lol-web/server)

(defun %streaming-deny (status reason)
  "Build a stand-alone Clack response triple for a streaming-gate denial.
   Does not touch *response-headers* / *env* / *path-params* — middleware
   runs outside the dynamic extent route-handler binds, so the triple has
   to be self-contained."
  (list status
        (list :content-type "text/plain; charset=utf-8")
        (list reason)))

(defun streaming-gate (app &key (max-requests 100)
                                (window-seconds 60)
                                (namespace :streaming))
  "Lack middleware: gate streaming-route requests on origin allowlist, auth
   callable, and per-IP rate limit before route-handler dispatches them.
   Non-streaming requests pass through with no policy lookup.

   Origin is matched verbatim by validate-origin against the entry's
   :origin list; empty list denies every request. An entry that sets
   :bearer-token (T or a one-arg token predicate) admits non-browser
   clients lacking Origin/Referer when they present Authorization: Bearer
   <T>; browser traffic carrying Origin still goes through the verbatim
   allowlist match. Auth is the entry's one-arg callable
   (env -> generalised boolean); NIL denies. Rate limit uses NAMESPACE
   (default :streaming, disjoint from :ip so flood traffic on /ws/* cannot
   evict :ip-bucket entries belonging to ordinary HTTP traffic). All three
   checks run before the handler body so a forged upgrade is rejected
   without ever touching the WebSocket / SSE driver."
  (lambda (env)
    (let* ((method (getf env :request-method))
           (path (getf env :path-info))
           (match (find-matching-streaming-route method path)))
      (cond
        ((null match)
         (funcall app env))
        (t
         (let* ((entry (car match))
                (allowed-origins (streaming-route-entry-origin entry))
                (auth-fn (streaming-route-entry-auth entry))
                (bearer-token (streaming-route-entry-bearer-token entry))
                (*env* env))
           (cond
             ((not (validate-origin :allowed-origins allowed-origins
                                    :bearer-token bearer-token))
              (%streaming-deny 403 "Origin not allowed"))
             ((not (funcall auth-fn env))
              (%streaming-deny 401 "Authentication required"))
             (t
              (let ((ip (client-ip)))
                (cond
                  ((or (null ip)
                       (and (stringp ip) (zerop (length ip))))
                   (%streaming-deny 400 "Client IP unavailable"))
                  ((not (check-rate-limit ip
                                          :max-requests max-requests
                                          :window-seconds window-seconds
                                          :namespace namespace))
                   (%streaming-deny 429 "Too many requests"))
                  (t
                   ;; Stamp the env so route-handler can prove this streaming
                   ;; entry's per-entry origin/auth/rate-limit policy was
                   ;; actually enforced before it dispatches the body. The key
                   ;; is internal and never set from request input.
                   (setf (getf env :lol-web.streaming.vetted) t)
                   (funcall app env))))))))))))
