;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/SERVER; Base: 10 -*-
;;;; Lack application builder and Clack integration
;;;;
;;;; Provides a composable application builder using Lack middleware,
;;;; abstracting away direct Hunchentoot usage.

(in-package :lol-web/server)

;;; ============================================================================
;;; ROUTE REGISTRY
;;; ============================================================================

(defvar *routes* (make-hash-table :test 'equal)
  "Route registry mapping (method . path) to handler functions.
   Methods are keywords (:GET, :POST, etc.), paths are strings.
   Handlers in *routes* take zero arguments and read state from *env*.")

(defvar *streaming-routes* (make-hash-table :test 'equal)
  "Streaming-route registry mapping (method . path) to streaming-route-entry
   structs. Each entry carries the handler body plus a declarative policy
   (auth callable + allowed-origins list) that streaming-gate reads to gate
   the upgrade. Populated by defstreaming-route, defws, defsse — never by
   defroute.

   Keeping these two registries disjoint replaces the prior sb-introspect
   heuristic (which was non-portable and silently misclassified handlers
   on non-SBCL implementations).")

(defvar *routes-lock* (bordeaux-threads:make-recursive-lock "lol-web/server routes")
  "Guards *routes* and *streaming-routes*. Plain hash-tables race under
   concurrent registration (multi-file load, hot-reload from a worker, parallel
   defroute calls). Recursive so find-matching-route's HEAD-as-GET fallback can
   chain a second %lookup-route without releasing.")

(defstruct streaming-route-entry
  "Declarative policy + body for a streaming route. BODY is a one-arg
   function (env -> Clack response) owning the connection lifecycle.
   AUTH is a one-arg predicate (env -> generalised boolean); streaming-gate
   denies with 401 when it returns NIL. ORIGIN is a list of allowed origin
   strings matched verbatim by validate-origin; an empty list denies every
   request. Both AUTH and ORIGIN are required at construction time so a
   misconfigured streaming endpoint cannot reach the dispatcher.

   BEARER-TOKEN is the non-browser escape: T to accept any non-empty
   Authorization: Bearer <T> (deferring validation to AUTH), or a one-arg
   predicate (token -> generalised boolean) that validates the token
   value directly. NIL (default) disables the escape so behaviour is
   unchanged for browser-only endpoints."
  body
  auth
  origin
  (bearer-token nil))

(defun %require-streaming-policy (kind auth origin)
  "Signal when AUTH or ORIGIN is missing for KIND (e.g., \"make-ws-handler\").
   Fail-closed at construction time so no streaming endpoint ships without
   an explicit per-route policy. ORIGIN must be a non-empty list of strings
   — an empty allowlist is a useless policy that would deny every request
   silently, so we surface it at construction instead."
  (unless (or (functionp auth) (and (symbolp auth) (fboundp auth)))
    (error "~A: :auth is required and must be a callable returning generalised ~
            boolean; got ~S (~S)"
           kind auth (type-of auth)))
  (unless (and (consp origin) (every #'stringp origin))
    (error "~A: :origin is required and must be a non-empty list of allowed ~
            origin strings (RFC 6454 verbatim); got ~S (~S)"
           kind origin (type-of origin))))

(defmacro! defstreaming-route (o!path (&key (method :get) auth origin) (env-var)
                                       &body body)
  "Register a streaming route: handler runs with ENV-VAR bound to the Clack
   env plist and is responsible for the response (WebSocket upgrade, SSE
   stream, long-poll, etc.). Unlike defroute, no with-response-headers /
   with-error-handling wrapper — streaming handlers manage their own headers
   and lifecycle.

   :AUTH and :ORIGIN are required — they declare the policy that
   streaming-gate enforces before the upgrade runs. :AUTH is a one-arg
   callable (env -> generalised boolean); :ORIGIN is a list of allowed
   origin strings (empty list denies).

   PATH and METHOD are each evaluated exactly once at registration time."
  (let ((handler-name (gensym "STREAMING-HANDLER-")))
    `(let ((,g!method ,method)
           (,g!auth ,auth)
           (,g!origin ,origin))
       (%require-streaming-policy "defstreaming-route" ,g!auth ,g!origin)
       (defun ,handler-name (,env-var) ,@body)
       (bordeaux-threads:with-recursive-lock-held (*routes-lock*)
         (setf (gethash (cons ,g!method ,g!path) *streaming-routes*)
               (make-streaming-route-entry :body #',handler-name
                                           :auth ,g!auth
                                           :origin ,g!origin)))
       (values ,g!path ,g!method))))

(defun clear-routes ()
  "Clear all registered routes (regular + streaming)."
  (bordeaux-threads:with-recursive-lock-held (*routes-lock*)
    (clrhash *routes*)
    (clrhash *streaming-routes*)))

(defun list-routes ()
  "List all registered routes (regular + streaming) as list of (method path) pairs."
  (bordeaux-threads:with-recursive-lock-held (*routes-lock*)
    (append
      (loop for key being the hash-keys of *routes*
            collect (list (car key) (cdr key)))
      (loop for key being the hash-keys of *streaming-routes*
            collect (list (car key) (cdr key))))))

;;; ============================================================================
;;; PATH PARAMETER SUPPORT
;;; ============================================================================

(defvar *path-params* nil
  "Alist of path parameters extracted from route matching.
   Bound during request handling for routes with :param segments.")

(defun path-param (name)
  "Get a path parameter value by name (string).
   Returns nil if parameter not found."
  (cdr (assoc name *path-params* :test #'string=)))

(define-condition unsafe-path-segment (error)
  ((segment :initarg :segment :reader unsafe-path-segment-segment))
  (:report
   (lambda (c stream)
     (format stream "Unsafe path segment: ~S"
             (unsafe-path-segment-segment c)))))

(defun safe-path-segment-p (segment)
  "Return T when SEGMENT is safe to expose as a path parameter."
  (and (stringp segment)
       (plusp (length segment))
       (not (member segment '("." "..") :test #'string=))
       (not (find-if (lambda (c)
                       (or (char= c #\/)
                           (char= c #\\)
                           (char= c #\Nul)))
                     segment))
       (let ((lower (string-downcase segment)))
         (not (or (search "%2e" lower)
                  (search "%2f" lower)
                  (search "%5c" lower))))))

(defun safe-path-segment (segment)
  "Return SEGMENT when safe, otherwise signal UNSAFE-PATH-SEGMENT."
  (unless (safe-path-segment-p segment)
    (error 'unsafe-path-segment :segment segment))
  segment)

(defun path-pattern-p (path)
  "Check if path contains parameter or splat segments (e.g., :slug, *rest)."
  (and (stringp path)
       (or (find #\: path)
           (find #\* path))))

(defun match-path-pattern (pattern request-path)
  "Match a path pattern against a request path.
   Pattern segments starting with `:` are single-segment parameters.
   A trailing segment starting with `*` is a splat that captures all
   remaining path segments joined by `/`; the splat must be the final
   pattern segment and matches one or more remaining segments.
   Returns alist of (name . value) if match, nil otherwise.
   Examples:
     (match-path-pattern \"/users/:id\" \"/users/123\")
       => ((\"id\" . \"123\"))
     (match-path-pattern \"/a/:b/*c\" \"/a/x/y/z\")
       => ((\"b\" . \"x\") (\"c\" . \"y/z\"))"
  (let* ((pattern-segments (remove "" (uiop:split-string pattern :separator "/") :test #'string=))
         (path-segments (remove "" (uiop:split-string request-path :separator "/") :test #'string=))
         (last-pat (car (last pattern-segments)))
         (splat-p (and last-pat
                       (> (length last-pat) 0)
                       (char= (char last-pat 0) #\*))))
    (cond
      ;; Splat pattern: every non-splat segment matches one-to-one with the
      ;; leading portion of the path; the splat captures the rest (at least
      ;; one segment) joined by `/`.
      (splat-p
       (let ((fixed (butlast pattern-segments)))
         (when (>= (length path-segments) (1+ (length fixed)))
           (let ((params nil)
                 (ok t))
             (loop for pat in fixed
                   for seg in path-segments
                   while ok
                   do (cond
                        ((and (> (length pat) 0) (char= (char pat 0) #\:))
                         (if (safe-path-segment-p seg)
                             (push (cons (subseq pat 1) seg) params)
                             (setf ok nil)))
                        ((string= pat seg) nil)
                        (t (setf ok nil))))
             (when ok
               (let ((rest (nthcdr (length fixed) path-segments)))
                 (when (every #'safe-path-segment-p rest)
                   (push (cons (subseq last-pat 1)
                               (format nil "~{~A~^/~}" rest))
                         params)
                   (nreverse params))))))))
      ;; Strict-arity pattern: segment counts must match exactly.
      ((= (length pattern-segments) (length path-segments))
       (loop with params = nil
             for pat in pattern-segments
             for seg in path-segments
             do (cond
                  ((and (> (length pat) 0) (char= (char pat 0) #\:))
                   (unless (safe-path-segment-p seg)
                     (return nil))
                   (push (cons (subseq pat 1) seg) params))
                  ((string= pat seg) nil)
                  (t (return nil)))
             finally (return (nreverse params)))))))

(defun %lookup-route (table method request-path)
  "Lookup METHOD/REQUEST-PATH in TABLE. Returns (handler . path-params) or NIL.
   All reads happen under *routes-lock* so concurrent registration cannot
   corrupt iteration state mid-request."
  (bordeaux-threads:with-recursive-lock-held (*routes-lock*)
    (let ((exact (gethash (cons method request-path) table)))
      (when exact
        (return-from %lookup-route (cons exact nil))))
    (loop for key being the hash-keys of table using (hash-value handler)
          for (route-method . route-path) = key
          when (and (eq route-method method)
                    (path-pattern-p route-path))
          do (let ((params (match-path-pattern route-path request-path)))
               (when params
                 (return (cons handler params)))))))

(defun find-route-for-method (method request-path)
  "Lookup a regular handler registered for METHOD on REQUEST-PATH.
   Returns (handler . path-params) or NIL."
  (%lookup-route *routes* method request-path))

(defun find-streaming-route-for-method (method request-path)
  "Lookup a streaming handler registered for METHOD on REQUEST-PATH.
   Returns (handler . path-params) or NIL."
  (%lookup-route *streaming-routes* method request-path))

(defun find-matching-route (method request-path)
  "Find a regular route handler that matches the request.
   First tries exact match, then pattern matching.
   HEAD requests fall back to GET handlers per RFC 7231 §4.3.2 — body is
   stripped by `route-handler' so headers stay identical to GET.
   Returns (handler . path-params) or nil."
  (or (find-route-for-method method request-path)
      (when (eq method :head)
        (find-route-for-method :get request-path))))

(defun find-matching-streaming-route (method request-path)
  "Find a streaming route handler that matches the request.
   Streaming routes do not respect HEAD-as-GET — a HEAD against a
   WebSocket endpoint should 405, not run the upgrade handler."
  (find-streaming-route-for-method method request-path))

;;; ============================================================================
;;; ROUTE DISPATCHER
;;; ============================================================================

(defvar *before-handler-hook* nil
  "Optional zero-arg function called within `with-response-headers' scope
   before each regular route handler runs.  Use `add-response-header' inside
   the hook to inject app-wide response headers (e.g. discovery affordances).
   Skipped on the streaming-handler path (WebSocket/SSE).")

(defvar *before-server-start-hook* nil
  "List of zero-arg functions run by START-SERVER before the Hunchentoot
   acceptor binds its port. Use to validate ambient state (registries,
   resolvable extractors, etc.) before requests can hit.

   Registration: (pushnew #'fn lol-web/server:*before-server-start-hook*)
   Execution:    (mapc #'funcall lol-web/server:*before-server-start-hook*)

   Ordering between registrants is unspecified — each fn must be
   order-independent and safely re-runnable. If a fn signals a condition,
   START-SERVER propagates it; the server does NOT come up.

   Distinct from *BEFORE-HANDLER-HOOK* (per-request) — different lifetimes,
   different intents. :LOL-WEB/EXTRACTORS pushes a sentinel onto this hook
   at file load time so DEFHANDLER references to unregistered KIND values
   are caught at startup rather than first-request.")

(defun strip-body-for-head (response)
  "Return RESPONSE with body removed if present.  Used to satisfy RFC 7231
   §4.3.2 for HEAD requests routed to GET handlers — same status, same
   headers, no body."
  (cond
    ;; Standard (status headers body) Clack response.
    ((and (consp response) (>= (length response) 2))
     (list (first response) (second response) nil))
    ;; Delayed/function response — leave alone; the underlying handler is
    ;; responsible for not streaming on HEAD.
    (t response)))

(defun route-handler (env)
  "Main route dispatcher for Clack.
   Streaming routes (WebSocket/SSE) are looked up first in *streaming-routes*
   and dispatched by extracting the streaming-route-entry body, which owns
   the connection lifecycle. streaming-gate runs ahead of this dispatcher
   to enforce per-entry origin / auth / rate-limit policy.
   Regular routes are looked up in *routes* and called with no arguments;
   the handler reads request state via the *env* dynamic binding.
   Path parameters (/users/:id) are bound via *path-params* in both cases."
  (lol-web/core:with-reactive-context
    (let* ((*env* env)
           (path (request-path))
           (method (request-method))
           (streaming-match (find-matching-streaming-route method path))
           (response
             (cond
               (streaming-match
                ;; Fail-closed at the dispatcher: only dispatch a streaming
                ;; entry whose per-entry origin/auth/rate-limit policy the
                ;; streaming-gate vetted (it stamps :lol-web.streaming.vetted
                ;; on the env). A request that reaches here ungated — gate
                ;; disabled, or make-app bypassed — is refused rather than
                ;; dispatched with no policy enforcement.
                (if (getf env :lol-web.streaming.vetted)
                    (let* ((*path-params* (cdr streaming-match))
                           (entry (car streaming-match)))
                      (funcall (streaming-route-entry-body entry) env))
                    (error-response 403 :message "Streaming route requires the streaming gate")))
               (t
                (let ((match (find-matching-route method path)))
                  (if match
                      (let ((*path-params* (cdr match))
                            (handler (car match)))
                        (with-response-headers ()
                          (when *before-handler-hook*
                            (funcall *before-handler-hook*))
                          (with-error-handling (format nil "~A ~A" method path)
                            (funcall handler))))
                      (error-response 404 :message "Not Found")))))))
      (if (eq method :head)
          (strip-body-for-head response)
          response))))

;;; ============================================================================
;;; APPLICATION BUILDER
;;; ============================================================================

(defun %make-auth-hooks-middleware (auth)
  "Build a Lack middleware that injects :lol-web.auth.hooks into *env*
   per request. The (authp-thunk . principal-thunk) cons is captured in
   this closure once at make-app time, so two make-app calls in one image
   carry independent conses — neither can clobber the other's hooks.
   Returns NIL when AUTH is NIL or holds no thunks, so make-app installs
   no middleware at all (with-auth then fail-closes)."
  (let* ((authp (getf auth :authenticated-p))
         (principal (getf auth :current-principal))
         (hooks (when (or authp principal)
                  (cons authp principal))))
    (when hooks
      (lambda (app)
        (lambda (env)
          (setf (getf env :lol-web.auth.hooks) hooks)
          (funcall app env))))))

(defun %make-hydration-hooks-middleware (secret-key)
  "Inject :LOL-WEB.FULLSTACK.HYDRATION-KEY into *env* per request. The
   octet-vector key is closure-captured once at make-app time so two apps
   in one image carry independent keys. Returns NIL when SECRET-KEY is
   NIL — verify-hydration-state then fail-closes (:NO-KEY)."
  (when secret-key
    (lambda (app)
      (lambda (env)
        (setf (getf env :lol-web.fullstack.hydration-key) secret-key)
        (funcall app env)))))

(defun %make-jschema-registry-middleware (registry-table)
  "Let-bind lol-web/jschema:*registry* per-request to a hash-table
   closure-captured at make-app time. Two apps in one image then
   register schemas into disjoint URI namespaces. Returns NIL when
   REGISTRY-TABLE is NIL — schema operations fall through to the
   image-global default *registry*. Delegates the dynamic binding to
   lol-web/jschema:call-with-registry so the let is established inside
   the package that owns the special."
  (when registry-table
    (lambda (app)
      (lambda (env)
        (lol-web/jschema:call-with-registry
          registry-table
          (lambda () (funcall app env)))))))

(define-condition middleware-order-error (error)
  ((order  :initarg :order  :reader middleware-order-error-order)
   (reason :initarg :reason :reader middleware-order-error-reason))
  (:report
   (lambda (c stream)
     (format stream "Invalid middleware composition order ~S: ~A"
             (middleware-order-error-order c)
             (middleware-order-error-reason c))))
  (:documentation
   "Signalled by %ASSERT-MIDDLEWARE-ORDER at MAKE-APP build time when the
    resolved middleware order breaks a security invariant — so an inverted
    composition fails construction instead of 500ing a production request."))

(defun app-middleware-order (&key use-cors use-static use-assets use-accesslog
                                  use-session use-csrf auth-present
                                  hydration-present registry-present
                                  use-streaming-gate)
  "Return the resolved middleware dispatch order for an app built with the
   given toggles: a list of layer-name keywords from outermost to innermost,
   ending in :ROUTE-HANDLER. This is the single source of truth for
   middleware composition order — MAKE-APP folds its middleware in exactly
   this order and asserts the security invariants against it at build time.

   AUTH-PRESENT / HYDRATION-PRESENT / REGISTRY-PRESENT are the resolved
   booleans for the optional closure-captured middlewares (a NIL :auth plist
   installs no auth layer, etc.)."
  (append
   (remove nil
           (list (and use-cors :cors)
                 (and use-static :static)
                 ;; :assets shares the static tier — outside session/csrf —
                 ;; so immutable, publicly cacheable assets never carry a
                 ;; Set-Cookie a shared cache could leak across users.
                 (and use-assets :assets)
                 (and use-accesslog :accesslog)
                 (and use-session :session)
                 (and use-csrf :csrf)
                 (and auth-present :auth)
                 (and hydration-present :hydration)
                 (and registry-present :registry)
                 (and use-streaming-gate :streaming-gate)))
   (list :route-handler)))

(defun %assert-middleware-order (order)
  "Signal MIDDLEWARE-ORDER-ERROR unless ORDER (the APP-MIDDLEWARE-ORDER
   shape: outermost-first, ending in :ROUTE-HANDLER) satisfies the security
   invariants: when both are present, :SESSION wraps :CSRF so :lack.session
   is populated before csrf-middleware reads it; and :STREAMING-GATE, when
   present, is innermost (immediately wrapping :ROUTE-HANDLER) so it sees a
   fully-populated env. Returns ORDER on success."
  (let ((sp (position :session order))
        (cp (position :csrf order))
        (gp (position :streaming-gate order))
        (rp (position :route-handler order)))
    (when (and sp cp (> sp cp))
      (error 'middleware-order-error :order order
             :reason ":csrf wraps :session — csrf-middleware would read ~
                      :lack.session before the session middleware sets it"))
    (when (and gp rp (/= gp (1- rp)))
      (error 'middleware-order-error :order order
             :reason ":streaming-gate is not innermost — it must immediately ~
                      wrap :route-handler")))
  order)

(defun make-app (&key (static-path "/static/")
                      (static-root #P"static/")
                      (use-session t)
                      (use-csrf t)
                      (use-accesslog t)
                      (use-static t)
                      (use-assets t)
                      (use-cors nil)
                      (cors-origin nil)
                      (cors-methods nil)
                      (cors-headers nil)
                      (use-streaming-gate t)
                      (streaming-rate-limit '(:max-requests 100
                                              :window-seconds 60
                                              :namespace :streaming))
                      (auth nil)
                      (hydration-secret-key nil)
                      (schema-registry nil)
                      (lock-debug-mode-p nil)
                      (rate-limit-namespaces '((:ip :max-entries 10000)
                                               (:login :max-entries 1000)
                                               (:streaming :max-entries 10000)))
                      (rate-limit-min-evict-age 60)
                      (rate-limit-eviction-every-n 64)
                      (rate-limit-eviction-interval 30)
                      (session-cookie-secure t)
                      (session-cookie-httponly t)
                      (session-cookie-samesite :lax))
  "Create a Lack application with configurable middleware stack.

   Middleware (applied bottom-up):
   - Session: Memory-backed session management (optional)
   - Auth-hooks: per-request injection of the (authp . principal) cons
     captured from :AUTH (optional, only installed when :AUTH supplies
     thunks)
   - CSRF: Cross-site request forgery protection (optional, requires session)
   - Accesslog: Request logging (optional)
   - Static: Static file serving (optional)
   - CORS: Access-Control-Allow-Origin + OPTIONS preflight (optional;
     short-circuits OPTIONS to 204 inside the middleware)

   CORS-ORIGIN / CORS-METHODS / CORS-HEADERS configure the CORS middleware
   when :USE-CORS is T. CORS-ORIGIN is the Access-Control-Allow-Origin value
   and is REQUIRED whenever :USE-CORS is T — there is no default. A wildcard
   is never implicit: pass :CORS-ORIGIN \"*\" to opt into the fully-open
   policy deliberately (the same explicit-opt-in discipline as
   *TRUSTED-PROXIES*). CORS-METHODS / CORS-HEADERS default to the
   middleware's own values when NIL.

   RATE-LIMIT-NAMESPACES configures the per-namespace bounded stores in
   *rate-limit-registry*. Each entry is (NAMESPACE-KEYWORD &key MAX-ENTRIES).
   Default carve-outs: :ip capped at 10000 entries and :login capped at
   1000. Repeated make-app calls update caps without dropping in-flight
   entries (idempotent via configure-rate-limit-namespace). A flood in
   one namespace cannot evict entries in another because the bounded
   stores are disjoint.

   RATE-LIMIT-MIN-EVICT-AGE (seconds, default 60) sets the fairness
   floor: an entry whose last-seen falls within this window is never
   evicted by the LRU pass. When the store hits cap and every candidate
   is too fresh, the inbound request is denied (rate-limit-store-full
   signalled) rather than dropping a legitimate user.

   RATE-LIMIT-EVICTION-EVERY-N (default 64; NIL disables) runs an
   amortised eviction sweep after every Nth allowed insert per
   namespace, keeping stores under cap before the next cap-hit forces a
   synchronous sweep.

   RATE-LIMIT-EVICTION-INTERVAL (seconds, default 30; NIL disables)
   arms an image-wide background thread that sweeps every namespace at
   the given cadence. Each make-app call reconciles the timer — the
   prior thread is stopped, the new one is started at the new interval.

   AUTH installs an opaque auth-hook thunk pair into per-request env via
   a middleware closure unique to this app. It is mechanism, not policy
   — the framework calls the thunks but never inspects what they return.
   - NIL (default) — no auth middleware; with-auth fail-closes.
   - plist (:authenticated-p FN :current-principal FN) — install both
     thunks (either may be NIL; missing authenticated-p fail-closes).
   The thunks are captured lexically; two apps in one image hold
   independent (authp . principal) conses with no shared mutable state.
   with-auth and current-principal read the cons via
   (getf *env* :lol-web.auth.hooks).

   HYDRATION-SECRET-KEY is an octet vector closure-captured into a
   middleware that exposes it on *env* as :LOL-WEB.FULLSTACK.HYDRATION-KEY.
   Endpoints requiring signed hydration envelopes (notably :SET-STATE on
   the component-API) verify against it. NIL (default) installs no
   middleware; verify-hydration-state then fail-closes with :NO-KEY and
   :set-state requests are refused.

   SCHEMA-REGISTRY is a hash-table closure-captured into a middleware that
   let-binds lol-web/jschema:*registry* per request. Two apps configured
   with distinct hash-tables hold disjoint URI namespaces in the same
   image. NIL (default) leaves the image-global *registry* in place.

   LOCK-DEBUG-MODE-P, when T, calls LOCK-DEBUG-MODE at app boot so that
   subsequent writes through SET-DEBUG-MODE (and the ENABLE/DISABLE
   wrappers) signal DEBUG-MODE-LOCKED-ERROR. Engaging the lock once is
   enough — it is one-way and image-global.

   Session cookie hardening defaults (only meaningful when use-session is T):
   - :session-cookie-secure T — cookie only sent over HTTPS. Pass NIL for
     local-dev over plain HTTP, otherwise the browser drops the cookie.
   - :session-cookie-httponly T — JavaScript cannot read the cookie.
   - :session-cookie-samesite :LAX — sent on top-level navigation only.

   Signals an error when (and use-csrf (not use-session)) — the CSRF
   middleware reads :lack.session from *env* and silently no-ops
   otherwise, which would yield a CSRF stack that protects nothing.

   Returns a function suitable for Clack:clackup."
  (when (and use-csrf (not use-session))
    (error ":use-csrf t requires :use-session t — the CSRF middleware reads ~
            :lack.session from the request env and silently no-ops when no ~
            session middleware is installed, leaving CSRF unprotected. ~
            Pass :use-csrf nil if you intentionally want neither."))
  (when (and use-cors (null cors-origin))
    (error ":use-cors t requires an explicit :cors-origin. CORS otherwise ~
            emitted Access-Control-Allow-Origin: * by default, silently ~
            opening cross-origin reads of every non-OPTIONS response. Pass a ~
            specific origin (e.g. \"https://app.example.com\"), or \"*\" to ~
            opt into the fully-open policy deliberately."))
  ;; Validate handler/extractor wiring before building anything, so an app
  ;; built with make-app and handed straight to clackup fails fast — exactly
  ;; as start-server does — rather than 500ing on its first request.
  (mapc #'funcall *before-server-start-hook*)
  ;; Engage one-way *debug-mode* lock if requested. Idempotent on repeat.
  (when lock-debug-mode-p
    (lock-debug-mode))
  ;; Configure rate-limit namespaces (idempotent — preserves in-flight
  ;; entries; only updates per-namespace caps).
  (dolist (cfg rate-limit-namespaces)
    (destructuring-bind (ns &key (max-entries 10000)) cfg
      (configure-rate-limit-namespace ns :max-entries max-entries)))
  ;; Rate-limit fairness + amortised eviction knobs are image-global
  ;; defparameters; last make-app wins. configure-rate-limit-eviction-timer
  ;; stops any prior thread and starts a fresh one at the new interval.
  ;;
  ;; Accepted limitation: the eviction *policy* (these three knobs + the
  ;; single background sweep thread) is process-wide, so two apps in one
  ;; image share one fairness configuration — the later make-app's values
  ;; win. The rate-limit *stores* are partitioned per namespace and remain
  ;; isolated regardless, so this affects only multi-app eviction fairness,
  ;; never correctness or cross-app counter leakage; single-app deployments
  ;; (the common case) are unaffected. Per-app policy isolation would need
  ;; per-namespace partitioning of both the knobs and the timer — and two
  ;; apps using the default :ip/:login namespaces would still collide — so
  ;; it is out of proportion to the impact. Left as a known, accepted gap.
  (setf *rate-limit-min-evict-age*     rate-limit-min-evict-age)
  (setf *rate-limit-eviction-every-n*  rate-limit-eviction-every-n)
  (setf *rate-limit-eviction-interval* rate-limit-eviction-interval)
  (configure-rate-limit-eviction-timer)
  ;; Build middleware wrappers keyed by layer name. APP-MIDDLEWARE-ORDER is
  ;; the single source of truth for composition order; the fold below obeys
  ;; it and %ASSERT-MIDDLEWARE-ORDER gates the security invariants (csrf
  ;; inside session, streaming-gate innermost) at build time, so an inverted
  ;; order fails MAKE-APP rather than 500ing a production request.
  (let ((mw-table '())
        (auth-mw (%make-auth-hooks-middleware auth))
        (hydration-mw (%make-hydration-hooks-middleware hydration-secret-key))
        (registry-mw (%make-jschema-registry-middleware schema-registry)))
    ;; CORS: outermost — sees OPTIONS before any inner dispatch and wraps
    ;; responses last so its headers ride out on every reply. The configured
    ;; origin is threaded in (required above when use-cors); methods/headers
    ;; fall through to the middleware defaults when NIL.
    (when use-cors
      (let ((cors-mw (lack/util:find-middleware :cors)))
        (push (cons :cors
                    (lambda (app)
                      (apply cors-mw app
                             :origin cors-origin
                             (append (when cors-methods (list :methods cors-methods))
                                     (when cors-headers (list :headers cors-headers))))))
              mw-table)))
    ;; Static file serving (checked before app-level routing). The ancestry
    ;; wrapper sits outside Lack's :static middleware: requests whose resolved
    ;; file would escape static-root return 404 before the file is read.
    (when use-static
      (let ((static-mw (lack/util:find-middleware :static)))
        (push (cons :static
                    (lambda (app)
                      (%static-ancestry-wrapper
                       (funcall static-mw app :path static-path :root static-root)
                       static-path static-root)))
              mw-table)))
    ;; Content-addressed external assets (register-asset / lol-web:page).
    ;; Shares the :static tier so it short-circuits OUTSIDE session/csrf —
    ;; an immutable public asset must never carry a Set-Cookie.
    (when use-assets
      (push (cons :assets (lambda (app) (asset-middleware app))) mw-table))
    (when use-accesslog
      (push (cons :accesslog (lack/util:find-middleware :accesslog)) mw-table))
    ;; Session: wraps CSRF so it lands outer and runs first — csrf-middleware
    ;; reads (getf env :lack.session), so session must populate that slot
    ;; before csrf inspects it. Cookie state hardened by default (Secure /
    ;; HttpOnly / SameSite=Lax); override per call for local plain HTTP.
    (when use-session
      (let ((session-mw (lack/util:find-middleware :session)))
        (push (cons :session
                    (lambda (app)
                      (funcall session-mw app
                               :state (lack/session/state/cookie:make-cookie-state
                                       :secure session-cookie-secure
                                       :httponly session-cookie-httponly
                                       :samesite session-cookie-samesite))))
              mw-table)))
    ;; CSRF (requires session, which wraps it). We wire our own
    ;; csrf-middleware instead of Lack's :csrf because the latter compares
    ;; tokens with #'equal — non-constant-time, leaks the matching prefix
    ;; length under a timing oracle. Ours uses constant-time-string= and
    ;; returns 403 to match with-csrf-validation.
    (when use-csrf
      (push (cons :csrf (lambda (app) (csrf-middleware app))) mw-table))
    ;; Auth-hooks: the last cross-cutting layer before per-route gating, so
    ;; with-auth and current-principal read :lol-web.auth.hooks from an env
    ;; already populated by session middleware.
    (when auth-mw
      (push (cons :auth auth-mw) mw-table))
    ;; Hydration-key injection. Alongside auth-hooks because both supply
    ;; closure-captured secrets to per-request gates; signed-envelope verify
    ;; in lol-web/fullstack reads :LOL-WEB.FULLSTACK.HYDRATION-KEY from *env*.
    (when hydration-mw
      (push (cons :hydration hydration-mw) mw-table))
    ;; Per-app jschema *registry* let-binding. Two apps with distinct tables
    ;; cannot collide URIs.
    (when registry-mw
      (push (cons :registry registry-mw) mw-table))
    ;; streaming-gate: innermost — runs immediately before route-handler so it
    ;; sees a fully-populated env and can enforce per-entry origin / auth /
    ;; rate-limit policy on the streaming-route dispatch path.
    (when use-streaming-gate
      (push (cons :streaming-gate
                  (lambda (app) (apply #'streaming-gate app streaming-rate-limit)))
            mw-table))
    (let ((order (%assert-middleware-order
                  (app-middleware-order
                   :use-cors use-cors :use-static use-static
                   :use-assets use-assets
                   :use-accesslog use-accesslog :use-session use-session
                   :use-csrf use-csrf :auth-present (and auth-mw t)
                   :hydration-present (and hydration-mw t)
                   :registry-present (and registry-mw t)
                   :use-streaming-gate use-streaming-gate))))
      ;; Fold innermost-first (reverse of the outermost-first ORDER, minus the
      ;; implicit :route-handler) so each layer wraps the already-wrapped
      ;; inner app, reproducing the documented dispatch order.
      (let ((wrapped (lack/component:to-app 'route-handler)))
        (dolist (name (reverse (butlast order)) wrapped)
          (let ((entry (assoc name mw-table)))
            (when entry
              (setf wrapped (funcall (cdr entry) wrapped)))))))))

;;; ============================================================================
;;; SERVER LIFECYCLE
;;; ============================================================================
;;; Note: defroute is defined in routes.lisp with enhanced security, error
;;; handling, and content-type support. The :lol-web/extractors sub-system
;;; layers defhandler on top of defroute.

(defvar *server* nil
  "Current Hunchentoot acceptor.")

(defvar *lack-app* nil
  "Current Lack application function.")

(defvar *client-socket* nil
  "Current client socket for streaming responses.")

;;; ----------------------------------------------------------------------------
;;; Clack-compatible client for streaming/WebSocket support
;;; ----------------------------------------------------------------------------

(defclass streaming-client ()
  ((stream :initarg :stream :reader client-stream)
   (socket :initarg :socket :reader client-socket)
   (read-callback :initform nil :accessor client-read-callback)
   (write-lock :initform (bordeaux-threads:make-lock "client-write")
               :reader client-write-lock)
   (write-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8)
                                            :adjustable t :fill-pointer 0)
                 :accessor client-write-buffer))
  (:documentation "Client wrapper for streaming responses and WebSocket."))

(defmethod clack.socket:read-callback ((client streaming-client))
  (client-read-callback client))

(defmethod (setf clack.socket:read-callback) (callback (client streaming-client))
  (setf (client-read-callback client) callback))

(defmethod clack.socket:write-sequence-to-socket ((client streaming-client) data &key callback)
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (let ((stream (client-stream client)))
      (write-sequence data stream)
      (force-output stream)))
  (when callback
    (funcall callback)))

(defmethod clack.socket:write-sequence-to-socket-buffer ((client streaming-client) data)
  "Buffer data for later flushing (used by WebSocket handshake)."
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (let ((buffer (client-write-buffer client)))
      (loop for byte across data
            do (vector-push-extend byte buffer)))))

(defmethod clack.socket:write-byte-to-socket-buffer ((client streaming-client) byte)
  "Buffer a single byte for later flushing."
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (vector-push-extend byte (client-write-buffer client))))

(defmethod clack.socket:close-socket ((client streaming-client))
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (finish-output (client-stream client))))

(defmethod clack.socket:flush-socket-buffer ((client streaming-client) &key callback)
  "Flush buffered data to the socket stream."
  (bordeaux-threads:with-lock-held ((client-write-lock client))
    (let ((buffer (client-write-buffer client))
          (stream (client-stream client)))
      (when (> (length buffer) 0)
        (write-sequence buffer stream)
        (setf (fill-pointer buffer) 0))
      (force-output stream)))
  (when callback
    (funcall callback)))

(defmethod clack.socket:socket-async-p ((client streaming-client))
  nil)

(defmethod clack.socket:socket-stream ((client streaming-client))
  (client-stream client))

;;; ----------------------------------------------------------------------------
;;; Lack Acceptor with streaming support
;;; ----------------------------------------------------------------------------

(defclass lack-acceptor (hunchentoot:easy-acceptor)
  ((app :initarg :app :accessor lack-app)
   (debug :initarg :debug :initform nil :accessor acceptor-debug))
  (:documentation "Hunchentoot acceptor that dispatches to a Lack app with streaming support."))

(defmethod hunchentoot:acceptor-log-message ((acceptor lack-acceptor) log-level format-string &rest format-arguments)
  "Filter out noisy connection errors from health check clients."
  (let ((message (apply #'format nil format-string format-arguments)))
    ;; Suppress connection-aborted and connection-reset errors (common with health checks)
    (unless (or (search "CONNECTION-ABORTED" message)
                (search "Connection reset by peer" message))
      (call-next-method))))

(defmethod hunchentoot:process-connection :around ((acceptor lack-acceptor) socket)
  "Capture client socket for streaming responses.
   Silently handles connection-aborted errors (common with health check clients)."
  (let ((*client-socket* socket))
    (handler-case (call-next-method)
      (usocket:connection-aborted-error ()
        ;; Client disconnected early - common with health checks, not an error
        nil))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor lack-acceptor) request)
  "Dispatch request through Lack app with streaming/delayed response support."
  (let ((app (lack-app acceptor)))
    (if app
        (handler-case
            (let* ((env (build-clack-env request))
                   (response (if (acceptor-debug acceptor)
                                 (funcall app env)
                                 (handler-case (funcall app env)
                                   (error (e)
                                     ;; Last-resort handler for errors escaping
                                     ;; middleware.  Route errors are caught by
                                     ;; with-error-handling in route-handler;
                                     ;; this only fires for middleware failures.
                                     (log-error (format nil "~A ~A (middleware)"
                                                        (getf env :request-method)
                                                        (getf env :path-info))
                                                e)
                                     (list 500
                                           '(:content-type "text/html; charset=utf-8")
                                           (list (handler-case
                                                     (render-error-page e :context "middleware")
                                                   (error ()
                                                     "Internal Server Error")))))))))
              (handle-lack-response response))
          (request-body-too-large (c)
            ;; The body cap fires inside build-clack-env, before the handler
            ;; chain has run. Short-circuit to 413 without touching the
            ;; route registry or invoking middleware (which would itself
            ;; try to consume the body).
            (handle-normal-response
             (list 413
                   '(:content-type "text/plain; charset=utf-8")
                   (list (format nil "Request body exceeds ~D-byte cap."
                                 (request-body-too-large-limit c)))))))
        (call-next-method))))

(defun handle-lack-response (response)
  "Handle Lack response - either normal (status headers body) or delayed (function)."
  (etypecase response
    (list (handle-normal-response response))
    (function (funcall response #'handle-normal-response))))

(defun %validate-response-triple-headers (headers)
  "Pass every (key value) pair in HEADERS through `validate-header-value'.
   Signals when any name or value carries CR/LF/NUL or other forbidden
   bytes — handler-returned response triples are the third header-emit
   path (alongside `add-response-header' and `redirect-response') and
   share the same trust boundary."
  (loop for (key val) on headers by #'cddr
        for name-string = (etypecase key
                            (keyword (symbol-name key))
                            (string  key))
        do (validate-header-value name-string)
           ;; CONTENT-LENGTH and similar numeric headers arrive as integers
           ;; from upstream middleware; only string values pass through the
           ;; CR/LF guard. Non-string non-integer values fail loud.
           (typecase val
             (string  (validate-header-value val))
             (integer nil)
             (null    nil)
             (t (error "%validate-response-triple-headers: header ~S has ~
                        unsupported value type ~S (~S)"
                       key (type-of val) val)))))

(defun handle-normal-response (response)
  "Handle a normal (status headers body) response."
  (destructuring-bind (status headers &optional body) response
    (%validate-response-triple-headers headers)
    (setf (hunchentoot:return-code*) status)
    ;; Emit headers.  A header key may appear more than once in the plist
    ;; — e.g. a global before-handler hook adds `Link: </llms.txt>; rel=
    ;; \"llms-txt\"` and a per-route handler adds another `Link` entry for
    ;; an alternate representation.  Per RFC 7230 §3.2.2 / RFC 8288 these
    ;; must be preserved as separate header lines (or comma-joined), not
    ;; collapsed.  `(setf hunchentoot:header-out)` has replace semantics,
    ;; so we use it only for the first occurrence and append subsequent
    ;; occurrences via the same `rplacd` pattern as `:set-cookie`.
    (let ((seen (make-hash-table :test 'eq)))
      (loop for (key val) on headers by #'cddr
            if (eq key :content-type)
              do (setf (hunchentoot:content-type*) val)
            else if (eq key :content-length)
              do (setf (hunchentoot:content-length*) val)
            else if (or (eq key :set-cookie) (gethash key seen))
              do (rplacd (last (hunchentoot:headers-out*))
                         (list (cons key val)))
            else
              do (setf (hunchentoot:header-out key) val)
                 (setf (gethash key seen) t)))
    ;; Handle body
    (unless body
      ;; No body provided - return streaming writer for delayed response
      (return-from handle-normal-response
        (let ((out (hunchentoot:send-headers)))
          (lambda (data &key (start 0) (end (length data)) close)
            (handler-case
                (etypecase data
                  (null nil)
                  (string
                   (write-sequence
                    (flexi-streams:string-to-octets
                     data :start start :end end
                     :external-format hunchentoot:*hunchentoot-default-external-format*)
                    out))
                  ((vector (unsigned-byte 8))
                   (write-sequence data out :start start :end end)))
              (error (e)
                (format *error-output* "~&Error writing to socket: ~A~%" e)))
            (if close
                (finish-output out)
                (force-output out))))))
    ;; Normal body handling
    (handler-case
        (etypecase body
          (null nil)
          (pathname
           (hunchentoot:handle-static-file body (getf headers :content-type)))
          (list
           (let ((out (hunchentoot:send-headers)))
             (dolist (chunk body)
               (write-sequence
                (flexi-streams:string-to-octets
                 chunk :external-format hunchentoot:*hunchentoot-default-external-format*)
                out))
             (finish-output out)))
          ((vector (unsigned-byte 8))
           (let ((out (hunchentoot:send-headers)))
             (write-sequence body out)
             (finish-output out))))
      (error (e)
        (format *error-output* "~&Error writing response: ~A~%" e)))))

(defun %read-raw-body-bytes (request)
  "Read the raw POST body of REQUEST into a fresh octet vector exactly once.
   Returns NIL if there is no body. Hunchentoot caches the bytes internally
   when called with :force-binary t, so callers of get/post-parameters can
   still see their parsed form.

   Signals REQUEST-BODY-TOO-LARGE when the declared Content-Length exceeds
   *MAX-REQUEST-BODY-BYTES* (refuses before allocation), or when the
   buffered body exceeds the cap after read (covers chunked transfer and
   headers that under-declare the body). Other errors collapse to NIL,
   preserving the pre-existing fault-tolerant contract for malformed
   transports."
  (let ((cl-string (hunchentoot:header-in :content-length request)))
    (when cl-string
      (let ((declared (parse-integer cl-string :junk-allowed t)))
        (%check-request-body-cap :declared declared))))
  (let ((bytes (handler-case
                   (hunchentoot:raw-post-data :request request :force-binary t)
                 (error () nil))))
    (when bytes
      (%check-request-body-cap :actual (length bytes)))
    (when (and bytes (plusp (length bytes)))
      bytes)))

(defun %make-cached-body-stream (bytes)
  "Wrap BYTES in a fresh flexi-stream-style readable stream. Each call to
   build-clack-env produces a new stream, so consumers reading :raw-body
   independently of the parsed parameters see the bytes from offset 0."
  (when bytes
    (flexi-streams:make-in-memory-input-stream bytes)))

(defun build-clack-env (request)
  "Build Clack environment plist from Hunchentoot request.
   Includes :clack.streaming and :clack.io for WebSocket/SSE support.

   Populates :query-parameters and :body-parameters so that the clack.lisp
   accessors (query-param, post-param, param) actually return values. The
   raw body bytes are read once and exposed via both :raw-body (a fresh
   stream over the cached bytes) and :lol/cached-body (the bytes themselves,
   for memoized request-body access)."
  (let* ((headers-ht (make-hash-table :test 'equal))
         (content-type (hunchentoot:header-in :content-type request))
         (raw-bytes (%read-raw-body-bytes request))
         ;; Hunchentoot's get-parameters parses the URI's query string
         ;; without reading the body. post-parameters parses both
         ;; application/x-www-form-urlencoded and multipart/form-data
         ;; bodies, which is exactly what we want exposed as
         ;; :body-parameters.
         (query-params (hunchentoot:get-parameters request))
         (body-params (when (%form-body-content-type-p content-type)
                        (hunchentoot:post-parameters request))))
    (dolist (header (hunchentoot:headers-in request))
      (setf (gethash (string-downcase (string (car header))) headers-ht)
            (cdr header)))
    (list :request-method (hunchentoot:request-method request)
          :script-name ""
          :path-info (hunchentoot:script-name request)
          :query-string (or (hunchentoot:query-string request) "")
          :server-name (hunchentoot:host request)
          :server-port (hunchentoot:acceptor-port (hunchentoot:request-acceptor request))
          :server-protocol (hunchentoot:server-protocol request)
          :request-uri (hunchentoot:request-uri request)
          :url-scheme (if (hunchentoot:ssl-p) "https" "http")
          :remote-addr (hunchentoot:remote-addr request)
          :remote-port (hunchentoot:remote-port request)
          :content-type content-type
          :content-length (alexandria:when-let (cl (hunchentoot:header-in :content-length request))
                            (parse-integer cl :junk-allowed t))
          :headers headers-ht
          :query-parameters query-params
          :body-parameters body-params
          ;; Memoized body bytes — handlers that need the raw payload
          ;; (e.g. JSON, multipart inspection) read this directly. Stays
          ;; usable across multiple reads since it is just a vector.
          :lol/cached-body raw-bytes
          ;; A fresh in-memory stream for legacy callers that read
          ;; :raw-body. Consumers can call request-body multiple times
          ;; because each call rebuilds the stream from the cached bytes.
          :raw-body (%make-cached-body-stream raw-bytes)
          ;; Streaming support for WebSocket/SSE
          ;; Use Hunchentoot's content-stream for bidirectional communication
          ;; (same as Clack's hunchentoot handler does)
          :clack.streaming t
          :clack.io (when *client-socket*
                      (make-instance 'streaming-client
                                     :socket *client-socket*
                                     :stream (hunchentoot::content-stream request))))))

(defun normalize-static-root (root)
  (uiop:ensure-directory-pathname
    (or root #P"static/")))

(defun default-static-root ()
  "Default static-root: STATIC_ROOT env var when set; otherwise the
   `static/` directory under the lol-web ASDF system source directory.

   Anchoring to the ASDF system directory rather than CWD removes a
   silent failure mode — Hunchentoot can be started from a CWD that
   bears no relation to the project layout, and a CWD-relative root
   either 404s every static request or, worse, serves the wrong tree
   if a directory of the same name happens to sit there."
  (let ((env-override (uiop:getenv "STATIC_ROOT")))
    (cond
      (env-override (normalize-static-root env-override))
      (t (normalize-static-root
          (merge-pathnames "static/"
                           (asdf:system-source-directory "lol-web")))))))

(defun %canonical-namestring (pathname)
  "Canonical namestring for PATHNAME (resolves symlinks via truename),
   or NIL if the path does not resolve."
  (let ((probed (ignore-errors (uiop:truename* pathname))))
    (when probed
      (namestring probed))))

(defun %path-has-prefix-p (path prefix)
  "True iff string PATH has PREFIX as a leading substring."
  (and (>= (length path) (length prefix))
       (string= path prefix :end1 (length prefix))))

(defun %static-request-under-root-p (request-path static-path static-root)
  "True iff the file resolved from REQUEST-PATH (a URL path) under
   STATIC-ROOT is a descendant of STATIC-ROOT after symlink resolution.

   Defends against `..` segments and symlink escape: even if the static
   middleware's join logic is lax, the truename of the candidate must
   sit under truename of the root."
  (when (%path-has-prefix-p request-path static-path)
    (let* ((suffix (subseq request-path (length static-path)))
           ;; Defang absolute-suffix forms (`/static//etc/passwd`) by
           ;; trimming leading slashes — merge-pathnames otherwise treats
           ;; them as absolute and discards the root.
           (suffix (string-left-trim "/" suffix))
           (candidate (merge-pathnames suffix static-root))
           (root-canon (%canonical-namestring static-root))
           (cand-canon (%canonical-namestring candidate)))
      (and root-canon cand-canon
           (%path-has-prefix-p cand-canon root-canon)))))

(defun %static-ancestry-wrapper (inner-static-app static-path static-root)
  "Wrap INNER-STATIC-APP (Lack `:static` middleware output) with an
   ancestry check. Requests whose URL prefix matches STATIC-PATH but
   whose resolved file is not a descendant of STATIC-ROOT return 404
   without consulting the inner app — the static middleware never gets
   the chance to serve a parent-of-root file."
  (lambda (env)
    (let ((path-info (getf env :path-info)))
      (cond
        ((and path-info
              (%path-has-prefix-p path-info static-path)
              (not (%static-request-under-root-p path-info static-path static-root)))
         (list 404
               '(:content-type "text/plain; charset=utf-8")
               '("Not Found")))
        (t (funcall inner-static-app env))))))

(defun start-server (&key (port 8080) debug
                          (static-path "/static/") (static-root (default-static-root))
                          (use-session t) (use-csrf t) (use-accesslog t) (use-static t)
                          (use-cors nil)
                          (cors-origin nil)
                          (cors-methods nil)
                          (cors-headers nil)
                          (auth nil)
                          (rate-limit-namespaces '((:ip :max-entries 10000)
                                                   (:login :max-entries 1000)))
                          (rate-limit-min-evict-age 60)
                          (rate-limit-eviction-every-n 64)
                          (rate-limit-eviction-interval 30)
                          (session-cookie-secure t)
                          (session-cookie-httponly t)
                          (session-cookie-samesite :lax))
  "Start the web server.

   PORT: Listen port (default 8080)
   DEBUG: Enable debug mode for verbose errors
   STATIC-PATH: URL path for static files (default /static/)
   STATIC-ROOT: Filesystem path for static files
   USE-SESSION: Enable session middleware (default t)
   USE-CSRF: Enable CSRF protection (default t)
   USE-ACCESSLOG: Enable access logging (default t)
   USE-STATIC: Enable static file serving (default t)
   USE-CORS: Enable CORS + OPTIONS preflight middleware (default nil)
   CORS-ORIGIN / CORS-METHODS / CORS-HEADERS: forwarded to make-app verbatim.
     CORS-ORIGIN is required when USE-CORS is T (no implicit wildcard); see
     make-app for the contract.
   RATE-LIMIT-NAMESPACES: forwarded to make-app verbatim. See make-app
     for the contract and default carve-outs.
   RATE-LIMIT-MIN-EVICT-AGE / RATE-LIMIT-EVICTION-EVERY-N /
   RATE-LIMIT-EVICTION-INTERVAL: forwarded to make-app verbatim. See
     make-app for the fairness + amortised-eviction contract.
   AUTH: forwarded to make-app verbatim. NIL (default) installs no auth
     middleware (with-auth fail-closes); a plist
     (:authenticated-p FN :current-principal FN) installs both thunks via
     a per-app middleware closure. See make-app for the full contract.
   SESSION-COOKIE-SECURE: HTTPS-only session cookie (default t; pass NIL for
     local-dev over plain HTTP or the browser drops the cookie)
   SESSION-COOKIE-HTTPONLY: Hide session cookie from JavaScript (default t)
   SESSION-COOKIE-SAMESITE: SameSite attribute on session cookie (default :lax)

   Returns server handle for stop-server, or NIL if port is already in use."
  (when *server*
    (error "Server already running. Call stop-server first."))
  ;; Run pre-server-start validators before allocating any state. A failing
  ;; hook signals out of START-SERVER without binding a port or mutating
  ;; *server* / *lack-app* — so the next call can retry once the user fixes
  ;; the underlying issue (typically a defhandler referencing an unregistered
  ;; extractor kind).
  (mapc #'funcall *before-server-start-hook*)
  (when debug
    (enable-debug-mode))
  (setf *lack-app* (make-app :static-path static-path
                             :static-root static-root
                             :use-session use-session
                             :use-csrf use-csrf
                             :use-accesslog use-accesslog
                             :use-static use-static
                             :use-cors use-cors
                             :cors-origin cors-origin
                             :cors-methods cors-methods
                             :cors-headers cors-headers
                             :auth auth
                             :rate-limit-namespaces rate-limit-namespaces
                             :rate-limit-min-evict-age rate-limit-min-evict-age
                             :rate-limit-eviction-every-n rate-limit-eviction-every-n
                             :rate-limit-eviction-interval rate-limit-eviction-interval
                             :session-cookie-secure session-cookie-secure
                             :session-cookie-httponly session-cookie-httponly
                             :session-cookie-samesite session-cookie-samesite))
  (setf *server* (make-instance 'lack-acceptor :port port :app *lack-app*))
  (handler-case
      (progn
        (hunchentoot:start *server*)
        (format t "~&Server started on port ~A~%" port)
        *server*)
    (usocket:address-in-use-error (c)
      (format *error-output* "~&[lol-reactive] Port ~A already in use: ~A~%" port c)
      (setf *server* nil)
      (setf *lack-app* nil)
      nil)))

(defun stop-server (&optional (server *server*))
  "Stop the web server.

   SERVER: Server handle from start-server (default *server*)"
  (when server
    (hunchentoot:stop server)
    (setf *server* nil)
    (setf *lack-app* nil)
    (format t "~&Server stopped~%")
    t))
