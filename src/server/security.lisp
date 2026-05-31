;;;; LOL-REACTIVE Security
;;;; Security headers and CSRF protection.
;;;;
;;;; Provides:
;;;; - add-security-headers: Standard security headers
;;;; - with-security: Wrap handlers with security
;;;; - generate-csrf-token / validate-csrf-token: CSRF surface
;;;; - check-rate-limit / with-rate-limit: rate limiting
;;;;
;;;; Output escape helpers (escape-html, escape-attribute, safe-url,
;;;; safe-url-allowlist) live in :lol-web/escape so they can be consumed
;;;; without dragging in the full server stack.
;;;;
;;;; Top-level state in :lol-web/server is image-wide by intent:
;;;;   *trusted-proxies*                       — deployment-wide IP allowlist
;;;;   *rate-limit-registry*                   — per-namespace bounded stores
;;;;   *routes*, *streaming-routes*,
;;;;   *routes-lock*, *path-params*            — handler registries (app.lisp)
;;;;   *before-handler-hook*,
;;;;   *before-server-start-hook*              — extension points (app.lisp)
;;;;   *server*, *lack-app*, *client-socket*   — singleton acceptor (app.lisp)
;;;;   *env*, *response-headers*               — per-request dynamic (clack.lisp)
;;;;   *debug-mode*, *error-log-path*          — process-wide error policy
;;;;
;;;; Per-app state (auth hooks) lives in the env key :lol-web.auth.hooks,
;;;; populated by a middleware closure that make-app builds when :auth is
;;;; supplied — so two apps in one image carry independent hooks.

(in-package :lol-web/server)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Input Validation Primitives
;;; ═══════════════════════════════════════════════════════════════════════════
;;;
;;; CRLF in a header value can forge Set-Cookie or split the response
;;; stream; a non-matching Origin can ride a same-session cookie into a
;;; cross-site request. Both primitives are foundations callers compose
;;; into middleware and per-handler gates.

(defun validate-header-value (value &key (max-length 8192))
  "Return VALUE when safe to emit as an HTTP response-header value; signal
   an error otherwise. Allowed characters per RFC 7230 §3.2.6: HTAB, SP,
   VCHAR (0x21-0x7E), obs-text (>= 0x80). Rejects values whose UTF-8
   octet length exceeds MAX-LENGTH."
  (unless (stringp value)
    (error "validate-header-value: expected a string, got ~S (~S)"
           value (type-of value)))
  (let ((len (babel:string-size-in-octets value :encoding :utf-8)))
    (when (> len max-length)
      (error "validate-header-value: value length ~D exceeds max ~D octets"
             len max-length)))
  (loop for ch across value
        for code = (char-code ch)
        do (cond
             ((or (= code 9) (= code 32)))
             ((or (and (>= code 33) (<= code 126))
                  (>= code 128)))
             (t
              (error "validate-header-value: forbidden control character ~
                      U+~4,'0X at position ~D"
                     code (position ch value)))))
  value)

(defun %scheme-default-port (scheme-str)
  "The RFC-default port for SCHEME-STR (http/ws 80, https/wss 443), or NIL for
   any other scheme."
  (cond ((member scheme-str '("http" "ws") :test #'string=) 80)
        ((member scheme-str '("https" "wss") :test #'string=) 443)
        (t nil)))

(defun %normalise-origin (value &key allow-path)
  "Return lowercase scheme+host[:port] for VALUE, or NIL when malformed.
   A port equal to the scheme's RFC default (http/ws 80, https/wss 443) is
   dropped so an explicit-default origin (https://x:443) and its implicit-
   default form (https://x) normalise to the same value and compare equal."
  (when (and (stringp value) (plusp (length value)))
    (handler-case
        (let* ((u (puri:parse-uri value))
               (scheme (puri:uri-scheme u))
               (host (puri:uri-host u))
               (port (puri:uri-port u))
               (path (puri:uri-path u))
               (query (puri:uri-query u))
               (fragment (puri:uri-fragment u)))
          (when (and scheme host
                     (or allow-path
                         (and (or (null path) (string= "" path))
                              (null query)
                              (null fragment))))
            (let* ((scheme-str (string-downcase (symbol-name scheme)))
                   (host-str (string-downcase host))
                   (default-port (%scheme-default-port scheme-str))
                   ;; puri strips an explicit default port to NIL on parse, so
                   ;; EFFECTIVE-PORT is set only for a non-default port; CANONICAL
                   ;; drops the default port and is what callers compare.
                   (effective-port (when (and port (not (eql port default-port)))
                                     port))
                   (canonical (if effective-port
                                  (format nil "~A://~A:~D" scheme-str host-str effective-port)
                                  (format nil "~A://~A" scheme-str host-str)))
                   ;; The strict round-trip still rejects userinfo or other
                   ;; puri-tolerated cruft: VALUE must spell exactly one of the
                   ;; forms that canonicalise to CANONICAL — the bare
                   ;; scheme://host[:non-default-port], or (since puri hides it)
                   ;; the same host carrying the scheme's explicit default port.
                   (accepted-forms
                     (if effective-port
                         (list canonical)
                         (cons (format nil "~A://~A" scheme-str host-str)
                               (when default-port
                                 (list (format nil "~A://~A:~D"
                                               scheme-str host-str default-port)))))))
              (when (or allow-path
                        (member (string-downcase value) accepted-forms :test #'string=))
                canonical))))
      (error () nil))))

(defun %allowed-origin-p (origin allowed-origins)
  (let ((normalised (%normalise-origin origin)))
    (and normalised
         (some (lambda (allowed)
                 (let ((allowed-normalised (%normalise-origin allowed)))
                   (and allowed-normalised
                        (string= normalised allowed-normalised))))
               allowed-origins))))

(defun %origin-from-referer (referer)
  "Reconstruct scheme+host[:port] from a Referer URL per RFC 6454 §6.1.
   Returns NIL when REFERER is missing, blank, or unparsable."
  (when (and (stringp referer) (plusp (length referer)))
    (%normalise-origin referer :allow-path t)))

(defun %request-bearer-token ()
  "Extract the bearer-token value from the current request's Authorization
   header, or NIL when no header is present or the scheme is not Bearer.
   Match is case-insensitive on the scheme name per RFC 6750 §2.1, with one
   or more WS chars separating scheme and token."
  (let ((header (request-header "Authorization")))
    (when (and (stringp header) (plusp (length header)))
      (let* ((trimmed (string-trim '(#\Space #\Tab) header))
             (space (position-if (lambda (c) (or (char= c #\Space) (char= c #\Tab)))
                                 trimmed)))
        (when space
          (let ((scheme (subseq trimmed 0 space))
                (rest   (string-left-trim '(#\Space #\Tab)
                                          (subseq trimmed space))))
            (when (and (string-equal scheme "Bearer")
                       (plusp (length rest)))
              rest)))))))

(defun validate-origin (&key allowed-origins
                          (origin-header "Origin")
                          referer-fallback
                          bearer-token)
  "Return T when the current request's Origin matches ALLOWED-ORIGINS, NIL
   otherwise. Scheme and host comparison is case-insensitive; explicit ports
   are preserved. Origin values with a path, query, or fragment deny. Empty
   allowlist denies.

   When Origin is absent and REFERER-FALLBACK is non-NIL, compare the
   scheme+host[:port] of Referer instead. When Origin IS present its value
   alone decides; Referer is never a tiebreaker.

   BEARER-TOKEN provides a non-browser escape: when supplied as a one-arg
   predicate (token -> generalised boolean), absence of any same-origin
   header (no Origin, no usable Referer) admits the request iff the
   request carries Authorization: Bearer <T> and (funcall BEARER-TOKEN T)
   returns truthy. When Origin IS present it still decides verbatim
   against ALLOWED-ORIGINS — the bearer path is unreachable on browser
   traffic, so an attacker cannot forge a token to slip past the allowlist.
   BEARER-TOKEN as T means accept any non-empty bearer (defer the
   actual validation to the auth-fn); NIL (default) disables the escape
   so behaviour is unchanged for existing callers.

   Reads from *env*. Caller chooses the deny response shape."
  (when allowed-origins
    (let ((origin (request-header origin-header)))
      (cond
        ((and origin (%allowed-origin-p origin allowed-origins))
         t)
        (origin
         nil)
        (referer-fallback
         (let ((ref-origin (%origin-from-referer (request-header "Referer"))))
           (cond
             ((and ref-origin
                   (some (lambda (allowed)
                           (let ((allowed-normalised (%normalise-origin allowed)))
                             (and allowed-normalised
                                  (string= ref-origin allowed-normalised))))
                         allowed-origins))
              t)
             (ref-origin
              nil)
             (bearer-token
              (let ((tok (%request-bearer-token)))
                (and tok
                     (if (eq bearer-token t) t (funcall bearer-token tok))
                     t)))
             (t nil))))
        (bearer-token
         (let ((tok (%request-bearer-token)))
           (and tok
                (if (eq bearer-token t) t (funcall bearer-token tok))
                t)))
        (t nil)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Security Headers
;;; ═══════════════════════════════════════════════════════════════════════════

(defun add-security-headers ()
  "Add standard security headers to the current response.
   Must be called within a with-response-headers context.

   Headers added:
   - X-Frame-Options: DENY - Prevent clickjacking
   - X-Content-Type-Options: nosniff - Prevent MIME sniffing
   - X-XSS-Protection: 1; mode=block - Enable XSS filter
   - Referrer-Policy: strict-origin-when-cross-origin - Control referrer info
   - Content-Security-Policy: Default CSP from add-csp-header"
  (add-response-header "X-Frame-Options" "DENY")
  (add-response-header "X-Content-Type-Options" "nosniff")
  (add-response-header "X-XSS-Protection" "1; mode=block")
  (add-response-header "Referrer-Policy" "strict-origin-when-cross-origin")
  (add-csp-header))

(defun add-csp-header (&key (default-src "'self'")
                            (script-src "'self'")
                            (style-src "'self' https://fonts.googleapis.com")
                            (font-src "'self' data: https://fonts.gstatic.com")
                            (img-src "'self' data: https:")
                            (connect-src "'self'"))
  "Add Content-Security-Policy header with customizable directives.
   Default allows self-served scripts and styles, plus Google Fonts. Inline
   script/style is an explicit caller opt-in via SCRIPT-SRC / STYLE-SRC.
   Must be called within a with-response-headers context.

   Example:
     (add-csp-header :script-src \"'self'\" :style-src \"'self'\")"
  (add-response-header "Content-Security-Policy"
                       (format nil "default-src ~A; script-src ~A; style-src ~A; font-src ~A; img-src ~A; connect-src ~A"
                               default-src script-src style-src font-src img-src connect-src)))

(defmacro with-security (&body body)
  "Wrap handler with security headers.
   Convenience macro that adds security headers before executing body.

   Example:
     (defroute \"/api/data\" (:method :get)
       (with-security
         (get-data-json)))"
  `(progn
     (add-security-headers)
     ,@body))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; CSRF Protection
;;; ═══════════════════════════════════════════════════════════════════════════

(defun generate-csrf-token ()
  "Generate a 128-bit CSRF token from the OS CSPRNG, hex-encoded.
   Delegates to :lol-web/crypto:random-bytes-hex which reads from the OS
   CSPRNG; plain CL `random` is a PRNG only and unsuitable for security tokens."
  (random-bytes-hex 16))

(defvar *csrf-token-lock*
  (bordeaux-threads:make-recursive-lock "lol-web csrf token")
  "Serialises lazy CSRF token creation for the current session.")

(defun get-csrf-token ()
  "Get current CSRF token from session, creating if needed.
   Uses Lack session middleware via *env*.
   Note: Uses string key to match Lack CSRF middleware configuration."
  (or (session-get "csrf-token")
      (bordeaux-threads:with-recursive-lock-held (*csrf-token-lock*)
        (or (session-get "csrf-token")
            (session-set "csrf-token" (generate-csrf-token))))))

(defun validate-csrf-token (token)
  "Validate CSRF token from request matches session token.
   Returns T if valid, NIL if invalid. Uses constant-time-string= so a
   timing side channel cannot leak the matching prefix length to an
   attacker controlling the submitted token.
   Note: Uses string key to match Lack CSRF middleware configuration."
  (when token
    (let ((session-token (session-get "csrf-token")))
      (and session-token
           (constant-time-string= token session-token)))))

(defun csrf-token-input ()
  "Generate hidden input field with CSRF token.
   Include this in all forms that modify data.
   Uses html-attrs for proper attribute escaping.

   Example (in cl-who):
     (:form :method \"post\" :action \"/submit\"
       (who:str (csrf-token-input))
       ...)"
  (format nil "<input~A/>"
          (html-attrs "type" "hidden"
                      "name" "csrf-token"
                      "value" (get-csrf-token))))

(defmacro! with-csrf-validation (&body body)
  "Wrap handler with CSRF token validation.
   Returns 403 Forbidden response if token is invalid.

   Example:
     (defroute \"/submit\" (:method :post)
       (with-csrf-validation
         (process-form)))

   The submitted-token variable is gensymed so the body may freely reference
   lol-web/server::token from outer lexical scope without anaphoric capture."
  `(let ((,g!token (post-param "csrf-token")))
     (if (validate-csrf-token ,g!token)
         (progn ,@body)
         (error-response 403
                         :content-type "text/html; charset=utf-8"
                         :message (minimal-error-html "403 Forbidden" "403" "Invalid or missing CSRF token")))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Request Body Content-Type Recognition
;;; ═══════════════════════════════════════════════════════════════════════════
;;;
;;; Case-insensitive substring match so parameterised forms
;;; (e.g. "application/json; charset=utf-8") match.

(defun %form-body-content-type-p (content-type)
  "True when CONTENT-TYPE names a body shape that hunchentoot:post-parameters
   knows how to parse — application/x-www-form-urlencoded for plain forms or
   multipart/form-data for file uploads. Browsers select the latter when any
   field has type=file (see forms/form-dsl.lisp render-form), so omitting it
   here causes :body-parameters to be NIL on every file-upload POST and
   (post-param ...) returns NIL even though the bytes arrived intact."
  (and content-type
       (or (search "application/x-www-form-urlencoded" content-type :test #'char-equal)
           (search "multipart/form-data" content-type :test #'char-equal))))

(defun %json-body-content-type-p (content-type)
  "True when CONTENT-TYPE names a JSON request body — application/json in its
   bare or parameterised form (e.g. \"application/json; charset=utf-8\"), or
   any media-type carrying a +json structured-suffix per RFC 6838 §4.2.8
   (e.g. application/vnd.api+json, application/ld+json)."
  (and content-type
       (or (search "application/json" content-type :test #'char-equal)
           (search "+json"            content-type :test #'char-equal))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; CSRF Middleware — constant-time replacement for lack/middleware/csrf
;;; ═══════════════════════════════════════════════════════════════════════════
;;;
;;; Drop-in for lack/middleware/csrf with constant-time-string= comparison
;;; (Lack's #'equal leaks prefix length under timing oracles). Token slot is
;;; selected by Content-Type: form-body parameter, JSON header/body field, or
;;; deny-by-default for unrecognised Content-Types on unsafe methods. Returns
;;; 403 (Lack returns 400) to match with-csrf-validation's deny shape.

(defun %csrf-deny ()
  "Build a stand-alone Clack response triple for CSRF rejection. Does not
   touch *response-headers*, *env*, or any dynamic state so it is safe to
   call from middleware (which runs outside the route-handler dynamic
   extent that binds those specials)."
  (list 403
        (list :content-type "text/html; charset=utf-8")
        (list (minimal-error-html "403 Forbidden" "403"
                                  "Invalid or missing CSRF token"))))

(defun %csrf-valid-token-p (env session session-key form-token-name)
  "Return T iff session holds a token under SESSION-KEY that constant-time
   matches the body parameter named FORM-TOKEN-NAME in ENV. NIL on any
   missing/mismatched component. Handles multipart bodies where Lack
   returns body-parameter values as a list of strings."
  (let ((session-token (gethash session-key session)))
    (when session-token
      (let* ((req (lack/request:make-request env))
             (received (cdr (assoc form-token-name
                                   (lack/request:request-body-parameters req)
                                   :test #'string=))))
        (when (listp received)
          (setf received (first received)))
        (and (stringp received)
             (constant-time-string= session-token received))))))

(defun %read-octets-up-to (stream cap)
  "Read up to CAP bytes from STREAM into a fresh simple octet vector, growing
   the buffer geometrically so a small body never pre-allocates CAP. Returns
   the bytes read, NIL when the stream is empty, or NIL when more than CAP
   bytes are available (over-cap → caller fails closed). CAP must be a
   positive integer."
  (let ((acc (make-array (min 4096 cap)
                         :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (chunk (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop
      (let ((n (read-sequence chunk stream)))
        (when (plusp n)
          (let ((needed (+ (fill-pointer acc) n)))
            (when (> needed cap)
              (return-from %read-octets-up-to nil))
            (when (> needed (array-total-size acc))
              (adjust-array acc (min cap (max needed (* 2 (array-total-size acc))))))
            (let ((start (fill-pointer acc)))
              (setf (fill-pointer acc) needed)
              (replace acc chunk :start1 start :end2 n))))
        (when (< n (length chunk))
          (return))))
    (when (plusp (fill-pointer acc))
      (subseq acc 0 (fill-pointer acc)))))

(defun %csrf-fallback-body-bytes (env)
  "Read request body bytes from ENV's :raw-body stream when the
   build-clack-env cache (:lol/cached-body) is absent — e.g. a stack that
   composed csrf-middleware without build-clack-env. Bounded by
   *max-request-body-bytes* and never signals: an oversize, empty, or
   unreadable body yields NIL so the CSRF check stays fail-closed. A known
   Content-Length pre-sizes the buffer (bounded by the cap); an absent length
   reads in bounded chunks via %read-octets-up-to so a small body never
   pre-allocates the full cap."
  (let* ((stream (getf env :raw-body))
         (cap *max-request-body-bytes*)
         (declared (getf env :content-length)))
    (when stream
      (handler-case
          (cond
            (declared
             (let* ((limit (if cap (min declared cap) declared))
                    (buf (make-array (1+ limit) :element-type '(unsigned-byte 8)))
                    (n (read-sequence buf stream)))
               ;; The (1+ limit) buffer detects an over-declared body: n > limit
               ;; means the stream carried more than the declared/capped length,
               ;; so fail closed rather than return the extra byte.
               (when (and (plusp n) (<= n limit))
                 (subseq buf 0 n))))
            (cap
             (%read-octets-up-to stream cap))
            (t
             (let ((bytes (alexandria:read-stream-content-into-byte-vector stream)))
               (when (plusp (length bytes)) bytes))))
        (error () nil)))))

(defun %csrf-token-from-json-body (env json-token-key)
  "Return the CSRF token string from a JSON request, or NIL. Prefers the
   X-CSRF-Token header over JSON-TOKEN-KEY in the JSON body. Reads
   :lol/cached-body (set once by build-clack-env) so the parse does not
   drain :raw-body; falls back to a bounded read of :raw-body for a stack
   that bypassed build-clack-env. Malformed JSON yields NIL — never signals
   from the CSRF middleware path."
  (let* ((headers (getf env :headers))
         (header  (when headers (gethash "x-csrf-token" headers))))
    (cond
      ((and (stringp header) (plusp (length header)))
       header)
      (t
       (let ((bytes (or (getf env :lol/cached-body)
                        (%csrf-fallback-body-bytes env))))
         (when bytes
           (let ((parsed (handler-case
                             (decode-json-string
                              (%decode-request-octets bytes))
                           (malformed-json-body () nil))))
             (when (consp parsed)
               (let ((value (cdr (assoc json-token-key parsed))))
                 (when (stringp value) value))))))))))

(defun %csrf-valid-json-token-p (env session session-key json-token-key)
  "Return T iff SESSION holds a token under SESSION-KEY that constant-time
   matches a JSON-envelope-supplied token (header or body field) in ENV."
  (let ((session-token (gethash session-key session))
        (received      (%csrf-token-from-json-body env json-token-key)))
    (and session-token
         received
         (constant-time-string= session-token received))))

(defun request-csrf-valid-p (&optional (env *env*)
                             &key (session-key "csrf-token")
                                  (form-token-name "csrf-token")
                                  (json-token-key :csrf-token))
  "Return T iff the request whose Clack ENV this is carries a CSRF token that
   constant-time matches the session token, dispatching on Content-Type
   exactly as CSRF-MIDDLEWARE does. NIL when no session is present, no token
   is supplied, the Content-Type carries no defined token slot, or the token
   mismatches.

   Lets a write-capable route self-assert CSRF instead of relying solely on
   app-level CSRF-MIDDLEWARE being installed — a route that must stay
   protected even under a :use-csrf nil app config calls this and refuses
   on NIL."
  (let ((session      (getf env :lack.session))
        (content-type (getf env :content-type)))
    (and session
         (cond
           ((%form-body-content-type-p content-type)
            (%csrf-valid-token-p env session session-key form-token-name))
           ((%json-body-content-type-p content-type)
            (%csrf-valid-json-token-p env session session-key json-token-key))
           (t nil))
         t)))

(defun csrf-middleware (app &key (session-key "csrf-token")
                                 (form-token-name "csrf-token")
                                 (json-token-key :csrf-token))
  "Lack-shaped CSRF middleware with constant-time token comparison. Safe
   methods (outside POST/PUT/DELETE/PATCH) pass through unchecked. Unsafe
   methods present their token in a slot chosen by Content-Type:

   - application/x-www-form-urlencoded / multipart/form-data: body
     parameter named FORM-TOKEN-NAME (default \"csrf-token\").
   - application/json (and any +json structured-suffix): X-CSRF-Token
     header, then JSON-body field JSON-TOKEN-KEY (default :csrf-token,
     keyword matching decode-json-string's alist shape). Header wins.
   - Any other Content-Type: 403 (no defined token slot, fail-closed).

   In every accepted shape the submitted token must constant-time-match
   SESSION's entry under SESSION-KEY (default \"csrf-token\") or 403.

   Signals on missing :lack.session so a CSRF stack above a non-session
   middleware fails loud. 403 (vs Lack's 400) matches
   with-csrf-validation so consumers see one deny shape."
  (lambda (env)
    (cond
      ((not (member (getf env :request-method)
                    '(:post :put :delete :patch)
                    :test #'eq))
       (funcall app env))
      (t
       (let ((session      (getf env :lack.session))
             (content-type (getf env :content-type)))
         (unless session
           (error "csrf-middleware: :lack.session missing in env. Install a ~
                   session middleware between csrf-middleware and the route ~
                   handler, or pass :use-csrf nil to make-app."))
         (cond
           ((%form-body-content-type-p content-type)
            (if (%csrf-valid-token-p env session session-key form-token-name)
                (funcall app env)
                (%csrf-deny)))
           ((%json-body-content-type-p content-type)
            (if (%csrf-valid-json-token-p env session session-key json-token-key)
                (funcall app env)
                (%csrf-deny)))
           (t
            (%csrf-deny))))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Rate Limiting — per-namespace bounded stores with LRU eviction
;;; ═══════════════════════════════════════════════════════════════════════════
;;;
;;; Each namespace owns a hash-table store, a lock, an entry-count cap, and
;;; an intrusive doubly-linked list threading the store's entries in
;;; last-seen order (head = most-recently-seen, tail = least). A flood in
;;; one namespace (say :login keyed on email) cannot evict entries in
;;; another (say :ip keyed on client IP) because the stores are disjoint.
;;; Eviction is O(1): the tail is the least-recently-seen entry, so a
;;; cap-full store drops the tail when it is older than the min-evict-age,
;;; or — because a tail younger than the min-age means EVERY entry is —
;;; refuses the newcomer. No maphash victim scan on the hot path. Steady
;;; legitimate users refresh last-seen (move to head) on every allowed
;;; insert and survive; attackers cycling fresh keys sink to the tail and
;;; get reaped.
;;;
;;; Per-entry triple: count + window-start + last-seen. window-start
;;; preserves the fixed-window rate decision (count resets on window roll-
;;; over); last-seen drives LRU eviction without polluting the window
;;; decision. Denied (over-cap) attempts do NOT refresh last-seen — if
;;; they did, an attacker could keep an entry alive indefinitely by
;;; spamming over-limit requests.

(defstruct rate-limit-entry
  "Per-key state inside a rate-limit-namespace store. COUNT is hits in the
   current window; WINDOW-START is the universal-time when the window
   opened; LAST-SEEN is the universal-time of the most recent allowed
   insert, used exclusively for LRU eviction. KEY mirrors the store hash key
   so an evicted entry can remhash itself in O(1). PREV/NEXT are the
   intrusive doubly-linked-list pointers keeping entries ordered by last-seen
   (head = most-recently-seen, tail = least): touching an entry moves it to
   the head, eviction drops the tail — both O(1)."
  (count        0 :type integer)
  (window-start 0 :type integer)
  (last-seen    0 :type integer)
  (key          nil)
  (prev         nil)
  (next         nil))

(defstruct rate-limit-namespace
  "Isolated bounded store for one rate-limit namespace. The lock is
   per-namespace so :login traffic does not contend with :ip traffic, and
   the cap is per-namespace so a flood in one cannot evict the other.
   NAME echoes the registry key so rate-limit-store-full reports which
   namespace refused to evict. INSERTS-SINCE-EVICTION drives the
   amortised eviction sweep — see *rate-limit-eviction-every-n*. LRU-HEAD /
   LRU-TAIL anchor the intrusive last-seen-ordered list over STORE's entries:
   LRU-HEAD is the most-recently-seen entry, LRU-TAIL the least — eviction
   drops the tail in O(1) without scanning the store."
  (name                    nil :type symbol)
  (store                   (make-hash-table :test 'equal) :type hash-table)
  (lock                    (bordeaux-threads:make-lock "rate-limit-namespace"))
  (max-entries             10000 :type integer)
  (inserts-since-eviction  0 :type integer)
  (lru-head                nil)
  (lru-tail                nil))

(defvar *rate-limit-registry* (make-hash-table)
  "Maps namespace keyword -> rate-limit-namespace. Namespaces partition
   rate-limit state so flood traffic in one (e.g., :login with attacker-
   cycled emails) cannot evict counters in another (e.g., :ip tracking
   the legitimate steady client). Default carve-outs :ip and :login are
   installed at image-load time. Reconfigure caps through make-app's
   :rate-limit-namespaces kwarg or configure-rate-limit-namespace.")

(defparameter *rate-limit-min-evict-age* 60
  "Seconds. An entry whose last-seen is within this window cannot be
   evicted by the LRU pass — eviction prefers refusing the new request
   over discarding fresh legitimate traffic. When every candidate fails
   the min-age check, %evict-namespace-entries signals
   rate-limit-store-full and returns NIL; the caller (check-rate-limit)
   then denies the inbound request. Set via make-app's
   :rate-limit-min-evict-age kwarg.")

(defparameter *rate-limit-eviction-every-n* 64
  "Amortised eviction interval. After every Nth allowed insert in a
   namespace, the inline path runs an eviction sweep proactively to keep
   the store under cap before the next cap-hit forces a synchronous one.
   NIL disables the amortised sweep. Set via make-app's
   :rate-limit-eviction-every-n kwarg.")

(defparameter *rate-limit-eviction-interval* 30
  "Seconds. When non-NIL, a background thread sweeps every namespace in
   *rate-limit-registry* at this interval, dropping LRU entries past
   min-age. NIL disables the timer. Set via make-app's
   :rate-limit-eviction-interval kwarg; reconcile with
   configure-rate-limit-eviction-timer.")

(define-condition rate-limit-store-full ()
  ((namespace   :initarg :namespace
                :reader rate-limit-store-full-namespace)
   (max-entries :initarg :max-entries
                :reader rate-limit-store-full-max-entries))
  (:documentation
   "Signalled (not errored) when an eviction pass finds the store at cap
    and every candidate's last-seen is within *rate-limit-min-evict-age*.
    The store is left untouched and the inbound request is denied —
    legitimate steady users are preserved over an attacker cycling fresh
    keys. Observers handler-bind this condition; unhandled signals are
    no-ops by design.")
  (:report
   (lambda (c s)
     (format s "rate-limit-store-full: namespace ~S at cap ~D and every ~
                eviction candidate is younger than ~D seconds."
             (rate-limit-store-full-namespace c)
             (rate-limit-store-full-max-entries c)
             *rate-limit-min-evict-age*))))

(defun configure-rate-limit-namespace (namespace &key (max-entries 10000))
  "Install or reconfigure NAMESPACE in *rate-limit-registry*. Idempotent:
   if NAMESPACE already exists, its max-entries cap is updated and the
   existing entries are preserved. Returns the rate-limit-namespace
   struct."
  (let ((ns (or (gethash namespace *rate-limit-registry*)
                (setf (gethash namespace *rate-limit-registry*)
                      (make-rate-limit-namespace :name namespace)))))
    (setf (rate-limit-namespace-max-entries ns) max-entries)
    ns))

(configure-rate-limit-namespace :ip    :max-entries 10000)
(configure-rate-limit-namespace :login :max-entries 1000)

(defun %ensure-namespace (namespace)
  "Return NAMESPACE's struct, creating it with defaults if absent. Caller
   has not yet acquired any lock."
  (or (gethash namespace *rate-limit-registry*)
      (configure-rate-limit-namespace namespace)))

;;; Intrusive last-seen-ordered list. All three run under the namespace lock
;;; the caller already holds (check-rate-limit / the eviction sweeps).

(defun %lru-unlink (ns entry)
  "Detach ENTRY from NS's last-seen-ordered list, repairing the neighbours'
   links and the head/tail anchors; ENTRY's own PREV/NEXT are cleared."
  (let ((prev (rate-limit-entry-prev entry))
        (next (rate-limit-entry-next entry)))
    (if prev
        (setf (rate-limit-entry-next prev) next)
        (setf (rate-limit-namespace-lru-head ns) next))
    (if next
        (setf (rate-limit-entry-prev next) prev)
        (setf (rate-limit-namespace-lru-tail ns) prev))
    (setf (rate-limit-entry-prev entry) nil
          (rate-limit-entry-next entry) nil)))

(defun %lru-push-front (ns entry)
  "Insert ENTRY at the head (most-recently-seen end) of NS's list. ENTRY must
   already be unlinked (PREV/NEXT NIL)."
  (let ((old-head (rate-limit-namespace-lru-head ns)))
    (setf (rate-limit-entry-prev entry) nil
          (rate-limit-entry-next entry) old-head)
    (if old-head
        (setf (rate-limit-entry-prev old-head) entry)
        (setf (rate-limit-namespace-lru-tail ns) entry))
    (setf (rate-limit-namespace-lru-head ns) entry)))

(defun %lru-touch (ns entry)
  "Move ENTRY to the head — it was just seen. O(1)."
  (%lru-unlink ns entry)
  (%lru-push-front ns entry))

(defun %evict-namespace-entries (ns window-seconds now)
  "Caller holds the namespace lock. O(1) LRU eviction: when the store is at
   or above the cap, drop the single least-recently-seen entry — the tail of
   the intrusive last-seen-ordered list — provided its last-seen is older
   than *rate-limit-min-evict-age*. Because the list is last-seen-ordered, a
   tail younger than the min-age means EVERY entry is younger, so no entry is
   eligible: SIGNAL rate-limit-store-full and return NIL, leaving the store
   untouched (a legitimate fresh entry survives; the newcomer is refused).
   Always resets inserts-since-eviction on exit. Returns T on eviction or a
   no-op (room available), NIL when eviction was refused. WINDOW-SECONDS is
   unused: last-seen LRU ordering makes a separate per-window expired scan
   unnecessary, since a stale entry sinks to the tail and is reaped there."
  (declare (ignore window-seconds))
  (let ((store (rate-limit-namespace-store ns))
        (result t))
    (when (>= (hash-table-count store)
              (rate-limit-namespace-max-entries ns))
      (let ((victim (rate-limit-namespace-lru-tail ns)))
        (cond
          ((and victim
                (>= (- now (rate-limit-entry-last-seen victim))
                    *rate-limit-min-evict-age*))
           (%lru-unlink ns victim)
           (remhash (rate-limit-entry-key victim) store))
          (t
           (signal 'rate-limit-store-full
                   :namespace (rate-limit-namespace-name ns)
                   :max-entries (rate-limit-namespace-max-entries ns))
           (setf result nil)))))
    (setf (rate-limit-namespace-inserts-since-eviction ns) 0)
    result))

(defun check-rate-limit (key &key (max-requests 100) (window-seconds 60)
                                  (max-key-bytes 128)
                                  (namespace :ip))
  "Check whether KEY is within the rate limit for NAMESPACE.
   Returns T if allowed, NIL if rate limited or if KEY is oversized.

   KEY is any equal-comparable value (usually a client IP or per-account
   string). NAMESPACE (default :ip) partitions the state — :ip and
   :login are pre-installed; any other keyword auto-installs a namespace
   with default caps on first use. A flood in one namespace cannot evict
   entries in another.

   Default: 100 requests per 60 seconds per key per namespace.

   :MAX-KEY-BYTES caps string keys at 128 UTF-8 octets by default. An
   attacker-controlled long key would amplify per-namespace store memory
   without ever-tripping the per-key throttle; rejecting at the gate
   keeps the store bounded by entry-count AND per-key size. Non-string
   keys bypass the byte check.

   Reads and writes happen under the namespace's lock so concurrent
   requests for the same key cannot lose increments. The per-key
   last-seen timestamp is refreshed on every allowed insert so eviction
   prefers stale entries over steady legitimate traffic. Over-cap denies
   do not refresh last-seen — keeping a denied entry alive by spamming
   would otherwise let an attacker survive eviction indefinitely."
  (when (and (stringp key)
             (> (babel:string-size-in-octets key :encoding :utf-8)
                max-key-bytes))
    (return-from check-rate-limit nil))
  (let ((ns (%ensure-namespace namespace)))
    (bordeaux-threads:with-lock-held ((rate-limit-namespace-lock ns))
      (let* ((store (rate-limit-namespace-store ns))
             (now (get-universal-time))
             (entry (gethash key store))
             (allowed
               (cond
                 ((null entry)
                  (when (and (>= (hash-table-count store)
                                 (rate-limit-namespace-max-entries ns))
                             (not (%evict-namespace-entries ns window-seconds now)))
                    (return-from check-rate-limit nil))
                  (let ((new-entry (make-rate-limit-entry :key key
                                                          :count 1
                                                          :window-start now
                                                          :last-seen now)))
                    (setf (gethash key store) new-entry)
                    (%lru-push-front ns new-entry))
                  t)
                 ((> (- now (rate-limit-entry-window-start entry)) window-seconds)
                  (setf (rate-limit-entry-count entry) 1
                        (rate-limit-entry-window-start entry) now
                        (rate-limit-entry-last-seen entry) now)
                  (%lru-touch ns entry)
                  t)
                 ((>= (rate-limit-entry-count entry) max-requests)
                  nil)
                 (t
                  (incf (rate-limit-entry-count entry))
                  (setf (rate-limit-entry-last-seen entry) now)
                  (%lru-touch ns entry)
                  t))))
        (when allowed
          (incf (rate-limit-namespace-inserts-since-eviction ns))
          (when (and *rate-limit-eviction-every-n*
                     (>= (rate-limit-namespace-inserts-since-eviction ns)
                         *rate-limit-eviction-every-n*))
            (%evict-namespace-entries ns window-seconds now)))
        allowed))))

(defun rate-limit-entry-of (key &key (namespace :ip))
  "Return the rate-limit-entry struct for KEY in NAMESPACE, or NIL when
   absent. Test/REPL utility — production code uses check-rate-limit or
   with-rate-limit. The returned struct is live: other threads may
   mutate it after this call returns. Copy fields you need to hold."
  (let ((ns (gethash namespace *rate-limit-registry*)))
    (when ns
      (bordeaux-threads:with-lock-held ((rate-limit-namespace-lock ns))
        (gethash key (rate-limit-namespace-store ns))))))

(defun rate-limit-namespace-count (&optional (namespace :ip))
  "Return the number of entries currently in NAMESPACE's store. 0 if the
   namespace is not registered."
  (let ((ns (gethash namespace *rate-limit-registry*)))
    (if ns
        (hash-table-count (rate-limit-namespace-store ns))
        0)))

(defun %reset-namespace-store (ns)
  "Caller holds the namespace lock. Empty the store and the intrusive
   last-seen list together so no dangling head/tail survives the clear."
  (clrhash (rate-limit-namespace-store ns))
  (setf (rate-limit-namespace-lru-head ns) nil
        (rate-limit-namespace-lru-tail ns) nil))

(defun clear-rate-limit-store (&optional namespace)
  "Test/REPL utility. With NAMESPACE specified, clears only that
   namespace's store. With no argument, clears every namespace in
   *rate-limit-registry*. Always returns T."
  (cond
    (namespace
     (let ((ns (gethash namespace *rate-limit-registry*)))
       (when ns
         (bordeaux-threads:with-lock-held ((rate-limit-namespace-lock ns))
           (%reset-namespace-store ns)))))
    (t
     (maphash (lambda (k ns)
                (declare (ignore k))
                (bordeaux-threads:with-lock-held ((rate-limit-namespace-lock ns))
                  (%reset-namespace-store ns)))
              *rate-limit-registry*)))
  t)

(defvar *rate-limit-eviction-thread* nil
  "Background thread running %rate-limit-eviction-loop, or NIL when the
   scheduled timer is disabled. Mutated only through
   configure-rate-limit-eviction-timer / %stop-rate-limit-eviction-timer.")

(defvar *rate-limit-eviction-stop-flag* nil
  "When T, the background eviction loop exits after its current sleep
   tick. Set by %stop-rate-limit-eviction-timer.")

(defvar *rate-limit-eviction-sweep-count* 0
  "Monotonic counter incremented each time the scheduled timer completes
   a sweep across *rate-limit-registry*. Test/REPL probe; production
   code should not depend on its absolute value.")

(defun %rate-limit-eviction-sweep-once ()
  "Run %evict-namespace-entries against every namespace in
   *rate-limit-registry*. The sweep skips the per-window expired pass
   (the timer has no caller window context); it only trims when a
   namespace is at or above its cap. Increments
   *rate-limit-eviction-sweep-count* on completion."
  (let ((now (get-universal-time)))
    (maphash
     (lambda (name ns)
       (declare (ignore name))
       (bordeaux-threads:with-lock-held ((rate-limit-namespace-lock ns))
         (handler-case
             (%evict-namespace-entries ns nil now)
           (rate-limit-store-full () nil))))
     *rate-limit-registry*)
    (incf *rate-limit-eviction-sweep-count*)))

(defun %rate-limit-eviction-loop (interval)
  "Background loop: sweep, then sleep INTERVAL seconds in short chunks
   so *rate-limit-eviction-stop-flag* is checked promptly between ticks."
  (loop
    (%rate-limit-eviction-sweep-once)
    (when *rate-limit-eviction-stop-flag* (return))
    (loop with waited of-type single-float = 0.0
          while (and (< waited interval)
                     (not *rate-limit-eviction-stop-flag*))
          do (sleep 0.1)
             (incf waited 0.1))
    (when *rate-limit-eviction-stop-flag* (return))))

(defun %stop-rate-limit-eviction-timer ()
  "Signal the background loop to exit and join its thread. Idempotent
   no-op when no thread is running."
  (when *rate-limit-eviction-thread*
    (setf *rate-limit-eviction-stop-flag* t)
    (handler-case
        (bordeaux-threads:join-thread *rate-limit-eviction-thread*)
      (error () nil))
    (setf *rate-limit-eviction-thread* nil)
    (setf *rate-limit-eviction-stop-flag* nil)))

(defun %start-rate-limit-eviction-timer (interval)
  "Stop any running timer, then start a fresh background thread sweeping
   every INTERVAL seconds."
  (%stop-rate-limit-eviction-timer)
  (setf *rate-limit-eviction-stop-flag* nil)
  (setf *rate-limit-eviction-thread*
        (bordeaux-threads:make-thread
         (lambda () (%rate-limit-eviction-loop interval))
         :name (format nil "lol-web rate-limit eviction (~As)" interval))))

(defun configure-rate-limit-eviction-timer ()
  "Reconcile the eviction-timer thread with *rate-limit-eviction-interval*.
   NIL stops any running timer; a non-NIL number restarts the timer at
   that interval. Idempotent — safe to call from every make-app."
  (cond
    ((null *rate-limit-eviction-interval*)
     (%stop-rate-limit-eviction-timer))
    (t
     (%start-rate-limit-eviction-timer *rate-limit-eviction-interval*))))

(defun %first-forwarded-ip (header)
  "Return the first non-empty IP in a comma-delimited X-Forwarded-For chain,
   trimmed of surrounding whitespace. NIL when the chain is empty or all-blank.
   Per RFC 7239 the leftmost address is the originating client; using the
   whole header as a rate-limit key lets attackers bypass limits trivially by
   appending '1.2.3.4, anything' to vary the key per request."
  (when header
    (loop for tok in (cl-ppcre:split "," header)
          for trimmed = (string-trim '(#\Space #\Tab) tok)
          when (plusp (length trimmed))
            return trimmed)))

(defparameter *trusted-proxies* nil
  "List of :remote-addr strings whose X-Forwarded-For / X-Real-IP headers are
   honoured by client-ip. Default NIL means the safe default: ignore those
   headers, return :remote-addr verbatim. Set this to your reverse-proxy IPs
   (e.g., '(\"127.0.0.1\")) when running behind nginx/HAProxy/Caddy on the
   same host, or to the LB pool addresses for cloud LB termination. Compared
   with #'string=.

   Trusting XFF/X-Real-IP unconditionally lets an attacker forge any client
   IP by setting the header on their request — defeating IP-keyed rate limits
   and IP-based audit logs. Trust must be opted into per deployment.")

(defun client-ip ()
  "Return the originating client IP for rate-limiting and audit purposes.
   When (getf *env* :remote-addr) is a member of *trusted-proxies* (string=),
   honour the leftmost X-Forwarded-For entry (RFC 7239), then X-Real-IP, then
   :remote-addr. Otherwise return :remote-addr directly — XFF and X-Real-IP
   are ignored as untrusted.

   Reads from *env*. Returns NIL when neither *trusted-proxies* matches nor
   :remote-addr is set."
  (let ((remote-addr (getf *env* :remote-addr)))
    (if (and remote-addr
             (member remote-addr *trusted-proxies* :test #'string=))
        (or (%first-forwarded-ip (request-header "X-Forwarded-For"))
            (request-header "X-Real-IP")
            remote-addr)
        remote-addr)))

(defun get-client-ip ()
  "Back-compat wrapper for client-ip. Prefer client-ip in new code."
  (client-ip))

(defmacro! with-rate-limit ((&key (max-requests 100)
                                  (window-seconds 60)
                                  (key '(get-client-ip))
                                  (namespace :ip))
                             &body body)
  "Wrap handler with rate limiting. Returns 429 Too Many Requests when
   the limit is exceeded.

   :KEY       Rate-limit bucket key — evaluated once per call. Defaults
              to (get-client-ip), preserving the per-IP behaviour of
              earlier callers. For per-account limits, supply explicitly
              (e.g. (format nil \"login:~A\" email)).
   :NAMESPACE Namespace partition (default :ip). Different namespaces
              have disjoint bounded stores; a flood in one cannot evict
              entries in another. Conventional namespaces: :ip for
              client-IP buckets, :login for per-account buckets. Any
              other keyword auto-installs a namespace with default caps.

   Per-account keying does NOT replace per-IP keying. An attacker can
   spam distinct fake account names; eviction prevents OOM, but cycling
   resets counters. Auth endpoints should layer both — IP-keyed outer
   (:namespace :ip), account-keyed inner (:namespace :login) — and let
   the consumer decide the thresholds.

   The bucket variable is gensymed so the body may freely reference
   lol-web/server::rate-limit-key from outer lexical scope without
   anaphoric capture."
  `(let ((,g!rate-limit-key ,key))
     (if (check-rate-limit ,g!rate-limit-key
                           :max-requests ,max-requests
                           :window-seconds ,window-seconds
                           :namespace ,namespace)
         (progn ,@body)
         (error-response 429
                         :content-type "text/html; charset=utf-8"
                         :message (minimal-error-html "429 Too Many Requests" "429"
                                                      "You're making too many requests. Please slow down.")))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Authentication Hooks (mechanism, not policy)
;;; ═══════════════════════════════════════════════════════════════════════════
;;;
;;; The framework owns the *gate*; the consumer owns the *decision*. Two
;;; thunks form an opaque adapter — AUTHENTICATED-P returns truthy iff the
;;; current request is authenticated; CURRENT-PRINCIPAL-THUNK returns an
;;; opaque value (keyword, plist, struct, hash-table, ID — the framework
;;; never inspects its shape).
;;;
;;; Installation: pass :auth (list :authenticated-p FN :current-principal FN)
;;; to make-app. The auth-hooks middleware (built inside make-app, see
;;; app.lisp:%make-auth-hooks-middleware) captures the thunks in a lexical
;;; closure and sets *env*'s :lol-web.auth.hooks key to a single (authp .
;;; principal) cons before each route runs. with-auth and current-principal
;;; read that cons once per invocation, so a paired (authp, principal) pair
;;; lands or doesn't, never one without the other. Two apps in one image
;;; carry independent closures and therefore independent hooks.

(defun %callable-thunk-p (x)
  "True when X is a thunk the auth gate may invoke — a function object, or
   NIL for an unsupplied half of the hooks cons. Symbols are deliberately
   excluded: the framework stores closures, so a symbol in this slot is a
   forged env value, not a framework-installed hook."
  (or (null x) (functionp x)))

(defun %valid-auth-hooks (hooks)
  "Return HOOKS when it is the framework-shaped auth cons — a cons whose car
   (authenticated-p thunk) and cdr (current-principal thunk) are each NIL or
   a function. Returns NIL for any other shape, so a forged or malformed
   :lol-web.auth.hooks env entry makes the auth gate fail closed instead of
   funcalling attacker-influenced data."
  (and (consp hooks)
       (%callable-thunk-p (car hooks))
       (%callable-thunk-p (cdr hooks))
       hooks))

(defun current-principal ()
  "Return the principal for the current request, or NIL when no auth
   middleware is installed. The returned value is opaque — consumers know
   its shape, the framework does not."
  (let ((hooks (%valid-auth-hooks (getf *env* :lol-web.auth.hooks))))
    (and hooks (cdr hooks) (funcall (cdr hooks)))))

(defun current-principal-of-env (env)
  "Return the principal for the request whose Clack environment is ENV,
   or NIL when no auth middleware is installed. Mirrors current-principal
   for callers that hold ENV directly — streaming-gate auth-fn callbacks
   run outside the route-handler dynamic extent that binds *env*, so the
   *env*-reading variant would observe a stale or unbound value."
  (let ((hooks (%valid-auth-hooks (getf env :lol-web.auth.hooks))))
    (and hooks (cdr hooks) (funcall (cdr hooks)))))

(defun session-get-of-env (env key)
  "Return ENV's :lack.session entry under KEY, or NIL when the session
   middleware is absent or KEY is unset. Mirrors session-get for callers
   that hold ENV directly (streaming-gate auth-fn callbacks, custom
   middleware) where *env* is not yet bound."
  (let ((session (getf env :lack.session)))
    (when session
      (gethash key session))))

(defun %dispatch-on-unauthorized (value)
  "Runtime arm for with-auth's :on-unauthorized when the form is not a
   literal integer or string. Dispatches on VALUE's runtime type:
   integer -> status response; string -> 302 redirect; function or
   fbound symbol -> funcall (consumer returns the full response triple).
   Anything else signals — a typo'd keyword must not silently open the
   route."
  (cond
    ((integerp value)
     (error-response value
                     :content-type "text/html; charset=utf-8"
                     :message (minimal-error-html
                               (format nil "~D ~A" value
                                       (http-status-text value))
                               (princ-to-string value)
                               "Authentication required.")))
    ((stringp value)
     (unless (safe-redirect-path-p value)
       (error "with-auth :on-unauthorized string must be a same-origin path ~
               (starts with `/`, not `//`, no scheme), got ~S"
              value))
     (redirect-response value))
    ((or (functionp value)
         (and (symbolp value) (fboundp value)))
     (funcall value))
    (t
     (error "with-auth :on-unauthorized runtime value must be an integer ~
             status, a string redirect path, or a callable returning a ~
             response triple, got ~S (~S)"
            value (type-of value)))))

(defmacro! with-auth ((&key (on-unauthorized 401)) &body body)
  "Wrap handler with auth gating. ON-UNAUTHORIZED selects the deny
   response shape:
     INTEGER status (default 401) — error-response with that status
     STRING redirect path         — 302 with Location header
     callable (function/fbound)   — funcall'd at deny time, must return
                                    a Clack response triple
   A LITERAL value that is neither integer nor string signals at macro
   expansion so a typo'd keyword cannot ship. Non-literal forms defer to
   runtime dispatch (see %dispatch-on-unauthorized) — this lets
   parameters and other indirections through:
     (defparameter *deny-status* 401)
     (with-auth (:on-unauthorized *deny-status*) ...)

   Fail-closed: when no auth middleware is installed (env carries no
   :lol-web.auth.hooks) or the authenticated-p thunk returns NIL, the
   route denies. The hook cons is captured once per invocation so a
   mid-request env mutation cannot swap the gate under the running
   handler.

   Example:
     (defroute \"/account\" (:method :get)
       (with-auth ()
         (render-account-page (current-principal))))

     (defroute \"/admin\" (:method :get)
       (with-auth (:on-unauthorized \"/sign-in\")
         (render-admin-page)))

     (defroute \"/api/private\" (:method :get)
       (with-auth (:on-unauthorized (lambda ()
                                      (json-response '(:error \"auth required\")
                                                     :status 401)))
         (api-payload)))"
  (cond
    ((integerp on-unauthorized)
     (let ((status on-unauthorized))
       `(let ((,g!hooks (%valid-auth-hooks (getf *env* :lol-web.auth.hooks))))
          (if (and ,g!hooks (car ,g!hooks) (funcall (car ,g!hooks)))
              (progn ,@body)
              (error-response ,status
                              :content-type "text/html; charset=utf-8"
                              :message (minimal-error-html
                                        ,(format nil "~D ~A" status
                                                 (http-status-text status))
                                        ,(princ-to-string status)
                                        "Authentication required."))))))
    ((stringp on-unauthorized)
     (unless (safe-redirect-path-p on-unauthorized)
       (error "with-auth :on-unauthorized literal string must be a ~
               same-origin path (starts with `/`, not `//`, no scheme), ~
               got ~S" on-unauthorized))
     `(let ((,g!hooks (%valid-auth-hooks (getf *env* :lol-web.auth.hooks))))
        (if (and ,g!hooks (car ,g!hooks) (funcall (car ,g!hooks)))
            (progn ,@body)
            (redirect-response ,on-unauthorized))))
    ((constantp on-unauthorized)
     (error "with-auth :on-unauthorized literal must be an integer status ~
             or a string redirect path, got ~S (~S)"
            on-unauthorized (type-of on-unauthorized)))
    (t
     `(let ((,g!hooks (%valid-auth-hooks (getf *env* :lol-web.auth.hooks)))
            (,g!on-unauth ,on-unauthorized))
        (if (and ,g!hooks (car ,g!hooks) (funcall (car ,g!hooks)))
            (progn ,@body)
            (%dispatch-on-unauthorized ,g!on-unauth))))))
