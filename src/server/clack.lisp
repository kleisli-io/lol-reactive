;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/SERVER; Base: 10 -*-
;;;; Clack request/response abstraction layer
;;;;
;;;; Provides a clean API over Clack's env plist for request handling
;;;; and standardized response builders.

(in-package :lol-web/server)

;;; ============================================================================
;;; REQUEST ENVIRONMENT
;;; ============================================================================

(defvar *env* nil
  "Current Clack request environment (plist).
   Bound during request handling.")

(defvar *response-headers* nil
  "Accumulated response headers for current request.
   Used by add-response-header and included in final response.")

;;; ============================================================================
;;; REQUEST BODY CAP
;;; ============================================================================
;;;
;;; Lives here (loaded before app.lisp) so request-body's no-cached-body
;;; fallback can refuse an oversize body before allocating, while
;;; %read-raw-body-bytes and the acceptor's 413 handler in app.lisp reach
;;; these as backward references.

(defparameter *max-request-body-bytes* (* 8 1024 1024)
  "Hard cap on raw request body bytes — pre-allocation check against the
   declared Content-Length, post-read check against actual byte count
   (catches chunked encodings and headers that under-declare the body).
   When exceeded, the reader signals REQUEST-BODY-TOO-LARGE and the
   dispatcher returns 413 without invoking the handler chain.
   NIL disables the cap (testing only).")

(define-condition request-body-too-large (error)
  ((limit    :initarg :limit    :reader request-body-too-large-limit)
   (declared :initarg :declared :initform nil :reader request-body-too-large-declared)
   (actual   :initarg :actual   :initform nil :reader request-body-too-large-actual))
  (:report
   (lambda (c stream)
     (format stream "Request body exceeds cap of ~D bytes (declared=~A actual=~A)"
             (request-body-too-large-limit c)
             (request-body-too-large-declared c)
             (request-body-too-large-actual c))))
  (:documentation
   "Signalled when the declared Content-Length exceeds
    *MAX-REQUEST-BODY-BYTES*, or when the buffered body length exceeds the
    cap after read. The dispatcher converts this to a 413 response. Tests
    use the reader accessors to confirm which arm fired."))

(defun %check-request-body-cap (&key declared actual)
  "Pure cap-check. Signals REQUEST-BODY-TOO-LARGE when DECLARED or ACTUAL
   exceeds the active *MAX-REQUEST-BODY-BYTES* cap; returns NIL otherwise.
   Each call passes exactly one of DECLARED or ACTUAL so the slot on the
   signalled condition pins which arm fired."
  (let ((cap *max-request-body-bytes*))
    (when cap
      (when (and declared (> declared cap))
        (error 'request-body-too-large :limit cap :declared declared))
      (when (and actual (> actual cap))
        (error 'request-body-too-large :limit cap :actual actual))))
  nil)

;;; ============================================================================
;;; REQUEST ACCESSORS
;;; ============================================================================

(defun request-path ()
  "Get request path from Clack env.
   Returns the path portion of the URL (e.g., \"/api/users\")."
  (getf *env* :path-info "/"))

(defun request-method ()
  "Get request method as keyword (:GET, :POST, :PUT, :DELETE, etc.)."
  (getf *env* :request-method :get))

(defun request-query-string ()
  "Get raw query string (without leading ?)."
  (getf *env* :query-string ""))

(defun request-header (name)
  "Get request header by name (case-insensitive).
   NAME can be a string or keyword."
  (let ((headers (getf *env* :headers)))
    (when headers
      (gethash (string-downcase (string name)) headers))))

(defun request-content-type ()
  "Get Content-Type header value."
  (getf *env* :content-type))

(defun request-content-length ()
  "Get Content-Length as integer, or NIL if not present."
  (getf *env* :content-length))

(defun %decode-request-octets (octets)
  "Decode OCTETS (a byte vector) as UTF-8 to a string — the single chokepoint
   for byte->string decoding of request bodies. On an invalid UTF-8 sequence
   it signals MALFORMED-JSON-BODY instead of letting babel's
   CHARACTER-DECODING-ERROR escape to the 500 handler. Both body-decode
   callers already handle MALFORMED-JSON-BODY — parse-request-json maps it to
   HTTP-BAD-REQUEST (400) and %csrf-token-from-json-body swallows it to NIL
   (fail-closed) — so a non-UTF-8 body fails clean/closed rather than 500.
   The MALFORMED-JSON-BODY condition is defined later in this file; the
   '(error 'malformed-json-body ...) reference resolves at call time."
  (handler-case
      (babel:octets-to-string octets :encoding :utf-8)
    (babel:character-decoding-error (e)
      (error 'malformed-json-body :input octets :reason e))))

(defun request-body ()
  "Get raw request body as a UTF-8 decoded string.
   Prefers the cached octet vector populated by build-clack-env so that
   repeated calls return the same body — historically reading :raw-body
   as a stream would silently return NIL on second access. Falls back
   to draining :raw-body for environments that bypass build-clack-env."
  (let ((cached (getf *env* :lol/cached-body)))
    (cond
      (cached
       (%decode-request-octets cached))
      (t
       (let ((body-stream (getf *env* :raw-body)))
         (when body-stream
           (let ((content-length (request-content-length)))
             (if content-length
                 (progn
                   ;; Refuse an oversize declared body before allocating it.
                   (%check-request-body-cap :declared content-length)
                   (let ((octets (make-array content-length :element-type '(unsigned-byte 8))))
                     (read-sequence octets body-stream)
                     (%decode-request-octets octets)))
                 (let ((octets (alexandria:read-stream-content-into-byte-vector body-stream)))
                   ;; No declared length: cap against the drained byte count.
                   (%check-request-body-cap :actual (length octets))
                   (when (> (length octets) 0)
                     (%decode-request-octets octets)))))))))))

;;; ============================================================================
;;; JSON ENCODE / DECODE
;;; ============================================================================
;;;
;;; The public API is encode-json-string and decode-json-string. Decoded values
;;; come back as alists with kebab-cased keyword keys, lists for arrays, and
;;; NIL for null. Encoding accepts the same shape: alists become JSON objects,
;;; proper lists become arrays, NIL becomes null. The internal helpers below
;;; bridge between this shape and the underlying jzon parser/stringifier.

(defun %camel-to-kebab-key (s)
  "Map a JSON object key to a keyword by inserting a hyphen at each
   lowercase→uppercase boundary then upcasing (\"componentId\" →
   \"COMPONENT-ID\"); resolve via SAFE-COERCE-KEYWORD. Unknown keys
   ride through as the original string so the pool stays bounded."
  (let ((normalised
          (with-output-to-string (out)
            (loop for c across s
                  for i from 0
                  do (when (and (> i 0)
                                (upper-case-p c)
                                (lower-case-p (char s (1- i))))
                       (write-char #\- out))
                     (write-char (char-upcase c) out)))))
    (or (safe-coerce-keyword normalised) s)))

(defun %jzon-to-alist-shape (elt)
  "Recursively convert a jzon-parsed element to the public decode shape:
   alists with keyword keys for known names, string keys otherwise;
   lists for arrays; NIL for null."
  (cond
    ((stringp elt) elt)
    ((hash-table-p elt)
     (loop for k being the hash-keys of elt using (hash-value v)
           collect (cons (%camel-to-kebab-key k)
                         (%jzon-to-alist-shape v))))
    ((vectorp elt)
     (map 'list #'%jzon-to-alist-shape elt))
    ((eq elt 'null) nil)
    (t elt)))

(defun %alist-of-conses-p (x)
  "True iff X is a non-empty list whose every element is (atom . anything).
   Heuristic for treating X as an alist-encoded JSON object."
  (and (consp x)
       (every (lambda (cell) (and (consp cell) (atom (car cell)))) x)))

(defun %coerce-key-to-string (k)
  (cond ((stringp k) k)
        ((keywordp k) (string-downcase (symbol-name k)))
        ((symbolp k) (string-downcase (symbol-name k)))
        (t (princ-to-string k))))

(defun %coerce-for-jzon (x)
  "Recursively coerce alist/list/scalar shapes into jzon-encodable forms.
   Alists → hash-tables (string keys), proper lists → vectors, NIL → null."
  (cond
    ((null x) 'null)
    ((eq x t) t)
    ((hash-table-p x) x)
    ((stringp x) x)
    ((numberp x) x)
    ((%alist-of-conses-p x)
     (let ((ht (make-hash-table :test 'equal)))
       (dolist (cell x ht)
         (setf (gethash (%coerce-key-to-string (car cell)) ht)
               (%coerce-for-jzon (cdr cell))))))
    ((vectorp x) (map 'vector #'%coerce-for-jzon x))
    ((listp x) (map 'vector #'%coerce-for-jzon x))
    ((symbolp x) (string-downcase (symbol-name x)))
    (t x)))

(defun encode-json-string (data)
  "Encode DATA to a JSON string. Auto-detects alists and encodes them as
   JSON objects; encodes proper lists as arrays; NIL as null; T as true."
  (com.inuoe.jzon:stringify (%coerce-for-jzon data)))

(defparameter *json-body-max-depth* 32
  "Cap on object/array nesting depth for request-body JSON parses.
   Tighter than jzon's 128 default — request bodies in lol-web routes
   never exceed a handful of nesting levels; a 32-deep payload is
   resource exhaustion, not a legitimate document.")

(defparameter *json-body-max-string-length* 65536
  "Cap on individual JSON string-key or string-value length for
   request-body parses, in characters. Tighter than jzon's 1 MiB
   default — a single field beyond this is over the same architectural
   class as the request body cap and would slip past it via
   chunk-boundary fragmentation if jzon's own limit stayed at default.")

(defconstant +json-null+ :json-null
  "Sentinel returned for a top-level JSON null body.")

(define-condition malformed-json-body (error)
  ((input :initarg :input :reader malformed-json-body-input)
   (reason :initarg :reason :reader malformed-json-body-reason))
  (:report
   (lambda (c stream)
     (format stream "Malformed JSON body: ~A"
             (malformed-json-body-reason c)))))

(defun decode-json-string (string)
  "Parse STRING as JSON. Returns alists with kebab-cased keyword keys,
   lists for arrays, +JSON-NULL+ for top-level null, and NIL for empty
   input. Malformed, over-depth, and over-length input signals
   MALFORMED-JSON-BODY."
  (when (and string (> (length string) 0))
    (handler-case
        (let ((parsed (com.inuoe.jzon:parse
                       string
                       :max-depth *json-body-max-depth*
                       :max-string-length *json-body-max-string-length*)))
          (if (eq parsed 'null)
              +json-null+
              (%jzon-to-alist-shape parsed)))
      (com.inuoe.jzon:json-error (e)
        (error 'malformed-json-body :input string :reason e)))))

(defun parse-request-json ()
  "Parse the request body as JSON, memoizing the result in *env* under
   :lol/cached-body-json. Returns NIL if the body is empty; malformed JSON
   signals HTTP-BAD-REQUEST.

   Single chokepoint for JSON-body parsing: every caller (request-body-json
   and the :json-body extractor in :lol-web/extractors) routes through this
   one to avoid double-decoding the same request payload. Returns the alist
   shape produced by decode-json-string so callers can use
   (cdr (assoc :foo body-json))."
  (let ((cached (getf *env* :lol/cached-body-json 'unbound)))
    (if (eq cached 'unbound)
        (handler-case
            (let ((parsed (decode-json-string (request-body))))
              ;; Cache even NIL so an empty body doesn't get re-parsed on
              ;; every accessor call.
              (setf (getf *env* :lol/cached-body-json) parsed)
              parsed)
          (malformed-json-body ()
            (error 'http-bad-request :body "Malformed JSON body")))
        cached)))

(defun request-body-json ()
  "Parse request body as JSON. Returns NIL if body is empty.
   Memoized via parse-request-json — calling this multiple times in one
   request hits the cache after the first decode."
  (parse-request-json))

;;; ============================================================================
;;; PARAMETER ACCESSORS
;;; ============================================================================

(defun query-param (name)
  "Get query parameter by name.
   NAME is a string. Returns NIL if not found."
  (let ((params (getf *env* :query-parameters)))
    (cdr (assoc name params :test #'string=))))

(defun query-params ()
  "Get all query parameters as alist of (name . value)."
  (getf *env* :query-parameters))

(defun post-param (name)
  "Get POST parameter by name.
   NAME is a string. Returns NIL if not found."
  (let ((params (getf *env* :body-parameters)))
    (cdr (assoc name params :test #'string=))))

(defun post-params ()
  "Get all POST parameters as alist of (name . value)."
  (getf *env* :body-parameters))

(defun param (name)
  "Get parameter by name, checking POST first, then query string.
   NAME is a string."
  (or (post-param name)
      (query-param name)))

;;; ============================================================================
;;; RESPONSE BUILDERS
;;; ============================================================================

(defun response (status &key headers body content-type)
  "Build a Clack response list: (status headers-plist body-list).

   STATUS: HTTP status code (integer)
   HEADERS: Additional headers as plist
   BODY: Response body (string or list of strings)
   CONTENT-TYPE: Convenience for setting Content-Type header

   Accumulated headers from *response-headers* are included automatically."
  (let ((all-headers (append
                      (when content-type
                        (list :content-type content-type))
                      *response-headers*
                      headers)))
    (list status
          all-headers
          (if (listp body) body (list body)))))

(defun html-response (body &key (status 200) headers)
  "Build an HTML response with proper Content-Type.

   BODY: HTML string
   STATUS: HTTP status code (default 200)
   HEADERS: Additional headers"
  (response status
            :content-type "text/html; charset=utf-8"
            :headers headers
            :body body))

(defun json-response (data &key (status 200) headers)
  "Build a JSON response, encoding DATA to JSON string.

   DATA: Lisp data structure to encode
   STATUS: HTTP status code (default 200)
   HEADERS: Additional headers"
  (response status
            :content-type "application/json; charset=utf-8"
            :headers headers
            :body (encode-json-string data)))

(defun text-response (body &key (status 200) headers)
  "Build a plain text response.

   BODY: Text string
   STATUS: HTTP status code (default 200)
   HEADERS: Additional headers"
  (response status
            :content-type "text/plain; charset=utf-8"
            :headers headers
            :body body))

(define-condition unsafe-redirect-error (error)
  ((url :initarg :url :reader unsafe-redirect-error-url)
   (reason :initarg :reason :reader unsafe-redirect-error-reason))
  (:report
   (lambda (c stream)
     (format stream "redirect-response refused unsafe URL ~S: ~A"
             (unsafe-redirect-error-url c)
             (unsafe-redirect-error-reason c)))))

(defparameter *canonical-host* nil
  "The server's canonical host token (host or host:port), lowercased — e.g.
   \"app.example.com\". The same-origin redirect gate (safe-host NIL) anchors
   to this value; the attacker-controllable Host header is never trusted as an
   anchor. A deployment that knows its own hostname pins it here so an absolute
   redirect must match the canonical origin. When NIL the gate has no origin to
   trust and refuses every absolute redirect (path-relative URLs still pass).")

(defun %url-has-backslash-p (url)
  "True when URL carries a literal backslash or its percent-encoding (%5c).
   Browsers normalise `\\` to `/`, so `/\\evil.com` becomes the
   protocol-relative `//evil.com` after normalisation while a naive parser
   still reads it as a same-origin path. Any backslash-bearing URL is unsafe
   as a redirect target — mirrors safe-path-segment-p's denylist."
  (and (stringp url)
       (or (find #\\ url)
           (search "%5c" url :test #'char-equal))
       t))

(defun %url-authority-host (url)
  "Return the host portion of URL, or one of:
     NIL          — URL is path-relative (no scheme, no authority);
                    always same-origin.
     :opaque      — URL carries a scheme but no // authority
                    (data:, javascript:, mailto:, ...); never a
                    same-origin redirect target.
     :malformed   — URL is empty or non-string.
   Otherwise returns the host string between // and the next / (or end).

   The check is intentionally permissive about port and userinfo — the
   caller compares hosts as strings, so `:8080` and `user@` are part of
   the host token if present in URL."
  (cond
    ((not (stringp url)) :malformed)
    ((zerop (length url)) :malformed)
    ;; A backslash (literal or %5c) lets `/\evil.com` normalise to the
    ;; protocol-relative `//evil.com` in the browser — never a safe
    ;; redirect target.
    ((%url-has-backslash-p url) :malformed)
    ;; Protocol-relative: //host/path
    ((and (>= (length url) 2)
          (char= (char url 0) #\/)
          (char= (char url 1) #\/))
     (let* ((rest (subseq url 2))
            (slash (position #\/ rest)))
       (if slash (subseq rest 0 slash) rest)))
    ;; Path-relative: starts with / but not //
    ((char= (char url 0) #\/)
     nil)
    ;; scheme:... — find the authority block
    ((let ((colon (position #\: url)))
       (and colon
            (every (lambda (c)
                     (or (alpha-char-p c) (digit-char-p c)
                         (char= c #\+) (char= c #\-) (char= c #\.)))
                   (subseq url 0 colon))
            colon))
     (let* ((colon (position #\: url))
            (after (subseq url (1+ colon))))
       (cond
         ((and (>= (length after) 2)
               (char= (char after 0) #\/)
               (char= (char after 1) #\/))
          (let* ((host-rest (subseq after 2))
                 (term (or (position-if (lambda (c)
                                          (or (char= c #\/) (char= c #\?) (char= c #\#)))
                                        host-rest)
                           (length host-rest))))
            (subseq host-rest 0 term)))
         (t :opaque))))
    ;; Bareword (no scheme, no leading /): treat as malformed for
    ;; redirect purposes.
    (t :malformed)))

(defun %request-host ()
  "Host token of the current request's Host header, port stripped,
   or NIL when no *env* is bound or the header is missing."
  (when *env*
    (let ((header (request-header :host)))
      (when header
        (let ((colon (position #\: header)))
          (string-downcase
            (if colon (subseq header 0 colon) header)))))))

(defun %redirect-anchor-host ()
  "Host token the same-origin redirect gate compares against: the configured
   *canonical-host* (port stripped, lowercased) when set, otherwise the
   request's Host header via %request-host. Pinning *canonical-host* refuses
   an attacker-forged Host header from widening the same-origin allowlist."
  (if *canonical-host*
      (let* ((header (string *canonical-host*))
             (colon (position #\: header)))
        (string-downcase (if colon (subseq header 0 colon) header)))
      (%request-host)))

(defun %safe-redirect-host-p (url safe-host)
  "True iff URL is safe to redirect to under SAFE-HOST policy.

   - URL path-relative          → always safe.
   - URL opaque/malformed       → never safe.
   - SAFE-HOST :any             → any host accepted (caller's responsibility).
   - SAFE-HOST list-of-strings  → URL's host must be a member (case-insensitive).
   - SAFE-HOST NIL (default)    → URL's host must equal *canonical-host*; when
                                  that is unset every absolute URL is refused.
                                  The Host header is never trusted as an anchor."
  (let ((host (%url-authority-host url)))
    (cond
      ((null host) t)
      ((or (eq host :opaque) (eq host :malformed)) nil)
      ((eq safe-host :any) t)
      ;; Same-origin default: trust only the configured canonical origin.
      ;; Unset *canonical-host* yields a NIL origin, refusing every absolute
      ;; URL; the attacker-controllable Host header is never an anchor here.
      ((null safe-host)
       (let ((origin (and *canonical-host* (%redirect-anchor-host))))
         (and origin (string= (string-downcase host) origin))))
      ((listp safe-host)
       (let ((lc (string-downcase host)))
         (some (lambda (allowed)
                 (string= lc (string-downcase allowed)))
               safe-host)))
      (t (let ((origin (%redirect-anchor-host)))
           (and origin (string= (string-downcase host) origin)))))))

(defun safe-redirect-path-p (path)
  "True iff PATH is a same-origin redirect path: a string starting with
   `/` and not with `//` (which would parse as protocol-relative).
   Empty strings, scheme-bearing strings (`javascript:...`,
   `http://...`), protocol-relative `//host/...`, and backslash-bearing
   paths (`/\\evil.com`, which browsers normalise to protocol-relative)
   all fail."
  (and (stringp path)
       (>= (length path) 1)
       (char= (char path 0) #\/)
       (or (= (length path) 1)
           (not (char= (char path 1) #\/)))
       (not (%url-has-backslash-p path))))

(defun redirect-response (url &key (status 302) headers (safe-host nil))
  "Build a redirect response.

   URL: Target URL for redirect (path-relative, protocol-relative, or
        scheme://host/path).
   STATUS: HTTP redirect status: 301, 302, 303, 307, or 308.
   HEADERS: Additional headers.
   SAFE-HOST: Host-policy gate. NIL (default) accepts only same-origin
              redirects — path-relative URLs always pass; an absolute URL
              must match *canonical-host*, and is refused outright when it
              is unset. :ANY skips host validation (caller takes
              responsibility). A list of hostname strings is an allowlist.

   Refuses opaque-scheme URLs (data:, javascript:, mailto:, ...)
   unconditionally — these are never same-origin and have no reason to
   appear in a Location header."
  (unless (%safe-redirect-host-p url safe-host)
    (error 'unsafe-redirect-error
           :url url
           :reason (let ((host (%url-authority-host url)))
                     (cond
                       ((eq host :opaque) "opaque-scheme URL")
                       ((eq host :malformed) "malformed URL")
                       ((eq safe-host :any) "internal: :any should accept")
                       ((null safe-host)
                        (if *canonical-host*
                            (format nil "host ~S is not the canonical origin ~S"
                                    host (%redirect-anchor-host))
                            (format nil "host ~S refused: no canonical origin configured"
                                    host)))
                       ((listp safe-host)
                        (format nil "host ~S not in allowlist ~S" host safe-host))
                       (t (format nil "host ~S does not match request origin ~S"
                                  host (%redirect-anchor-host)))))))
  (unless (member status '(301 302 303 307 308))
    (error "redirect-response status must be one of 301, 302, 303, 307, 308; got ~S."
           status))
  (validate-header-value url)
  (response status
            :headers (append (list :location url) headers)
            :body nil))

(defun error-response (status &key message headers content-type)
  "Build an error response.

   STATUS: HTTP error status code
   MESSAGE: Error message (optional)
   HEADERS: Additional headers
   CONTENT-TYPE: Response content type"
  (response status
            :content-type (or content-type "text/plain; charset=utf-8")
            :headers headers
            :body (or message (http-status-text status))))

;;; ============================================================================
;;; RESPONSE HEADER ACCUMULATION
;;; ============================================================================

(defmacro with-response-headers (() &body body)
  "Execute BODY with fresh response header accumulation.
   Headers added via add-response-header will be included in final response."
  `(let ((*response-headers* nil))
     ,@body))

(defun add-response-header (name value)
  "Add a header to the current response.
   NAME: Header name (string or keyword)
   VALUE: Header value (string)

   NAME and VALUE both pass through `validate-header-value' so a CR/LF
   in either signals at the accumulation site instead of forging a
   header line at emission. Must be called within with-response-headers
   context."
  (let ((name-string (etypecase name
                       (keyword (symbol-name name))
                       (string  name))))
    (validate-header-value name-string)
    (validate-header-value value))
  (push value *response-headers*)
  (push (if (keywordp name)
            name
            (intern (string-upcase name) :keyword))
        *response-headers*))

(defun get-response-headers ()
  "Get currently accumulated response headers as plist."
  *response-headers*)

;;; ============================================================================
;;; HTTP STATUS HELPERS
;;; ============================================================================

(defun http-status-text (code)
  "Get standard text for HTTP status code."
  (case code
    (200 "OK")
    (201 "Created")
    (204 "No Content")
    (301 "Moved Permanently")
    (302 "Found")
    (304 "Not Modified")
    (400 "Bad Request")
    (401 "Unauthorized")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (409 "Conflict")
    (422 "Unprocessable Entity")
    (429 "Too Many Requests")
    (500 "Internal Server Error")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (otherwise "Unknown Status")))

;;; ============================================================================
;;; SESSION ACCESSORS (Lack middleware integration)
;;; ============================================================================

(defun session-get (key)
  "Get value from Lack session.
   KEY can be a symbol or string.
   Returns NIL if session not available or key not found."
  (let ((session (getf *env* :lack.session)))
    (when session
      (gethash key session))))

(defun session-set (key value)
  "Set value in Lack session.
   KEY can be a symbol or string.
   VALUE is any serializable Lisp value.
   Returns VALUE, or NIL if session not available."
  (let ((session (getf *env* :lack.session)))
    (when session
      (setf (gethash key session) value))))

(defun session-delete (key)
  "Remove key from Lack session.
   Returns T if key was present, NIL otherwise."
  (let ((session (getf *env* :lack.session)))
    (when session
      (remhash key session))))

(defun session-clear ()
  "Clear all session data.
   Returns T if session was available, NIL otherwise."
  (let ((session (getf *env* :lack.session)))
    (when session
      (clrhash session)
      t)))

(defun session-keys ()
  "Get list of all session keys."
  (let ((session (getf *env* :lack.session)))
    (when session
      (loop for key being the hash-keys of session collect key))))

(define-condition streaming-session-rotate-error (error)
  ()
  (:documentation "Signaled by session-rotate when *env* carries
   :clack.streaming t. Streaming handlers write directly to *client-socket*
   and never return a Lack-shaped response; the session middleware's
   FINALIZE step that honours :change-id is therefore never invoked. A
   silent T return would suggest rotation succeeded while the SID, cookie,
   and store entry actually stayed put — fixation persists through the
   stream. Callers must rotate from a non-streaming endpoint before
   initiating the stream.")
  (:report
   (lambda (c stream)
     (declare (ignore c))
     (format stream
             "session-rotate called inside a streaming handler ~
              (:clack.streaming t in *env*). Lack's session FINALIZE step ~
              is never invoked for streamed responses, so rotation would ~
              silently no-op. Rotate from a non-streaming endpoint before ~
              initiating the stream."))))

(defun session-rotate (&key scrub (preserve '()))
  "Rotate the current session ID and regenerate the CSRF token. Call
   immediately after a successful login or any privilege escalation to
   defeat session fixation: an attacker who planted a known SID in the
   victim's browser cannot reuse it once authenticated.

   Implementation: flips the :change-id flag in the Lack session
   middleware's options plist. On the response leg, Lack's FINALIZE step
   generates a fresh SID, removes the session under the old SID, stores
   the session hash under the new SID, and rewrites the cookie.

   The session entry under \"csrf-token\" is unconditionally removed so
   the next request forces a fresh token (anti-fixation: a token read
   pre-auth from a public page must not flow into the authenticated
   session). \"csrf-token\" is on a permanent deny-list — listing it in
   :preserve signals an error.

   :SCRUB T  Also remove every session entry not listed in :PRESERVE.
             Without :scrub the default keeps everything except the
             always-cleared csrf-token; with :scrub the default keeps
             nothing.
   :PRESERVE List of session-key strings whose entries survive a
             :scrub t rotation. Empty by default (scrub keeps nothing).
             Listing \"csrf-token\" signals an error.

   Signals STREAMING-SESSION-ROTATE-ERROR when called inside a streaming
   handler (:clack.streaming t in *env*) — Lack's FINALIZE step never
   runs for streamed responses, so rotation would silently no-op.

   Returns T when the rotation flag was set, NIL if no session middleware
   is bound (e.g., a route running outside a session context)."
  (when (member "csrf-token" preserve :test #'string=)
    (error "session-rotate :preserve must not include \"csrf-token\" — ~
            csrf-token is on a permanent deny-list because preserving it ~
            across rotation re-enables the pre-auth-token fixation vector ~
            that unconditional regeneration is here to close."))
  (when (getf *env* :clack.streaming)
    (error 'streaming-session-rotate-error))
  (let ((opts (getf *env* :lack.session.options)))
    (when opts
      (let ((session (getf *env* :lack.session)))
        (when (hash-table-p session)
          (cond
            (scrub
             (let ((kept (loop for key in preserve
                               for val = (gethash key session)
                               when val collect (cons key val))))
               (clrhash session)
               (dolist (kv kept)
                 (setf (gethash (car kv) session) (cdr kv)))))
            (t
             (remhash "csrf-token" session)))))
      (setf (getf opts :change-id) t)
      (setf (getf *env* :lack.session.options) opts)
      t)))

(defun session-expire ()
  "Expire the current session at end-of-request. Call on logout. Lack's
   FINALIZE will remove the session under the current SID and set an
   expired cookie.

   Returns T on success, NIL if no session middleware is bound."
  (let ((opts (getf *env* :lack.session.options)))
    (when opts
      (setf (getf opts :expire) t)
      (setf (getf *env* :lack.session.options) opts)
      t)))

(defun current-session-id ()
  "Return the current Lack session ID string, or NIL if no session
   middleware is bound. Primarily useful for audit logging — never echo
   the session ID into HTML or URLs.

   Named CURRENT-SESSION-ID rather than SESSION-ID to mirror the parallel
   CURRENT-PRINCIPAL form and to avoid colliding with local variable names
   in modules that :USE :LOL-WEB/SERVER (notably :LOL-WEB/WIZARDS, which
   binds SESSION-ID as a lambda parameter throughout wizards.lisp)."
  (getf (getf *env* :lack.session.options) :id))

;;; ============================================================================
;;; CSRF INTEGRATION (Lack middleware)
;;; ============================================================================

(defun csrf-token ()
  "Get current CSRF token from session.
   Works with both Lack CSRF middleware and custom CSRF (security.lisp).
   Returns NIL if session not available."
  (session-get "csrf-token"))
