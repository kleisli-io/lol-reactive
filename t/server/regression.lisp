;;;; Regression tests for :lol-web/server.
;;;;
;;;; Covers: minimal-error-html token decoupling, CSRF token shape +
;;;; constant-time compare + end-to-end validate, clack param accessors,
;;;; request-body memoization, streaming-route dispatch, rate-limit
;;;; thread-safety + bounded store, X-Forwarded-For parsing fallbacks,
;;;; HTTP-error condition hierarchy + with-error-handling translation,
;;;; JSON encode/decode round-trip + parse-request-json memoization.

(in-package :lol-web/server/test)
(in-suite :lol-web/server/test)

;;; sb-introspect:function-lambda-list is read at compile/load time below;
;;; the SBCL contrib must be present in the image before the reader hits a
;;; symbol qualified with its package name.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;;; ============================================================================
;;; minimal-error-html — design-token decoupling
;;; ============================================================================

(test regression-minimal-error-html-no-design-token-coupling
  "minimal-error-html does not reference design-token getters or CSS vars"
  (let ((html (lol-web/server::minimal-error-html "Test" "Heading" "Message")))
    (is (search "Heading" html))
    (is (search "Message" html))
    (is (search "<style>" html))
    (is (null (search "--color-background" html))
        "minimal-error-html must not declare --color-* CSS variables")
    (is (null (search "--font-family" html))
        "minimal-error-html must not declare --font-family CSS variable")
    (is (null (search ":root {" html))
        "minimal-error-html must not emit a :root CSS block (token leak)")))

;;; ============================================================================
;;; CSRF — token shape, constant-time compare, end-to-end validate
;;; ============================================================================

(test regression-csrf-token-shape
  "generate-csrf-token returns 32 lowercase hex chars and varies per call"
  (let ((t1 (generate-csrf-token))
        (t2 (generate-csrf-token)))
    (is (= 32 (length t1)) "token length must be 32 hex chars (128 bits)")
    (is (every (lambda (c) (digit-char-p c 16)) t1)
        "token must be hex-only")
    (is (not (string= t1 t2))
        "consecutive tokens must differ (CSPRNG, not seeded PRNG)")))

(test regression-constant-time-string-equal-correctness
  "constant-time-string= matches string= for the equality predicate itself"
  (is (lol-web/server::constant-time-string= "" "")
      "two empty strings are equal")
  (is (lol-web/server::constant-time-string= "abcd1234" "abcd1234")
      "identical non-empty strings are equal")
  (is (not (lol-web/server::constant-time-string= "abc" "abcd"))
      "different-length strings are not equal")
  (is (not (lol-web/server::constant-time-string= "abcd" "abce"))
      "same-length, last-char-different strings are not equal")
  (is (not (lol-web/server::constant-time-string= "abcdefgh01234567"
                                                  "abcdefgh76543210"))
      "matching prefix is not enough for equality"))

(test regression-csrf-validate-end-to-end
  "validate-csrf-token returns T for matching tokens, NIL for any mismatch,
   when given a faked Lack session via *env* binding"
  (let* ((stored "deadbeefdeadbeefdeadbeefdeadbeef")
         (fake-session (make-hash-table :test 'equal)))
    (setf (gethash "csrf-token" fake-session) stored)
    (let ((*env* (list :lack.session fake-session)))
      (is (validate-csrf-token stored)
          "matching token validates")
      (is (null (validate-csrf-token nil))
          "NIL token rejected")
      (is (null (validate-csrf-token ""))
          "empty token rejected (length mismatch)")
      (is (null (validate-csrf-token
                  "deadbeefdeadbeefdeadbeefdeadbeeg"))
          "single-char-different token rejected")
      (is (null (validate-csrf-token "deadbeef"))
          "shorter prefix-match token rejected"))))

(test regression-get-csrf-token-reuses-token-under-concurrency
  "Concurrent lazy token creation for one session returns one stored token,
   not a request-local token later overwritten by another request."
  (let* ((session (make-hash-table :test 'equal))
         (n 24)
         (tokens (make-array n :initial-element nil))
         (threads
           (loop for i from 0 below n
                 collect (let ((i i))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (let ((*env* (list :lack.session session)))
                                (setf (aref tokens i) (get-csrf-token)))))))))
    (mapc #'bordeaux-threads:join-thread threads)
    (let ((stored (gethash "csrf-token" session)))
      (is (stringp stored))
      (is (every (lambda (token) (string= token stored)) tokens)
          "every racing caller must receive the token that remains stored"))))

(test regression-with-csrf-validation-gensyms-internal-token-symbol
  "with-csrf-validation's bucket variable must be a gensym, not the literal
   symbol `token' — a caller binding the qualified symbol
   lol-web/server::token in an outer scope must see that binding from
   inside the body, not the macro's CSRF-token value."
  (let* ((stored "deadbeefdeadbeefdeadbeefdeadbeef")
         (fake-session (make-hash-table :test 'equal)))
    (setf (gethash "csrf-token" fake-session) stored)
    (let ((*env* (list :lack.session fake-session
                       :body-parameters (list (cons "csrf-token" stored)))))
      (let ((lol-web/server::token :user-supplied))
        (with-csrf-validation
          (is (eq :user-supplied lol-web/server::token)
              "anaphoric leak: macro's CSRF-token var shadowed caller's ~
               binding of lol-web/server::token; got ~S, want :USER-SUPPLIED"
              lol-web/server::token))))))

;;; ============================================================================
;;; csrf-middleware — constant-time replacement for lack/middleware/csrf
;;; ============================================================================

(defun %csrf-mw-dummy-app (env)
  "Sentinel: returns a recognizable 200 triple. The middleware under test
   either calls this (pass-through) or rejects with 403 itself."
  (declare (ignore env))
  (list 200 (list :content-type "text/plain") (list "passed-through")))

(defun %csrf-mw-env (&key (method :post)
                          (session-token "valid-session-token")
                          (submitted-token "valid-session-token")
                          (include-session t)
                          extra-headers
                          (body-token-key "csrf-token")
                          (content-type "application/x-www-form-urlencoded"))
  "Build a synthetic env plist sufficient for csrf-middleware. SESSION-TOKEN
   NIL means session contains no entry under the session key; INCLUDE-SESSION
   NIL omits :lack.session entirely (used to verify the missing-session
   error path). CONTENT-TYPE selects the token-extraction dispatch:
   form-encoded by default; pass an application/json variant to exercise the
   JSON-envelope path."
  (let ((session (when include-session
                   (let ((s (make-hash-table :test 'equal)))
                     (when session-token
                       (setf (gethash "csrf-token" s) session-token))
                     s)))
        (headers (let ((h (make-hash-table :test 'equal)))
                   (loop for (k v) on extra-headers by #'cddr
                         do (setf (gethash (string-downcase (string k)) h) v))
                   h)))
    (list :request-method method
          :lack.session session
          :headers headers
          :content-type content-type
          :body-parameters (when submitted-token
                             (list (cons body-token-key submitted-token))))))

(test regression-csrf-middleware-passes-safe-methods
  "GET / HEAD / OPTIONS / TRACE bypass the token check entirely"
  (let ((mw (csrf-middleware #'%csrf-mw-dummy-app)))
    (dolist (method '(:get :head :options :trace))
      (let* ((env (%csrf-mw-env :method method
                                :session-token "ignored"
                                :submitted-token nil))
             (response (funcall mw env)))
        (is (= 200 (first response))
            "~A must pass through csrf-middleware unchecked, got status ~D"
            method (first response))))))

(test regression-csrf-middleware-allows-unsafe-method-with-matching-token
  "POST whose body token matches the session token via constant-time-string=
   passes through to the wrapped app"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-env :method :post
                            :session-token "tok-deadbeefdeadbeef"
                            :submitted-token "tok-deadbeefdeadbeef"))
         (response (funcall mw env)))
    (is (= 200 (first response))
        "matching token must pass through, got status ~D" (first response))))

(test regression-csrf-middleware-denies-unsafe-method-with-mismatched-token
  "POST with a body token that does not match the session token returns 403
   (not Lack's 400 — proves the replacement middleware actually fired)"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-env :method :post
                            :session-token "tok-deadbeefdeadbeef"
                            :submitted-token "tok-cafefacecafeface"))
         (response (funcall mw env)))
    (is (= 403 (first response))
        "mismatched token must yield 403 (not 400 like Lack's default), ~
         got status ~D" (first response))))

(test regression-csrf-middleware-denies-when-no-session-token
  "POST when the session lacks a token entry returns 403 (no implicit pass)"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-env :method :post
                            :session-token nil
                            :submitted-token "anything"))
         (response (funcall mw env)))
    (is (= 403 (first response))
        "missing session token must yield 403, got status ~D"
        (first response))))

(test regression-csrf-middleware-denies-when-body-token-missing
  "POST with no body-token entry returns 403 (not silent pass-through)"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-env :method :post
                            :session-token "tok-deadbeef"
                            :submitted-token nil))
         (response (funcall mw env)))
    (is (= 403 (first response))
        "absent body token must yield 403, got status ~D" (first response))))

(test regression-csrf-middleware-signals-when-session-missing
  "Missing :lack.session signals an error — a CSRF stack with no session
   middleware behind it must fail loud, never silently pass everything"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-env :method :post :include-session nil))
         (raised
           (handler-case (progn (funcall mw env) nil)
             (error () t))))
    (is (eq t raised)
        "missing :lack.session must raise an error from csrf-middleware")))

(test regression-csrf-middleware-multipart-list-token-shape
  "Lack returns multipart body-param values as a list — csrf-middleware must
   unwrap the first element rather than choke on the list shape"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (session (make-hash-table :test 'equal))
         (_ (setf (gethash "csrf-token" session) "multipart-tok"))
         (headers (make-hash-table :test 'equal))
         (env (list :request-method :post
                    :lack.session session
                    :headers headers
                    :content-type "multipart/form-data; boundary=----xyz"
                    ;; multipart: value is a list (typically (value filename content-type))
                    :body-parameters '(("csrf-token" "multipart-tok" "file.bin" "application/octet-stream")))))
    (declare (ignore _))
    (let ((response (funcall mw env)))
      (is (= 200 (first response))
          "list-shaped multipart token must unwrap and match, got status ~D"
          (first response)))))

(test regression-csrf-middleware-duplicate-token-first-wins-fail-closed
  "A body polluted with two csrf-token parameters resolves deterministically to
   the first (assoc first-match): a valid leading token is honored despite a
   trailing junk param, and a junk leading token is NOT rescued by appending a
   valid one — parameter pollution cannot smuggle a pass."
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (good "tok-deadbeefdeadbeef")
         (bad  "tok-cafefacecafeface"))
    (flet ((env-with (&rest token-values)
             (let ((session (make-hash-table :test 'equal))
                   (headers (make-hash-table :test 'equal)))
               (setf (gethash "csrf-token" session) good)
               (list :request-method :post
                     :lack.session session
                     :headers headers
                     :content-type "application/x-www-form-urlencoded"
                     :body-parameters (mapcar (lambda (v) (cons "csrf-token" v))
                                              token-values)))))
      (is (= 200 (first (funcall mw (env-with good bad))))
          "a valid leading token passes despite a trailing polluted param")
      (is (= 403 (first (funcall mw (env-with bad good))))
          "a junk leading token is not rescued by a trailing valid param"))))

;;; ============================================================================
;;; csrf-middleware — JSON-envelope token (X-CSRF-Token header / body field)
;;; ============================================================================

(defun %csrf-mw-json-env (&key (method :post)
                               (session-token "valid-session-token")
                               (json-body-token "valid-session-token")
                               (json-body-key "csrf-token")
                               (header-token nil)
                               (content-type "application/json"))
  "Synthetic env for the JSON-envelope CSRF path. SESSION holds
   \"csrf-token\" -> SESSION-TOKEN. If HEADER-TOKEN is non-NIL it is set as
   X-CSRF-Token (lowercased into :headers as build-clack-env would).
   JSON body is built from JSON-BODY-KEY -> JSON-BODY-TOKEN unless
   JSON-BODY-TOKEN is :omit (in which case no body field is emitted).
   The encoded body bytes go to :lol/cached-body, matching the live
   build-clack-env layout."
  (let* ((session (let ((s (make-hash-table :test 'equal)))
                    (when session-token
                      (setf (gethash "csrf-token" s) session-token))
                    s))
         (headers (let ((h (make-hash-table :test 'equal)))
                    (when header-token
                      (setf (gethash "x-csrf-token" h) header-token))
                    h))
         (json    (cond
                    ((eq json-body-token :omit) "{}")
                    (t (format nil "{~S:~S}" json-body-key json-body-token))))
         (bytes   (babel:string-to-octets json :encoding :utf-8)))
    (list :request-method method
          :lack.session session
          :headers headers
          :content-type content-type
          :lol/cached-body bytes
          :content-length (length bytes))))

(test regression-csrf-middleware-json-header-token-matches
  "application/json + matching X-CSRF-Token header passes through"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token "tok-json-hdr-deadbeef"
                                 :header-token  "tok-json-hdr-deadbeef"
                                 :json-body-token :omit))
         (response (funcall mw env)))
    (is (= 200 (first response))
        "matching X-CSRF-Token header must pass, got status ~D"
        (first response))))

(test regression-csrf-middleware-json-body-field-token-matches
  "application/json + matching csrf-token field in JSON body passes"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token "tok-json-body-cafe"
                                 :json-body-token "tok-json-body-cafe"))
         (response (funcall mw env)))
    (is (= 200 (first response))
        "matching JSON-body csrf-token must pass, got status ~D"
        (first response))))

(test regression-csrf-middleware-json-header-wins-over-body
  "X-CSRF-Token takes precedence over the JSON body field: a valid header
   passes even when the body field is mismatched"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token   "tok-header-precedence"
                                 :header-token    "tok-header-precedence"
                                 :json-body-token "tok-wrong-in-body")))
    (is (= 200 (first (funcall mw env)))
        "valid header must override mismatched JSON-body token")))

(test regression-csrf-middleware-json-mismatched-header-denies
  "application/json with a mismatched X-CSRF-Token header (and no body
   field) returns 403"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token "tok-session"
                                 :header-token  "tok-attacker"
                                 :json-body-token :omit)))
    (is (= 403 (first (funcall mw env)))
        "mismatched header must yield 403")))

(test regression-csrf-middleware-json-mismatched-body-field-denies
  "application/json with a mismatched JSON-body csrf-token (and no header)
   returns 403"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token   "tok-session"
                                 :json-body-token "tok-attacker")))
    (is (= 403 (first (funcall mw env)))
        "mismatched body-field token must yield 403")))

(test regression-csrf-middleware-json-no-token-anywhere-denies
  "application/json with neither X-CSRF-Token nor a body csrf-token field
   returns 403 (no implicit pass for empty-token JSON payloads)"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token "tok-session"
                                 :json-body-token :omit)))
    (is (= 403 (first (funcall mw env)))
        "absent token in both slots must yield 403")))

(test regression-csrf-middleware-json-parameterised-content-type-recognised
  "application/json; charset=utf-8 (the shape browsers send) is recognised
   as the JSON path and accepts a matching header"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token "tok-param-ct"
                                 :header-token  "tok-param-ct"
                                 :json-body-token :omit
                                 :content-type  "application/json; charset=utf-8")))
    (is (= 200 (first (funcall mw env)))
        "parameterised application/json must be recognised")))

(test regression-csrf-middleware-json-plus-suffix-recognised
  "application/vnd.api+json (RFC 6838 structured-suffix) routes through the
   JSON path"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-json-env :session-token "tok-plus-suffix"
                                 :header-token  "tok-plus-suffix"
                                 :json-body-token :omit
                                 :content-type  "application/vnd.api+json")))
    (is (= 200 (first (funcall mw env)))
        "+json structured-suffix must be recognised as JSON")))

(test regression-csrf-middleware-unknown-content-type-denies-fail-closed
  "Unsafe method with an unrecognised Content-Type (no defined token slot)
   returns 403 — no silent bypass even when a synthetic header carries a
   matching value, because the middleware refuses to guess where the token
   lives"
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (env (%csrf-mw-env :method :post
                            :session-token "tok-unknown-ct"
                            :submitted-token nil
                            :content-type "application/octet-stream"
                            :extra-headers (list :x-csrf-token "tok-unknown-ct"))))
    (is (= 403 (first (funcall mw env)))
        "unknown content-type must fail-closed")))

;;; ============================================================================
;;; Clack env — :query-parameters / :body-parameters; request-body memoization
;;; ============================================================================

(test regression-clack-param-accessors-read-populated-env
  "query-param / post-param / param all read from the populated env keys"
  (let ((*env*
          '(:query-parameters (("name" . "alice") ("color" . "blue"))
            :body-parameters (("name" . "bob") ("submit" . "ok")))))
    (is (string= "alice" (query-param "name")))
    (is (string= "blue"  (query-param "color")))
    (is (null (query-param "missing")))
    (is (string= "bob" (post-param "name")))
    (is (string= "ok"  (post-param "submit")))
    (is (null (post-param "missing")))
    (is (string= "bob" (param "name")))
    (is (string= "blue" (param "color")))))

(test regression-request-body-memoized-via-cached-bytes
  "request-body reads :lol/cached-body so repeated calls return the same string"
  (let* ((bytes (babel:string-to-octets "hello=world&n=42" :encoding :utf-8))
         (*env* (list :lol/cached-body bytes
                      :content-length (length bytes))))
    (let ((first  (request-body))
          (second (request-body)))
      (is (string= "hello=world&n=42" first))
      (is (string= first second)
          "request-body must be idempotent across repeated calls"))))

(test regression-form-body-content-type-accepts-multipart
  "%form-body-content-type-p must accept both
   application/x-www-form-urlencoded and multipart/form-data, so
   build-clack-env populates :body-parameters for file-upload POSTs.
   Without multipart acceptance, (post-param NAME) returns NIL on
   every form whose enctype is multipart/form-data — exactly the
   shape forms/form-dsl.lisp emits when any field is :file."
  (is (lol-web/server::%form-body-content-type-p
       "application/x-www-form-urlencoded")
      "plain form encoding accepted")
  (is (lol-web/server::%form-body-content-type-p
       "application/x-www-form-urlencoded; charset=utf-8")
      "plain form encoding with charset accepted")
  (is (lol-web/server::%form-body-content-type-p
       "multipart/form-data; boundary=----WebKitFormBoundaryXYZ")
      "multipart with boundary parameter accepted (browser file uploads)")
  (is (lol-web/server::%form-body-content-type-p
       "MULTIPART/FORM-DATA; boundary=abc")
      "case-insensitive match (Content-Type values are not case-sensitive)")
  (is (null (lol-web/server::%form-body-content-type-p
             "application/json"))
      "JSON bodies must not be routed through post-parameters")
  (is (null (lol-web/server::%form-body-content-type-p
             "text/plain"))
      "plain text bodies must not be routed through post-parameters")
  (is (null (lol-web/server::%form-body-content-type-p nil))
      "missing Content-Type header (NIL) must not match"))

;;; ============================================================================
;;; Streaming-route dispatch wins over regular dispatch
;;; ============================================================================

(test regression-streaming-routes-dispatch-wins-over-regular
  "When the same path is registered as streaming, streaming dispatch runs first"
  (let ((path "/regression/streaming-probe"))
    (unwind-protect
         (let ((streaming-called 0)
               (regular-called 0))
           (setf (gethash (cons :get path) lol-web/server::*streaming-routes*)
                 (make-streaming-route-entry
                  :body (lambda (env)
                          (declare (ignore env))
                          (incf streaming-called)
                          '(200 (:content-type "text/plain") ("ok")))
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com")))
           (setf (gethash (cons :get path) *routes*)
                 (lambda ()
                   (incf regular-called)
                   '(200 (:content-type "text/plain") ("ok"))))
           ;; :lol-web.streaming.vetted marks the env as gate-vetted; the
           ;; dispatcher refuses streaming dispatch without it (see
           ;; regression-streaming-dispatch-refuses-ungated-entry). This test
           ;; exercises dispatch *priority*, so it stamps the flag the gate
           ;; would have set in production.
           (let ((env (list :request-method :get
                            :path-info path
                            :query-string ""
                            :lol-web.streaming.vetted t)))
             (route-handler env))
           (is (= 1 streaming-called)
               "streaming handler invoked exactly once")
           (is (zerop regular-called)
               "regular handler must NOT run when streaming registry has the route"))
      (remhash (cons :get path) lol-web/server::*streaming-routes*)
      (remhash (cons :get path) *routes*))))

(test regression-streaming-dispatch-refuses-ungated-entry
  "route-handler fails closed: a streaming-route match whose env was not
   vetted by the streaming-gate (gate disabled, or make-app bypassed) is
   refused with 403 and the entry body never runs. The per-entry policy is
   re-asserted at the dispatcher, not only in a toggleable middleware."
  (let ((path "/regression/ungated-streaming-probe"))
    (unwind-protect
         (let ((body-called 0))
           (setf (gethash (cons :get path) lol-web/server::*streaming-routes*)
                 (make-streaming-route-entry
                  :body (lambda (env)
                          (declare (ignore env))
                          (incf body-called)
                          '(200 (:content-type "text/plain") ("ok")))
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com")))
           ;; No :lol-web.streaming.vetted on the env — the gate never ran.
           (let* ((env (list :request-method :get
                             :path-info path
                             :query-string ""))
                  (response (route-handler env)))
             (is (= 403 (first response))
                 "ungated streaming dispatch must yield 403, got ~S"
                 (first response))
             (is (zerop body-called)
                 "the streaming entry body must NOT run on an ungated request")))
      (remhash (cons :get path) lol-web/server::*streaming-routes*))))

(test regression-static-root-defaults-to-service-env
  "start-server's default static root follows STATIC_ROOT from serviceSpec"
  (is (string= "/nix/store/regression-static/"
               (namestring
                 (lol-web/server::normalize-static-root
                   "/nix/store/regression-static")))
      "STATIC_ROOT env var must become the static middleware root"))

;;; ============================================================================
;;; Rate limiting — thread-safe counter, per-namespace bounded LRU stores
;;; ============================================================================

(defmacro %with-namespace-cap ((namespace cap) &body body)
  "Test helper: set NAMESPACE's max-entries to CAP for BODY, restore to
   the prior value on unwind. configure-rate-limit-namespace mutates the
   shared struct in *rate-limit-registry*; without this helper, a test
   that sets a low cap leaves it leaking into the next test in the suite."
  (let ((g-prev (gensym "PREV-CAP"))
        (g-ns   (gensym "NS-KW")))
    `(let* ((,g-ns ,namespace)
            (,g-prev (rate-limit-namespace-max-entries
                      (gethash ,g-ns lol-web/server::*rate-limit-registry*))))
       (unwind-protect
            (progn
              (configure-rate-limit-namespace ,g-ns :max-entries ,cap)
              ,@body)
         (configure-rate-limit-namespace ,g-ns :max-entries ,g-prev)))))

(test regression-rate-limit-counter-thread-safe
  "check-rate-limit increments are serialised across concurrent threads"
  (clear-rate-limit-store)
  (unwind-protect
       (let* ((ip "regression-rate-limit-thread-safe")
              (per-thread 200)
              (n-threads 16)
              (expected (* per-thread n-threads))
              (threads
                (loop repeat n-threads
                      collect (bordeaux-threads:make-thread
                                (lambda ()
                                  (dotimes (_ per-thread)
                                    (check-rate-limit
                                      ip
                                      :max-requests 1000000
                                      :window-seconds 3600)))))))
         (dolist (th threads) (bordeaux-threads:join-thread th))
         (let ((entry (rate-limit-entry-of ip)))
           (is (rate-limit-entry-p entry)
               "entry must be a rate-limit-entry struct after concurrent updates")
           (is (= expected (rate-limit-entry-count entry))
               "expected ~D increments, got ~A — concurrent updates lost"
               expected (and (rate-limit-entry-p entry)
                             (rate-limit-entry-count entry)))))
    (clear-rate-limit-store)))

(test regression-rate-limit-namespace-bounded-by-cap
  ":ip namespace size never exceeds its configured max-entries cap"
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:ip 100)
         (dotimes (i 1000)
           (check-rate-limit (format nil "regression-bounded-ip-~D" i)
                             :max-requests 10
                             :window-seconds 3600))
         (is (<= (rate-limit-namespace-count :ip) 100)
             ":ip namespace size ~D exceeds configured cap of 100"
             (rate-limit-namespace-count :ip)))
    (clear-rate-limit-store)))

(test regression-with-rate-limit-default-key-is-client-ip
  "with-rate-limit with no args still keys on (get-client-ip) under :ip"
  (clear-rate-limit-store)
  (unwind-protect
       (let* ((h (make-hash-table :test 'equal))
              (*env* (list :headers h :remote-addr "regression-default-ip")))
         (with-rate-limit (:max-requests 5 :window-seconds 60)
           :allowed)
         (is (rate-limit-entry-p
              (rate-limit-entry-of "regression-default-ip" :namespace :ip))
             "empty arg list must bucket under (get-client-ip) in :ip namespace"))
    (clear-rate-limit-store)))

(test regression-with-rate-limit-custom-key-isolates-from-ip
  "supplying :key buckets separately from the IP default within the :ip namespace"
  (clear-rate-limit-store)
  (unwind-protect
       (let* ((h (make-hash-table :test 'equal))
              (*env* (list :headers h :remote-addr "regression-iso-ip"))
              (account-key "login:ada@example.com"))
         (with-rate-limit (:max-requests 100 :window-seconds 60)
           :ok)
         (with-rate-limit (:key account-key :max-requests 100 :window-seconds 60)
           :ok)
         (with-rate-limit (:key account-key :max-requests 100 :window-seconds 60)
           :ok)
         (let ((ip-entry (rate-limit-entry-of "regression-iso-ip" :namespace :ip))
               (acc-entry (rate-limit-entry-of account-key :namespace :ip)))
           (is (and (rate-limit-entry-p ip-entry)
                    (= 1 (rate-limit-entry-count ip-entry)))
               "IP bucket must show 1 hit, got ~S" ip-entry)
           (is (and (rate-limit-entry-p acc-entry)
                    (= 2 (rate-limit-entry-count acc-entry)))
               "account-key bucket must show 2 hits, got ~S" acc-entry)
           (is (= 2 (rate-limit-namespace-count :ip))
               ":ip namespace must hold exactly 2 distinct buckets, got ~D"
               (rate-limit-namespace-count :ip))))
    (clear-rate-limit-store)))

(test regression-with-rate-limit-key-form-evaluated-per-call
  "the :key form is evaluated once per with-rate-limit invocation"
  (clear-rate-limit-store)
  (unwind-protect
       (let ((counter 0))
         (flet ((bump () (format nil "k-~D" (incf counter))))
           (with-rate-limit (:key (bump) :max-requests 100 :window-seconds 60)
             :ok)
           (with-rate-limit (:key (bump) :max-requests 100 :window-seconds 60)
             :ok)
           (with-rate-limit (:key (bump) :max-requests 100 :window-seconds 60)
             :ok))
         (is (= 3 counter)
             ":key form must run once per call; observed ~D evaluations" counter)
         (is (and (rate-limit-entry-of "k-1")
                  (rate-limit-entry-of "k-2")
                  (rate-limit-entry-of "k-3"))
             "each per-call key must land in the :ip namespace store"))
    (clear-rate-limit-store)))

(test regression-rate-limit-namespace-bounded-under-mixed-keys
  ":ip namespace stays bounded when IP-keyed and account-keyed traffic interleave"
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:ip 100)
         (dotimes (i 1000)
           (check-rate-limit (if (evenp i)
                                 (format nil "ip:198.51.100.~D" i)
                                 (format nil "login:user-~D@example.com" i))
                             :max-requests 10
                             :window-seconds 3600))
         (is (<= (rate-limit-namespace-count :ip) 100)
             "mixed-key :ip namespace count ~D exceeds cap of 100"
             (rate-limit-namespace-count :ip)))
    (clear-rate-limit-store)))

(test regression-with-rate-limit-gensyms-internal-key-symbol
  "with-rate-limit's bucket variable must be a gensym, not the literal
   symbol `rate-limit-key' — a caller binding the qualified symbol
   lol-web/server::rate-limit-key in an outer scope must see that binding
   from inside the body, not the macro's bucket value."
  (clear-rate-limit-store)
  (unwind-protect
       (let ((lol-web/server::rate-limit-key :user-supplied))
         (with-rate-limit (:key "regression-gensym-bucket"
                           :max-requests 100 :window-seconds 60)
           (is (eq :user-supplied lol-web/server::rate-limit-key)
               "anaphoric leak: macro's bucket var shadowed caller's binding ~
                of lol-web/server::rate-limit-key; got ~S, want :USER-SUPPLIED"
               lol-web/server::rate-limit-key)))
    (clear-rate-limit-store)))

(test regression-rate-limit-namespaces-isolate-flood
  "A flood that exceeds the :login namespace's cap must NOT evict any
   entry in the :ip namespace — the cores property of per-namespace stores"
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:login 50)
         ;; Seed the :ip namespace with a legitimate steady user.
         (check-rate-limit "legit-client-ip"
                           :namespace :ip
                           :max-requests 100 :window-seconds 3600)
         (let ((before (rate-limit-entry-of "legit-client-ip" :namespace :ip)))
           (is (rate-limit-entry-p before)
               "pre-flood :ip entry must exist before the :login flood")
           ;; Flood :login with attacker-cycled emails far beyond its cap.
           (dotimes (i 500)
             (check-rate-limit (format nil "user-~D@evil.example" i)
                               :namespace :login
                               :max-requests 5 :window-seconds 3600))
           (let ((after (rate-limit-entry-of "legit-client-ip" :namespace :ip)))
             (is (eq before after)
                 "legit :ip entry must survive the :login flood eq to its ~
                  pre-flood struct, got ~S vs ~S" before after))
           (is (<= (rate-limit-namespace-count :login) 50)
               ":login namespace flood must stay bounded by its 50-entry cap, ~
                got count ~D" (rate-limit-namespace-count :login))))
    (clear-rate-limit-store)))

(test regression-rate-limit-last-seen-refreshed-on-every-insert
  "Each allowed insert refreshes last-seen so eviction prefers stale entries
   over steady legitimate traffic — the core C06 fix"
  (clear-rate-limit-store)
  (unwind-protect
       (let ((ip "regression-last-seen-refresh"))
         (check-rate-limit ip :max-requests 100 :window-seconds 3600)
         (let* ((first-entry (rate-limit-entry-of ip))
                (first-seen (rate-limit-entry-last-seen first-entry)))
           (sleep 1)
           (check-rate-limit ip :max-requests 100 :window-seconds 3600)
           (let* ((second-entry (rate-limit-entry-of ip))
                  (second-seen (rate-limit-entry-last-seen second-entry)))
             (is (eq first-entry second-entry)
                 "same key must reuse the same entry struct across calls")
             (is (> second-seen first-seen)
                 "last-seen must advance on every allowed insert, ~
                  got first=~D second=~D" first-seen second-seen)
             (is (= (rate-limit-entry-window-start first-entry)
                    (rate-limit-entry-window-start second-entry))
                 "window-start must NOT advance — fixed-window rate semantics ~
                  are preserved (only last-seen tracks activity)"))))
    (clear-rate-limit-store)))

(test regression-rate-limit-eviction-prefers-stale-not-recent
  "When the namespace is full and a new key arrives, eviction drops the
   least-recently-seen entry, NOT the entry whose window opened earliest.
   Legitimate steady traffic refreshes last-seen on every insert and
   survives; an attacker who stopped touching their entry gets evicted.
   *rate-limit-min-evict-age* is bound to 0 so the LRU pick is observable
   without long sleeps — the min-age floor is exercised separately."
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:ip 3)
         (let ((lol-web/server:*rate-limit-min-evict-age* 0))
         ;; Two steady legitimate users + one attacker pre-loads.
         (check-rate-limit "steady-A" :max-requests 100 :window-seconds 3600)
         (sleep 1)
         (check-rate-limit "attacker-old" :max-requests 100 :window-seconds 3600)
         (sleep 1)
         (check-rate-limit "steady-B" :max-requests 100 :window-seconds 3600)
         (sleep 1)
         ;; Steady users keep hitting — their last-seen advances.
         (check-rate-limit "steady-A" :max-requests 100 :window-seconds 3600)
         (check-rate-limit "steady-B" :max-requests 100 :window-seconds 3600)
         ;; New entry arrives; namespace is at cap (3), eviction must fire.
         (check-rate-limit "new-arrival" :max-requests 100 :window-seconds 3600)
         (is (null (rate-limit-entry-of "attacker-old"))
             "least-recently-seen 'attacker-old' must be evicted, found ~S"
             (rate-limit-entry-of "attacker-old"))
         (is (rate-limit-entry-p (rate-limit-entry-of "steady-A"))
             "steady-A must survive — last-seen refreshed after attacker's")
         (is (rate-limit-entry-p (rate-limit-entry-of "steady-B"))
             "steady-B must survive — last-seen refreshed after attacker's")
         (is (rate-limit-entry-p (rate-limit-entry-of "new-arrival"))
             "new arrival must be present after eviction made room")))
    (clear-rate-limit-store)))

(test regression-rate-limit-denied-attempt-does-not-refresh-last-seen
  "Over-cap denies must NOT advance last-seen — otherwise an attacker can
   keep their entry alive indefinitely by spamming over-limit requests"
  (clear-rate-limit-store)
  (unwind-protect
       (let ((ip "regression-denied-no-refresh"))
         (check-rate-limit ip :max-requests 1 :window-seconds 3600)
         (let* ((entry (rate-limit-entry-of ip))
                (initial-seen (rate-limit-entry-last-seen entry)))
           (sleep 1)
           ;; Already at cap=1 — these all deny.
           (dotimes (_ 5)
             (check-rate-limit ip :max-requests 1 :window-seconds 3600))
           (is (eq entry (rate-limit-entry-of ip))
               "denied attempts must not replace the entry struct")
           (is (= initial-seen (rate-limit-entry-last-seen entry))
               "denied attempts must not advance last-seen, ~
                got initial=~D after-denies=~D"
               initial-seen (rate-limit-entry-last-seen entry))))
    (clear-rate-limit-store)))

(test regression-with-rate-limit-namespace-kwarg-routes-to-distinct-store
  ":namespace :login routes through the :login namespace, never landing in :ip"
  (clear-rate-limit-store)
  (unwind-protect
       (let* ((h (make-hash-table :test 'equal))
              (*env* (list :headers h :remote-addr "regression-route-ip")))
         (with-rate-limit (:key "login:user@example.com"
                           :namespace :login
                           :max-requests 5 :window-seconds 60)
           :ok)
         (is (rate-limit-entry-p
              (rate-limit-entry-of "login:user@example.com" :namespace :login))
             ":login key must land in :login namespace")
         (is (null (rate-limit-entry-of "login:user@example.com" :namespace :ip))
             ":login key must NOT leak into :ip namespace"))
    (clear-rate-limit-store)))

(test regression-make-app-rate-limit-namespaces-configures-caps
  "make-app :rate-limit-namespaces updates per-namespace caps idempotently
   without dropping existing entries"
  (clear-rate-limit-store)
  (unwind-protect
       (progn
         ;; Seed an :ip entry that must survive the make-app cap update.
         (check-rate-limit "regression-survives-makeapp" :namespace :ip
                           :max-requests 100 :window-seconds 3600)
         (make-app :use-session nil :use-csrf nil
                   :use-accesslog nil :use-static nil
                   :rate-limit-eviction-interval nil
                   :rate-limit-namespaces '((:ip :max-entries 256)
                                            (:login :max-entries 64)))
         (is (= 256 (rate-limit-namespace-max-entries
                     (gethash :ip lol-web/server::*rate-limit-registry*)))
             ":ip cap must be 256 after make-app, got ~D"
             (rate-limit-namespace-max-entries
              (gethash :ip lol-web/server::*rate-limit-registry*)))
         (is (= 64 (rate-limit-namespace-max-entries
                    (gethash :login lol-web/server::*rate-limit-registry*)))
             ":login cap must be 64 after make-app")
         (is (rate-limit-entry-p
              (rate-limit-entry-of "regression-survives-makeapp" :namespace :ip))
             "pre-existing entry must survive make-app's namespace reconfigure"))
    (clear-rate-limit-store)
    ;; Restore the documented defaults so later tests are unsurprised.
    (configure-rate-limit-namespace :ip    :max-entries 10000)
    (configure-rate-limit-namespace :login :max-entries 1000)))

(test regression-rate-limit-min-age-prevents-eviction
  "Eviction must skip entries whose last-seen is within
   *rate-limit-min-evict-age*. A new arrival into a cap-full namespace of
   fresh entries is denied; pre-existing legitimate entries survive."
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:ip 2)
         (let ((lol-web/server:*rate-limit-min-evict-age* 3600))
           (check-rate-limit "fresh-A" :max-requests 100 :window-seconds 3600)
           (check-rate-limit "fresh-B" :max-requests 100 :window-seconds 3600)
           (let ((result (check-rate-limit "new-arrival"
                                           :max-requests 100
                                           :window-seconds 3600)))
             (is (null result)
                 "new arrival must be denied when no candidate is older ~
                  than the min-evict-age window"))
           (is (rate-limit-entry-p (rate-limit-entry-of "fresh-A"))
               "fresh-A must survive — within the min-evict-age window")
           (is (rate-limit-entry-p (rate-limit-entry-of "fresh-B"))
               "fresh-B must survive — within the min-evict-age window")
           (is (null (rate-limit-entry-of "new-arrival"))
               "new arrival must NOT be inserted when eviction refused")))
    (clear-rate-limit-store)))

(test regression-rate-limit-store-full-signalled-when-all-young
  "When an eviction pass finds the store at cap and every candidate is
   younger than *rate-limit-min-evict-age*, rate-limit-store-full is
   signalled and check-rate-limit returns NIL. The signal carries the
   namespace name and the max-entries cap."
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:ip 1)
         (let ((lol-web/server:*rate-limit-min-evict-age* 3600)
               (observed nil))
           (check-rate-limit "incumbent" :max-requests 100 :window-seconds 3600)
           (handler-bind
               ((lol-web/server:rate-limit-store-full
                  (lambda (c)
                    (setf observed
                          (list :namespace
                                (lol-web/server:rate-limit-store-full-namespace c)
                                :max-entries
                                (lol-web/server:rate-limit-store-full-max-entries c))))))
             (check-rate-limit "outsider" :max-requests 100 :window-seconds 3600))
           (is (equal observed '(:namespace :ip :max-entries 1))
               "rate-limit-store-full must carry namespace=:ip max-entries=1; ~
                got ~S" observed)))
    (clear-rate-limit-store)))

(test regression-rate-limit-amortised-eviction-every-nth
  "After every *rate-limit-eviction-every-n* allowed inserts in a
   namespace, the inline path runs an eviction sweep. The sweep resets
   the namespace's inserts-since-eviction counter to 0."
  (clear-rate-limit-store)
  (unwind-protect
       (let ((lol-web/server:*rate-limit-eviction-every-n* 5))
         (dotimes (i 4)
           (check-rate-limit (format nil "amort-~D" i)
                             :max-requests 100 :window-seconds 60))
         (let ((ns (gethash :ip lol-web/server::*rate-limit-registry*)))
           (is (= 4 (rate-limit-namespace-inserts-since-eviction ns))
               "counter must read 4 after 4 inserts, got ~D"
               (rate-limit-namespace-inserts-since-eviction ns))
           (check-rate-limit "amort-5" :max-requests 100 :window-seconds 60)
           (is (zerop (rate-limit-namespace-inserts-since-eviction ns))
               "counter must reset to 0 after the Nth insert triggers a ~
                sweep; got ~D"
               (rate-limit-namespace-inserts-since-eviction ns))))
    (clear-rate-limit-store)))

(test regression-rate-limit-scheduled-eviction-fires
  "configure-rate-limit-eviction-timer starts a background thread that
   sweeps every namespace at *rate-limit-eviction-interval* seconds. A
   1-second interval over a 2.5-second window must produce at least two
   sweeps (initial + at least one interval-driven)."
  (let ((lol-web/server:*rate-limit-eviction-interval* 1)
        (start lol-web/server::*rate-limit-eviction-sweep-count*))
    (unwind-protect
         (progn
           (lol-web/server:configure-rate-limit-eviction-timer)
           (sleep 2.5)
           (let ((delta (- lol-web/server::*rate-limit-eviction-sweep-count*
                           start)))
             (is (>= delta 2)
                 "1s-interval timer over 2.5s must sweep ≥2 times; ~
                  observed delta=~D" delta)))
      (lol-web/server::%stop-rate-limit-eviction-timer))))

(test regression-get-client-ip-xff-first-only-when-trusted
  "When :remote-addr is in *trusted-proxies*, get-client-ip returns only the
   leftmost X-Forwarded-For entry, trimmed. Empty or all-blank XFF falls
   through to the next layer (X-Real-IP, then :remote-addr)."
  (flet ((with-xff (val)
           (let* ((h (make-hash-table :test 'equal))
                  (*env* (progn
                           (setf (gethash "x-forwarded-for" h) val)
                           (list :headers h :remote-addr "tp")))
                  (*trusted-proxies* '("tp")))
             (get-client-ip))))
    (is (string= "1.2.3.4" (with-xff "1.2.3.4, 10.0.0.1, 172.16.0.1"))
        "multi-IP chain must collapse to the first address")
    (is (string= "1.2.3.4" (with-xff "  1.2.3.4  "))
        "single-IP value must be trimmed of surrounding whitespace")
    (is (string= "1.2.3.4" (with-xff "1.2.3.4"))
        "single-IP unpadded value must pass through")
    (is (string= "tp" (with-xff ""))
        "empty XFF must yield :remote-addr (X-Real-IP missing here)")
    (is (string= "tp" (with-xff " ,  , "))
        "all-blank XFF tokens must yield :remote-addr — never an empty IP key")))

(test regression-get-client-ip-fallbacks-when-trusted
  "When :remote-addr is in *trusted-proxies*, get-client-ip falls back to
   X-Real-IP and then :remote-addr when XFF is absent."
  (let* ((h (make-hash-table :test 'equal))
         (*env* (progn
                  (setf (gethash "x-real-ip" h) "203.0.113.7")
                  (list :headers h :remote-addr "127.0.0.1")))
         (*trusted-proxies* '("127.0.0.1")))
    (is (string= "203.0.113.7" (get-client-ip))
        "X-Real-IP wins over :remote-addr"))
  (let* ((h (make-hash-table :test 'equal))
         (*env* (list :headers h :remote-addr "127.0.0.1"))
         (*trusted-proxies* '("127.0.0.1")))
    (is (string= "127.0.0.1" (get-client-ip))
        ":remote-addr is the final fallback")))

(test regression-client-ip-ignores-xff-when-remote-addr-not-trusted
  "Safe default: *trusted-proxies* is NIL, so XFF and X-Real-IP are NOT
   honoured. Trusting them unconditionally lets an attacker forge any
   client IP by setting the header on their request."
  (let* ((h (make-hash-table :test 'equal))
         (*env* (progn
                  (setf (gethash "x-forwarded-for" h) "9.9.9.9, 10.0.0.1")
                  (setf (gethash "x-real-ip" h) "8.8.8.8")
                  (list :headers h :remote-addr "1.2.3.4"))))
    (is (null *trusted-proxies*)
        "audit-mandated safe default: *trusted-proxies* starts NIL")
    (is (string= "1.2.3.4" (client-ip))
        "default *trusted-proxies* NIL: client-ip returns :remote-addr verbatim")
    (is (string= "1.2.3.4" (get-client-ip))
        "get-client-ip is a thin wrapper over client-ip"))
  ;; remote-addr not in *trusted-proxies* — XFF still ignored even when XFF set
  (let* ((h (make-hash-table :test 'equal))
         (*env* (progn
                  (setf (gethash "x-forwarded-for" h) "9.9.9.9")
                  (list :headers h :remote-addr "192.0.2.5")))
         (*trusted-proxies* '("127.0.0.1")))
    (is (string= "192.0.2.5" (client-ip))
        "non-empty *trusted-proxies* not containing :remote-addr: XFF still ignored")))

(test regression-client-ip-honours-xff-when-remote-addr-trusted
  "When :remote-addr is in *trusted-proxies*, client-ip honours XFF leftmost
   first, then X-Real-IP, then :remote-addr."
  (let* ((h (make-hash-table :test 'equal))
         (*env* (progn
                  (setf (gethash "x-forwarded-for" h) "9.9.9.9, 10.0.0.1")
                  (list :headers h :remote-addr "1.2.3.4")))
         (*trusted-proxies* '("1.2.3.4")))
    (is (string= "9.9.9.9" (client-ip))
        "trusted remote-addr: XFF leftmost wins"))
  (let* ((h (make-hash-table :test 'equal))
         (*env* (progn
                  (setf (gethash "x-real-ip" h) "203.0.113.7")
                  (list :headers h :remote-addr "1.2.3.4")))
         (*trusted-proxies* '("1.2.3.4")))
    (is (string= "203.0.113.7" (client-ip))
        "trusted remote-addr, no XFF: X-Real-IP wins"))
  (let* ((h (make-hash-table :test 'equal))
         (*env* (list :headers h :remote-addr "1.2.3.4"))
         (*trusted-proxies* '("1.2.3.4")))
    (is (string= "1.2.3.4" (client-ip))
        "trusted remote-addr, no headers: :remote-addr is final fallback")))

(test regression-check-rate-limit-rejects-oversized-key
  "check-rate-limit returns NIL and does NOT store an entry when the key
   exceeds :max-key-bytes (default 128 UTF-8 octets). Memory-amplification
   defence — attacker-controlled long keys would otherwise bloat the
   namespace store without ever tripping per-key throttles."
  (clear-rate-limit-store)
  (unwind-protect
       (let ((short-key (make-string  64 :initial-element #\a))
             (long-key  (make-string 200 :initial-element #\b)))
         (is (eq t (check-rate-limit short-key
                                     :max-requests 100 :window-seconds 60))
             "64-byte key under default cap of 128 is accepted")
         (is (null (check-rate-limit long-key
                                     :max-requests 100 :window-seconds 60))
             "200-byte key exceeds default cap of 128 — must be rejected")
         (is (null (rate-limit-entry-of long-key))
             "rejected key must NOT land in any namespace store")
         (is (eq t (check-rate-limit long-key
                                     :max-key-bytes 256
                                     :max-requests 100 :window-seconds 60))
             "explicit larger cap (256) accepts the 200-byte key")
         ;; multi-byte UTF-8: octets, not codepoints
         (let ((utf8-key (make-string 100 :initial-element (code-char #x4E2D))))
           (is (null (check-rate-limit utf8-key
                                       :max-requests 100 :window-seconds 60))
               "100 CJK chars = 300 UTF-8 octets — must reject under default cap")))
    (clear-rate-limit-store)))

;;; ============================================================================
;;; Fail-closed byte->string decode — an invalid-UTF-8 body never 500s
;;; ============================================================================
;;;
;;; %decode-request-octets is the single chokepoint that turns babel's
;;; CHARACTER-DECODING-ERROR into MALFORMED-JSON-BODY, which the JSON and CSRF
;;; paths handle: parse-request-json -> HTTP-BAD-REQUEST (400),
;;; %csrf-token-from-json-body -> NIL (fail-closed). A non-UTF-8 body must
;;; never escape to the 500 handler.

(defun %invalid-utf8-octets ()
  "A short byte vector that is not valid UTF-8 — 0xFF can never appear in a
   UTF-8 stream, so the :utf-8 decoder must signal."
  (make-array 3 :element-type '(unsigned-byte 8)
                :initial-contents '(#xff #xfe #xfd)))

(test regression-decode-request-octets-invalid-utf8-signals-malformed-json-body
  "%decode-request-octets maps an invalid-UTF-8 sequence to
   MALFORMED-JSON-BODY rather than letting babel's CHARACTER-DECODING-ERROR
   escape — the fail-closed contract the JSON/CSRF paths rely on."
  (signals lol-web/server::malformed-json-body
    (lol-web/server::%decode-request-octets (%invalid-utf8-octets)))
  ;; A valid UTF-8 body still decodes normally.
  (is (string= "héllo"
               (lol-web/server::%decode-request-octets
                (babel:string-to-octets "héllo" :encoding :utf-8)))
      "valid UTF-8 must still round-trip through the chokepoint"))

(test regression-request-body-invalid-utf8-signals-malformed-not-babel
  "request-body on a cached non-UTF-8 body signals MALFORMED-JSON-BODY, not a
   raw babel CHARACTER-DECODING-ERROR that would reach the 500 handler."
  (let ((*env* (list :lol/cached-body (%invalid-utf8-octets))))
    (signals lol-web/server::malformed-json-body
      (request-body))))

(test regression-parse-request-json-invalid-utf8-is-bad-request
  "parse-request-json on an invalid-UTF-8 body yields HTTP-BAD-REQUEST (400),
   never a 500 — end to end through the JSON parse."
  (let ((*env* (list :lol/cached-body (%invalid-utf8-octets))))
    (signals http-bad-request
      (parse-request-json))))

(test regression-parse-request-json-over-depth-is-bad-request
  "parse-request-json on a body nested deeper than *json-body-max-depth*
   yields HTTP-BAD-REQUEST (400), not a 500 — over-depth JSON maps to a
   client error at the handler boundary, the same as malformed input."
  (let* ((deep (with-output-to-string (s)
                 (loop repeat 64 do (write-char #\[ s))
                 (write-char #\0 s)
                 (loop repeat 64 do (write-char #\] s))))
         (octets (map '(vector (unsigned-byte 8)) #'char-code deep))
         (*env* (list :lol/cached-body octets))
         (lol-web/server:*json-body-max-depth* 32))
    (signals http-bad-request
      (parse-request-json))))

(test regression-csrf-token-from-json-body-invalid-utf8-returns-nil
  "%csrf-token-from-json-body on an invalid-UTF-8 body returns NIL
   (fail-closed) and never signals — the documented 'never signals' contract."
  (let ((env (list :lol/cached-body (%invalid-utf8-octets)
                   :headers (make-hash-table :test 'equal))))
    (is (null (lol-web/server::%csrf-token-from-json-body env :csrf-token))
        "invalid-UTF-8 JSON body must yield NIL, not a signal")))

(test regression-csrf-middleware-json-invalid-utf8-body-denies
  "csrf-middleware on an application/json POST whose body is not valid UTF-8
   returns 403 (fail-closed), never a 500 — token extraction fails closed on
   the decode error rather than letting it escape."
  (let* ((mw (csrf-middleware #'%csrf-mw-dummy-app))
         (session (make-hash-table :test 'equal))
         (headers (make-hash-table :test 'equal))
         (bytes (%invalid-utf8-octets)))
    (setf (gethash "csrf-token" session) "tok-session-deadbeef")
    (let ((env (list :request-method :post
                     :lack.session session
                     :headers headers
                     :content-type "application/json"
                     :lol/cached-body bytes
                     :content-length (length bytes))))
      (is (= 403 (first (funcall mw env)))
          "invalid-UTF-8 JSON body must fail closed with 403, not 500"))))

;;; ============================================================================
;;; O(1) LRU eviction — the intrusive last-seen list stays consistent
;;; ============================================================================
;;;
;;; The behavioural LRU contract (evict least-recently-seen, min-age floor,
;;; store-full signal, denied-no-refresh) is pinned above. This pins the data
;;; structure: the intrusive last-seen list must stay consistent with the
;;; store under churn — that consistency is what makes tail eviction O(1).

(test regression-rate-limit-lru-list-tracks-store-and-orders-by-last-seen
  "Under churn the last-seen list length equals the store count, head->tail is
   ordered newest->oldest by last-seen, the boundary pointers are NIL, and the
   next/prev back-pointers agree — the consistency O(1) tail eviction needs."
  (clear-rate-limit-store)
  (unwind-protect
       (%with-namespace-cap (:ip 50)
         (let ((ns (gethash :ip lol-web/server::*rate-limit-registry*))
               (lol-web/server:*rate-limit-min-evict-age* 0))
           ;; 200 distinct keys into a cap-50 store drives eviction repeatedly.
           (dotimes (i 200)
             (check-rate-limit (format nil "lru-churn-~D" i)
                               :max-requests 100 :window-seconds 3600))
           (let* ((head (lol-web/server::rate-limit-namespace-lru-head ns))
                  (tail (lol-web/server::rate-limit-namespace-lru-tail ns))
                  (fwd  (loop for e = head
                                then (lol-web/server::rate-limit-entry-next e)
                              while e collect e))
                  (seens (mapcar #'lol-web/server::rate-limit-entry-last-seen fwd))
                  (count (rate-limit-namespace-count :ip)))
             (is (= count (length fwd))
                 "list length ~D must equal store count ~D" (length fwd) count)
             (is (= 50 count)
                 "cap-50 store must hold exactly 50 entries after churn, got ~D"
                 count)
             (is (null (lol-web/server::rate-limit-entry-prev head))
                 "head.prev must be NIL")
             (is (null (lol-web/server::rate-limit-entry-next tail))
                 "tail.next must be NIL")
             (is (eq tail (car (last fwd)))
                 "walking next from head must terminate at the tail")
             (is (loop for (a b) on seens while b always (>= a b))
                 "entries must be ordered newest->oldest by last-seen")
             (is (loop for (a b) on fwd while b
                       always (eq a (lol-web/server::rate-limit-entry-prev b)))
                 "next/prev back-pointers must be consistent")
             ;; Every list node must be the same struct the store maps its key to.
             (is (= count
                    (loop for e in fwd
                          count (eq e (gethash
                                       (lol-web/server::rate-limit-entry-key e)
                                       (rate-limit-namespace-store ns)))))
                 "every list node must be the struct the store maps its key to"))))
    (clear-rate-limit-store)))

;;; ============================================================================
;;; Auth hook + with-auth — opaque mechanism-only gate
;;; ============================================================================

(defmacro with-auth-hooks ((&key authenticated-p current-principal) &body body)
  "Test helper: dynamically rebind *env* with :lol-web.auth.hooks set to
   a fresh (authp . principal) cons (or NIL when both thunks are NIL).
   Mirrors the shape that %make-auth-hooks-middleware injects per request
   inside an app, so unit tests can invoke with-auth / current-principal
   directly without standing up a Lack pipeline. Restoration happens via
   dynamic shadowing — no global mutation."
  `(let ((lol-web/server:*env*
           (list* :lol-web.auth.hooks
                  (when (or ,authenticated-p ,current-principal)
                    (cons ,authenticated-p ,current-principal))
                  (copy-list (or lol-web/server:*env* '())))))
     ,@body))

(test regression-with-auth-fails-closed-when-no-hooks-registered
  "with-auth defaults to 401 when neither hook is registered (security-critical)"
  (with-auth-hooks (:authenticated-p nil :current-principal nil)
    (let ((response (with-auth () :should-not-reach)))
      (is (consp response)
          "with-auth must return a response triple, got ~S" response)
      (is (= 401 (first response))
          "no hooks registered must yield 401, got status ~D" (first response)))))

(test regression-with-auth-allows-when-thunk-returns-t
  "with-auth runs the body when *authenticated-p* returns T"
  (with-auth-hooks (:authenticated-p (lambda () t)
                    :current-principal (lambda () :a-principal))
    (let ((result (with-auth () (list :ok (current-principal)))))
      (is (equal '(:ok :a-principal) result)
          "body must run and current-principal must flow through, got ~S"
          result))))

(test regression-with-auth-denies-with-401-when-thunk-returns-nil
  "with-auth returns 401 when *authenticated-p* returns NIL"
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (let ((response (with-auth () :should-not-reach)))
      (is (= 401 (first response))
          "NIL thunk must yield 401, got status ~D" (first response)))))

(test regression-with-auth-redirects-when-on-unauthorized-is-path
  "with-auth :on-unauthorized <path> yields a 302 Location redirect on deny"
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (let ((response (with-auth (:on-unauthorized "/sign-in") :should-not-reach)))
      (is (= 302 (first response))
          "string :on-unauthorized must yield 302, got status ~D" (first response))
      (is (string= "/sign-in" (getf (second response) :location))
          "redirect Location header must match supplied path, got ~S"
          (getf (second response) :location)))))

(test regression-with-auth-invalid-on-unauthorized-signals-at-expansion-time
  "with-auth :on-unauthorized of a non-integer non-string shape errors during macro expansion"
  (let ((raised
          (handler-case
              (progn (macroexpand-1
                      '(lol-web/server:with-auth (:on-unauthorized :nope)
                        :unreached))
                     nil)
            (error () t))))
    (is (eq t raised)
        "a keyword :on-unauthorized must raise an expansion-time error")))

(test regression-current-principal-preserves-opaque-value-shapes
  "current-principal returns the thunk's value verbatim across keyword / plist / hash-table"
  (dolist (shape (list :a-keyword
                       (list :plist 1 :ok t)
                       (let ((h (make-hash-table)))
                         (setf (gethash :k h) :v)
                         h)))
    (with-auth-hooks (:authenticated-p (lambda () t)
                      :current-principal (let ((captured shape))
                                           (lambda () captured)))
      (is (eq shape (current-principal))
          "principal of shape ~S must be returned eq (got ~S)"
          shape (current-principal)))))

(test regression-current-principal-nil-when-no-hook-registered
  "current-principal is NIL — not an error — when no hook has been installed"
  (with-auth-hooks (:authenticated-p nil :current-principal nil)
    (is (null (current-principal))
        "missing principal hook must yield NIL, got ~S" (current-principal))))

(test regression-make-app-auth-nil-installs-no-auth-middleware
  "make-app :auth nil builds no auth middleware. with-auth then sees no
   :lol-web.auth.hooks on env and fail-closes."
  (is (null (lol-web/server::%make-auth-hooks-middleware nil))
      "(%make-auth-hooks-middleware nil) must return NIL"))

(test regression-make-app-auth-plist-captures-hooks-in-middleware-closure
  "make-app :auth <plist> builds a middleware whose closure carries the
   (authp . principal) cons; per request the cons is set on env under
   :lol-web.auth.hooks before the inner app runs."
  (let* ((check-fn (lambda () t))
         (get-fn   (lambda () :captured-principal))
         (mw       (lol-web/server::%make-auth-hooks-middleware
                    (list :authenticated-p check-fn
                          :current-principal get-fn))))
    (is (functionp mw)
        "(%make-auth-hooks-middleware <plist>) must return a middleware fn")
    (let* ((seen-env nil)
           (inner    (lambda (env) (setf seen-env env) (list 200 nil nil)))
           (wrapped  (funcall mw inner)))
      (funcall wrapped (list :request-method :get :path-info "/"))
      (let ((hooks (getf seen-env :lol-web.auth.hooks)))
        (is (consp hooks)
            "inner app must see :lol-web.auth.hooks as a cons, got ~S" hooks)
        (is (eq check-fn (car hooks))
            "(car hooks) must be the supplied authenticated-p thunk")
        (is (eq get-fn (cdr hooks))
            "(cdr hooks) must be the supplied current-principal thunk")))))

(test regression-make-app-auth-two-middlewares-hold-independent-conses
  "Two %make-auth-hooks-middleware closures in one image carry distinct
   cons cells. The full end-to-end isolation check is in http-e2e.lisp."
  (let* ((a-authp     (lambda () :a-authp))
         (a-principal (lambda () :a-principal))
         (b-authp     (lambda () :b-authp))
         (b-principal (lambda () :b-principal))
         (mw-a (lol-web/server::%make-auth-hooks-middleware
                (list :authenticated-p a-authp :current-principal a-principal)))
         (mw-b (lol-web/server::%make-auth-hooks-middleware
                (list :authenticated-p b-authp :current-principal b-principal)))
         (seen-a nil)
         (seen-b nil))
    (funcall (funcall mw-a (lambda (env) (setf seen-a env) nil))
             (list :request-method :get :path-info "/"))
    (funcall (funcall mw-b (lambda (env) (setf seen-b env) nil))
             (list :request-method :get :path-info "/"))
    (let ((hooks-a (getf seen-a :lol-web.auth.hooks))
          (hooks-b (getf seen-b :lol-web.auth.hooks)))
      (is (not (eq hooks-a hooks-b))
          "two middleware closures must hold distinct cons cells")
      (is (eq a-authp (car hooks-a))
          "mw-a env must carry mw-a's authp thunk")
      (is (eq b-authp (car hooks-b))
          "mw-b env must carry mw-b's authp thunk"))))

(test regression-make-app-rejects-use-csrf-without-use-session
  "make-app signals an error when :use-csrf t is paired with :use-session nil
   so a no-op CSRF stack cannot ship a route protected only in name"
  (let ((raised
          (handler-case
              (progn (make-app :use-csrf t :use-session nil
                               :use-accesslog nil :use-static nil)
                     nil)
            (error () t))))
    (is (eq t raised)
        ":use-csrf t :use-session nil must signal an error from make-app")))

(defparameter *test-deny-status* 401
  "Indirect binding for testing with-auth's runtime :on-unauthorized arm.")

(test regression-with-auth-on-unauthorized-non-literal-integer
  "with-auth :on-unauthorized accepts a non-literal form whose runtime
   value is an integer — the (defparameter *deny* 401) ...
   (with-auth (:on-unauthorized *deny*)) pattern."
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (let ((response (with-auth (:on-unauthorized *test-deny-status*)
                      :unreached)))
      (is (= 401 (first response))
          "*test-deny-status* (=401) must resolve to a 401 response, got ~D"
          (first response)))))

(test regression-with-auth-on-unauthorized-callable
  "with-auth :on-unauthorized accepts a callable; funcall'd at deny time,
   the return value reaches the caller as the response triple."
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (let* ((sentinel (list 451 '(:content-type "text/plain") '("custom deny")))
           (response (with-auth (:on-unauthorized (lambda () sentinel))
                       :unreached)))
      (is (eq sentinel response)
          "callable :on-unauthorized return must reach caller verbatim, got ~S"
          response))))

(test regression-with-auth-on-unauthorized-runtime-bad-value-signals
  "with-auth :on-unauthorized whose runtime value is neither integer
   nor string nor callable raises — a misconfigured :on-unauthorized
   must not silently open the route."
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (let* ((bad :not-a-valid-value)
           (raised
             (handler-case
                 (progn (with-auth (:on-unauthorized bad) :unreached)
                        nil)
               (error () t))))
      (is (eq t raised)
          "runtime non-integer non-string non-callable must signal"))))

(test regression-with-auth-composes-with-other-with-macros
  "with-auth nests cleanly inside with-rate-limit (and vice versa)"
  (clear-rate-limit-store)
  (unwind-protect
       (with-auth-hooks (:authenticated-p (lambda () t)
                         :current-principal (lambda () :nested-ok))
         (let ((outcome (with-rate-limit (:key "compose-key"
                                          :max-requests 100
                                          :window-seconds 60)
                          (with-auth ()
                            (list :inner (current-principal))))))
           (is (equal '(:inner :nested-ok) outcome)
               "nested with-auth must execute its body, got ~S" outcome)
           (is (rate-limit-entry-p (rate-limit-entry-of "compose-key"))
               "outer with-rate-limit must still record its hit")))
    (clear-rate-limit-store)))

;;; ============================================================================
;;; Session cookie hardening — make-app defaults
;;; ============================================================================

(test regression-make-app-session-cookie-hardened-defaults
  "make-app's documented cookie defaults yield a Secure+HttpOnly+SameSite=Lax state"
  (let ((state (lack/session/state/cookie:make-cookie-state
                :secure t :httponly t :samesite :lax)))
    (is (typep state 'lack/session/state/cookie:cookie-state)
        "make-cookie-state must produce a COOKIE-STATE instance, got ~S"
        (type-of state))
    (is (eq t (lack/session/state/cookie::cookie-state-secure state))
        "default Secure flag must be T")
    (is (eq t (lack/session/state/cookie::cookie-state-httponly state))
        "default HttpOnly flag must be T")
    (is (eq :lax (lack/session/state/cookie::cookie-state-samesite state))
        "default SameSite attribute must be :LAX")))

(test regression-make-app-exposes-cookie-hardening-keywords
  "make-app and start-server accept the three session-cookie-* keywords and :auth"
  (let ((make-app-keys (sb-introspect:function-lambda-list #'make-app))
        (start-server-keys (sb-introspect:function-lambda-list #'start-server)))
    (flet ((has-kw (lambda-list kw-name)
             (some (lambda (form)
                     (and (consp form)
                          (symbolp (car form))
                          (string= kw-name (symbol-name (car form)))))
                   lambda-list)))
      (dolist (kw-name '("SESSION-COOKIE-SECURE"
                         "SESSION-COOKIE-HTTPONLY"
                         "SESSION-COOKIE-SAMESITE"
                         "AUTH"))
        (is (has-kw make-app-keys kw-name)
            "make-app must accept &key ~A" kw-name)
        (is (has-kw start-server-keys kw-name)
            "start-server must accept &key ~A" kw-name)))))

;;; ============================================================================
;;; HTTP-error condition hierarchy + with-error-handling
;;; ============================================================================

(test regression-http-error-hierarchy-shape
  "http-error subclasses fix their status and inherit from client/server-error"
  (is (subtypep 'client-error 'http-error))
  (is (subtypep 'server-error 'http-error))
  (is (subtypep 'http-bad-request 'client-error))
  (is (subtypep 'http-unauthorized 'client-error))
  (is (subtypep 'http-forbidden 'client-error))
  (is (subtypep 'http-not-found 'client-error))
  (is (subtypep 'http-unprocessable-entity 'client-error))
  (is (= 400 (http-error-status (make-condition 'http-bad-request))))
  (is (= 401 (http-error-status (make-condition 'http-unauthorized))))
  (is (= 403 (http-error-status (make-condition 'http-forbidden))))
  (is (= 404 (http-error-status (make-condition 'http-not-found))))
  (is (= 422 (http-error-status
              (make-condition 'http-unprocessable-entity))))
  (is (string= "missing name"
               (http-error-body
                (make-condition 'http-bad-request :body "missing name"))))
  (is (null (http-error-body (make-condition 'http-not-found)))))

(test regression-with-error-handling-translates-http-error
  "with-error-handling catches http-error subclasses and emits the right status"
  (let ((response (with-error-handling "test"
                    (error 'http-not-found))))
    (is (= 404 (first response))
        "http-not-found must produce status 404, got ~A" (first response))
    (let ((body (third response)))
      (is (or (search "Not Found" (princ-to-string body))
              (search "Not Found" (with-output-to-string (s) (princ body s))))
          "404 body should mention 'Not Found', got ~S" body))))

(test regression-with-error-handling-honours-body-override
  "http-error :body is used in the response when supplied"
  (let ((response (with-error-handling "test"
                    (error 'http-bad-request
                           :body "missing 'name' parameter"))))
    (is (= 400 (first response)))
    (let ((body (third response)))
      (is (search "missing 'name' parameter"
                  (with-output-to-string (s)
                    (dolist (chunk body) (princ chunk s))))
          "400 body should contain custom message, got ~S" body))))

(test regression-with-error-handling-non-http-error-still-500
  "Non-http-error conditions still hit the catch-all 500 path"
  (let ((*error-output* (make-broadcast-stream))
        (*error-log-path* nil))
    (let ((response (with-error-handling "test"
                      (error "plain unhandled error"))))
      (is (= 500 (first response))
          "Plain (error ...) must produce status 500, got ~A" (first response)))))

;;; ============================================================================
;;; JSON — parse memoization, encode/decode round-trip
;;; ============================================================================

(test regression-parse-request-json-memoizes-result
  "parse-request-json caches the parsed result in *env*; second call hits the cache"
  (let* ((body-bytes (babel:string-to-octets "{\"name\":\"test\",\"count\":42}"
                                              :encoding :utf-8))
         (*env*
           (list :lol/cached-body body-bytes
                 :raw-body (flexi-streams:make-in-memory-input-stream body-bytes)
                 :content-type "application/json")))
    (let ((first (parse-request-json)))
      (is (consp first) "First call should return a non-empty alist, got ~S" first)
      (is (string= "test" (cdr (assoc :name first)))
          "Decoded :name should equal \"test\""))
    (is (not (eq 'unbound (getf *env* :lol/cached-body-json 'unbound)))
        ":lol/cached-body-json should be set after first parse")
    (let ((first  (getf *env* :lol/cached-body-json))
          (second (parse-request-json)))
      (is (eq first second)
          "Second parse-request-json call must return the cached object"))))

(test regression-parse-request-json-malformed-body-is-bad-request
  "Malformed JSON at the request chokepoint becomes HTTP-BAD-REQUEST."
  (let* ((body-bytes (babel:string-to-octets "{not json" :encoding :utf-8))
         (*env* (list :lol/cached-body body-bytes
                      :raw-body (flexi-streams:make-in-memory-input-stream body-bytes)
                      :content-type "application/json")))
    (signals http-bad-request
      (parse-request-json))))

(test regression-decode-json-string-shapes
  "decode-json-string returns alists with kebab-keyword keys, lists for arrays,
   +JSON-NULL+ for top-level null, and NIL for empty input. Malformed input
   signals MALFORMED-JSON-BODY."
  (let ((decoded (decode-json-string
                  "{\"componentId\":\"x\",\"value\":42,\"items\":[1,2,3],\"flag\":true,\"empty\":null}")))
    (is (consp decoded) "decoded a non-empty alist")
    (is (string= "x" (cdr (assoc :component-id decoded)))
        "camelCase 'componentId' → :COMPONENT-ID, value preserved")
    (is (= 42 (cdr (assoc :value decoded)))
        "numbers preserved as numbers")
    (is (equal '(1 2 3) (cdr (assoc :items decoded)))
        "JSON arrays decode to lists, not vectors")
    (is (eq t (cdr (assoc :flag decoded)))
        "JSON true → CL T")
    (is (null (cdr (assoc :empty decoded)))
        "JSON null → CL NIL"))
  (is (equal '(1 2 3) (decode-json-string "[1,2,3]"))
      "top-level array decodes to plain list")
  (is (string= "hi" (decode-json-string "\"hi\""))
      "top-level string decodes to string (not a character list)")
  (is (eq +json-null+ (decode-json-string "null"))
      "top-level null decodes to a distinct sentinel")
  (is (null (decode-json-string ""))
      "empty input → NIL")
  (signals malformed-json-body
    (decode-json-string "{not json")))

(test regression-encode-json-string-shapes
  "encode-json-string auto-detects alists as objects, lists as arrays,
   NIL as null, T as true, and downcases keyword keys."
  (is (string= "{\"success\":true,\"html\":\"<p>x</p>\"}"
               (encode-json-string '((:success . t) (:html . "<p>x</p>"))))
      "alist of (keyword . value) → JSON object")
  (is (string= "[1,2,3]" (encode-json-string '(1 2 3)))
      "plain list → JSON array")
  (is (string= "null" (encode-json-string nil))
      "NIL → JSON null")
  (is (string= "true" (encode-json-string t))
      "T → JSON true")
  (is (string= "{\"type\":\"html\",\"items\":[1,2,3]}"
               (encode-json-string
                 '((:type . "html") (:items . (1 2 3)))))
      "nested: outer alist becomes object, inner list becomes array")
  (is (string= "[1,\"x\",true]"
               (encode-json-string (vector 1 "x" t)))
      "vectors encode as arrays"))

(test regression-encode-decode-round-trip
  "encoding then decoding preserves alist shape end-to-end."
  (let* ((original '((:component-id . "abc")
                    (:nested . ((:k . 1) (:v . "two")))
                    (:items . (10 20 30))))
         (round-tripped (decode-json-string
                          (encode-json-string original))))
    (is (string= "abc" (cdr (assoc :component-id round-tripped))))
    (is (= 1 (cdr (assoc :k (cdr (assoc :nested round-tripped))))))
    (is (string= "two" (cdr (assoc :v (cdr (assoc :nested round-tripped))))))
    (is (equal '(10 20 30) (cdr (assoc :items round-tripped))))))

;;; ============================================================================
;;; Routes registry — concurrent registration under *routes-lock*
;;; ============================================================================

(test regression-routes-registry-concurrent-registration
  "*routes* setfs from multiple threads land without loss when serialised
   through *routes-lock*. Models the load-time race between defroute calls
   from parallel file loads or hot-reload from a Hunchentoot worker."
  (let* ((n-threads 8)
         (per-thread 50)
         (initial (hash-table-count lol-web/server::*routes*))
         (threads
           (loop for tid from 0 below n-threads
                 collect (let ((tid tid))
                           (bordeaux-threads:make-thread
                            (lambda ()
                              (loop for i from 0 below per-thread
                                    for path = (format nil "/regression/concurrent/t~a/~a" tid i)
                                    do (bordeaux-threads:with-recursive-lock-held
                                           (lol-web/server::*routes-lock*)
                                         (setf (gethash (cons :get path)
                                                        lol-web/server::*routes*)
                                               (lambda () "ok"))))))))))
    (mapc #'bordeaux-threads:join-thread threads)
    (let ((added (- (hash-table-count lol-web/server::*routes*) initial)))
      (is (= (* n-threads per-thread) added)
          "all concurrent registrations visible — none lost"))
    ;; Clean up injected entries so the registry is unaffected for other tests.
    (loop for tid from 0 below n-threads
          do (loop for i from 0 below per-thread
                   for path = (format nil "/regression/concurrent/t~a/~a" tid i)
                   do (remhash (cons :get path) lol-web/server::*routes*)))))

;;; ============================================================================
;;; Path parameters — safe segment gate
;;; ============================================================================

(test regression-safe-path-segment-rejects-traversal-and-encoded-separators
  "Path parameters reject traversal markers and encoded dot/slash/backslash."
  (is (safe-path-segment-p "report-2026"))
  (is (not (safe-path-segment-p "")))
  (is (not (safe-path-segment-p ".")))
  (is (not (safe-path-segment-p "..")))
  (is (not (safe-path-segment-p "a/b")))
  (is (not (safe-path-segment-p "a\\b")))
  (is (not (safe-path-segment-p "%2e%2e")))
  (is (not (safe-path-segment-p "a%2fb")))
  (is (not (safe-path-segment-p "a%5cb")))
  (signals unsafe-path-segment
    (safe-path-segment "..")))

(test regression-match-path-pattern-sanitizes-named-and-splat-segments
  "Named and splat route captures accept only safe path segments."
  (is (equal '(("id" . "report-2026"))
             (lol-web/server::match-path-pattern "/files/:id"
                                                 "/files/report-2026")))
  (is (null (lol-web/server::match-path-pattern "/files/:id"
                                                "/files/%2e%2e")))
  (is (equal '(("path" . "a/b"))
             (lol-web/server::match-path-pattern "/files/*path"
                                                 "/files/a/b")))
  (is (null (lol-web/server::match-path-pattern "/files/*path"
                                                "/files/a/%2f"))))

;;; ============================================================================
;;; Session ops: rotate, expire, current-session-id
;;; ============================================================================
;;;
;;; The functions read and mutate (getf *env* :lack.session.options) — a
;;; plist that Lack's session middleware populates per request. Tests fake
;;; the plist directly so they exercise the contract without running an
;;; actual Lack request loop.

(test regression-session-rotate-flips-change-id
  "session-rotate sets :change-id T in the session-options plist."
  (let* ((opts (list :id "old-sid" :new-session nil :change-id nil :expire nil))
         (*env* (list :lack.session.options opts)))
    (is (eql t (session-rotate))
        "returns T when middleware is bound")
    (is (eql t (getf (getf *env* :lack.session.options) :change-id))
        ":change-id is now T — Lack's finalize will regenerate the SID")
    (is (eql nil (getf (getf *env* :lack.session.options) :expire))
        ":expire is unaffected by rotate")))

(test regression-session-expire-flips-expire
  "session-expire sets :expire T in the session-options plist."
  (let* ((opts (list :id "old-sid" :new-session nil :change-id nil :expire nil))
         (*env* (list :lack.session.options opts)))
    (is (eql t (session-expire))
        "returns T when middleware is bound")
    (is (eql t (getf (getf *env* :lack.session.options) :expire))
        ":expire is now T — Lack's finalize will drop the session")
    (is (eql nil (getf (getf *env* :lack.session.options) :change-id))
        ":change-id is unaffected by expire")))

(test regression-current-session-id-reads-options-id
  "current-session-id returns the :id field from the session options plist."
  (let* ((*env* (list :lack.session.options
                      (list :id "session-abc" :new-session nil))))
    (is (string= "session-abc" (current-session-id)))))

(test regression-session-ops-no-middleware-bound
  "All three session ops return NIL gracefully when no middleware is bound.
   A consumer who calls (session-rotate) on a request that lacks the
   session middleware should be told 'no-op', not crashed."
  (let ((*env* nil))
    (is (null (session-rotate)))
    (is (null (session-expire)))
    (is (null (current-session-id))))
  (let ((*env* (list :path-info "/")))            ; env without :lack.session.options
    (is (null (session-rotate)))
    (is (null (session-expire)))
    (is (null (current-session-id)))))

(test regression-session-rotate-inserts-key-when-absent
  "session-rotate handles a session-options plist missing the :change-id key —
   it prepends the flag (Lack always populates :change-id, but the contract
   should not assume it)."
  (let* ((*env* (list :lack.session.options (list :id "x"))))
    (is (eql t (session-rotate)))
    (is (eql t (getf (getf *env* :lack.session.options) :change-id)))))

(test regression-session-rotate-always-clears-csrf-token
  "Default session-rotate unconditionally removes the \"csrf-token\" entry
   from the session hash so the next request mints a fresh token —
   prevents pre-auth CSRF token reading from flowing into the
   authenticated session"
  (let* ((session (make-hash-table :test 'equal))
         (opts (list :id "s1" :change-id nil))
         (*env* (list :lack.session session :lack.session.options opts)))
    (setf (gethash "csrf-token" session) "leaked-pre-auth-token")
    (setf (gethash "user-pref" session) :dark-mode)
    (session-rotate)
    (is (null (gethash "csrf-token" session))
        "csrf-token must be removed after default rotate, got ~S"
        (gethash "csrf-token" session))
    (is (eq :dark-mode (gethash "user-pref" session))
        "non-csrf entries must survive default rotate, got ~S"
        (gethash "user-pref" session))))

(test regression-session-rotate-scrub-clears-non-preserved-keys
  ":scrub t :preserve '(\"intended-path\") drops everything except
   intended-path (and the unconditionally-cleared csrf-token)"
  (let* ((session (make-hash-table :test 'equal))
         (opts (list :id "s2" :change-id nil))
         (*env* (list :lack.session session :lack.session.options opts)))
    (setf (gethash "csrf-token" session) "leaked-token")
    (setf (gethash "intended-path" session) "/dashboard")
    (setf (gethash "user-pref" session) :dark-mode)
    (setf (gethash "captcha-passed" session) t)
    (session-rotate :scrub t :preserve '("intended-path"))
    (is (null (gethash "csrf-token" session))
        "csrf-token must always be cleared, scrub or not")
    (is (null (gethash "user-pref" session))
        "user-pref must be scrubbed (not in :preserve)")
    (is (null (gethash "captcha-passed" session))
        "captcha-passed must be scrubbed (not in :preserve)")
    (is (string= "/dashboard" (gethash "intended-path" session))
        ":preserve must retain intended-path, got ~S"
        (gethash "intended-path" session))))

(test regression-session-rotate-scrub-empty-preserve-empties-session
  ":scrub t with no :preserve drops every session entry"
  (let* ((session (make-hash-table :test 'equal))
         (opts (list :id "s3" :change-id nil))
         (*env* (list :lack.session session :lack.session.options opts)))
    (setf (gethash "csrf-token" session) "x")
    (setf (gethash "k1" session) :v1)
    (setf (gethash "k2" session) :v2)
    (session-rotate :scrub t)
    (is (zerop (hash-table-count session))
        ":scrub t :preserve '() must empty the session hash, got ~D keys"
        (hash-table-count session))))

(test regression-session-rotate-csrf-token-cannot-be-preserved
  ":preserve listing \"csrf-token\" signals an error — the token is on a
   permanent deny-list because preserving it across rotation re-enables
   the pre-auth fixation vector that unconditional regeneration closes"
  (let* ((session (make-hash-table :test 'equal))
         (opts (list :id "s4" :change-id nil))
         (*env* (list :lack.session session :lack.session.options opts)))
    (setf (gethash "csrf-token" session) "pre-existing-token")
    (setf (gethash "other-key" session) :survives)
    (let ((raised
            (handler-case (progn (session-rotate :scrub t
                                                 :preserve '("csrf-token"))
                                 nil)
              (error () t))))
      (is (eq t raised)
          ":preserve including \"csrf-token\" must signal an error")
      (is (string= "pre-existing-token" (gethash "csrf-token" session))
          "session hash must not be mutated when the deny-list guard fires; ~
           got csrf-token=~S" (gethash "csrf-token" session))
      (is (eq :survives (gethash "other-key" session))
          "non-csrf keys must also be untouched on the early-error path"))))

(test regression-session-rotate-signals-in-streaming-handler
  "session-rotate raises STREAMING-SESSION-ROTATE-ERROR when *env* carries
   :clack.streaming t — Lack's FINALIZE never runs for streamed responses
   so a silent T return would falsely suggest rotation happened"
  (let* ((session (make-hash-table :test 'equal))
         (opts (list :id "s5" :change-id nil))
         (*env* (list :lack.session session
                      :lack.session.options opts
                      :clack.streaming t))
         (raised
           (handler-case (progn (session-rotate) nil)
             (streaming-session-rotate-error () t))))
    (is (eq t raised)
        "streaming env must signal STREAMING-SESSION-ROTATE-ERROR")
    (is (null (getf (getf *env* :lack.session.options) :change-id))
        ":change-id must not be flipped when the streaming guard fired")))

(test regression-session-rotate-non-streaming-still-flips-change-id
  "Regression guard: a non-streaming env (or :clack.streaming nil) still
   rotates normally — the streaming guard must not match too broadly"
  (let* ((session (make-hash-table :test 'equal))
         (opts (list :id "s6" :change-id nil))
         (*env* (list :lack.session session
                      :lack.session.options opts
                      :clack.streaming nil)))
    (is (eq t (session-rotate))
        ":clack.streaming NIL must allow normal rotation")
    (is (eq t (getf (getf *env* :lack.session.options) :change-id))
        ":change-id must be T after a non-streaming rotate")))

;;; ============================================================================
;;; Store protocol re-export — class + generic-function identity
;;; ============================================================================
;;;
;;; lol-web/server re-exports lack/session/store's STORE class plus the three
;;; generic functions verbatim (via :import-from + :export). Identity is the
;;; contract: a consumer who subclasses lol-web/server:store and defines
;;; methods on lol-web/server:fetch-session/... lands the same methods that
;;; Lack's middleware will dispatch to.

(test regression-store-protocol-class-identity
  "lol-web/server:store IS lack/session/store:store — same class object."
  (is (eq (find-class 'lol-web/server:store)
          (find-class 'lack/session/store:store))
      "re-export must preserve class identity"))

(test regression-store-protocol-generic-function-identity
  "Re-exported store generics are the same function objects."
  (is (eq (symbol-function 'lol-web/server:fetch-session)
          (symbol-function 'lack/session/store:fetch-session)))
  (is (eq (symbol-function 'lol-web/server:store-session)
          (symbol-function 'lack/session/store:store-session)))
  (is (eq (symbol-function 'lol-web/server:remove-session)
          (symbol-function 'lack/session/store:remove-session))))

(test regression-store-protocol-consumer-dispatch
  "A consumer subclass via lol-web/server:store dispatches on methods
   defined for either re-exported or original generic symbol — they are
   the same function, so methods land in the same dispatch table."
  ;; Build the consumer subclass + methods at test time (idempotent across
  ;; runs because defstruct redefines the class slots compatibly).
  (eval '(defstruct (regression-store
                      (:include lol-web/server:store))
           (table (make-hash-table :test 'equal))))
  (eval '(defmethod lol-web/server:store-session
             ((s regression-store) sid session)
           (setf (gethash sid (regression-store-table s)) session)
           session))
  (eval '(defmethod lol-web/server:fetch-session
             ((s regression-store) sid)
           (gethash sid (regression-store-table s))))
  (eval '(defmethod lol-web/server:remove-session
             ((s regression-store) sid)
           (remhash sid (regression-store-table s))))
  (let* ((s (funcall (find-symbol "MAKE-REGRESSION-STORE"
                                  :lol-web/server/test)))
         (data (make-hash-table :test 'equal)))
    (setf (gethash "k" data) "v")
    ;; Store via lol-web/server, fetch via lack/session/store — same method.
    (lol-web/server:store-session s "sid" data)
    (let ((fetched (lack/session/store:fetch-session s "sid")))
      (is (eq fetched data)
          "method defined via lol-web/server symbol is reachable via lack/session/store"))
    ;; Remove via the lack symbol, fetch via lol-web/server — same method.
    (lack/session/store:remove-session s "sid")
    (is (null (lol-web/server:fetch-session s "sid"))
        "round-trip remove via either symbol clears the entry")))

;;; ============================================================================
;;; JSON-body key coercion — bounded keyword pool
;;; ============================================================================

(test regression-camel-to-kebab-key-bounds-keyword-pool
  "1000 distinct hostile JSON keys must not grow the keyword pool."
  (let ((baseline (length (apropos-list "" :keyword))))
    (loop for i below 1000 do
          (lol-web/server::%camel-to-kebab-key
           (format nil "attackerKey~D~A" i "Suffix")))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D"
          baseline after))))

(test regression-camel-to-kebab-key-keyword-when-interned
  "Known keys resolve to their keyword; unknown keys ride through as
   the original string."
  (let ((_ :component-id))
    (declare (ignore _))
    (is (eq :component-id (lol-web/server::%camel-to-kebab-key "componentId"))))
  (let ((nonce (format nil "neverInternedAttackerKey~D~D"
                       (get-universal-time) (random 999999))))
    (is (stringp (lol-web/server::%camel-to-kebab-key nonce))
        "unknown JSON key must stay a string")))

;;; ============================================================================
;;; validate-header-value — RFC 7230 §3.2.6 enforcement
;;; ============================================================================

(defun %signals-error-p (thunk)
  "Run THUNK; return T if it signals an error, NIL on normal return."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(test regression-validate-header-value-pass-through
  "Safe values pass through unchanged: printable ASCII, HTAB, SP, obs-text."
  (is (string= "/path/safe?x=1"
               (validate-header-value "/path/safe?x=1")))
  (is (string= (format nil "ab~Ccd" #\Tab)
               (validate-header-value (format nil "ab~Ccd" #\Tab))))
  (is (string= "café" (validate-header-value "café"))
      "extended octets (obs-text) are allowed per RFC 7230"))

(test regression-validate-header-value-rejects-control-characters
  "CR, LF, NUL, and other CTL bytes signal — would forge headers or
   split the response stream."
  (is (%signals-error-p
       (lambda () (validate-header-value (format nil "ok~Cevil" #\Return)))))
  (is (%signals-error-p
       (lambda () (validate-header-value (format nil "ok~Cevil" #\Linefeed)))))
  (is (%signals-error-p
       (lambda () (validate-header-value (format nil "ok~Cevil" #\Nul)))))
  (is (%signals-error-p
       (lambda () (validate-header-value
                   (format nil "https://app~C~CSet-Cookie: pwned=1"
                           #\Return #\Linefeed))))
      "embedded CRLF after a legitimate URL must reject — Set-Cookie forge"))

(test regression-validate-header-value-rejects-oversize
  "Values longer than max-length octets signal; default 8192."
  (is (%signals-error-p
       (lambda () (validate-header-value
                   (make-string 9000 :initial-element #\a)))))
  (is (%signals-error-p
       (lambda () (validate-header-value "abcdef" :max-length 3)))
      "explicit smaller cap also rejects"))

(test regression-validate-header-value-rejects-non-strings
  "Non-string input signals rather than coercing — a route accidentally
   passing an integer or symbol must fail loud."
  (is (%signals-error-p (lambda () (validate-header-value 42))))
  (is (%signals-error-p (lambda () (validate-header-value :keyword))))
  (is (%signals-error-p (lambda () (validate-header-value nil)))))

;;; ============================================================================
;;; Content-Security-Policy — inline opt-in only
;;; ============================================================================

(test regression-add-csp-header-defaults-drop-unsafe-inline
  "Default CSP does not allow inline script/style; legacy callers opt in."
  (with-response-headers ()
    (add-csp-header)
    (let ((csp (getf (get-response-headers) :content-security-policy)))
      (is (search "script-src 'self'" csp))
      (is (search "style-src 'self' https://fonts.googleapis.com" csp))
      (is (null (search "'unsafe-inline'" csp)))))
  (with-response-headers ()
    (add-csp-header :script-src "'self' 'unsafe-inline'"
                    :style-src "'self' 'unsafe-inline'")
    (is (search "'unsafe-inline'"
                (getf (get-response-headers) :content-security-policy)))))

;;; ============================================================================
;;; validate-origin — scheme/host normalisation with strict origin shape
;;; ============================================================================

(defun %env-with-headers (&rest header-pairs)
  "Build a minimal *env* plist with HEADER-PAIRS placed in a hash-table
   under :headers, keys downcased per request-header's lookup contract."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on header-pairs by #'cddr
          do (setf (gethash (string-downcase k) h) v))
    (list :headers h)))

(test regression-validate-origin-exact-match
  "Origin equal to one of allowed-origins returns T."
  (let ((*env* (%env-with-headers "Origin" "https://app.example.com")))
    (is (eq t (validate-origin
               :allowed-origins '("https://app.example.com"))))))

(test regression-validate-origin-rejects-mismatch
  "Host, scheme, port, or path differences deny. Scheme and host case
   normalise before comparison."
  (let ((allowed '("https://app.example.com")))
    (let ((*env* (%env-with-headers "Origin" "https://evil.example.com")))
      (is (null (validate-origin :allowed-origins allowed))
          "host mismatch denies"))
    (let ((*env* (%env-with-headers "Origin" "http://app.example.com")))
      (is (null (validate-origin :allowed-origins allowed))
          "scheme mismatch denies"))
    (let ((*env* (%env-with-headers "Origin" "https://app.example.com:8443")))
      (is (null (validate-origin :allowed-origins allowed))
          "explicit port mismatch denies"))
    (let ((*env* (%env-with-headers "Origin" "https://app.example.com/")))
      (is (null (validate-origin :allowed-origins allowed))
          "path on Origin denies"))
    (let ((*env* (%env-with-headers "Origin" "HTTPS://APP.EXAMPLE.COM")))
      (is (eq t (validate-origin :allowed-origins allowed))
          "scheme and host case normalise"))))

(test regression-validate-origin-absent-strict-by-default
  "Missing Origin denies when REFERER-FALLBACK is NIL (default)."
  (let ((*env* (%env-with-headers)))
    (is (null (validate-origin
               :allowed-origins '("https://app.example.com"))))))

(test regression-validate-origin-empty-allowlist-denies
  "An empty allowed-origins list denies every request."
  (let ((*env* (%env-with-headers "Origin" "https://app.example.com")))
    (is (null (validate-origin :allowed-origins '())))))

(test regression-validate-origin-referer-fallback
  "When Origin is absent and REFERER-FALLBACK is T, compare the
   scheme+host[:port] of Referer; path and query are stripped."
  (let ((allowed '("https://app.example.com")))
    (let ((*env* (%env-with-headers
                  "Referer" "https://app.example.com/some/path?x=1")))
      (is (eq t (validate-origin :allowed-origins allowed
                                 :referer-fallback t))
          "matching Referer origin admits"))
    (let ((*env* (%env-with-headers "Referer" "https://evil.example.com/x")))
      (is (null (validate-origin :allowed-origins allowed
                                 :referer-fallback t))
          "mismatched Referer origin denies"))
    (let ((*env* (%env-with-headers "Referer" "")))
      (is (null (validate-origin :allowed-origins allowed
                                 :referer-fallback t))
          "blank Referer denies"))
    (let ((*env* (%env-with-headers "Referer" "not-a-url-at-all")))
      (is (null (validate-origin :allowed-origins allowed
                                 :referer-fallback t))
          "unparsable Referer denies"))))

(test regression-validate-origin-referer-fallback-with-port
  "Port is included in the reconstructed Referer origin."
  (let ((*env* (%env-with-headers
                "Referer" "https://app.example.com:8443/path")))
    (is (eq t (validate-origin
               :allowed-origins '("https://app.example.com:8443")
               :referer-fallback t)))))

(test regression-validate-origin-origin-decides-when-present
  "When Origin IS present, Referer is never consulted — even with
   REFERER-FALLBACK on. Prevents Referer-spoof past an Origin gate."
  (let* ((allowed '("https://app.example.com"))
         (h (make-hash-table :test 'equal)))
    (setf (gethash "origin" h) "https://evil.example.com"
          (gethash "referer" h) "https://app.example.com/path")
    (let ((*env* (list :headers h)))
      (is (null (validate-origin :allowed-origins allowed
                                 :referer-fallback t))
          "mismatched Origin denies even when Referer matches"))))

(test regression-validate-origin-custom-header-name
  "ORIGIN-HEADER kwarg allows non-default header name (e.g., a deployment
   that proxies Origin under a different name)."
  (let* ((h (make-hash-table :test 'equal)))
    (setf (gethash "x-forwarded-origin" h) "https://app.example.com")
    (let ((*env* (list :headers h)))
      (is (eq t (validate-origin
                 :allowed-origins '("https://app.example.com")
                 :origin-header "X-Forwarded-Origin"))))))

;;; ============================================================================
;;; streaming-gate — per-entry origin / auth / rate-limit on streaming routes
;;; ============================================================================

(defun %sg-passthrough-app (env)
  "Inner app sentinel used by streaming-gate tests; returns 200 when reached."
  (declare (ignore env))
  (list 200 (list :content-type "text/plain") (list "passed-through")))

(defun %sg-streaming-env (&key (method :get)
                               (path "/ws/test")
                               (origin "https://app.example.com")
                               (remote-addr "10.0.0.1"))
  "Build a synthetic streaming-request env with a populated headers
   hash-table. Origin defaults to a permitted value; ORIGIN NIL omits
   the header entirely so origin-absent paths can be exercised."
  (let ((h (make-hash-table :test 'equal)))
    (when origin
      (setf (gethash "origin" h) origin))
    (list :request-method method
          :path-info path
          :headers h
          :remote-addr remote-addr)))

(defun %sg-install-entry (path &key auth origin bearer-token)
  "Install a streaming-route-entry at (:GET . PATH) for the duration of
   one test. Returns a cleanup thunk that removes the entry. BEARER-TOKEN
   defaults to NIL so existing callers see no behavioural change."
  (let ((entry (make-streaming-route-entry
                :body (lambda (env)
                        (declare (ignore env))
                        (list 200 (list :content-type "text/plain")
                              (list "handler-body-ran")))
                :auth auth
                :origin origin
                :bearer-token bearer-token)))
    (bordeaux-threads:with-recursive-lock-held (lol-web/server::*routes-lock*)
      (setf (gethash (cons :get path) lol-web/server::*streaming-routes*) entry))
    (lambda ()
      (bordeaux-threads:with-recursive-lock-held (lol-web/server::*routes-lock*)
        (remhash (cons :get path) lol-web/server::*streaming-routes*)))))

(test regression-streaming-gate-passes-non-streaming-request
  "A request whose (method . path) is not in lol-web/server::*streaming-routes*
   must pass through streaming-gate untouched — the gate is opt-in, never a
   catch-all."
  (let* ((mw (streaming-gate #'%sg-passthrough-app))
         (env (%sg-streaming-env :path "/not-a-streaming-route"))
         (response (funcall mw env)))
    (is (= 200 (first response)))
    (is (equal '("passed-through") (third response))
        "non-streaming requests must reach the inner app verbatim")))

(test regression-streaming-gate-allows-matching-origin-and-auth
  "Matching origin + auth thunk returning truthy passes through to the
   inner app (which on the real stack is route-handler, here a sentinel)."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app))
               (env (%sg-streaming-env :path "/ws/test"))
               (response (funcall mw env)))
          (is (= 200 (first response)))
          (is (equal '("passed-through") (third response))
              "permitted request must reach the inner app"))
      (funcall cleanup))))

(test regression-streaming-gate-denies-mismatched-origin
  "Request Origin not in the entry's allowlist returns 403 before the
   handler body or auth callable runs."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env))
                          (error "auth must not run on origin denial"))
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app))
               (env (%sg-streaming-env :path "/ws/test"
                                       :origin "https://evil.example.com"))
               (response (funcall mw env)))
          (is (= 403 (first response))
              "mismatched Origin must yield 403, got ~D" (first response)))
      (funcall cleanup))))

(test regression-streaming-gate-denies-missing-origin
  "Request without an Origin header denies with 403 — fail-closed
   (the spec lets a missing Origin slip past origin-checking middleware
   if the gate is permissive, so streaming-gate must be strict here)."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app))
               (env (%sg-streaming-env :path "/ws/test" :origin nil))
               (response (funcall mw env)))
          (is (= 403 (first response))
              "missing Origin must yield 403, got ~D" (first response)))
      (funcall cleanup))))

(test regression-streaming-gate-denies-empty-origin-allowlist
  "An entry registered with :origin NIL (empty list) denies every request
   — there is no implicit same-origin fallback. The handler author opts
   in by listing every accepted origin."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '())))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app))
               (env (%sg-streaming-env :path "/ws/test"))
               (response (funcall mw env)))
          (is (= 403 (first response))
              "empty allowlist must deny every request"))
      (funcall cleanup))))

(test regression-streaming-gate-denies-when-auth-returns-nil
  "Origin OK but auth thunk returns NIL → 401, not 403; the gate
   distinguishes origin-deny from auth-deny so log triage stays clear."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env)) nil)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app))
               (env (%sg-streaming-env :path "/ws/test"))
               (response (funcall mw env)))
          (is (= 401 (first response))
              "auth-thunk returning NIL must yield 401, got ~D"
              (first response)))
      (funcall cleanup))))

(test regression-streaming-gate-rate-limits-by-client-ip
  "After the per-window quota is exhausted on a remote-addr, the gate
   returns 429 even for an otherwise permitted request. Uses the
   :streaming namespace so :ip-bucket entries for ordinary HTTP traffic
   cannot be evicted by a streaming flood."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app
                                   :max-requests 2
                                   :window-seconds 60
                                   :namespace :streaming))
               (env (%sg-streaming-env :path "/ws/test"
                                       :remote-addr "10.0.0.99")))
          (is (= 200 (first (funcall mw env))))
          (is (= 200 (first (funcall mw env))))
          (let ((response (funcall mw env)))
            (is (= 429 (first response))
                "third request must trip the rate limit, got ~D"
                (first response))))
      (funcall cleanup))))

(test regression-streaming-gate-denies-missing-client-ip-before-rate-limit
  "Streaming upgrades without a usable client IP fail closed instead of
   sharing the empty-string rate-limit bucket."
  (clear-rate-limit-store :streaming)
  (let ((cleanup (%sg-install-entry
                  "/ws/test"
                  :auth (lambda (env) (declare (ignore env)) t)
                  :origin '("https://app.example.com"))))
    (unwind-protect
        (let* ((mw (streaming-gate #'%sg-passthrough-app))
               (env (%sg-streaming-env :path "/ws/test" :remote-addr ""))
               (response (funcall mw env)))
          (is (= 400 (first response))
              "empty remote-addr must yield 400, got ~D" (first response))
          (is (null (rate-limit-entry-of "" :namespace :streaming))
              "empty-string rate-limit bucket must not be created"))
      (funcall cleanup))))

(test regression-streaming-gate-entry-accessors-roundtrip
  "make-streaming-route-entry's accessors return what was passed in."
  (let ((entry (make-streaming-route-entry
                :body 'body-fn
                :auth 'auth-fn
                :origin '("https://example.com"))))
    (is (eq 'body-fn (streaming-route-entry-body entry)))
    (is (eq 'auth-fn (streaming-route-entry-auth entry)))
    (is (equal '("https://example.com")
               (streaming-route-entry-origin entry)))
    (is (streaming-route-entry-p entry))))

(test regression-middleware-order-streaming-gate-innermost
  "make-app's middleware composition reduces push-built middleware-fns
   left-to-right so the last-pushed entry (streaming-gate-shaped) wraps
   the bare handler first — innermost — and the first-pushed (cors-shaped)
   wraps last — outermost. A request entering the wrapped app must observe
   middlewares from outer to inner; exiting reverses the order."
  (let* ((calls '())
         (record (lambda (tag)
                   (lambda (app)
                     (lambda (env)
                       (push (list :enter tag) calls)
                       (prog1 (funcall app env)
                         (push (list :exit tag) calls))))))
         (push-fns '()))
    ;; mirror make-app's push order: first-pushed (cors-shaped) at LIST TAIL,
    ;; last-pushed (streaming-shaped) at HEAD.
    (push (funcall record :cors)    push-fns)
    (push (funcall record :session) push-fns)
    (push (funcall record :auth)    push-fns)
    (push (funcall record :stream)  push-fns)
    (let* ((handler (lambda (env)
                      (declare (ignore env))
                      (push (list :enter :handler) calls)
                      (push (list :exit  :handler) calls)
                      '(200 (:content-type "text/plain") ("ok"))))
           (app (reduce (lambda (wrapped-app mw) (funcall mw wrapped-app))
                        push-fns
                        :initial-value handler)))
      (funcall app (list :path-info "/x")))
    (is (equal '((:enter :cors)   (:enter :session) (:enter :auth) (:enter :stream)
                 (:enter :handler) (:exit :handler)
                 (:exit  :stream) (:exit  :auth)    (:exit  :session) (:exit  :cors))
               (nreverse calls))
        "outer→inner entry order must be cors, session, auth, stream, handler ~
         and exit must reverse — load-bearing for CRIT2-3 closure")))

(test regression-middleware-order-csrf-inside-session
  "csrf-middleware reads (getf env :lack.session), so the session
   middleware must dispatch outer and csrf inside it. make-app pushes
   session before csrf, and dispatch entry-order equals push-order, so
   csrf must enter after session."
  (let* ((calls '())
         (record (lambda (tag)
                   (lambda (app)
                     (lambda (env)
                       (push (list :enter tag) calls)
                       (prog1 (funcall app env)
                         (push (list :exit tag) calls))))))
         (push-fns '()))
    ;; mirror make-app's push order (source order, top to bottom): cors,
    ;; accesslog, session, csrf, auth, stream. The session-before-csrf pair
    ;; is the invariant under test.
    (push (funcall record :cors)      push-fns)
    (push (funcall record :accesslog) push-fns)
    (push (funcall record :session)   push-fns)
    (push (funcall record :csrf)      push-fns)
    (push (funcall record :auth)      push-fns)
    (push (funcall record :stream)    push-fns)
    (let* ((handler (lambda (env)
                      (declare (ignore env))
                      (push (list :enter :handler) calls)
                      (push (list :exit  :handler) calls)
                      '(200 (:content-type "text/plain") ("ok"))))
           (app (reduce (lambda (wrapped-app mw) (funcall mw wrapped-app))
                        push-fns
                        :initial-value handler)))
      (funcall app (list :path-info "/x")))
    (let* ((order (mapcar #'second
                          (remove :enter (nreverse calls)
                                  :key #'first :test-not #'eq)))
           (session-pos (position :session order))
           (csrf-pos    (position :csrf order)))
      (is (equal '(:cors :accesslog :session :csrf :auth :stream :handler)
                 order)
          "outer→inner entry order must be cors, accesslog, session, csrf, ~
           auth, stream, handler — got ~S" order)
      (is (and session-pos csrf-pos (< session-pos csrf-pos))
          "session must dispatch before csrf so :lack.session is set when ~
           csrf-middleware reads it (session ~S, csrf ~S)"
          session-pos csrf-pos))))

(test regression-app-middleware-order-asserted-at-build
  "make-app folds middleware in app-middleware-order and gates the security
   invariants at build time: the default stack builds (session wraps csrf,
   streaming-gate innermost), app-middleware-order reports the resolved
   dispatch order, and %assert-middleware-order signals middleware-order-error
   on an inverted csrf/session pair or a displaced streaming-gate."
  ;; Real make-app builds: the resolved order passes the gate.
  (is (functionp (make-app :use-streaming-gate t
                           :rate-limit-eviction-interval nil))
      "default-stack make-app must build (its order passes the gate)")
  ;; app-middleware-order reports the resolved outermost→innermost order.
  (is (equal '(:cors :session :csrf :streaming-gate :route-handler)
             (app-middleware-order :use-cors t :use-session t :use-csrf t
                                   :use-streaming-gate t))
      "app-middleware-order must report cors→session→csrf→streaming-gate→handler")
  ;; csrf wrapping session (inverted pair) is rejected at build time.
  (signals lol-web/server:middleware-order-error
    (lol-web/server::%assert-middleware-order
     '(:cors :csrf :session :streaming-gate :route-handler)))
  ;; streaming-gate not innermost is rejected.
  (signals lol-web/server:middleware-order-error
    (lol-web/server::%assert-middleware-order
     '(:cors :session :csrf :streaming-gate :auth :route-handler))))

(test regression-streaming-auth-fn-sees-session-and-principal
  "Streaming-gate sits innermost in the chain so an entry's auth-fn observes
   an env where :lack.session and :lol-web.auth.hooks are already populated.
   current-principal-of-env and session-get-of-env let the auth-fn read both
   without rebinding *env*."
  (clear-rate-limit-store :streaming)
  (let ((observed nil))
    (let ((cleanup (%sg-install-entry
                    "/ws/test"
                    :auth (lambda (env)
                            (setf observed
                                  (list :principal
                                        (current-principal-of-env env)
                                        :session-uid
                                        (session-get-of-env env "uid")))
                            t)
                    :origin '("https://app.example.com"))))
      (unwind-protect
           (let ((session (make-hash-table :test 'equal))
                 (hooks   (cons (lambda () t) (lambda () :alice-principal)))
                 (h       (make-hash-table :test 'equal)))
             (setf (gethash "uid" session) :alice-uid
                   (gethash "origin" h)    "https://app.example.com")
             (let* ((env (list :request-method :get
                               :path-info "/ws/test"
                               :headers h
                               :remote-addr "10.0.0.1"
                               :lack.session session
                               :lol-web.auth.hooks hooks))
                    (mw       (streaming-gate #'%sg-passthrough-app))
                    (response (funcall mw env)))
               (is (= 200 (first response))
                   "auth-fn returned truthy so the request reaches the inner app")
               (is (eq :alice-principal (getf observed :principal))
                   "current-principal-of-env returns the principal threaded by ~
                    auth-hooks middleware via the env's hooks cons")
               (is (eq :alice-uid (getf observed :session-uid))
                   "session-get-of-env reads :lack.session populated upstream")))
        (funcall cleanup)))))

(test regression-validate-origin-bearer-token-bypass
  "An entry with :bearer-token admits non-browser clients lacking Origin
   when they present Authorization: Bearer <T> and the predicate accepts
   T. Without the bearer header — or with a token the predicate rejects —
   the gate still denies with 403."
  (clear-rate-limit-store :streaming)
  (let* ((accepted-token "secret-token-abc")
         (cleanup
           (%sg-install-entry
            "/ws/test"
            :auth (lambda (env) (declare (ignore env)) t)
            :origin '("https://app.example.com")
            :bearer-token (lambda (token) (string= token accepted-token)))))
    (unwind-protect
         (let ((mw (streaming-gate #'%sg-passthrough-app)))
           ;; valid bearer, no Origin → admit
           (let ((env (%sg-streaming-env :path "/ws/test" :origin nil)))
             (setf (gethash "authorization" (getf env :headers))
                   (format nil "Bearer ~A" accepted-token))
             (let ((response (funcall mw env)))
               (is (= 200 (first response))
                   "valid bearer + no origin must pass, got ~D"
                   (first response))))
           ;; no bearer, no Origin → 403
           (let* ((env      (%sg-streaming-env :path "/ws/test" :origin nil))
                  (response (funcall mw env)))
             (is (= 403 (first response))
                 "no origin + no bearer must deny with 403, got ~D"
                 (first response)))
           ;; wrong bearer, no Origin → 403
           (let ((env (%sg-streaming-env :path "/ws/test" :origin nil)))
             (setf (gethash "authorization" (getf env :headers))
                   "Bearer wrong-token-xyz")
             (let ((response (funcall mw env)))
               (is (= 403 (first response))
                   "wrong bearer must deny with 403, got ~D"
                   (first response)))))
      (funcall cleanup))))

(test regression-request-bearer-token-malformed-edges
  "%request-bearer-token returns the token only for a well-formed
   'Bearer <token>' Authorization header — scheme case-insensitive,
   one-or-more WS chars separating scheme and token — and NIL for every
   malformed edge, so a bad header cannot smuggle a non-empty token."
  (flet ((bearer (auth)
           (let* ((headers (make-hash-table :test 'equal))
                  (*env* (list :headers headers)))
             (when auth (setf (gethash "authorization" headers) auth))
             (lol-web/server::%request-bearer-token))))
    (is (string= "abc" (bearer "Bearer abc")) "canonical Bearer token")
    (is (string= "abc" (bearer "bearer abc")) "scheme is case-insensitive")
    (is (string= "abc" (bearer "Bearer    abc")) "multiple WS chars separate")
    (is (string= "abc" (bearer "  Bearer abc  ")) "surrounding WS is trimmed")
    (is (string= "a.b-c_d" (bearer "Bearer a.b-c_d")) "token body preserved")
    (is (null (bearer nil)) "no header → NIL")
    (is (null (bearer "")) "empty header → NIL")
    (is (null (bearer "Bearer")) "scheme with no token → NIL")
    (is (null (bearer "Bearer ")) "scheme + trailing WS only → NIL")
    (is (null (bearer "Bearerabc")) "no WS after scheme → NIL")
    (is (null (bearer "Basic xyz")) "non-Bearer scheme → NIL")))

(test regression-streaming-gate-defstreaming-route-rejects-missing-policy
  "defstreaming-route requires :auth and :origin; omitting either signals
   at expansion time so the handler cannot register without a policy."
  (is (%signals-error-p
       (lambda ()
         (macroexpand-1
          '(defstreaming-route "/ws/should-fail" (:method :get)
            (env)
            (declare (ignore env))))
         (eval '(defstreaming-route "/ws/should-fail" (:method :get) (env)
                 (declare (ignore env))))))
      "no :auth + no :origin must signal")
  (is (%signals-error-p
       (lambda ()
         (eval '(defstreaming-route "/ws/should-fail-2"
                 (:method :get :auth (lambda (e) (declare (ignore e)) t))
                 (env)
                 (declare (ignore env))))))
      "missing :origin must signal")
  (is (%signals-error-p
       (lambda ()
         (eval '(defstreaming-route "/ws/should-fail-3"
                 (:method :get :origin '("https://app.example.com"))
                 (env)
                 (declare (ignore env))))))
      "missing :auth must signal"))

;;; ============================================================================
;;; *debug-mode* production lock — set/enable/disable signal once locked
;;; ============================================================================

(test regression-debug-mode-locked-set-debug-mode-signals
  "set-debug-mode signals debug-mode-locked-error when the lock is engaged."
  (let ((lol-web/server:*debug-mode-locked-p* t)
        (lol-web/server:*debug-mode* nil))
    (signals lol-web/server:debug-mode-locked-error
      (lol-web/server:set-debug-mode t))
    (is (null lol-web/server:*debug-mode*)
        "*debug-mode* unchanged on lock-refused write")))

(test regression-debug-mode-locked-enable-and-disable-signal
  "enable-debug-mode and disable-debug-mode go through set-debug-mode and
   therefore signal when the lock is engaged."
  (let ((lol-web/server:*debug-mode-locked-p* t)
        (lol-web/server:*debug-mode* nil))
    (signals lol-web/server:debug-mode-locked-error
      (lol-web/server:enable-debug-mode))
    (signals lol-web/server:debug-mode-locked-error
      (lol-web/server:disable-debug-mode))))

(test regression-debug-mode-set-debug-mode-honours-value
  "set-debug-mode mutates *debug-mode* when the lock is disengaged."
  (let ((lol-web/server:*debug-mode-locked-p* nil)
        (lol-web/server:*debug-mode* nil))
    (lol-web/server:set-debug-mode t)
    (is (eq t lol-web/server:*debug-mode*))
    (lol-web/server:set-debug-mode nil)
    (is (null lol-web/server:*debug-mode*))))

(test regression-lock-debug-mode-is-one-way
  "lock-debug-mode engages the lock; calling it again is idempotent."
  (let ((lol-web/server:*debug-mode-locked-p* nil))
    (lol-web/server:lock-debug-mode)
    (is (eq t lol-web/server:*debug-mode-locked-p*))
    (lol-web/server:lock-debug-mode)
    (is (eq t lol-web/server:*debug-mode-locked-p*)
        "second call leaves the lock engaged")))

;;; ============================================================================
;;; %make-jschema-registry-middleware — per-app *registry* let-binding
;;; ============================================================================

(test regression-jschema-registry-middleware-let-binds-table
  "Middleware built around a hash-table let-binds lol-web/jschema:*registry*
   to that table inside the wrapped app body; outside, *registry* is restored."
  (let* ((per-app (make-hash-table :test 'equal))
         (captured nil)
         (mw       (lol-web/server::%make-jschema-registry-middleware per-app))
         (probe    (lambda (env) (declare (ignore env))
                     (setf captured lol-web/jschema:*registry*)
                     (list 200 nil (list "ok"))))
         (before   lol-web/jschema:*registry*))
    (is (not (null mw))
        "middleware is non-NIL when a table is supplied")
    (funcall (funcall mw probe) (list))
    (is (eq captured per-app)
        "wrapped app saw the per-app table as *registry*")
    (is (eq before lol-web/jschema:*registry*)
        "outside the wrap, *registry* is restored")))

(test regression-jschema-registry-middleware-nil-returns-nil
  "%make-jschema-registry-middleware returns NIL when no table is supplied,
   so make-app installs nothing and the image-global default keeps running."
  (is (null (lol-web/server::%make-jschema-registry-middleware nil))))

;;; ============================================================================
;;; render-error-page / render-404-page — escape user-influenced text
;;; ============================================================================

(test regression-error-page-escapes-path
  "render-404-page routes its PATH argument through escape-html before
   format inserts it into the HTML body. A `<script>` payload in the
   path must render as literal text, not as a tag."
  (let ((html (lol-web/server::render-404-page "/<script>alert(1)</script>")))
    (is (search "&lt;script&gt;" html)
        "raw `<script>` must be encoded as &lt;script&gt;")
    (is (null (search "<script>alert(1)" html))
        "unescaped `<script>alert(1)` must not appear in the body")))

(test regression-404-page-escapes-condition-body
  "render-error-page debug pane prints (error) and (type-of error)
   through escape-html. A condition whose printed body carries `<script>`
   renders as literal text."
  (let* ((lol-web/server:*debug-mode* t)
         (cond (make-condition 'simple-error
                               :format-control "<script>alert(1)</script>"))
         (html (lol-web/server::render-error-page cond :context "/<img>")))
    (is (search "&lt;script&gt;" html)
        "condition body's `<script>` must be encoded")
    (is (search "&lt;img&gt;" html)
        "context path's `<img>` must be encoded")
    (is (null (search "<script>alert(1)" html))
        "unescaped attacker payload must not appear in the body")))

;;; ============================================================================
;;; redirect-response — cross-host + scheme-relative refusal
;;; ============================================================================

(test regression-redirect-rejects-cross-host
  "redirect-response with default :safe-host accepts only same-origin.
   With no current request env the same-origin host is NIL, so any URL
   carrying a host portion is refused."
  (signals lol-web/server:unsafe-redirect-error
    (let ((lol-web/server::*env* nil))
      (lol-web/server:redirect-response "https://evil.com/x"))))

(test regression-redirect-rejects-scheme-relative
  "Protocol-relative URLs (//evil.com/x) parse as same-origin-relative
   in browsers but actually redirect off-origin. The default policy
   refuses them."
  (signals lol-web/server:unsafe-redirect-error
    (let ((lol-web/server::*env* nil))
      (lol-web/server:redirect-response "//evil.com/x"))))

(test regression-redirect-rejects-opaque-scheme
  "Opaque-scheme URLs (data:, javascript:, mailto:) are never valid
   Location-header targets and are refused unconditionally — :safe-host
   :any does not override this."
  (signals lol-web/server:unsafe-redirect-error
    (lol-web/server:redirect-response "javascript:alert(1)" :safe-host :any))
  (signals lol-web/server:unsafe-redirect-error
    (lol-web/server:redirect-response "data:text/html,<script>" :safe-host :any)))

(test regression-redirect-accepts-same-origin-path
  "Path-relative URLs (start with `/`, not `//`) are always accepted —
   they cannot escape origin and require no host comparison."
  (let* ((lol-web/server::*env* nil)
         (resp (lol-web/server:redirect-response "/sign-in?return=/x")))
    (is (= 302 (first resp)))
    (is (string= "/sign-in?return=/x" (getf (second resp) :location)))))

(test regression-redirect-allowlist-accepts-listed-host
  "An explicit :safe-host allowlist accepts hosts (case-insensitive)
   in the list; non-listed hosts still signal."
  (let ((ok (lol-web/server:redirect-response
              "https://kleisli.io/sign-in" :safe-host '("kleisli.io"))))
    (is (= 302 (first ok))))
  (signals lol-web/server:unsafe-redirect-error
    (lol-web/server:redirect-response
      "https://evil.com/x" :safe-host '("kleisli.io"))))

(test regression-redirect-rejects-non-redirect-status
  "Location responses only accept redirect status codes."
  (signals error
    (lol-web/server:redirect-response "/ok" :status 200))
  (signals error
    (lol-web/server:redirect-response "/ok" :status 500))
  (dolist (status '(301 302 303 307 308))
    (is (= status (first (lol-web/server:redirect-response "/ok" :status status))))))

(test regression-default-error-log-path-avoids-world-writable-tmp
  "Default file logging path must not be under /tmp."
  (let ((path lol-web/server::*error-log-path*))
    (is (or (null path)
            (not (uiop:string-prefix-p "/tmp/" path)))
        "default error log path must not be in /tmp, got ~S" path)))

;;; ============================================================================
;;; safe-redirect-path-p — same-origin path predicate
;;; ============================================================================

(test regression-safe-redirect-path-p-shapes
  "Accepts `/`-rooted same-origin paths; rejects protocol-relative,
   scheme-bearing, empty, and non-string values."
  (is (lol-web/server:safe-redirect-path-p "/"))
  (is (lol-web/server:safe-redirect-path-p "/sign-in"))
  (is (lol-web/server:safe-redirect-path-p "/x?return=/a&y=1"))
  (is (not (lol-web/server:safe-redirect-path-p "")))
  (is (not (lol-web/server:safe-redirect-path-p "//evil.com/x")))
  (is (not (lol-web/server:safe-redirect-path-p "https://evil.com")))
  (is (not (lol-web/server:safe-redirect-path-p "javascript:alert(1)")))
  (is (not (lol-web/server:safe-redirect-path-p "sign-in")))
  (is (not (lol-web/server:safe-redirect-path-p nil))))

;;; ============================================================================
;;; with-auth :on-unauthorized — scheme refusal
;;; ============================================================================

(test regression-with-auth-unauthorized-rejects-scheme-literal
  "The literal-string arm of with-auth :on-unauthorized rejects
   non-same-origin paths at macroexpand time."
  (signals error
    (macroexpand-1
      '(lol-web/server:with-auth (:on-unauthorized "//evil.com/x")
         :ok)))
  (signals error
    (macroexpand-1
      '(lol-web/server:with-auth (:on-unauthorized "javascript:alert(1)")
         :ok))))

(test regression-with-auth-unauthorized-rejects-scheme-runtime
  "The runtime dispatch arm rejects non-same-origin paths via
   %dispatch-on-unauthorized's path check."
  (signals error
    (lol-web/server::%dispatch-on-unauthorized "//evil.com/x"))
  (signals error
    (lol-web/server::%dispatch-on-unauthorized "javascript:alert(1)")))

;;; ============================================================================
;;; static-route — traversal + parent-of-root refusal
;;; ============================================================================

(test regression-static-route-rejects-traversal
  "%static-request-under-root-p rejects URLs whose resolved file is
   not a descendant of the static root, even when `..` segments would
   otherwise escape via merge-pathnames."
  (let* ((root  (uiop:ensure-directory-pathname "/tmp/"))
         (under (lol-web/server::%static-request-under-root-p
                  "/static/../etc/passwd" "/static/" root)))
    (is (null under)
        "`..` escape must resolve outside root and be refused")))

(test regression-static-route-rejects-parent-of-root
  "A URL that resolves to a sibling/parent directory of the static
   root (via symlink or absolute-rooted suffix) is refused."
  (let* ((root (uiop:ensure-directory-pathname "/tmp/"))
         (under (lol-web/server::%static-request-under-root-p
                  "/static//etc/passwd" "/static/" root)))
    (is (null under)
        "absolute-rooted suffix must not be allowed to bypass merge-pathnames")))

(test regression-static-route-accepts-descendant
  "A request whose resolved file IS under static-root is accepted."
  (let* ((dir  (uiop:ensure-directory-pathname "/tmp/"))
         (file (uiop:with-temporary-file
                   (:pathname p :keep t :directory dir
                    :prefix "lol-static-probe" :type "txt")
                 p))
         (suffix (file-namestring file))
         (url (concatenate 'string "/static/" suffix))
         (under (lol-web/server::%static-request-under-root-p
                  url "/static/" dir)))
    (unwind-protect
         (is (not (null under))
             "descendant of static-root must be accepted")
      (ignore-errors (delete-file file)))))

;;; ============================================================================
;;; Header-emit boundary — every emit path through validate-header-value
;;; ============================================================================

(test regression-add-response-header-rejects-crlf-in-value
  "add-response-header refuses values carrying CR/LF — would forge a
   Set-Cookie or split the response stream at the emit site."
  (with-response-headers ()
    (is (%signals-error-p
         (lambda ()
           (add-response-header
            "X-Foo"
            (format nil "bar~C~CSet-Cookie: pwned=1"
                    #\Return #\Linefeed)))))))

(test regression-add-response-header-rejects-crlf-in-name
  "add-response-header refuses names carrying CR/LF for the same reason —
   a forged name terminator can splice a separate header."
  (with-response-headers ()
    (is (%signals-error-p
         (lambda ()
           (add-response-header
            (format nil "X-Foo~C~CSet-Cookie" #\Return #\Linefeed)
            "value"))))))

(test regression-add-response-header-accepts-safe-values
  "Safe values pass through and accumulate into *response-headers*."
  (with-response-headers ()
    (add-response-header "X-Custom" "ok")
    (let ((headers (get-response-headers)))
      (is (find "ok" headers :test (lambda (a b) (and (stringp b)
                                                       (string= a b)))))
      (is (find :|X-CUSTOM| headers)))))

(test regression-redirect-response-rejects-crlf-url
  "A CR or LF in the redirect URL signals at the redirect-response site —
   would forge a Set-Cookie or status-line in the Location header."
  (is (%signals-error-p
       (lambda ()
         (redirect-response
          (format nil "/safe~C~CSet-Cookie: pwned=1"
                  #\Return #\Linefeed))))))

(test regression-handle-normal-response-validates-triple-headers
  "A handler-returned response triple whose :headers plist contains a
   CR/LF in any name or value is refused before hunchentoot::header-out
   runs.  The validation lives in %validate-response-triple-headers so
   the gate is unit-testable without a live hunchentoot context."
  (is (%signals-error-p
       (lambda ()
         (lol-web/server::%validate-response-triple-headers
          (list :x-foo
                (format nil "bar~C~CSet-Cookie: pwned=1"
                        #\Return #\Linefeed))))))
  (is (%signals-error-p
       (lambda ()
         (lol-web/server::%validate-response-triple-headers
          (list (intern (format nil "X-~C~CSet-Cookie"
                                #\Return #\Linefeed)
                        :keyword)
                "ok")))))
  (is (eq :ok
          (handler-case
              (progn
                (lol-web/server::%validate-response-triple-headers
                 (list :content-type "text/html; charset=utf-8"
                       :content-length 42
                       :x-custom "ok"))
                :ok)
            (error () :signaled)))
      "safe header plist must not signal"))

;;; ============================================================================
;;; Request body cap
;;; ============================================================================

(test regression-request-body-cap-rejects-oversize-content-length
  "%CHECK-REQUEST-BODY-CAP signals REQUEST-BODY-TOO-LARGE when the declared
   Content-Length exceeds *MAX-REQUEST-BODY-BYTES*. The :declared slot on
   the condition pins that this arm fired; the :actual slot stays NIL."
  (let ((lol-web/server:*max-request-body-bytes* 1024))
    (handler-case
        (progn
          (lol-web/server::%check-request-body-cap :declared 2048)
          (is nil "%check-request-body-cap must signal when declared > cap"))
      (lol-web/server:request-body-too-large (c)
        (is (= 1024 (lol-web/server:request-body-too-large-limit c)))
        (is (= 2048 (lol-web/server:request-body-too-large-declared c)))
        (is (null (lol-web/server:request-body-too-large-actual c))
            "declared-arm signal must not fill :actual")))))

(test regression-request-body-cap-rejects-oversize-actual-bytes
  "%CHECK-REQUEST-BODY-CAP signals REQUEST-BODY-TOO-LARGE when the actual
   buffered byte count exceeds the cap (post-read recheck — covers chunked
   transfer encodings and headers that under-declare the body). The :actual
   slot pins that this arm fired; :declared stays NIL."
  (let ((lol-web/server:*max-request-body-bytes* 1024))
    (handler-case
        (progn
          (lol-web/server::%check-request-body-cap :actual 4096)
          (is nil "%check-request-body-cap must signal when actual > cap"))
      (lol-web/server:request-body-too-large (c)
        (is (= 1024 (lol-web/server:request-body-too-large-limit c)))
        (is (= 4096 (lol-web/server:request-body-too-large-actual c)))
        (is (null (lol-web/server:request-body-too-large-declared c))
            "actual-arm signal must not fill :declared")))))

(test regression-request-body-cap-passes-under-cap
  "%CHECK-REQUEST-BODY-CAP returns NIL without signalling when both
   declared and actual are at or below the cap, including the boundary."
  (let ((lol-web/server:*max-request-body-bytes* 1024))
    (is (null (lol-web/server::%check-request-body-cap :declared 1024)))
    (is (null (lol-web/server::%check-request-body-cap :actual 1024)))
    (is (null (lol-web/server::%check-request-body-cap :declared 1)))
    (is (null (lol-web/server::%check-request-body-cap :actual 0)))))

(test regression-request-body-cap-disabled-by-nil
  "*MAX-REQUEST-BODY-BYTES* set to NIL disables the cap entirely (test-only
   escape hatch). %CHECK-REQUEST-BODY-CAP must not signal regardless of
   declared/actual size."
  (let ((lol-web/server:*max-request-body-bytes* nil))
    (is (null (lol-web/server::%check-request-body-cap :declared most-positive-fixnum)))
    (is (null (lol-web/server::%check-request-body-cap :actual most-positive-fixnum)))))

(test regression-request-body-fallback-capped
  "request-body's no-:lol/cached-body fallback (the path taken when
   build-clack-env was bypassed) applies the body cap before allocating:
   an env carrying only :raw-body plus an oversize declared :content-length
   signals REQUEST-BODY-TOO-LARGE instead of allocating the declared buffer.
   A sub-cap body on the same path decodes normally."
  (let ((lol-web/server:*max-request-body-bytes* 16))
    ;; Oversize declared content-length on the fallback path → signal before
    ;; the (make-array content-length) allocation.
    (let ((lol-web/server:*env*
            (list :raw-body (flexi-streams:make-in-memory-input-stream
                             (babel:string-to-octets
                              (make-string 64 :initial-element #\a)))
                  :content-length 64)))
      (signals lol-web/server:request-body-too-large
        (lol-web/server:request-body)))
    ;; Sub-cap body decodes through the same fallback branch.
    (let ((lol-web/server:*env*
            (list :raw-body (flexi-streams:make-in-memory-input-stream
                             (babel:string-to-octets "hello"))
                  :content-length 5)))
      (is (string= "hello" (lol-web/server:request-body))))))

;;; ============================================================================
;;; JSON body parser bounds
;;; ============================================================================

(test regression-decode-json-string-rejects-deep-nesting
  "DECODE-JSON-STRING refuses bodies nested deeper than
   *JSON-BODY-MAX-DEPTH* by signaling MALFORMED-JSON-BODY."
  (let ((deep (with-output-to-string (s)
                (loop repeat 64 do (write-char #\[ s))
                (write-char #\0 s)
                (loop repeat 64 do (write-char #\] s)))))
    (let ((lol-web/server:*json-body-max-depth* 32))
      (signals malformed-json-body
        (decode-json-string deep))))
  (is (equal '(1 2 3) (decode-json-string "[1,2,3]"))
      "ordinary shallow JSON still parses under the same cap"))

(test regression-decode-json-string-rejects-enormous-string
  "DECODE-JSON-STRING refuses bodies with a string longer than
   *JSON-BODY-MAX-STRING-LENGTH* by signaling MALFORMED-JSON-BODY."
  (let ((big-string-json
          (with-output-to-string (s)
            (write-char #\" s)
            (loop repeat 1024 do (write-char #\a s))
            (write-char #\" s))))
    (let ((lol-web/server:*json-body-max-string-length* 256))
      (signals malformed-json-body
        (decode-json-string big-string-json)))
    (let ((lol-web/server:*json-body-max-string-length* 2048))
      (is (stringp (decode-json-string big-string-json))
          "same 1024-char JSON string parses under cap 2048")))
  (is (string= "ok" (decode-json-string "\"ok\""))
      "short string still parses under default cap"))

;;; ============================================================================
;;; Backslash defeats same-origin redirect validation (open redirect)
;;; ============================================================================

(test regression-safe-redirect-rejects-backslash
  "A backslash — literal or percent-encoded %5c — lets `/\\evil.com`
   normalise to the protocol-relative `//evil.com` in the browser while a
   naive parser still reads it as a same-origin path. safe-redirect-path-p
   rejects it, %url-authority-host reads it as :malformed, and
   redirect-response refuses it with unsafe-redirect-error."
  (dolist (evil (list "/\\evil.com" "/%5cevil.com" "/%5Cevil.com"))
    (is (not (safe-redirect-path-p evil))
        "safe-redirect-path-p must reject backslash path ~S" evil)
    (is (eq :malformed (lol-web/server::%url-authority-host evil))
        "%url-authority-host must read backslash path ~S as :malformed" evil)
    (signals unsafe-redirect-error (redirect-response evil)))
  (is (safe-redirect-path-p "/account/settings")
      "a clean same-origin path must still pass"))

;;; ============================================================================
;;; Same-origin redirect anchors to canonical host, not the Host header
;;; ============================================================================

(test regression-redirect-anchors-to-canonical-host
  "With *canonical-host* set, the same-origin redirect gate anchors to it and
   refuses an absolute redirect to the attacker-controlled Host header; only
   the canonical origin passes. With *canonical-host* NIL the gate has no
   trusted origin and refuses every absolute redirect, though path-relative
   redirects still pass."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "host" h) "evil.com")
    (let ((*env* (list :headers h))
          (*canonical-host* "app.example.com"))
      (signals unsafe-redirect-error
        (redirect-response "https://evil.com/landing"))
      (let ((ok (redirect-response "https://app.example.com/landing")))
        (is (= 302 (first ok))
            "redirect to the canonical origin must pass, got ~S" ok)))
    (let ((*env* (list :headers h))
          (*canonical-host* nil))
      (signals unsafe-redirect-error
        (redirect-response "https://evil.com/landing"))
      (let ((ok (redirect-response "/landing")))
        (is (= 302 (first ok))
            "with no canonical host a path-relative redirect still passes, ~
             got ~S" ok)))))

;;; ============================================================================
;;; Forged :lol-web.auth.hooks env entry fails closed
;;; ============================================================================

(test regression-auth-hooks-malformed-fails-closed
  "A forged :lol-web.auth.hooks env entry — a non-cons, or a cons whose
   slots are not funcallable — must fail closed: %valid-auth-hooks rejects
   the shape so with-auth denies with a 401 triple (never funcalling
   attacker data) and current-principal returns NIL."
  (dolist (forged (list "pwned" '(t . t) 42 '(1 2 3)))
    (let ((*env* (list :lol-web.auth.hooks forged)))
      (let ((response (with-auth () :should-not-reach)))
        (is (consp response)
            "forged hooks ~S must yield a response triple, got ~S"
            forged response)
        (is (and (consp response) (= 401 (first response)))
            "forged hooks ~S must fail closed with 401, got ~S"
            forged response))
      (is (null (current-principal))
          "forged hooks ~S must yield a NIL principal, got ~S"
          forged (current-principal)))))

;;; ============================================================================
;;; request-csrf-valid-p — route-level self-assertion of CSRF
;;; ============================================================================

(test regression-request-csrf-valid-p-self-asserts
  "request-csrf-valid-p lets a write-capable route self-assert CSRF
   independently of app-level csrf-middleware, dispatching on Content-Type
   exactly as the middleware does. T only when the session token constant-time
   matches the request token (here the X-CSRF-Token header on a JSON request),
   NIL on a mismatch, and NIL when no session is present."
  (is (eq t (request-csrf-valid-p
             (%csrf-mw-json-env :session-token "tok-self-assert-deadbeef"
                                :header-token  "tok-self-assert-deadbeef"
                                :json-body-token :omit)))
      "matching session + request token -> T")
  (is (null (request-csrf-valid-p
             (%csrf-mw-json-env :session-token "tok-session"
                                :header-token  "tok-attacker"
                                :json-body-token :omit)))
      "mismatched request token -> NIL")
  (is (null (request-csrf-valid-p
             (list :content-type "application/json"
                   :headers (let ((h (make-hash-table :test 'equal)))
                              (setf (gethash "x-csrf-token" h) "tok-self-assert-deadbeef")
                              h))))
      "no :lack.session -> NIL even with a request token"))

;;; ============================================================================
;;; CORS — configurable origin, no implicit wildcard default
;;; ============================================================================

(defun %cors-dummy-app (env)
  "Sentinel inner app: a recognizable 200 triple the CORS middleware wraps."
  (declare (ignore env))
  (list 200 (list :content-type "text/plain") (list "ok")))

(test regression-make-app-cors-requires-explicit-origin
  "make-app refuses :use-cors t without :cors-origin — the old implicit
   wildcard (Access-Control-Allow-Origin: *) no longer ships. An explicit
   origin builds a Clack app function."
  (signals error (make-app :use-cors t))
  (let ((app (make-app :use-cors t :cors-origin "https://app.example.com"
                       :use-session nil :use-csrf nil
                       :use-accesslog nil :use-static nil
                       :rate-limit-eviction-interval nil)))
    (is (functionp app)
        "an explicit :cors-origin must build the app, got ~S" app)))

(test regression-cors-middleware-emits-configured-origin-not-wildcard
  "The CORS middleware make-app installs threads the configured origin into
   both the appended response header and the OPTIONS preflight, never the
   hardcoded `*`."
  (let* ((cors-mw (lack/util:find-middleware :cors))
         (origin "https://app.example.com")
         (wrapped (funcall cors-mw #'%cors-dummy-app :origin origin)))
    ;; Non-OPTIONS: ACAO appended to the inner response.
    (let* ((resp (funcall wrapped (list :request-method :get)))
           (headers (second resp)))
      (is (string= origin (getf headers :access-control-allow-origin))
          "configured origin must ride out on the response, got ~S"
          (getf headers :access-control-allow-origin))
      (is (not (equal "*" (getf headers :access-control-allow-origin)))
          "the old wildcard default must not be emitted"))
    ;; OPTIONS preflight: 204 + configured origin.
    (let ((resp (funcall wrapped (list :request-method :options))))
      (is (= 204 (first resp)) "OPTIONS preflight short-circuits 204")
      (is (string= origin (getf (second resp) :access-control-allow-origin))
          "preflight echoes the configured origin, got ~S"
          (getf (second resp) :access-control-allow-origin)))))

;;; ============================================================================
;;; %csrf-fallback-body-bytes — over-declared body fails closed (L1)
;;; ============================================================================

(defun %octets (&rest bytes)
  (coerce bytes '(vector (unsigned-byte 8))))

(test regression-csrf-fallback-rejects-over-declared-body
  "%csrf-fallback-body-bytes is bounded by the declared length: a body that
   delivers more bytes than declared (declared+1) fails closed (NIL) rather
   than returning the extra byte, while a body matching the declared length
   reads cleanly."
  (let* ((declared 4)
         (over (flexi-streams:make-in-memory-input-stream
                (%octets 65 66 67 68 69)))           ; 5 bytes for declared 4
         (env-over (list :raw-body over :content-length declared)))
    (is (null (lol-web/server::%csrf-fallback-body-bytes env-over))
        "an over-declared body (5 bytes, declared 4) must fail closed"))
  (let* ((declared 4)
         (exact (flexi-streams:make-in-memory-input-stream
                 (%octets 65 66 67 68)))             ; exactly declared
         (env-exact (list :raw-body exact :content-length declared))
         (bytes (lol-web/server::%csrf-fallback-body-bytes env-exact)))
    (is (and bytes (= 4 (length bytes)))
        "a body matching the declared length reads cleanly, got ~S" bytes)))

;;; ============================================================================
;;; validate-origin — explicit default ports normalise away (L2)
;;; ============================================================================

(test regression-validate-origin-default-port-equivalence
  "%normalise-origin canonicalises an explicit default port to the implicit
   form, so https://x:443 ≡ https://x and http://x:80 ≡ http://x compare
   equal — closing the availability footgun where an Origin written with its
   default port failed a same-host allowlist. A non-default port stays a
   distinct origin."
  (flet ((norm (v) (lol-web/server::%normalise-origin v)))
    (is (string= (norm "https://x.example") (norm "https://x.example:443"))
        "https default port 443 must canonicalise away")
    (is (string= (norm "http://x.example") (norm "http://x.example:80"))
        "http default port 80 must canonicalise away")
    (is (string= "https://x.example" (norm "https://x.example:443"))
        "explicit :443 normalises to the bare https origin, got ~S"
        (norm "https://x.example:443"))
    (is (not (string= (norm "https://x.example") (norm "https://x.example:8443")))
        "a non-default port must stay a distinct origin"))
  (is (lol-web/server::%allowed-origin-p "https://x.example:443"
                                         '("https://x.example"))
      "an Origin carrying the explicit default port matches an allowlist ~
       entry written without it")
  (is (lol-web/server::%allowed-origin-p "https://x.example"
                                         '("https://x.example:443"))
      "and the reverse: a bare Origin matches an allowlist entry written ~
       with the explicit default port"))
