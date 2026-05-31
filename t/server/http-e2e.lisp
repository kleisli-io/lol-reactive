;;;; HTTP-level end-to-end tests for :lol-web/server.
;;;;
;;;; Drives the make-app result with synthetic requests through
;;;; lack/test:request and asserts on observable response bytes:
;;;;
;;;;   - Set-Cookie headers carry Secure; HttpOnly; SameSite=Lax
;;;;   - SID after session-rotate differs from the prior SID
;;;;   - csrf-token in the session after rotate differs from the prior token
;;;;   - with-auth returns 401 when authp-thunk returns NIL
;;;;   - with-auth returns 302 when :on-unauthorized is a redirect path
;;;;
;;;; The constructor-only cookie tests in regression.lisp prove that
;;;; make-cookie-state accepts the keywords; this file proves the keywords
;;;; reach the response wire. Constructor parity does not imply wire parity.

(in-package :lol-web/server/test)
(in-suite :lol-web/server/test)

;;; ============================================================================
;;; Route fixtures
;;; ============================================================================
;;;
;;; Paths are namespaced under /__e2e/ so the routes registered at file load
;;; cannot collide with other suites running in the same image. The routes
;;; pass :secure nil so add-security-headers does not pull design tokens
;;; into the response — these tests assert on cookie and status bytes only.

(defroute "/__e2e/cookie-set" (:method :get :secure nil)
  (session-set "marker" "set")
  "ok")

(defroute "/__e2e/cookie-probe" (:method :get :secure nil)
  (or (session-get "marker") ""))

(defroute "/__e2e/rotate" (:method :get :secure nil)
  ;; Ensure a csrf-token exists in the session BEFORE rotate so the
  ;; observation has something to compare against.
  (get-csrf-token)
  (session-rotate)
  "rotated")

(defroute "/__e2e/csrf-probe" (:method :get :secure nil)
  ;; get-csrf-token mints if missing and stores under "csrf-token". The
  ;; rotate handler clears the slot via session-rotate's unconditional
  ;; remhash; the next probe materialises a fresh token here.
  (get-csrf-token))

(defroute "/__e2e/auth-status" (:method :get :secure nil)
  (with-auth ()
    "ok"))

(defroute "/__e2e/auth-redirect" (:method :get :secure nil)
  (with-auth (:on-unauthorized "/__e2e/sign-in")
    "ok"))

(defroute "/__e2e/whoami" (:method :get :secure nil)
  ;; Returns the principal in PRIN1 form so a keyword principal :alpha
  ;; renders as ":ALPHA" in the response body — deterministic to search
  ;; for cross-leak from a sibling app's hooks.
  (format nil "~S" (current-principal)))

(defroute "/__e2e/post-echo" (:method :post :secure nil)
  ;; A 200 target reached only after csrf-middleware accepts the token.
  "posted")

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(defun %extract-sid (set-cookie-text)
  "Pull the lack.session=<sid> value out of a Set-Cookie header chunk.
   lack/test:request comma-joins repeated Set-Cookie headers into one
   string, so the parse stops at the first `;` or `,` (whichever comes
   first) after the lack.session= prefix."
  (when set-cookie-text
    (let* ((needle "lack.session=")
           (start (search needle set-cookie-text)))
      (when start
        (let* ((vstart (+ start (length needle)))
               (semi   (position #\; set-cookie-text :start vstart))
               (comma  (position #\, set-cookie-text :start vstart))
               (end    (cond ((and semi comma) (min semi comma))
                             (semi semi)
                             (comma comma)
                             (t (length set-cookie-text)))))
          (subseq set-cookie-text vstart end))))))

;;; ============================================================================
;;; Cookie hardening — wire-level assertions
;;; ============================================================================

(test e2e-set-cookie-carries-secure-httponly-samesite-lax
  "make-app's documented cookie defaults must reach the response wire:
   Set-Cookie carries Secure; HttpOnly; SameSite=Lax. The constructor
   test in regression.lisp proves make-cookie-state accepts the
   keywords; this test proves they survive the round-trip through
   the session middleware and reach the client."
  (lack/test:testing-app (make-app :use-csrf nil
                                   :use-static nil
                                   :use-accesslog nil)
    (multiple-value-bind (body status headers)
        (lack/test:request "/__e2e/cookie-set")
      (declare (ignore body status))
      (let* ((set-cookie (gethash "set-cookie" headers))
             ;; RFC 6265 cookie attributes are case-insensitive. Lack emits
             ;; lowercase `secure` and `HttpOnly` / `SameSite=Lax` mixed;
             ;; lowercase the haystack so the asserts pin the attribute,
             ;; not its capitalisation.
             (lower (when set-cookie (string-downcase set-cookie))))
        (is (not (null set-cookie))
            "response must include a Set-Cookie header")
        (is (search "secure" lower)
            "Set-Cookie must carry Secure, got: ~A" set-cookie)
        (is (search "httponly" lower)
            "Set-Cookie must carry HttpOnly, got: ~A" set-cookie)
        (is (search "samesite=lax" lower)
            "Set-Cookie must carry SameSite=Lax, got: ~A" set-cookie)))))

;;; ============================================================================
;;; Session rotation — wire-level assertions
;;; ============================================================================

(test e2e-session-rotate-changes-sid
  "After session-rotate the next response's lack.session value differs
   from the prior SID. Without behavioural coverage this property is
   silently broken by any refactor that drops the :change-id flip."
  (lack/test:testing-app (make-app :use-csrf nil
                                   :use-static nil
                                   :use-accesslog nil)
    (let ((jar (cl-cookie:make-cookie-jar)))
      ;; Establish a session and capture the initial SID.
      (multiple-value-bind (body1 status1 hdrs1)
          (lack/test:request "/__e2e/cookie-set" :cookie-jar jar)
        (declare (ignore body1 status1))
        (let ((sid-before (%extract-sid (gethash "set-cookie" hdrs1))))
          (is (not (null sid-before))
              "first response must yield a session SID")
          ;; Hit the rotate route; the response Set-Cookie carries the new SID.
          (multiple-value-bind (body2 status2 hdrs2)
              (lack/test:request "/__e2e/rotate" :cookie-jar jar)
            (declare (ignore body2 status2))
            (let ((sid-after (%extract-sid (gethash "set-cookie" hdrs2))))
              (is (not (null sid-after))
                  "rotate response must emit Set-Cookie carrying the new SID")
              (is (not (string= sid-before sid-after))
                  "SID after rotate must differ from SID before rotate ~
                   (got ~A both before and after)" sid-before))))))))

(test e2e-session-rotate-regenerates-csrf-token
  "session-rotate unconditionally removes \"csrf-token\" from the session
   hash so the next handler that calls get-csrf-token mints a fresh
   token. The token observed after a second rotate must differ from
   the token observed after the first."
  (lack/test:testing-app (make-app :use-csrf nil
                                   :use-static nil
                                   :use-accesslog nil)
    (let ((jar (cl-cookie:make-cookie-jar)))
      ;; First rotate stores+rotates; second rotate stores a fresh token
      ;; and rotates. The probe between them captures token1; the probe
      ;; after the second rotate captures token2.
      (lack/test:request "/__e2e/rotate" :cookie-jar jar)
      (let ((token1 (lack/test:request "/__e2e/csrf-probe" :cookie-jar jar)))
        (lack/test:request "/__e2e/rotate" :cookie-jar jar)
        (let ((token2 (lack/test:request "/__e2e/csrf-probe" :cookie-jar jar)))
          (is (plusp (length token1))
              "first csrf probe must yield a non-empty token, got ~S" token1)
          (is (plusp (length token2))
              "second csrf probe must yield a non-empty token, got ~S" token2)
          (is (not (string= token1 token2))
              "csrf-token must change across session-rotate ~
               (got ~A both times)" token1))))))

;;; ============================================================================
;;; with-auth — wire-level assertions
;;; ============================================================================

(test e2e-with-auth-returns-401-when-authp-returns-nil
  "with-auth default :on-unauthorized 401 must surface as a 401 status
   on the response wire when the registered authp-thunk returns NIL."
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (lack/test:testing-app (make-app :use-csrf nil
                                     :use-static nil
                                     :use-accesslog nil)
      (multiple-value-bind (body status)
          (lack/test:request "/__e2e/auth-status")
        (declare (ignore body))
        (is (= 401 status)
            "expected 401 from with-auth when authp returns NIL, got ~D"
            status)))))

(test e2e-with-auth-returns-302-when-on-unauthorized-is-path
  "with-auth :on-unauthorized <path> must surface as a 302 redirect on
   the response wire with the Location header pointing at the path.
   :max-redirects 0 keeps the test from auto-following into a 404."
  (with-auth-hooks (:authenticated-p (lambda () nil)
                    :current-principal (lambda () nil))
    (lack/test:testing-app (make-app :use-csrf nil
                                     :use-static nil
                                     :use-accesslog nil)
      (multiple-value-bind (body status headers)
          (lack/test:request "/__e2e/auth-redirect" :max-redirects 0)
        (declare (ignore body))
        (is (= 302 status)
            "expected 302 from with-auth :on-unauthorized path string, got ~D"
            status)
        (let ((location (gethash "location" headers)))
          (is (not (null location))
              "302 response must include a Location header")
          (is (search "/__e2e/sign-in" location)
              "Location must point to the supplied :on-unauthorized path, got ~A"
              location))))))

;;; ============================================================================
;;; Two-app auth-hook isolation — the closure-capture invariant
;;; ============================================================================
;;;
;;; The auth-hooks middleware that make-app builds captures the supplied
;;; (authp . principal) cons in its lexical closure, so two apps built in
;;; one image must hold independent hook state. The check fires real
;;; requests through each app's wrapper and inspects the body for the
;;; other app's principal — any cross-leak would surface here.

(test e2e-two-apps-independent-auth-state
  "Two apps built in one image with distinct :auth plists hold
   independent (authp . principal) conses. A request through app A
   observes only app A's principal; a request through app B observes
   only app B's. Cross-leak (e.g., a shared image-global) would surface
   as one body carrying the other's principal."
  (let ((app-a (make-app :use-csrf nil :use-static nil :use-accesslog nil
                         :auth (list :authenticated-p (lambda () t)
                                     :current-principal (lambda () :alpha))))
        (app-b (make-app :use-csrf nil :use-static nil :use-accesslog nil
                         :auth (list :authenticated-p (lambda () t)
                                     :current-principal (lambda () :bravo)))))
    (let ((body-a (lack/test:testing-app app-a
                    (lack/test:request "/__e2e/whoami")))
          (body-b (lack/test:testing-app app-b
                    (lack/test:request "/__e2e/whoami"))))
      (is (search ":ALPHA" body-a)
          "request to app A must observe principal :ALPHA, got ~A" body-a)
      (is (search ":BRAVO" body-b)
          "request to app B must observe principal :BRAVO, got ~A" body-b)
      (is (null (search ":BRAVO" body-a))
          "request to app A must NOT observe app B's principal, got ~A" body-a)
      (is (null (search ":ALPHA" body-b))
          "request to app B must NOT observe app A's principal, got ~A" body-b))))

;;; ============================================================================
;;; Default-stack write path — csrf inside session
;;; ============================================================================
;;;
;;; Drives make-app's default stack (use-csrf t + use-session t) through a
;;; POST. csrf-middleware reads :lack.session, so session must dispatch
;;; outer; otherwise the token check reads a nil session and errors. A
;;; matching token yields 200, a missing/wrong token 403, never 500.

(test e2e-default-stack-post-csrf
  "A default-stack (use-csrf t, use-session t) POST round-trips a CSRF
   token: matching → 200, missing/wrong → 403, never 500."
  (lack/test:testing-app (make-app :use-static nil :use-accesslog nil)
    (let ((jar (cl-cookie:make-cookie-jar)))
      ;; GET mints a csrf-token into the session and sets the session cookie
      ;; in the jar. GETs pass csrf-middleware unchecked.
      (let ((token (lack/test:request "/__e2e/csrf-probe" :cookie-jar jar)))
        (is (plusp (length token))
            "csrf-probe must yield a non-empty token, got ~S" token)
        ;; Matching token → 200, no 500.
        (multiple-value-bind (body status)
            (lack/test:request "/__e2e/post-echo"
                               :method :post
                               :cookie-jar jar
                               :content "{}"
                               :headers (list '("content-type" . "application/json")
                                              (cons "x-csrf-token" token)))
          (is (= 200 status)
              "matching-token POST through the default stack must be 200, ~
               got ~D" status)
          (is (search "posted" body)
              "200 body must come from the post-echo route, got ~S" body))
        ;; Wrong token → 403, no 500.
        (multiple-value-bind (body status)
            (lack/test:request "/__e2e/post-echo"
                               :method :post
                               :cookie-jar jar
                               :content "{}"
                               :headers (list '("content-type" . "application/json")
                                              '("x-csrf-token" . "not-the-token")))
          (declare (ignore body))
          (is (= 403 status)
              "wrong-token POST must be 403 (not 500), got ~D" status))
        ;; Missing token → 403, no 500.
        (multiple-value-bind (body status)
            (lack/test:request "/__e2e/post-echo"
                               :method :post
                               :cookie-jar jar
                               :content "{}"
                               :headers (list '("content-type" . "application/json")))
          (declare (ignore body))
          (is (= 403 status)
              "missing-token POST must be 403 (not 500), got ~D" status))))))
