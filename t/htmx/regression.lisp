;;;; Regression tests for :lol-web/htmx.
;;;;
;;;; Covers: oob-swap duplicate-id avoidance, find-tag-end quote awareness,
;;;; inject-oob-attribute self-closing handling, content-starts-with-id-p
;;;; quote awareness, and the htmx-runtime-js cluster composition contract
;;;; (every behavioural marker from each runtime/* sub-cluster must appear
;;;; in the composed output, and each cluster helper must return an even-
;;;; length list of (string-key, value) pairs).

(in-package :lol-web/htmx/test)
(in-suite :lol-web/htmx/test)

;;; ============================================================================
;;; oob-swap — duplicate ID avoidance
;;; ============================================================================

(test regression-oob-swap-no-duplicate-id
  "oob-swap with outerHTML doesn't wrap when content has target ID"
  (let ((html (lol-web/html:make-safe-html-string
               "<div id=\"target\">content</div>")))
    (let ((result (oob-swap "target" html :swap "outerHTML")))
      (is (null (search "<div id=\"target\"><div id=\"target\"" result)))
      (is (search "hx-swap-oob" result)))))

(test regression-oob-swap-wrap-when-no-id
  "oob-swap still wraps when content lacks target ID"
  (let ((html (lol-web/html:make-safe-html-string "<span>some content</span>")))
    (let ((result (oob-swap "my-target" html :swap "innerHTML")))
      (is (search "id=\"my-target\"" result))
      (is (search "hx-swap-oob" result)))))

(test regression-oob-swap-innerhtml-always-wraps
  "oob-swap innerHTML strategy always wraps"
  (let ((html (lol-web/html:make-safe-html-string
               "<div id=\"target\">content</div>")))
    (let ((result (oob-swap "target" html :swap "innerHTML")))
      (is (search "hx-swap-oob" result)))))

;;; ============================================================================
;;; find-tag-end — quoted > handling
;;; ============================================================================

(test regression-find-tag-end-simple
  "find-tag-end works with simple tags"
  (is (= 4 (lol-web/htmx::find-tag-end "<div>")))
  (is (= 17 (lol-web/htmx::find-tag-end "<div class=\"test\">x"))))

(test regression-find-tag-end-quoted-gt
  "find-tag-end handles > inside quoted attributes"
  (let ((html "<input value=\"a > b\" />"))
    (is (= 22 (lol-web/htmx::find-tag-end html)))))

(test regression-find-tag-end-multiple-quoted-gt
  "find-tag-end handles multiple > in quotes"
  (let ((html "<div title=\"x > y > z\" class=\"a > b\">content"))
    (let ((pos (lol-web/htmx::find-tag-end html)))
      (is (numberp pos))
      (is (char= #\> (char html pos))))))

(test regression-find-tag-end-self-closing
  "find-tag-end works with self-closing tags"
  (is (= 4 (lol-web/htmx::find-tag-end "<br/>")))
  (is (= 5 (lol-web/htmx::find-tag-end "<br />")))
  (is (= 30 (lol-web/htmx::find-tag-end "<input type=\"text\" value=\">\" />"))))

;;; ============================================================================
;;; inject-oob-attribute — self-closing tag boundary
;;; ============================================================================

(test regression-inject-oob-self-closing
  "inject-oob-attribute correctly handles self-closing tags"
  (let ((html "<input type=\"text\" />"))
    (let ((result (lol-web/htmx::inject-oob-attribute html "outerHTML")))
      (is (search "hx-swap-oob=\"outerHTML\"" result))
      (is (search "/>" result)))))

(test regression-inject-oob-regular-tag
  "inject-oob-attribute works with regular tags"
  (let ((html "<div class=\"test\">content</div>"))
    (let ((result (lol-web/htmx::inject-oob-attribute html "true")))
      (is (search "hx-swap-oob=\"true\"" result)))))

;;; ============================================================================
;;; content-starts-with-id-p — quote awareness
;;; ============================================================================

(test regression-content-starts-with-id-basic
  "content-starts-with-id-p detects ID in simple element"
  (is (lol-web/htmx::content-starts-with-id-p
        "<div id=\"target\">content</div>" "target"))
  (is (not (lol-web/htmx::content-starts-with-id-p
             "<div id=\"other\">content</div>" "target"))))

(test regression-content-starts-with-id-quoted-gt
  "content-starts-with-id-p handles > in quoted attributes"
  (is (lol-web/htmx::content-starts-with-id-p
        "<div title=\"x > y\" id=\"target\">content</div>" "target"))
  (is (not (lol-web/htmx::content-starts-with-id-p
             "<div title=\"id=target\">content</div>" "target"))))

;;; ============================================================================
;;; HTMX runtime composition — every cluster's marker must survive ps:ps* splice
;;; ============================================================================

(test regression-htmx-runtime-composition-markers
  "Composed htmx-runtime-js contains every behavioural marker from each cluster"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    ;; Config cluster
    (is (search "HTMX" js) "HTMX object name missing")
    (is (search "0.3.1" js) "version string missing")
    (is (search "defaultSwapStyle" js) "config.defaultSwapStyle missing")
    (is (search "abortControllers" js) "AbortController storage missing")
    (is (search "observers" js) "IntersectionObserver storage missing")
    ;; Swap cluster
    (is (search "innerHTML" js) "swap innerHTML strategy missing")
    (is (search "outerHTML" js) "swap outerHTML strategy missing")
    (is (search "beforebegin" js) "swap beforebegin strategy missing")
    (is (search "afterbegin" js) "swap afterbegin strategy missing")
    (is (search "beforeend" js) "swap beforeend strategy missing")
    (is (search "afterend" js) "swap afterend strategy missing")
    (is (search "textContent" js) "swap textContent strategy missing")
    (is (search "hx-swap-oob" js) "OOB swap selector missing")
    ;; AJAX cluster
    (is (search "issueRequest" js) "issueRequest method missing")
    (is (search "AbortController" js) "AbortController constructor reference missing")
    (is (search "FormData" js) "FormData reference missing")
    (is (search "URLSearchParams" js) "URLSearchParams reference missing")
    (is (search "csrf-token" js) "CSRF token meta selector missing")
    (is (search "htmx:beforeRequest" js) "htmx:beforeRequest event missing")
    (is (search "htmx:configRequest" js) "htmx:configRequest event missing")
    (is (search "htmx:beforeSwap" js) "htmx:beforeSwap event missing")
    (is (search "htmx:afterSwap" js) "htmx:afterSwap event missing")
    (is (search "htmx:afterSettle" js) "htmx:afterSettle event missing")
    (is (search "htmx:afterRequest" js) "htmx:afterRequest event missing")
    (is (search "htmx:responseError" js) "htmx:responseError event missing")
    (is (search "htmx:sendError" js) "htmx:sendError event missing")
    ;; Triggers cluster
    (is (search "parseTrigger" js) "parseTrigger missing")
    (is (search "parseInterval" js) "parseInterval missing")
    (is (search "addTriggerHandler" js) "addTriggerHandler missing")
    (is (search "processElement" js) "processElement missing")
    (is (search "processHxOn" js) "processHxOn missing")
    (is (search "IntersectionObserver" js) "IntersectionObserver constructor missing")
    (is (search "MutationObserver" js) "MutationObserver constructor missing")
    (is (search "setupAutocomplete" js) "setupAutocomplete missing")
    (is (search "highlightOption" js) "highlightOption missing")
    (is (search "clearHighlights" js) "clearHighlights missing")
    (is (search "ArrowDown" js) "ArrowDown navigation missing")
    (is (search "ArrowUp" js) "ArrowUp navigation missing")
    (is (search "Escape" js) "Escape keypress missing")
    ;; Public API cluster
    (is (search "process" js) "htmx.process API missing")
    (is (search "ajax" js) "htmx.ajax API missing")
    (is (search "trigger" js) "htmx.trigger API missing")
    (is (search "onLoad" js) "htmx.onLoad API missing")
    (is (search "init" js) "init method missing")
    ;; Boot cluster
    (is (search "DOMContentLoaded" js) "DOMContentLoaded boot listener missing")
    (is (search "window.htmx" js) "window.htmx alias missing")))

(test regression-htmx-runtime-cluster-helpers-return-pairs
  "Each runtime/* helper returns a flat list with even length and string keys"
  (dolist (helper '(lol-web/htmx::htmx-runtime-config-pairs
                    lol-web/htmx::htmx-runtime-swap-pairs
                    lol-web/htmx::htmx-runtime-ajax-pairs
                    lol-web/htmx::htmx-runtime-triggers-pairs
                    lol-web/htmx::htmx-runtime-public-api-pairs))
    (let ((pairs (funcall helper)))
      (is (listp pairs)
          "~A did not return a list" helper)
      (is (evenp (length pairs))
          "~A returned ~D entries — must be even (key/value pairs)"
          helper (length pairs))
      (loop for (k v) on pairs by #'cddr
            do (is (stringp k)
                   "~A produced non-string key ~S" helper k)))))

;;; ============================================================================
;;; htmx-indicator-css — keyword-as-property bug
;;; ============================================================================

(test regression-htmx-indicator-css-lowercase-properties
  "htmx-indicator-css produces lowercase CSS property names. (css-rules
   formats keys via ~A; keyword keys came out uppercase as 'OPACITY: 0.7'
   which browsers don't recognise. Property keys must now be strings.)"
  (let ((css (htmx-indicator-css)))
    (is (search "opacity:" css)
        "must contain lowercase 'opacity:' property")
    (is (search "cursor:" css)
        "must contain lowercase 'cursor:' property")
    (is (search "display:" css)
        "must contain lowercase 'display:' property")
    (is (null (search "OPACITY:" css))
        "must NOT contain uppercase 'OPACITY:' (regression: keyword formatting)")
    (is (null (search "CURSOR:" css))
        "must NOT contain uppercase 'CURSOR:'")
    (is (null (search "DISPLAY:" css))
        "must NOT contain uppercase 'DISPLAY:'")))

;;; ============================================================================
;;; with-htmx-response — runtime stringp on TRIGGER
;;; ============================================================================

(defun %find-header (name plist)
  "Walk *response-headers* plist by 2; return the value for the (case-
   insensitive) header NAME, or NIL. add-response-header downcases the
   header key on insertion."
  (loop for (k v) on plist by #'cddr
        when (string-equal k name)
        return v))

(test regression-with-htmx-response-trigger-string-literal
  "Literal string TRIGGER lands in HX-Trigger header verbatim — no JSON wrap."
  (lol-web/server:with-response-headers ()
    (with-htmx-response (:trigger "cartUpdated")
      "<p>x</p>")
    (is (equal "cartUpdated"
               (%find-header "hx-trigger" (lol-web/server:get-response-headers)))
        "string literal must pass through unencoded")))

(test regression-with-htmx-response-trigger-runtime-stringp
  "Variable holding a string at runtime must NOT be double-JSON-encoded.
   Regression: previous (if (stringp trigger) ...) ran at macroexpansion
   on the symbol form, taking the encode-json-string branch and emitting
   '\"cartUpdated\"' (with embedded quotes) instead of 'cartUpdated'."
  (let ((evt "cartUpdated"))
    (lol-web/server:with-response-headers ()
      (with-htmx-response (:trigger evt)
        "<p>x</p>")
      (is (equal "cartUpdated"
                 (%find-header "hx-trigger"
                               (lol-web/server:get-response-headers)))
          "runtime string must not be JSON-encoded a second time"))))

(test regression-with-htmx-response-trigger-non-string-encodes
  "Non-string TRIGGER (alist for HX-Trigger detail map) JSON-encodes at runtime."
  (lol-web/server:with-response-headers ()
    (with-htmx-response (:trigger '(("cartUpdated" . ((item . "x")))))
      "<p>x</p>")
    (let ((val (%find-header "hx-trigger"
                             (lol-web/server:get-response-headers))))
      (is (stringp val) "header value must be a string")
      (is (search "cartUpdated" val)
          "encoded JSON must include the event name")
      (is (search "{" val)
          "encoded JSON for an alist must include object braces"))))

;;; ============================================================================
;;; hx-get/post/put/delete URL sanitization
;;; ============================================================================

(test regression-hx-get-rejects-javascript-scheme
  "javascript: URLs produce no hx-get attribute. safe-url returns NIL for
   the unsafe scheme; the format string then suppresses the entire hx-get
   pair so the payload cannot reach the rendered HTML."
  (let ((s (hx-get "javascript:alert(1)")))
    (is (null (search "javascript:" s)))
    (is (null (search "hx-get" s))
        "without a safe URL the hx-get attr must be suppressed")))

(test regression-hx-get-rejects-data-and-vbscript-schemes
  "data: and vbscript: are equally dangerous and equally rejected."
  (is (null (search "data:" (hx-get "data:text/html,<script>alert(1)</script>"))))
  (is (null (search "vbscript:" (hx-get "vbscript:msgbox(1)")))))

(test regression-hx-get-allows-safe-urls
  "https://, root-relative paths, and query strings all pass through."
  (is (search "hx-get=\"https://example.com\"" (hx-get "https://example.com")))
  (is (search "hx-get=\"/api/users\"" (hx-get "/api/users")))
  (is (search "hx-get=\"/search?q=lol\"" (hx-get "/search?q=lol"))))

(test regression-hx-get-escapes-quote-in-url
  "A literal `\"` inside a safe-scheme URL is HTML-attribute-escaped so it
   cannot close the surrounding `\"...\"` and inject sibling attributes."
  (let ((s (hx-get "/search?q=evil\"onclick=alert(1)")))
    (is (null (search "evil\"onclick" s)))
    (is (search "&quot;onclick=alert(1)" s))))

(test regression-hx-post-put-delete-also-sanitize
  "All four hx-* helpers share the sanitization path."
  (is (null (search "javascript:" (hx-post "javascript:x"))))
  (is (null (search "javascript:" (hx-put "javascript:x"))))
  (is (null (search "javascript:" (hx-delete "javascript:x")))))

(test regression-hx-get-target-trigger-attribute-escaped
  "A `\"` in target or trigger is escaped so caller-controlled values
   cannot break out of the attribute."
  (let ((s (hx-get "/api/x"
                   :target "evil\" onerror=alert(1)"
                   :trigger "click consume")))
    (is (null (search "evil\" onerror" s)))
    (is (search "&quot; onerror=alert(1)" s))
    (is (search "hx-trigger=\"click consume\"" s))))

;;; ============================================================================
;;; HX-* response-header hygiene — CR/LF + URL scheme guards
;;; ============================================================================

(defun %signals-error-p (thunk)
  "Run THUNK; return T if it signals an error, NIL on normal return."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(test regression-set-htmx-redirect-rejects-script-schemes
  "javascript:, data:, and vbscript: URLs signal — safe-url-allowlist
   refuses them and the helper turns the miss into an error."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p (lambda () (set-htmx-redirect "javascript:alert(1)"))))
    (is (%signals-error-p
         (lambda () (set-htmx-redirect "data:text/html,<script>"))))
    (is (%signals-error-p (lambda () (set-htmx-redirect "vbscript:msgbox(1)"))))))

(test regression-set-htmx-redirect-rejects-crlf
  "A CR or LF inside the URL is a header-split vector; validate-header-value
   refuses it."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (set-htmx-redirect
                     (format nil "/safe~C~CSet-Cookie: pwned=1"
                             #\Return #\Linefeed)))))))

(test regression-set-htmx-redirect-accepts-safe-url
  "https and root-relative URLs pass through and land in HX-Redirect verbatim."
  (lol-web/server:with-response-headers ()
    (set-htmx-redirect "https://example.com/ok")
    (is (equal "https://example.com/ok"
               (%find-header "hx-redirect"
                             (lol-web/server:get-response-headers)))))
  (lol-web/server:with-response-headers ()
    (set-htmx-redirect "/api/users")
    (is (equal "/api/users"
               (%find-header "hx-redirect"
                             (lol-web/server:get-response-headers))))))

(test regression-set-htmx-location-rejects-script-scheme
  "The path sub-field routes through safe-url-allowlist whether or not
   :target / :swap are supplied."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (set-htmx-location "javascript:alert(1)"))))
    (is (%signals-error-p
         (lambda () (set-htmx-location "javascript:alert(1)"
                                       :target "#main" :swap "innerHTML"))))))

(test regression-set-htmx-location-encodes-json-with-target
  "With :target or :swap the header value is a JSON object carrying path,
   target, and/or swap."
  (lol-web/server:with-response-headers ()
    (set-htmx-location "/dashboard" :target "#main" :swap "innerHTML")
    (let ((val (%find-header "hx-location"
                             (lol-web/server:get-response-headers))))
      (is (stringp val))
      (is (search "/dashboard" val))
      (is (search "#main" val))
      (is (search "innerHTML" val))
      (is (search "{" val)))))

(test regression-set-htmx-trigger-symbol-event-name-becomes-string
  "A symbol event-name jsonifies to its downcased name (a JS string
   literal shape), never a bare identifier the browser could coerce."
  (lol-web/server:with-response-headers ()
    (set-htmx-trigger 'cartUpdated)
    (is (equal "cartupdated"
               (%find-header "hx-trigger"
                             (lol-web/server:get-response-headers))))))

(test regression-set-htmx-trigger-rejects-cons-event-name
  "A cons in event-name has no string shape; jsonify signals."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (set-htmx-trigger '(alert "click"))))))
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (set-htmx-trigger '(alert "click") '((item . "x"))))))))

(test regression-set-htmx-trigger-rejects-crlf-in-name
  "CR or LF in a string event-name signals via validate-header-value."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (set-htmx-trigger
                     (format nil "evt~CSet-Cookie: x=1" #\Return)))))))

(test regression-set-htmx-trigger-detail-uses-sanitized-key
  "With event-detail the JSON key is the jsonified event-name — symbols
   become downcased strings rather than appearing as Lisp identifiers."
  (lol-web/server:with-response-headers ()
    (set-htmx-trigger 'cartUpdated '((item . "x")))
    (let ((val (%find-header "hx-trigger"
                             (lol-web/server:get-response-headers))))
      (is (stringp val))
      (is (search "cartupdated" val)
          "JSON key must be the downcased symbol name")
      (is (search "\"cartupdated\"" val)
          "key must be quoted as a JSON string"))))

(test regression-with-htmx-response-retarget-rejects-crlf
  "CR or LF in any header-value-shaped field signals."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (with-htmx-response
                        (:retarget (format nil "#main~CX-Pwned: 1" #\Return))
                      "<p>x</p>")))))
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (with-htmx-response
                        (:reswap (format nil "innerHTML~C" #\Linefeed))
                      "<p>x</p>"))))))

(test regression-with-htmx-response-push-url-rejects-script-scheme
  ":push-url and :replace-url route through safe-url-allowlist."
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (with-htmx-response (:push-url "javascript:alert(1)")
                      "<p>x</p>")))))
  (lol-web/server:with-response-headers ()
    (is (%signals-error-p
         (lambda () (with-htmx-response (:replace-url "data:text/html,x")
                      "<p>x</p>"))))))

(test regression-with-htmx-response-trigger-symbol-becomes-string
  "A bare symbol :trigger jsonifies to its downcased name."
  (let ((evt 'cartUpdated))
    (lol-web/server:with-response-headers ()
      (with-htmx-response (:trigger evt)
        "<p>x</p>")
      ;; jsonify of a symbol returns its downcased name as a string;
      ;; validate-header-value then accepts it verbatim.
      (is (equal "cartupdated"
                 (%find-header "hx-trigger"
                               (lol-web/server:get-response-headers)))))))

;;; ============================================================================
;;; OOB selector allowlist + signed-token escape hatch
;;; ============================================================================

(defun %signals-unsafe-oob-p (thunk)
  "Run THUNK; return T iff it signals UNSAFE-OOB-SELECTOR (or its subclass)."
  (handler-case (progn (funcall thunk) nil)
    (lol-web/htmx:unsafe-oob-selector () t)))

(test regression-oob-target-allowlist-rejects-body
  "validate-oob-selector refuses every selector in the default denylist —
   `body`, `head`, `html`, top-level `form` — even when *oob-selector-allowlist*
   is left permissive (NIL). Re-admitting these via a signed token also
   fails: the denylist is the hard gate."
  (let ((lol-web/htmx:*oob-selector-allowlist* nil)
        (lol-web/htmx:*oob-signed-selector-secret* nil))
    (dolist (sel '("body" "head" "html" "form" "FORM" "form[name='x']"))
      (is (%signals-unsafe-oob-p
           (lambda () (lol-web/htmx:validate-oob-selector sel)))
          "selector ~S must be refused" sel))))

(test regression-oob-allowlist-tightens-when-set
  "Setting *oob-selector-allowlist* to a non-NIL list tightens validation:
   selectors that previously passed (permissive default) must now match
   one of the allowlist entries or fail :not-allowlisted."
  (let ((lol-web/htmx:*oob-selector-allowlist*
         '((:id "comp-1") (:class "card")))
        (lol-web/htmx:*oob-signed-selector-secret* nil))
    (is (string= "#comp-1"
                 (lol-web/htmx:validate-oob-selector "#comp-1")))
    (is (string= ".card"
                 (lol-web/htmx:validate-oob-selector ".card")))
    (is (%signals-unsafe-oob-p
         (lambda () (lol-web/htmx:validate-oob-selector "#comp-2")))
        "selector outside the allowlist must signal")))

(test regression-oob-signed-selector-accepted
  "A signed token whose payload string-equals the selector re-admits an
   otherwise-disallowed selector through a tightened allowlist; the
   denylist still refuses regardless of the token."
  (let* ((secret (ironclad:random-data 32))
         (lol-web/htmx:*oob-selector-allowlist* '((:id "comp-1")))
         (lol-web/htmx:*oob-signed-selector-secret* secret)
         (selector "#escape-hatch")
         (token (lol-web/htmx:mint-oob-selector-token selector)))
    (is (string= selector
                 (lol-web/htmx:validate-oob-selector selector :signed-token token))
        "matching token re-admits the selector")
    (is (%signals-unsafe-oob-p
         (lambda ()
           (lol-web/htmx:validate-oob-selector "body" :signed-token token)))
        "denylist still refuses even with a signed token in hand")
    (let ((bad (lol-web/htmx:mint-oob-selector-token "#different-target")))
      (is (%signals-unsafe-oob-p
           (lambda ()
             (lol-web/htmx:validate-oob-selector selector :signed-token bad)))
          "token whose payload differs from the selector is refused"))))

;;; ============================================================================
;;; OOB primitives — safe-html-string contract at every entry
;;; ============================================================================

(test regression-oob-swap-refuses-raw-content
  "oob-swap signals when content is a bare string — the producer must
   pass a safe-html-string so the type carries the safety claim across
   the call boundary."
  (is (%signals-error-p
       (lambda () (oob-swap "target" "<b>raw</b>")))))

(test regression-make-oob-swap-refuses-raw-content
  "make-oob-swap mirrors oob-swap's type discipline."
  (let ((lol-web/htmx:*oob-selector-allowlist* '((:id "ok"))))
    (is (%signals-error-p
         (lambda () (make-oob-swap "#ok" "<b>raw</b>"))))))

(test regression-oob-content-refuses-raw-content
  "oob-content refuses raw strings."
  (is (%signals-error-p
       (lambda () (oob-content "counter" "42")))))

(test regression-render-with-oob-refuses-raw-content
  "render-with-oob refuses raw strings in primary content and in each
   OOB update slot — every content boundary takes safe-html-string."
  (is (%signals-error-p
       (lambda () (render-with-oob "<main>raw</main>"))))
  (is (%signals-error-p
       (lambda () (render-with-oob
                    nil
                    (list "count" "5"))))))

(test regression-render-with-oob-accepts-safe-html
  "Wrapping primary content and per-update content in safe-html-string
   passes the check-type; the rendered string contains both."
  (let ((result (render-with-oob
                  (lol-web/html:make-safe-html-string "<main>ok</main>")
                  (list "count"
                        (lol-web/html:make-safe-html-string "5")))))
    (is (stringp result))
    (is (search "<main>ok</main>" result))
    (is (search "id=\"count\"" result))
    (is (search "5" result))))

;;; ============================================================================
;;; sanitize-hx-on-attrs — broadcast-wire strip
;;; ============================================================================

(test regression-sanitize-hx-on-attrs-strips-double-quoted
  "Double-quoted hx-on-* attributes are removed entirely; the rest of
   the tag survives."
  (let ((out (lol-web/escape:sanitize-hx-on-attrs
              "<button id=\"x\" hx-on-click=\"alert(1)\">y</button>")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert(1)" out)))
    (is (search "id=\"x\"" out))
    (is (search ">y</button>" out))))

(test regression-sanitize-hx-on-attrs-strips-single-quoted
  "Single-quoted attribute values are equally removed."
  (let ((out (lol-web/escape:sanitize-hx-on-attrs
              "<a hx-on-click='evil()' href=\"/ok\">link</a>")))
    (is (not (search "hx-on" out)))
    (is (not (search "evil()" out)))
    (is (search "href=\"/ok\"" out))))

(test regression-sanitize-hx-on-attrs-strips-htmx-event-variants
  "hx-on-htmx-* and hx-on--* (long-form and short-form htmx event syntax)
   are both stripped."
  (let ((out (lol-web/escape:sanitize-hx-on-attrs
              "<div hx-on-htmx-after-swap=\"a()\" hx-on--load=\"b()\">z</div>")))
    (is (not (search "hx-on" out)))
    (is (not (search "a()" out)))
    (is (not (search "b()" out)))
    (is (search ">z</div>" out))))

(test regression-sanitize-hx-on-attrs-leaves-text-mentions-alone
  "Text content like `hx-on-click` (no `=` after the identifier) is left
   alone — only attribute syntax is stripped."
  (let ((out (lol-web/escape:sanitize-hx-on-attrs
              "<p>Use hx-on-click to bind handlers.</p>")))
    (is (search "hx-on-click" out)
        "prose mentions outside attribute syntax must survive")))

(test regression-sanitize-hx-on-attrs-leaves-other-attrs-alone
  "Non-hx-on attributes (hx-get, hx-trigger, data-*) are untouched."
  (let ((out (lol-web/escape:sanitize-hx-on-attrs
              "<div hx-get=\"/api\" data-foo=\"bar\">x</div>")))
    (is (search "hx-get=\"/api\"" out))
    (is (search "data-foo=\"bar\"" out))))

;;; ============================================================================
;;; emit-hx-attrs / hx-morph / oob-content / render-autocomplete attribute sinks
;;; ============================================================================

(test regression-emit-hx-attrs-rejects-javascript-url
  "emit-hx-attrs runs the URL through safe-url; a javascript: scheme
   yields NIL and the format string suppresses the entire hx-<verb> pair.
   target/swap/trigger are attribute-escaped on the happy path."
  (let ((s (emit-hx-attrs "get" "javascript:alert(1)" nil nil nil)))
    (is (null (search "javascript" s)))
    (is (null (search "hx-get" s))))
  (let ((s (emit-hx-attrs "post" "/api/x" "#out" "innerHTML" "click")))
    (is (search "hx-post=\"/api/x\"" s))
    (is (search "hx-target=\"#out\"" s))
    (is (search "hx-swap=\"innerHTML\"" s))))

(test regression-hx-morph-escapes-attributes
  "hx-morph routes url/target/trigger through emit-hx-attrs: a `\"` in a
   value is attribute-escaped so it cannot break out, and a javascript:
   url is suppressed entirely."
  (let ((s (hx-morph "/api/search"
                     :target "evil\" onerror=alert(1)"
                     :trigger "input")))
    (is (search "hx-get=\"/api/search\"" s))
    (is (search "&quot; onerror=alert(1)" s))
    (is (null (search "evil\" onerror" s))))
  (is (null (search "javascript" (hx-morph "javascript:alert(1)")))))

(test regression-oob-content-validates-selector
  "oob-content routes its `#ID` through validate-oob-selector, so a
   tightened allowlist accepts an in-list id and refuses one outside it."
  (let ((lol-web/htmx:*oob-selector-allowlist* '((:id "good")))
        (lol-web/htmx:*oob-signed-selector-secret* nil))
    (is (search "innerHTML:#good"
                (oob-content "good" (lol-web/html:make-safe-html-string "x"))))
    (is (%signals-unsafe-oob-p
         (lambda ()
           (oob-content "evil" (lol-web/html:make-safe-html-string "x")))))))

(test regression-autocomplete-endpoint-safe-href
  "render-autocomplete emits its endpoint through safe-href: a
   javascript: endpoint produces no hx-get attribute; a safe relative
   endpoint passes through."
  (let ((s (render-autocomplete :id "ac" :endpoint "javascript:alert(1)")))
    (is (null (search "javascript" s)))
    (is (null (search "hx-get" s))))
  (let ((s (render-autocomplete :id "ac" :endpoint "/api/search")))
    (is (search "hx-get=" s))
    (is (search "/api/search" s))))

(test regression-oob-swap-escapes-hostile-id
  "oob-swap routes its id and swap through safe-attr, so a `\"` in the id
   cannot close the attribute and graft a sibling event handler. The
   default NIL allowlist lets the hostile id reach the splice — escaping,
   not selector validation, is what closes the breakout."
  (let ((lol-web/htmx:*oob-selector-allowlist* nil)
        (lol-web/htmx:*oob-signed-selector-secret* nil))
    (let ((s (oob-swap "x\" onmouseover=\"alert(1)"
                       (lol-web/html:make-safe-html-string "ok"))))
      (is (search "&quot;" s))
      (is (null (search "\" onmouseover=\"alert" s))
          "raw quote breakout must not survive into the id attribute")
      (is (search "ok" s) "the safe body still emits"))))

(test regression-oob-content-escapes-hostile-id
  "oob-content folds the id into the hx-swap-oob value; routing the whole
   `innerHTML:#ID` through safe-attr keeps a hostile id from closing the
   attribute."
  (let ((lol-web/htmx:*oob-selector-allowlist* nil)
        (lol-web/htmx:*oob-signed-selector-secret* nil))
    (let ((s (oob-content "x\" onmouseover=\"alert(1)"
                          (lol-web/html:make-safe-html-string "ok"))))
      (is (search "&quot;" s))
      (is (null (search "\" onmouseover=\"alert" s)))
      (is (search "innerHTML:#x" s) "the selector prefix is preserved"))))

(test regression-autocomplete-escapes-attributes
  "render-autocomplete routes id/placeholder/class through safe-attr, so a
   placeholder closing the input and opening a <script> is neutralized."
  (let ((s (render-autocomplete
            :id "ac"
            :endpoint "/api/search"
            :placeholder "x\"><script>alert(1)</script>")))
    (is (null (search "<script>alert(1)" s))
        "no raw <script> may survive the placeholder splice")
    (is (search "&lt;script" s))
    (is (search "&quot;" s)))
  (let ((s (render-autocomplete
            :id "x\"><img src=x onerror=alert(1)>"
            :endpoint "/api/search")))
    (is (null (search "\"><img" s))
        "a hostile id cannot break out of any of its attribute sinks")
    (is (search "&lt;img" s))))

;;; ============================================================================
;;; find-tag-end / inject-oob-attribute — single-quoted attribute values
;;; ============================================================================

(test regression-find-tag-end-single-quoted-gt
  "find-tag-end skips a `>` inside a single-quoted attribute value (not only
   double-quoted), and a quote of the opposite kind inside the active run is a
   literal, so the returned offset is the real opening-tag close."
  (is (= 17 (lol-web/htmx::find-tag-end "<div data-x='a>b'>"))
      "single-quoted attribute: the `>` at index 14 is inside the quotes")
  (is (= 18 (lol-web/htmx::find-tag-end "<div data-x='a\">b'>"))
      "a double-quote inside a single-quoted run is literal, not a toggle")
  (is (= 17 (lol-web/htmx::find-tag-end "<div data-x=\"a>b\">"))
      "double-quoted handling still holds (single-quote support is a superset)"))

(test regression-inject-oob-single-quoted-gt
  "inject-oob-attribute finds the real tag end past a single-quoted attribute
   whose value contains `>`, so hx-swap-oob lands after the attribute (not
   mid-value) and the attribute survives intact."
  (let ((result (lol-web/htmx::inject-oob-attribute
                 "<div data-x='a>b'>content</div>" "true")))
    (is (search "data-x='a>b'" result)
        "the single-quoted attribute value is preserved verbatim")
    (is (search "data-x='a>b' hx-swap-oob=\"true\">" result)
        "hx-swap-oob is injected at the real tag end, right after the attribute")))

