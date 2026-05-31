;;;; Regression tests for html-page's pre-computed payload parameters.
;;;;
;;;; html-page used to call tailwind-config / generate-css-variables /
;;;; generate-all-component-css / htmx-indicator-css / htmx-runtime-js /
;;;; surgery-css / surgery-runtime-js / get-csrf-token directly — pulling
;;;; css-tokens, htmx, surgery, and CSRF machinery into :lol-web/html as
;;;; hard dependencies. Each generator now has a parameter; the helper is
;;;; the lazy default via (or PARAM (HELPER)). Callers pre-compute once
;;;; and thread payloads on each request, decoupling html-page from those
;;;; subsystems.
;;;;
;;;; Payload kwargs (head-extra, body, base-css, component-css, reactive-
;;;; runtime, htmx-runtime, tailwind-script, surgery-css, surgery-runtime,
;;;; htmx-indicator-css) consult SAFE-HTML-STRING for verbatim emit; raw
;;;; strings are escape-html-ed. Producers wrap their output via
;;;; MAKE-SAFE-HTML-STRING to opt content into raw emit.

(in-package :lol-web/html/test)
(in-suite :lol-web/html/test)

(test regression-html-page-accepts-precomputed-safe-html-strings
  "html-page emits SAFE-HTML-STRING payloads verbatim, bypassing internal generators"
  (let ((html (html-page
                :title "Probe"
                :base-css (make-safe-html-string "/* CUSTOM-BASE-CSS */")
                :component-css (make-safe-html-string "/* CUSTOM-COMPONENT-CSS */")
                :csrf-token "CUSTOM-CSRF-TOKEN"
                :reactive-runtime (make-safe-html-string "/* CUSTOM-REACTIVE-RUNTIME */")
                :htmx-runtime (make-safe-html-string "/* CUSTOM-HTMX-RUNTIME */")
                :tailwind-script (make-safe-html-string "/* CUSTOM-TAILWIND-SCRIPT */")
                :htmx-indicator-css (make-safe-html-string "/* CUSTOM-HTMX-INDICATOR-CSS */"))))
    (is (search "CUSTOM-BASE-CSS" html)
        "BASE-CSS override missing from page")
    (is (search "CUSTOM-COMPONENT-CSS" html)
        "COMPONENT-CSS override missing")
    (is (search "CUSTOM-CSRF-TOKEN" html)
        "CSRF-TOKEN override missing — page generator still calls get-csrf-token")
    (is (search "CUSTOM-REACTIVE-RUNTIME" html)
        "REACTIVE-RUNTIME override missing")
    (is (search "CUSTOM-HTMX-RUNTIME" html)
        "HTMX-RUNTIME override missing")
    (is (search "CUSTOM-TAILWIND-SCRIPT" html)
        "TAILWIND-SCRIPT override missing")
    (is (search "CUSTOM-HTMX-INDICATOR-CSS" html)
        "HTMX-INDICATOR-CSS override missing")))

(test regression-html-page-escapes-raw-string-payload
  "Raw string in payload position is escape-html-ed, never emitted verbatim"
  (let ((html (html-page
                :title "Probe"
                :body "<script>alert(1)</script>")))
    (is (null (search "<script>alert(1)</script>" html))
        "raw <script> body must not be emitted verbatim")
    (is (search "&lt;script&gt;alert(1)&lt;/script&gt;" html)
        "raw body must appear escape-html-ed")))

(test regression-html-page-include-htmx-nil-suppresses
  "INCLUDE-HTMX NIL suppresses HTMX assets even when overrides are provided"
  (let ((html (html-page
                :title "NoHtmx"
                :include-htmx nil
                :htmx-runtime (make-safe-html-string "/* CUSTOM-HTMX-RUNTIME */")
                :htmx-indicator-css (make-safe-html-string "/* CUSTOM-HTMX-INDICATOR-CSS */")
                :csrf-token "CUSTOM-CSRF-TOKEN")))
    (is (null (search "CUSTOM-HTMX-RUNTIME" html))
        "INCLUDE-HTMX NIL must suppress HTMX runtime even with explicit override")
    (is (null (search "CUSTOM-HTMX-INDICATOR-CSS" html))
        "INCLUDE-HTMX NIL must suppress HTMX indicator CSS")
    (is (null (search "CUSTOM-CSRF-TOKEN" html))
        "INCLUDE-HTMX NIL must suppress the CSRF meta tag")))

;;; ============================================================================
;;; Attribute-position escaping — meta / og / csrf / url fields
;;; ============================================================================
;;;
;;; cl-who does not escape runtime attribute values, so every kwarg spliced
;;; into a :content / :href position must be escaped at the call site. A
;;; value carrying a quote-then-tag breakout must come out entity-escaped,
;;; with no raw markup surviving into the document.

(test regression-html-page-meta-attribute-breakout
  "description / og-title / csrf-token spliced into :content attributes must
   be attribute-escaped: the closing quote becomes &quot; and no raw <img
   tag survives the meta attribute."
  (let* ((payload "x\"><img src=x onerror=alert(1)>")
         (html (html-page :title "T"
                          :description payload
                          :og-title payload
                          :csrf-token payload
                          :include-htmx t)))
    (is (null (search "<img src=x" html))
        "no raw <img must survive a meta :content attribute")
    (is (search "&quot;&gt;&lt;img" html)
        "the quote-then-tag breakout must be entity-escaped in the attribute")))

(test regression-html-page-url-attribute-breakout
  "A URL field whose scheme passes the allow-list but whose path carries
   attribute-breaking characters must be escaped: passing the scheme check
   is not the same as being safe to splice into an href."
  (let ((html (html-page :title "T"
                         :canonical "https://evil/\"><script>alert(1)</script>")))
    ;; evil/ prefix disambiguates the attacker breakout from the legitimate
    ;; <body class=""><script> runtime sequence elsewhere in the page.
    (is (null (search "evil/\"><script>" html))
        "raw quote-then-script must not survive the canonical href")
    (is (search "&quot;&gt;&lt;script&gt;" html)
        "url breakout chars must be entity-escaped in the href")))

(test regression-html-page-or-fallback-title-escaped
  "When og-title is nil, og:title falls back to title; that fallback is
   spliced into an attribute and must be escaped just like an explicit
   og-title."
  (let* ((payload "T\"><img src=x onerror=alert(1)>")
         (html (html-page :title payload
                          :og-image "https://example.com/card.png")))
    (is (null (search "<img src=x" html))
        "no raw <img must survive the og:title fallback attribute")
    (is (search "og:title" html)
        "the og:title fallback must still be emitted")))

;;; ============================================================================
;;; highlight-sexp — content escape
;;; ============================================================================

(test regression-highlight-sexp-escapes-html-in-string-content
  "A form whose string slot contains <script> must not round-trip through
   highlight-sexp as raw HTML. With escape-html running first, < and > are
   inert entities and the rendered span body cannot inject markup."
  (let ((out (highlight-sexp '("<script>alert(1)</script>"))))
    (is (null (search "<script>" out))
        "raw <script> tag must not appear in output")
    (is (search "&lt;script&gt;" out)
        "tag chars must be HTML-escaped to &lt;/&gt;")
    (is (search "alert(" out)
        "string content itself is preserved (digits get wrapped in number spans)")))

(test regression-highlight-sexp-escapes-ampersand
  "Bare `&` in a string is escaped to `&amp;` before regex passes run, so
   no unintentional entity-like sequences leak through."
  (let ((out (highlight-sexp '("a&b"))))
    (is (search "a&amp;b" out))
    (is (null (search "a&b" out)))))

(test regression-highlight-sexp-still-tags-keywords-and-numbers
  "Keyword and number highlighting still works after the escape pass."
  (let ((out (highlight-sexp '(:k 42))))
    (is (search "<span class=\"sexp-keyword\">:K</span>" out))
    (is (search "<span class=\"sexp-number\">42</span>" out))))

(test regression-highlight-sexp-truncates-at-max-length
  "A form whose printed representation exceeds *highlight-sexp-max-length*
   is truncated to the cap before escaping, so the output's textual content
   stays bounded — a hostile or runaway form cannot drive unbounded markup."
  (let* ((lol-web/html::*highlight-sexp-max-length* 32)
         (long (make-string 500 :initial-element #\a))
         (out (highlight-sexp long)))
    (is (< (length out) (* 6 32))
        "output length ~D must be bounded near the 32-char cap" (length out))
    (is (null (search (make-string 64 :initial-element #\a) out))
        "no run longer than the cap should survive truncation")))

(test regression-safe-fmt-rejects-dynamic-control-string
  "safe-fmt only accepts literal control strings."
  (signals error
    (macroexpand-1 '(safe-fmt control "~A"))))

;;; ============================================================================
;;; html-attrs — attribute-name validation
;;; ============================================================================

(test regression-html-attrs-rejects-unsafe-name-runtime
  "Dynamic attribute name with whitespace/quote/= must signal UNSAFE-ATTRIBUTE-NAME"
  (let ((injected " onclick=alert(1) x"))
    (signals unsafe-attribute-name (html-attrs injected "v"))))

(test regression-html-attrs-rejects-unsafe-name-leading-digit
  "Attribute name must start with an ASCII letter — leading digit rejected.
   Routed through FUNCALL so the compiler-macro doesn't fire at the
   source-literal call site; the test exercises the runtime gate."
  (signals unsafe-attribute-name (funcall #'html-attrs "1bad" "v")))

(test regression-html-attrs-accepts-data-aria-and-xml-ns
  "Common safe attribute shapes pass: data-*, aria-*, xml:lang.
   Keyword names print uppercase via ~A — matches pre-existing html-attrs
   behaviour; HTML attribute matching is case-insensitive."
  (is (search "DATA-COMPONENT-ID=\"x\"" (html-attrs :data-component-id "x")))
  (is (search "ARIA-LEVEL=\"2\"" (html-attrs :aria-level "2")))
  (is (search "xml:lang=\"en\"" (html-attrs "xml:lang" "en"))))

(test regression-safe-attribute-name-p-predicate
  "Predicate accepts letter-start + alnum/-/_/:, rejects whitespace/quote/="
  (is (safe-attribute-name-p "data-test-1"))
  (is (safe-attribute-name-p :aria-label))
  (is (safe-attribute-name-p "xml:lang"))
  (is (not (safe-attribute-name-p "")))
  (is (not (safe-attribute-name-p "1leading-digit")))
  (is (not (safe-attribute-name-p " has-space")))
  (is (not (safe-attribute-name-p "has\"quote")))
  (is (not (safe-attribute-name-p "has=equals")))
  (is (not (safe-attribute-name-p "onclick")))
  (is (not (safe-attribute-name-p "onerror")))
  (is (not (safe-attribute-name-p :onload))))

(test regression-html-attrs-rejects-event-handler-names
  "Dynamic event-handler attribute names signal; escaping the value cannot
   make inline JavaScript safe."
  (signals unsafe-attribute-name (funcall #'html-attrs "onclick" "alert(1)"))
  (signals unsafe-attribute-name (funcall #'html-attrs :onerror "alert(1)")))

(test regression-safe-attribute-name-p-rejects-hx-on
  "safe-attribute-name-p must reject htmx inline-handler attribute NAMES
   (hx-on:click / hx-on-click, either case, string or symbol) — the client
   runtime lifts any hx-on* attribute to a handler, so escaping the value
   cannot make it safe, exactly as for classic on* names. Non-handler hx-*
   attributes (hx-get, hx-target) stay valid."
  (is (not (safe-attribute-name-p "hx-on:click")))
  (is (not (safe-attribute-name-p "hx-on-click")))
  (is (not (safe-attribute-name-p "hx-on::after-request")))
  (is (not (safe-attribute-name-p "HX-ON:click")))
  (is (not (safe-attribute-name-p :hx-on-click)))
  (is (safe-attribute-name-p "hx-get"))
  (is (safe-attribute-name-p "hx-target"))
  (signals unsafe-attribute-name (funcall #'html-attrs "hx-on:click" "alert(1)"))
  (signals unsafe-attribute-name (funcall #'html-attrs :hx-on-click "alert(1)")))

;;; ============================================================================
;;; html-page — text-field escape
;;; ============================================================================

(test regression-html-page-title-escapes
  "Attacker-controlled title is escape-html-ed in the <title> element"
  (let ((html (html-page :title "<script>alert(1)</script>")))
    (is (null (search "<title><script>alert(1)</script></title>" html))
        "raw <script> in title must not appear verbatim")
    (is (search "<title>&lt;script&gt;alert(1)&lt;/script&gt;</title>" html)
        "title must be escape-html-ed to entity form")))

(test regression-html-page-url-fields-reject-unsafe-schemes
  "URL-shaped html-page kwargs accept relative/http/https only."
  (signals error
    (html-page :canonical "javascript:alert(1)" :include-htmx nil))
  (signals error
    (html-page :og-url "data:text/html,pwn" :include-htmx nil))
  (signals error
    (html-page :og-image "javascript:alert(1)" :include-htmx nil))
  (signals error
    (html-page :css-href "data:text/css,body{}" :include-htmx nil))
  (let ((html (html-page :canonical "/docs"
                         :og-image "https://cdn.example.com/card.png"
                         :css-href "/static/app.css"
                         :include-htmx nil)))
    (is (search "href=\"/docs\"" html))
    (is (search "content=\"https://cdn.example.com/card.png\"" html))
    (is (search "href=\"/static/app.css\"" html))))

(test regression-html-page-lang-and-body-class-escape-once
  "lang and body-class are escaped by cl-who's attribute context once."
  (let ((html (html-page :title "Probe"
                         :lang "en&x"
                         :body-class "a&b"
                         :include-htmx nil)))
    (is (search "lang=\"en&amp;x\"" html))
    (is (search "class=\"a&amp;b\"" html))
    (is (null (search "en&amp;amp;x" html)))
    (is (null (search "a&amp;amp;b" html)))))

;;; ============================================================================
;;; safe-html-string — bypass contract
;;; ============================================================================

(test regression-safe-html-string-bypasses-escape
  "Wrapping a payload in MAKE-SAFE-HTML-STRING bypasses coerce-html-emit escape"
  (let ((html (html-page
                :title "Probe"
                :body (make-safe-html-string "<article>hello</article>"))))
    (is (search "<article>hello</article>" html)
        "safe-html-string body must be emitted verbatim")))

(test regression-make-safe-html-string-idempotent
  "MAKE-SAFE-HTML-STRING on an already-tagged value returns it unchanged"
  (let* ((once (make-safe-html-string "raw"))
         (twice (make-safe-html-string once)))
    (is (eq once twice)
        "idempotent: re-tagging must return the existing tagged value")))

(test regression-coerce-html-emit-routes-by-type
  "coerce-html-emit: SAFE-HTML-STRING → verbatim; string → escape; nil → empty"
  (is (string= (coerce-html-emit (make-safe-html-string "<x>")) "<x>"))
  (is (string= (coerce-html-emit "<x>") "&lt;x&gt;"))
  (is (string= (coerce-html-emit nil) "")))

;;; ============================================================================
;;; html-page Tailwind CDN — subresource integrity
;;; ============================================================================

(test regression-html-page-tailwind-cdn-has-sri
  "html-page pins the Tailwind Play CDN to an explicit version and emits
   the integrity= + crossorigin= attribute pair. Without SRI, a CDN
   compromise serves arbitrary JavaScript to every page load."
  (let ((html (html-page :title "SRI Probe" :include-tailwind t
                         :include-htmx nil)))
    (is (search "cdn.tailwindcss.com/" html)
        "Tailwind URL must include an explicit version segment")
    (is (search "integrity=\"sha384-" html)
        "integrity= attribute with sha384- prefix must be present")
    (is (search "crossorigin=\"anonymous\"" html)
        "crossorigin=\"anonymous\" is required for SRI to apply to a cross-origin script")))

(test regression-html-page-tailwind-cdn-suppressed-by-css-href
  "Supplying CSS-HREF suppresses the CDN tag entirely; integrity= must
   not appear in the head when the consumer ships compiled CSS."
  (let ((html (html-page :title "Compiled" :css-href "/static/app.css"
                         :include-htmx nil)))
    (is (null (search "cdn.tailwindcss.com" html)))
    (is (null (search "integrity=\"sha384-" html)))))

(test regression-component-html-id-escaped
  "component->html routes the component id through safe-attr at both the
   id and data-component-id sinks, so a breakout id is attribute-escaped
   rather than closing the wrapper div."
  (let ((*component-render-hook* nil)
        (component (lambda (msg)
                     (ecase msg
                       (:id "x\"><script>alert(1)</script>")
                       (:render "<p>body</p>")))))
    (let ((out (component->html component)))
      (is (search "&quot;" out))
      (is (search "&lt;script" out))
      (is (null (search "\"><script" out)))
      (is (search "<p>body</p>" out)))))

;;; ============================================================================
;;; highlight-sexp — cyclic / deep forms print under a bounded printer
;;; ============================================================================

(test regression-highlight-sexp-cycle-safe
  "highlight-sexp prints FORM under *print-circle* plus *print-level* /
   *print-length* caps, so a circular or pathologically deep form terminates
   with a bounded string instead of driving prin1-to-string unbounded before
   the length cap can apply."
  (let* ((circular (let ((x (list 1 2 3)))
                     (setf (cdr (last x)) x)
                     x))
         (out (highlight-sexp circular)))
    (is (stringp out)
        "a circular form must print to a string, not loop forever")
    (is (<= (length out) (* 6 lol-web/html::*highlight-sexp-max-length*))
        "circular-form output ~D must stay bounded" (length out)))
  (let* ((deep (let ((v 0))
                 (dotimes (_ 5000) (setf v (list v)))
                 v))
         (out (highlight-sexp deep)))
    (is (stringp out)
        "a deeply nested form must print to a string, not overflow the stack")
    (is (<= (length out) (* 6 lol-web/html::*highlight-sexp-max-length*))
        "deep-form output ~D must stay bounded" (length out))))
