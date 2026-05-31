;;;; Tests for the zero-inline CSP conformance analyzer
;;;; (src/html/csp-conformance.lisp).
;;;;
;;;; The analyzer encodes the contract Phase 1's external-asset emit path was
;;;; built to satisfy: a strict no-'unsafe-inline' CSP refuses inline <style>
;;;; and executable inline <script>, but allows non-executable data blocks
;;;; (application/json, application/ld+json, text/tikz) and same-origin
;;;; external <link>/<script src>. These tests pin it green on real
;;;; lol-web:page output + embed-json-data blocks, and red on inlined markup
;;;; and a disallowed external origin — so the migration phases (3–6) can
;;;; reuse lol-web/html:csp-inline-violations to assert each site renders
;;;; CSP-clean.

(in-package :lol-web/server/test)
(in-suite :lol-web/server/test)

(defun %violations (html &rest kw)
  (apply #'lol-web/html:csp-inline-violations html kw))

;;; ============================================================================
;;; GREEN — conformant real output
;;; ============================================================================

(test csp-invariant-passes-on-page-output
  "lol-web:page externalises every constant asset, so its rendered HTML has
   zero inline <style>/executable <script> and only same-origin links."
  (clear-asset-registry)
  (let ((html (page :base-css (lol-web/html:make-safe-html-string ":root{--a:1}")
                    :component-css (lol-web/html:make-safe-html-string ".c{color:red}")
                    :body (lol-web/html:make-safe-html-string "<main>hi</main>")
                    :include-tailwind nil :include-htmx t)))
    (is (null (%violations html)) "page output is CSP-clean")
    (is (search "<link" html) "page still emits an external stylesheet link")
    (is (search "<script src=" html) "page still emits an external runtime script")
    (is (null (search "<style" html)) "page emits no inline <style>")))

(test csp-invariant-passes-on-embed-json-data
  "embed-json-data emits a non-executable application/json data block, which
   is not script-src-gated — allowed. ld+json and tikz data blocks too."
  (is (null (%violations (lol-web/html:safe-html-string-value (embed-json-data "tg" #(1 2 3))))))
  (is (null (%violations "<script type=\"application/ld+json\">{}</script>")))
  (is (null (%violations "<script type=\"text/tikz\">\\node{};</script>"))))

(test csp-invariant-allows-same-origin-external-assets
  "Relative <link rel=stylesheet>/<script src> are same-origin → allowed; a
   non-stylesheet absolute <link> (canonical/og) is never gated → ignored."
  (is (null (%violations "<link rel=\"stylesheet\" href=\"/_lol/a/x.css\">")))
  (is (null (%violations "<script src=\"/_lol/a/r.js\"></script>")))
  (is (null (%violations "<link rel=\"canonical\" href=\"https://kleisli.io/\">"))
      "an absolute canonical link is not a stylesheet and must not be flagged"))

;;; ============================================================================
;;; RED — the constructs a strict CSP refuses, each named in the message
;;; ============================================================================

(test csp-invariant-flags-inline-style
  "Any inline <style> is a violation whose message names the tag."
  (let ((v (%violations "<head><style>body{margin:0}</style></head>")))
    (is (= 1 (length v)))
    (is (search "<style" (first v)) "message names the offending <style> tag")
    (is (search "style-src" (first v)))))

(test csp-invariant-flags-executable-inline-script
  "Inline <script> with no type, text/javascript, or module is executable →
   gated by script-src → forbidden."
  (dolist (frag '("<script>alert(1)</script>"
                  "<script type=\"text/javascript\">x()</script>"
                  "<script type=\"application/javascript\">x()</script>"
                  "<script type=\"module\">import 'x'</script>"))
    (let ((v (%violations frag)))
      (is (= 1 (length v)) "exactly one violation for ~A" frag)
      (is (search "<script" (first v)) "message names the offending <script> tag")
      (is (search "script-src" (first v))))))

(test csp-invariant-flags-unrecognised-inline-script-type
  "An inline <script> whose type is neither executable nor a known data
   block is flagged by default, but DATA-SCRIPT-TYPES can admit it."
  (let ((v (%violations "<script type=\"text/template\">x</script>")))
    (is (= 1 (length v)))
    (is (search "text/template" (first v)) "message names the unrecognised type"))
  (is (null (%violations "<script type=\"text/template\">x</script>"
                         :data-script-types '("text/template")))
      "an explicitly admitted data type passes"))

(test csp-invariant-flags-disallowed-external-origin
  "A cross-origin <script src>/<link rel=stylesheet> is forbidden unless its
   origin prefix is in ALLOWED-ORIGINS; same-origin always passes."
  (is (%violations "<script src=\"https://evil.example.com/x.js\"></script>")
      "cross-origin script is flagged")
  (is (%violations "<script src=\"//cdn.example.com/x.js\"></script>")
      "protocol-relative script is cross-origin and flagged")
  (is (%violations "<link rel=\"stylesheet\" href=\"https://fonts.example.com/c.css\">")
      "cross-origin stylesheet is flagged")
  (is (null (%violations "<script src=\"https://evil.example.com/x.js\"></script>"
                         :allowed-origins '("https://evil.example.com")))
      "an allow-listed origin passes")
  (is (null (%violations "<link rel=\"stylesheet\" href=\"https://fonts.example.com/c.css\">"
                         :allowed-origins '("https://fonts.example.com")))
      "an allow-listed stylesheet origin passes"))

;;; ============================================================================
;;; RED — the inline ATTRIBUTE surface a strict CSP also refuses
;;; ============================================================================

(test csp-invariant-flags-inline-style-attribute
  "style-src 'self' gates inline style= attributes on ANY element, not only
   <style> elements. A non-empty style= attribute is a violation naming the
   tag and style-src."
  (dolist (frag '("<div style=\"top:10%;left:20%\">x</div>"
                  "<span class=\"c\" style=\"color:red\">y</span>"
                  "<line x1=\"0\" y1=\"0\" style=\"stroke:red\"/>"))
    (let ((v (%violations frag)))
      (is (= 1 (length v)) "exactly one violation for ~A" frag)
      (is (search "style=" (first v)) "message names the inline style= attribute")
      (is (search "style-src" (first v))))))

(test csp-invariant-flags-inline-event-handler
  "script-src 'self' gates inline on*= event-handler attributes; each is a
   violation naming the handler and script-src."
  (dolist (frag '("<button onclick=\"f()\">go</button>"
                  "<body onload=\"init()\"></body>"
                  "<img src=\"/a.png\" onerror=\"boom()\">"))
    (let ((v (%violations frag)))
      (is (= 1 (length v)) "exactly one violation for ~A" frag)
      (is (search "event-handler" (first v)) "message names it an event handler")
      (is (search "script-src" (first v))))))

(test csp-invariant-flags-javascript-url
  "A javascript: URL in href/src is script-src-gated and forbidden on any
   element; the scheme match is case-insensitive."
  (dolist (frag '("<a href=\"javascript:void(0)\">x</a>"
                  "<a href=\"JavaScript:alert(1)\">x</a>"
                  "<iframe src=\"javascript:doIt()\"></iframe>"))
    (let ((v (%violations frag)))
      (is (= 1 (length v)) "exactly one violation for ~A" frag)
      (is (search "javascript:" (first v)) "message names the javascript: URL")
      (is (search "script-src" (first v))))))

(test csp-invariant-allows-presentation-attrs-and-class-positioning
  "The attribute checks must NOT over-match: SVG presentation attributes
   (x1/y1/cx/fill) are not style= and render fine; the externalised remediation
   pattern (positioning via a same-origin CSS class) is clean; an empty style=
   applies nothing; a path that merely contains \"javascript\" is not a scheme."
  (is (null (%violations "<line x1=\"0\" y1=\"0\" x2=\"10\" y2=\"10\" stroke=\"red\"/>"))
      "SVG presentation attributes are not style= attributes")
  (is (null (%violations "<circle cx=\"5\" cy=\"5\" r=\"2\" fill=\"red\"/>"))
      "SVG fill/cx/cy are not style= attributes")
  (is (null (%violations "<div class=\"nv-pos-3\"><span class=\"nv-lpos-3\">T</span></div>"))
      "class-based positioning (the remediation pattern) is CSP-clean")
  (is (null (%violations "<div style=\"\">x</div>"))
      "an empty style= attribute applies nothing and is not flagged")
  (is (null (%violations "<a href=\"/javascript-guide\">x</a>"))
      "a path containing the text javascript is not the javascript: scheme"))

;;; ============================================================================
;;; REGRESSION — the analyzer catches the bug this task fixes
;;; ============================================================================

(test csp-invariant-catches-pre-fix-inlining
  "The pre-fix render path — a bare html-page (inline reactive runtime) and
   the inline base-css escape hatch — is exactly what a strict CSP refuses;
   the analyzer must flag it, proving the invariant has teeth."
  (is (%violations (lol-web/html:html-page :include-tailwind nil :include-htmx nil))
      "bare html-page inlines an executable reactive runtime → flagged")
  (let ((v (%violations (lol-web/html:html-page
                         :base-css (lol-web/html:make-safe-html-string ":root{--x:1}")
                         :include-tailwind nil :include-htmx nil))))
    (is (some (lambda (s) (search "<style" s)) v) "inline base-css <style> flagged")
    (is (some (lambda (s) (search "<script" s)) v) "inline reactive <script> flagged")))

(test csp-invariant-catches-inline-positioned-node
  "The deploy-caught gap: a network-viz node positioned via style=\"top:%;left:%\"
   passed the element-only analyzer (NIL), built and deployed green, then
   collapsed to the origin in-browser because style-src dropped every style
   attribute. The hardened analyzer must flag the node markup and pass the
   externalised class-based remediation."
  (let ((v (%violations
            "<div class=\"network-node\" style=\"top:21%;left:64%\"></div>")))
    (is (= 1 (length v)) "the inline-positioned node is flagged")
    (is (search "style-src" (first v))))
  (is (null (%violations "<div class=\"network-node nv-pos-2\"></div>"))
      "the externalised positioning (class only) is CSP-clean"))
