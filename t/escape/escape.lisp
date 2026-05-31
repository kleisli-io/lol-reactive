(in-package :lol-web/escape/test)
(in-suite :lol-web/escape/test)

;;; ============================================================================
;;; escape-html
;;; ============================================================================

(test escape-html-escapes-five-special-chars
  "escape-html escapes the five HTML/XML metacharacters: & < > \" '"
  (is (string= "&lt;script&gt;alert(1)&lt;/script&gt;"
               (escape-html "<script>alert(1)</script>")))
  (is (string= "Tom &amp; Jerry" (escape-html "Tom & Jerry")))
  (is (string= "&quot;hi&quot;" (escape-html "\"hi\"")))
  (is (string= "&#39;hi&#39;"   (escape-html "'hi'"))))

(test escape-html-noop-on-safe-input
  (is (string= "hello world" (escape-html "hello world")))
  (is (string= "" (escape-html ""))))

(test escape-html-nil-passthrough
  (is (null (escape-html nil))))

;;; ============================================================================
;;; escape-attribute
;;; ============================================================================

(test escape-attribute-escapes-quote-pair
  (let ((out (escape-attribute "a\"b'c")))
    (is (search "&quot;" out))
    (is (search "&#39;" out))
    (is (null (search "\"" out)))
    (is (null (find #\' out)))))

;;; ============================================================================
;;; safe-url — reject-list guard
;;; ============================================================================

(test safe-url-allows-safe-schemes
  (is (string= "https://example.com/path?q=1"
               (safe-url "https://example.com/path?q=1")))
  (is (string= "/relative/path" (safe-url "/relative/path")))
  (is (string= "#anchor"        (safe-url "#anchor")))
  (is (string= "mailto:a@b"     (safe-url "mailto:a@b"))))

(test safe-url-blocks-script-bearing-schemes
  (is (null (safe-url "javascript:alert(1)")))
  (is (null (safe-url "JaVaScRiPt:alert(1)")))
  (is (null (safe-url "data:text/html,<script>")))
  (is (null (safe-url "vbscript:msgbox 1"))))

(test safe-url-nil-passthrough
  (is (null (safe-url nil))))

(test safe-url-strips-leading-c0-controls
  "An HTML5 URL parser strips C0 controls and ASCII whitespace before scheme
   resolution. Detection must do the same, otherwise `\\tjavascript:` slips
   through the regex and the browser still navigates to javascript:."
  (is (null (safe-url (format nil "~Cjavascript:alert(1)" #\Tab))))
  (is (null (safe-url (format nil "~Cjavascript:alert(1)" #\Newline))))
  (is (null (safe-url (format nil "~Cjavascript:alert(1)" #\Return))))
  (is (null (safe-url (format nil "~Cjavascript:alert(1)" #\Page))))
  (is (null (safe-url (format nil "~Cjavascript:alert(1)" (code-char #o0)))))
  (is (null (safe-url (format nil "~Cjavascript:alert(1)" (code-char #o1))))))

(test safe-url-strips-embedded-c0-controls
  "Embedded C0 controls inside the scheme (the HTML5-parser bypass): the
   browser parses `jav\\tascript:` as `javascript:` after attribute decoding.
   The verifier must collapse the same set before scheme matching."
  (is (null (safe-url (format nil "jav~Cascript:alert(1)" #\Tab))))
  (is (null (safe-url (format nil "java~Cscript:alert(1)" #\Newline))))
  (is (null (safe-url (format nil "j~Ca~Cv~Ca~Cscript:alert(1)"
                                #\Tab #\Newline #\Return #\Page))))
  (is (null (safe-url (format nil "data~C:text/html,<script>" (code-char 0))))))

(test safe-url-keeps-safe-after-control-strip
  "Stripping controls must not turn a safe URL into NIL — control-free
   safe schemes still pass through."
  (let ((url "https://example.com/p"))
    (is (string= url (safe-url url)))))

;;; ============================================================================
;;; safe-url-allowlist — allow-list guard
;;; ============================================================================

(test safe-url-allowlist-default-schemes
  "Default allowlist accepts http, https, mailto, tel; refuses anything else."
  (is (string= "https://example.com" (safe-url-allowlist "https://example.com")))
  (is (string= "http://example.com"  (safe-url-allowlist "http://example.com")))
  (is (string= "mailto:a@b"          (safe-url-allowlist "mailto:a@b")))
  (is (string= "tel:+15551234567"    (safe-url-allowlist "tel:+15551234567"))))

(test safe-url-allowlist-rejects-non-allowlisted-schemes
  "Schemes outside the allowlist (ftp, file, javascript, data, vbscript, custom)
   all return NIL regardless of payload."
  (is (null (safe-url-allowlist "ftp://example.com")))
  (is (null (safe-url-allowlist "file:///etc/passwd")))
  (is (null (safe-url-allowlist "javascript:alert(1)")))
  (is (null (safe-url-allowlist "data:text/html,<script>")))
  (is (null (safe-url-allowlist "vbscript:msgbox 1")))
  (is (null (safe-url-allowlist "custom-app:foo"))))

(test safe-url-allowlist-accepts-relative-and-fragment
  "Relative paths and fragment-only URLs have no scheme; allowlist passes them."
  (is (string= "/path"     (safe-url-allowlist "/path")))
  (is (string= "#anchor"   (safe-url-allowlist "#anchor")))
  (is (string= "page.html" (safe-url-allowlist "page.html"))))

(test safe-url-allowlist-custom-schemes
  "Caller-supplied allowlist replaces the default; only listed schemes pass."
  (is (string= "ftp://example.com"
               (safe-url-allowlist "ftp://example.com" :allowed-schemes '("ftp"))))
  (is (null (safe-url-allowlist "https://example.com" :allowed-schemes '("ftp"))))
  (is (null (safe-url-allowlist "mailto:a@b" :allowed-schemes '("ftp")))))

(test safe-url-allowlist-strips-controls-before-scheme
  "Allowlist matches the post-strip scheme, not the raw input."
  (is (null (safe-url-allowlist
             (format nil "~Cjavascript:alert(1)" #\Tab))))
  (is (null (safe-url-allowlist
             (format nil "jav~Cascript:alert(1)" #\Tab))))
  (is (string= (format nil "~Chttps://example.com" #\Tab)
               (safe-url-allowlist
                (format nil "~Chttps://example.com" #\Tab)))))

(test safe-url-allowlist-case-insensitive-scheme
  (is (string= "HTTPS://example.com" (safe-url-allowlist "HTTPS://example.com")))
  (is (string= "MaIlTo:a@b"          (safe-url-allowlist "MaIlTo:a@b"))))

(test safe-url-allowlist-nil-passthrough
  (is (null (safe-url-allowlist nil))))

(test regression-safe-url-is-verdict-not-escape-for-emit
  "A-L01: safe-url / safe-url-allowlist are VERDICTS — a scheme-safe URL is
   returned verbatim, NOT attribute-escaped. A survivor can still carry
   attribute-breaking characters, so splicing the return value straight into
   an href is an XSS footgun; safe-href is the escape-for-emit path."
  (let ((breakout "https://ok/\"><script>alert(1)</script>"))
    ;; verdict: scheme is safe, URL returned raw (un-escaped)
    (is (string= breakout (safe-url breakout)))
    (is (string= breakout (safe-url-allowlist breakout)))
    (is (search "\"><script>" (safe-url breakout))
        "verdict returns attribute-breaking chars verbatim — the footgun")
    ;; emit path: scheme allow-list THEN escape-attribute
    (let ((emitted (safe-href breakout)))
      (is (null (search "\"><script>" emitted))
          "safe-href must not leave a raw quote-then-tag breakout")
      (is (search "&quot;&gt;&lt;script&gt;" emitted)
          "safe-href entity-escapes the attribute-breaking characters"))))

;;; ============================================================================
;;; safe-coerce-keyword — bounded keyword resolution
;;; ============================================================================

(test safe-coerce-keyword-returns-existing-keyword
  "When the upcased string names an already-interned keyword, the function
   returns that keyword. The keywords used here are interned by the
   :keyword references in this very TEST form, so the lookup succeeds."
  (is (eq :red    (safe-coerce-keyword "red")))
  (is (eq :red    (safe-coerce-keyword "RED")))
  (is (eq :red    (safe-coerce-keyword "Red")))
  (is (eq :green  (safe-coerce-keyword "green")))
  (is (eq :blue   (safe-coerce-keyword "blue"))))

(test safe-coerce-keyword-returns-nil-on-unknown
  "An input whose upcased form does not name any interned keyword returns
   NIL — the function never calls INTERN, so the pool does not grow."
  (let* ((nonce (format nil "never-interned-~D-~D"
                        (get-universal-time) (random 999999)))
         (baseline (length (apropos-list nonce :keyword)))
         (result (safe-coerce-keyword nonce)))
    (is (null result))
    (is (= baseline (length (apropos-list nonce :keyword)))
        "find-symbol must not intern; baseline ~D, after ~D"
        baseline (length (apropos-list nonce :keyword)))))

(test safe-coerce-keyword-nil-input
  (is (null (safe-coerce-keyword nil))))

(test safe-coerce-keyword-empty-string
  (is (null (safe-coerce-keyword ""))))

(test safe-coerce-keyword-non-string-input
  (is (null (safe-coerce-keyword 42)))
  (is (null (safe-coerce-keyword :red)))
  (is (null (safe-coerce-keyword '(:red)))))

(test safe-coerce-keyword-length-cap
  "Inputs longer than MAX-LENGTH return NIL without consulting the keyword
   package. Default cap is 64; caller may raise or lower it."
  (let ((big (make-string 65 :initial-element #\A)))
    (is (null (safe-coerce-keyword big)))
    (is (null (safe-coerce-keyword big :max-length 64)))
    ;; Lifted cap admits the lookup, but the keyword still doesn't exist —
    ;; result is NIL because find-symbol fails, not because of the cap.
    (is (null (safe-coerce-keyword big :max-length 100))))
  ;; A short, known keyword fits a low cap.
  (is (eq :red (safe-coerce-keyword "red" :max-length 3))))

(test regression-safe-coerce-keyword-caps-upcased-length
  "The post-case-conversion string is also bounded before FIND-SYMBOL."
  (is (null (safe-coerce-keyword "red" :max-length 2))))

(test safe-coerce-keyword-allowlist-accepts-member
  (is (eq :next     (safe-coerce-keyword "next"     :allowed '(:next :back :complete))))
  (is (eq :back     (safe-coerce-keyword "back"     :allowed '(:next :back :complete))))
  (is (eq :complete (safe-coerce-keyword "complete" :allowed '(:next :back :complete)))))

(test safe-coerce-keyword-allowlist-rejects-non-member
  "An interned keyword outside the allowlist still rejects. Tests both
   the case where the input names :red (interned by this file) but :red
   isn't in the allowed set."
  (is (null (safe-coerce-keyword "red" :allowed '(:next :back :complete))))
  (is (null (safe-coerce-keyword "GREEN" :allowed '(:red :blue)))))

(test safe-coerce-keyword-on-miss-signal
  "ON-MISS :SIGNAL turns a miss into an error condition instead of NIL,
   for extractor sites where missing/bad input is a 400, not a silent drop."
  (signals error
    (safe-coerce-keyword "definitely-not-a-known-keyword" :on-miss :signal))
  (signals error
    (safe-coerce-keyword "red" :allowed '(:next) :on-miss :signal))
  (signals error
    (safe-coerce-keyword "" :on-miss :signal)))

(test safe-coerce-keyword-bounds-keyword-pool
  "1000 distinct hostile inputs do not grow the keyword pool by even one
   entry — the find-symbol invariant."
  (let ((baseline (length (apropos-list "" :keyword))))
    (loop for i below 1000 do
          (safe-coerce-keyword (format nil "attacker-key-~D-~D-pad-~A" i
                                       (get-universal-time)
                                       "ABCDEFGHIJ")))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D"
          baseline after))))

(test safe-coerce-keyword-bounds-pool-under-allowlist
  "Same bound under :ALLOWED — the allowlist filter applies after the
   find-symbol lookup."
  (let ((baseline (length (apropos-list "" :keyword))))
    (loop for i below 1000 do
          (safe-coerce-keyword (format nil "attacker-key-~D-~D" i
                                       (get-universal-time))
                               :allowed '(:next :back :complete)))
    (let ((after (length (apropos-list "" :keyword))))
      (is (= baseline after)
          "keyword pool grew from ~D to ~D under allowlist"
          baseline after))))

;;; ============================================================================
;;; %scan-bounded
;;; ============================================================================

(test scan-bounded-returns-match-like-scan
  "For a normal pattern %scan-bounded behaves like cl-ppcre:scan — a match
   returns a non-NIL start index, a non-match returns NIL."
  (is (%scan-bounded "^abc" "abcdef"))
  (is (null (%scan-bounded "^abc" "xyz"))))

#+sbcl
(test scan-bounded-aborts-redos-pattern-within-budget
  "A catastrophic-backtracking pattern against an adversarial input aborts to
   NIL at the time budget instead of hanging — the ReDoS bound that protects
   the jschema pattern and form-dsl validators. Unbounded cl-ppcre:scan would
   backtrack for many seconds here."
  (let* ((*scan-match-timeout-seconds* 0.1)
         (adversarial (concatenate 'string
                                   (make-string 30 :initial-element #\a) "!"))
         (start (get-internal-real-time))
         (result (%scan-bounded "^(a+)+$" adversarial))
         (elapsed (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    (is (null result) "ReDoS scan must abort to NIL, got ~S" result)
    (is (< elapsed 2)
        "bounded scan must return well under the unbounded hang; took ~Fs"
        (float elapsed))))

;;; ============================================================================
;;; hx-on-attribute-name-p / sanitize-hx-on-attrs
;;; ============================================================================

(test hx-on-attribute-name-p-matches-canonical-shape
  "The predicate accepts every separator form the client runtime lifts and
   rejects names that only resemble the prefix."
  (is (hx-on-attribute-name-p "hx-on:click"))
  (is (hx-on-attribute-name-p "hx-on-click"))
  (is (hx-on-attribute-name-p "hx-on::after-request"))
  (is (hx-on-attribute-name-p "hx-on--after-swap"))
  (is (hx-on-attribute-name-p "HX-ON:click"))
  (is (null (hx-on-attribute-name-p "href")))
  (is (null (hx-on-attribute-name-p "hx-get")))
  (is (null (hx-on-attribute-name-p nil))))

(test sanitize-hx-on-strips-colon-form
  "The colon form hx-on:click — which the client runtime lifts to a native
   onclick — is stripped on the wire, closing the dash-only gap. The dash form
   stays stripped, and identifier text with no `=' is left intact."
  (is (not (search "hx-on" (sanitize-hx-on-attrs "<b hx-on:click=\"x\">"))))
  (is (not (search "hx-on" (sanitize-hx-on-attrs "<b hx-on::after-request='y()'>"))))
  (is (not (search "hx-on" (sanitize-hx-on-attrs "<b hx-on-click=\"x\">"))))
  (is (search "hx-on-click"
              (sanitize-hx-on-attrs "<p>see hx-on-click means a handler</p>"))))

(test regression-hx-on-whitespace-anchor-bypass-stripped
  "A handler abutting a closing quote with no leading whitespace is still a live
   attribute under HTML5 boundary recovery, so it is stripped; the benign
   attribute it abuts survives."
  (let ((out (sanitize-hx-on-attrs "<b id=\"x\"hx-on:click=\"alert(document.cookie)\">")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert" out)))
    (is (search "id=\"x\"" out))))

(test regression-hx-on-two-handler-bypass-stripped
  "Stripping the first of two adjacent handlers must not leave the second
   un-anchored — both are removed."
  (let ((out (sanitize-hx-on-attrs "<b hx-on:a=\"x\"hx-on:click=\"alert(1)\">")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert" out)))))

(test regression-hx-on-embedded-quote-bypass-stripped
  "A quoted value that terminated the old scanner early must not let a trailing
   handler survive: the parse-based walk strips by attribute name, not by a
   regex over the serialized value."
  (let ((out (sanitize-hx-on-attrs "<b hx-on:click=\"a\"hx-on:dblclick=\"alert(1)\">")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert" out)))))

(test regression-hx-on-dash-and-colon-forms-stripped
  "Both the dash form (hx-on-click) and the colon form (hx-on:click) are
   stripped from one tag, and a `>' inside a quoted handler value does not end
   the tag early — the trailing benign attribute survives."
  (let ((out (sanitize-hx-on-attrs "<div hx-on-click=\"a>b\" hx-on:keyup=\"c\" id=\"k\">")))
    (is (not (search "hx-on" out)))
    (is (search "id=\"k\"" out))))

(test regression-hx-on-abrupt-empty-comment-close-stripped
  "<!--> abruptly closes an empty comment in a browser, so the markup after it
   parses as live elements. The walker must close the comment there too and
   strip the trailing handler — not hunt for a `-->' that never comes and emit
   the remainder verbatim."
  (let ((out (sanitize-hx-on-attrs
              "<div>x<!--><span hx-on:click=\"alert(1)\">y</span></div>")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert" out)))))

(test regression-hx-on-abrupt-comment-close-stripped
  "<!---> abruptly closes the comment (comment-start-dash + `>'); the following
   handler is live and must be stripped."
  (let ((out (sanitize-hx-on-attrs
              "<div>x<!---><span hx-on:click=\"alert(1)\">y</span></div>")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert" out)))))

(test regression-hx-on-comment-end-bang-close-stripped
  "--!> is a comment-end-bang close in a browser; the walker must close there
   and strip the trailing handler."
  (let ((out (sanitize-hx-on-attrs
              "<div>x<!-- c --!><span hx-on:click=\"alert(1)\">y</span></div>")))
    (is (not (search "hx-on" out)))
    (is (not (search "alert" out)))))

(test regression-hx-on-unclosed-comment-to-eof
  "An unterminated <!-- runs to EOF as a comment in both the walker and a
   browser, so a handler after it stays inside the comment and is never lifted.
   The walker emits the tail verbatim — bytes in equal bytes out — so it
   neither closes the comment early nor exposes the trailing handler as live."
  (let ((in "chat<!--<span hx-on:click=\"alert(1)\">y</span>"))
    (is (string= in (sanitize-hx-on-attrs in)))))

(test regression-hx-on-wellformed-comment-control
  "Control: a well-formed <!-- ... --> comment is left intact — its content,
   even text resembling a handler, is not a live attribute and survives — while
   a real handler after the close is still stripped. Pins that the scanner does
   not over-scrub a well-formed comment."
  (let ((out (sanitize-hx-on-attrs
              "<!-- comment hx-on:keep --><span hx-on:click=\"alert(1)\">y</span>")))
    (is (search "<!-- comment hx-on:keep -->" out))
    (is (search "<span" out))
    (is (not (search "alert" out)))))

;;; ============================================================================
;;; safe-attr / safe-href — context-correct attribute emitters
;;; ============================================================================

(test regression-safe-attr-escapes-breakout
  "safe-attr forces escape-attribute at the sink so quotes and angle
   brackets in a runtime value cannot close the attribute. NIL passes
   through as NIL; non-strings are coerced before escaping."
  (let ((out (safe-attr "x\"><img src=x onerror=alert(1)>")))
    (is (search "&quot;" out))
    (is (search "&lt;img" out))
    (is (null (search "<img" out)))
    (is (null (search "\"><img" out))))
  (is (null (safe-attr nil)))
  (is (string= "42" (safe-attr 42))))

(test regression-safe-href-rejects-breakout-after-scheme
  "safe-href escapes attribute-breaking characters that survive the
   scheme allowlist, and returns NIL for a rejected scheme so the sink
   emits no attribute at all."
  (let ((out (safe-href "https://example.com/\"><script>")))
    (is (search "&quot;" out))
    (is (search "&lt;script" out))
    (is (null (search "\"><script" out))))
  (is (null (safe-href "javascript:alert(1)")))
  (is (string= "/api/users" (safe-href "/api/users"))))

;;; ============================================================================
;;; neutralize-script-close — <script>-body close-tag defuse
;;; ============================================================================

(test regression-neutralize-script-close-defuses-close-tag
  "A value carrying </script> can close a <script> element early even when
   JS-quoted, because the HTML tokenizer ends script data at the literal
   </ before the JS ever runs. neutralize-script-close breaks every </ so
   no live close tag survives; a clean value is returned verbatim."
  (let ((out (neutralize-script-close "</script><script>alert(1)</script>")))
    (is (null (search "</script" out))
        "no live </script close tag may survive")
    (is (search "<\\/script" out)
        "the close tag is defused to <\\/ which the tokenizer ignores"))
  (is (string= "abc123-def" (neutralize-script-close "abc123-def"))
      "a value with no </ is returned unchanged")
  (is (null (neutralize-script-close nil)))
  (is (string= "<\\/x" (neutralize-script-close "</x"))
      "lone </ is also defused"))
