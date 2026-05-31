;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTML; Base: 10 -*-
;;;; Zero-inline CSP conformance analyzer.
;;;;
;;;; CSP-INLINE-VIOLATIONS scans a rendered HTML string and returns the list
;;;; of constructs a strict no-'unsafe-inline' Content-Security-Policy would
;;;; refuse, each described by a message naming the offending tag:
;;;;
;;;;   - ANY inline <style> element           (always gated by style-src);
;;;;   - an EXECUTABLE inline <script> — no type, or text/javascript,
;;;;     application/javascript, ecmascript, module (gated by script-src);
;;;;   - an inline <script> whose type is not a recognised data block;
;;;;   - a <link rel=stylesheet> or <script src> whose URL is neither
;;;;     same-origin (relative) nor in ALLOWED-ORIGINS;
;;;;   - an inline style= ATTRIBUTE on ANY element (gated by style-src);
;;;;   - an inline on*= event-handler ATTRIBUTE on ANY element (script-src);
;;;;   - a javascript: URL in an href/src ATTRIBUTE (gated by script-src).
;;;;
;;;; The attribute checks make the model SOUND: style-src/script-src gate not
;;;; only <style>/<script> ELEMENTS but also the inline style=/on*= attribute
;;;; surface and javascript: URLs. A page clean of inline elements yet carrying
;;;; style="…" positioning still breaks under the strict CSP, so it must flag.
;;;; (JS-set styling via CSSOM — el.style.x — is NOT a CSP-gated HTML attribute
;;;; and is correctly never inspected, since it is text/string, not markup.)
;;;;
;;;; Non-executable DATA blocks (<script type="application/json" |
;;;; "application/ld+json" | "text/tikz">) are NOT script-src-gated and so
;;;; are allowed — they carry per-request data read by an external script.
;;;;
;;;; The analyzer is a pure function of the HTML string (no server/DOM
;;;; dependency), so every lol-web consumer site can assert the invariant on
;;;; its own rendered pages. It is the executable form of the contract that
;;;; html-page's external-asset emit path was built to satisfy. Same-origin
;;;; is decided structurally: a relative URL is same-origin; a leading "//"
;;;; or any scheme (http:, https:, data:, blob:, …) is cross-origin and must
;;;; be allow-listed. A non-stylesheet <link> (canonical, og, preconnect) is
;;;; never gated and so is never inspected.

(in-package :lol-web/html)

(defparameter +executable-script-types+
  '("" "text/javascript" "application/javascript"
    "text/ecmascript" "application/ecmascript" "module")
  "Inline <script> type values the browser executes (a missing type is also
   executable). Such a script is gated by script-src and forbidden inline
   under a strict CSP.")

(defparameter +default-data-script-types+
  '("application/json" "application/ld+json" "text/tikz")
  "Inline <script> type values that are NON-executable data blocks: not
   gated by script-src, so permitted inline. Extend per call via
   CSP-INLINE-VIOLATIONS's DATA-SCRIPT-TYPES.")

(defun %tag-attr (tag name)
  "Value of attribute NAME (case-insensitive) in the opening-tag string TAG,
   or NIL when absent. Whitespace/start is required before NAME so SRC does
   not match inside SRCSET or DATA-SRC. A boolean attribute yields \"\"."
  (cl-ppcre:register-groups-bind (dq sq bare)
      ((concatenate 'string "(?i)(?:^|\\s)" (cl-ppcre:quote-meta-chars name)
                    "\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
       tag)
    (or dq sq bare "")))

(defun %same-origin-url-p (url)
  "True when URL is a same-origin reference: a relative path. A leading //
   (protocol-relative) or any scheme (http:, data:, …) is cross-origin."
  (let ((u (string-trim '(#\Space #\Tab #\Newline #\Return) url)))
    (not (or (and (>= (length u) 2) (char= (char u 0) #\/) (char= (char u 1) #\/))
             (cl-ppcre:scan "^[a-zA-Z][a-zA-Z0-9+.-]*:" u)))))

(defun %origin-allowed-p (url allowed-origins)
  "True when URL is same-origin or begins with one of ALLOWED-ORIGINS."
  (let ((u (string-trim '(#\Space #\Tab #\Newline #\Return) url)))
    (or (%same-origin-url-p u)
        (some (lambda (o) (and (>= (length u) (length o))
                               (string-equal o u :end2 (length o))))
              allowed-origins))))

(defun %event-handler-attr (tag)
  "Name (lowercased) of the first inline on*= event-handler attribute in the
   opening-tag string TAG, or NIL. Any on-prefixed attribute with a value is
   an inline event handler, gated by script-src under a strict CSP. The
   leading whitespace/start guard keeps it from matching DATA-ON… or an on…
   substring inside another attribute name."
  (cl-ppcre:register-groups-bind (name)
      ("(?i)(?:^|\\s)(on[a-z]+)\\s*=" tag)
    (string-downcase name)))

(defun %javascript-url-p (url)
  "True when URL uses the javascript: scheme (case-insensitive, leading
   whitespace tolerated). Such a URL in an href/src attribute is script-src-
   gated and forbidden under a strict CSP; a path that merely contains the
   text \"javascript\" is not."
  (let ((u (string-left-trim '(#\Space #\Tab #\Newline #\Return) url)))
    (and (>= (length u) 11) (string-equal "javascript:" u :end2 11))))

(defun %tag-snippet (tag)
  "Whitespace-trimmed TAG, truncated, for a violation message."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) tag)))
    (if (> (length s) 100) (concatenate 'string (subseq s 0 100) "…") s)))

(defun csp-inline-violations (html &key allowed-origins
                                        (data-script-types +default-data-script-types+))
  "Return the list of strict-CSP (no 'unsafe-inline') violations in the
   rendered HTML string HTML, or NIL when it conforms. Each element is a
   human-readable string naming the offending tag.

   Forbidden: every inline <style>; every executable inline <script> (no
   type or a JS/module type); an inline <script> whose type is not in
   DATA-SCRIPT-TYPES; a <link rel=stylesheet>/<script src> whose URL is
   cross-origin and not prefixed by some string in ALLOWED-ORIGINS (a
   relative URL is same-origin and always allowed); a non-empty inline
   style= attribute on ANY element; an inline on*= event-handler attribute
   on ANY element; and a javascript: URL in an href/src attribute.
   ALLOWED-ORIGINS holds origin prefixes still served off-site under the CSP
   (e.g. a font host)."
  (let ((violations '()))
    (flet ((push-v (fmt &rest args) (push (apply #'format nil fmt args) violations)))
      (cl-ppcre:do-matches-as-strings (tag "(?i)<style\\b[^>]*>" html)
        (push-v "inline <style> forbidden (gated by style-src): ~A" (%tag-snippet tag)))
      (cl-ppcre:do-matches-as-strings (tag "(?i)<script\\b[^>]*>" html)
        (let ((src (%tag-attr tag "src")))
          (if src
              (unless (%origin-allowed-p src allowed-origins)
                (push-v "cross-origin <script src> not same-origin and not allow-listed: ~A"
                        (%tag-snippet tag)))
              (let ((type (let ((tp (%tag-attr tag "type")))
                            (and tp (string-downcase (string-trim '(#\Space #\Tab) tp))))))
                (cond
                  ((or (null type) (member type +executable-script-types+ :test #'string=))
                   (push-v "executable inline <script> forbidden (gated by script-src): ~A"
                           (%tag-snippet tag)))
                  ((member type data-script-types :test #'string-equal) nil)
                  (t
                   (push-v "inline <script> with unrecognised type ~S (not a known data block): ~A"
                           type (%tag-snippet tag))))))))
      (cl-ppcre:do-matches-as-strings (tag "(?i)<link\\b[^>]*>" html)
        (let ((rel (%tag-attr tag "rel"))
              (href (%tag-attr tag "href")))
          (when (and rel href (search "stylesheet" (string-downcase rel)))
            (unless (%origin-allowed-p href allowed-origins)
              (push-v "cross-origin <link rel=stylesheet> not same-origin and not allow-listed: ~A"
                      (%tag-snippet tag))))))
      ;; Inline attribute surface, gated on EVERY opening tag: style= (style-src),
      ;; on*= handlers and javascript: URLs in href/src (script-src).
      (cl-ppcre:do-matches-as-strings (tag "(?i)<[a-z][a-z0-9]*\\b[^>]*>" html)
        (let ((style (%tag-attr tag "style")))
          (when (and style (plusp (length style)))
            (push-v "inline style= attribute forbidden (gated by style-src): ~A"
                    (%tag-snippet tag))))
        (let ((handler (%event-handler-attr tag)))
          (when handler
            (push-v "inline ~A= event-handler attribute forbidden (gated by script-src): ~A"
                    handler (%tag-snippet tag))))
        (when (or (let ((h (%tag-attr tag "href"))) (and h (%javascript-url-p h)))
                  (let ((s (%tag-attr tag "src")))  (and s (%javascript-url-p s))))
          (push-v "javascript: URL in href/src forbidden (gated by script-src): ~A"
                  (%tag-snippet tag)))))
    (nreverse violations)))
