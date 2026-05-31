;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTML; Base: 10 -*-
;;;; HTML page template using cl-who
;;;;
;;;; GENERIC INFRASTRUCTURE - NO hardcoded colors, fonts, or theme styles.
;;;; Apps provide their own visual identity by setting *colors*, *typography*, etc.
;;;;
;;;; Shared macros (htm, htm-str), component rendering (render-component,
;;;; component->html), highlight-sexp, and cl-who config live in html/elements.lisp.
;;;; This file provides only the full-page template and client-side runtime.

(in-package :lol-web/html)

;;; ============================================================================
;;; PAGE TEMPLATE (GENERIC)
;;;
;;; Provides infrastructure only. Apps define aesthetics via:
;;; - Setting *colors*, *typography*, etc. before rendering
;;; - Registering CSS modules via defcss
;;; - Passing custom head-extra content
;;; ============================================================================

(defun %html-page-url (value field)
  "Return VALUE as an attribute-escaped http/https or relative URL string,
   or signal. The allow-list rejects unsafe schemes; escape-attribute then
   neutralises attribute-breaking characters so a scheme-valid URL carrying
   a quote-then-tag cannot break out of the href."
  (let ((url (princ-to-string value)))
    (if (safe-url-allowlist url :allowed-schemes '("http" "https"))
        (escape-attribute url)
        (error "html-page ~A refused unsafe URL ~S" field url))))

(defun html-page (&key (title "LOL-REACTIVE")
                       (lang "en")
                       (body-class "")
                       head-extra
                       body
                       css-href
                       (include-tailwind t)
                       (include-htmx t)
                       (include-surgery nil)
                       tailwind-script
                       base-css
                       component-css
                       htmx-indicator-css
                       base-css-href
                       component-css-href
                       htmx-indicator-css-href
                       csrf-token
                       reactive-runtime
                       htmx-runtime
                       surgery-css
                       surgery-runtime
                       reactive-runtime-src
                       htmx-runtime-src
                       surgery-css-href
                       surgery-runtime-src
                       description
                       canonical
                       (og-type "website")
                       og-title
                       og-description
                       og-url
                       og-image
                       og-image-alt
                       og-site-name)
  "Generate a complete HTML page with token-driven CSS variables.

   NO hardcoded colors, fonts, or styles - apps provide their own theme.

   Interpolation contract:
   - Text-like kwargs (TITLE, DESCRIPTION, OG-*, LANG, BODY-CLASS,
     CSRF-TOKEN) are escaped by their HTML context before emit.
   - URL-shaped kwargs (CANONICAL, OG-URL, OG-IMAGE, CSS-HREF) must be
     relative or carry an http/https scheme; unsafe schemes signal.
   - Payload kwargs (HEAD-EXTRA, BODY, BASE-CSS, COMPONENT-CSS,
     HTMX-INDICATOR-CSS, REACTIVE-RUNTIME, HTMX-RUNTIME, SURGERY-CSS,
     SURGERY-RUNTIME, TAILWIND-SCRIPT) accept SAFE-HTML-STRING for
     verbatim emit; raw strings are escape-html-ed (lossy for HTML/JS/
     CSS payloads — wrap producer output in MAKE-SAFE-HTML-STRING).
     By design the SAFE-HTML-STRING type IS the contract for the <script>
     and <style> payload sinks: a producer-asserted-safe payload is emitted
     verbatim with no further neutralize-script-close net, so the producer
     owns </script>/</style> safety. A bare string is escape-html-ed and so
     cannot reach those sinks unwrapped.

   Asset strings — each, when non-NIL, is emitted into the page; NIL
   means the corresponding asset block is omitted. There are no
   internal helper fallbacks: this file stays decoupled from the css/htmx/
   server/devtools sub-systems, so callers must pre-compute and pass:
   - TAILWIND-SCRIPT: Tailwind config JS
   - BASE-CSS: token-derived CSS variables
   - COMPONENT-CSS: registered component CSS
   - HTMX-INDICATOR-CSS: HTMX indicator CSS (only honoured when INCLUDE-HTMX)
   - CSRF-TOKEN: CSRF token string for the meta tag (only when INCLUDE-HTMX)
   - REACTIVE-RUNTIME: Parenscript reactive runtime JS
   - HTMX-RUNTIME: Parenscript HTMX runtime JS (only when INCLUDE-HTMX)
   - SURGERY-CSS, SURGERY-RUNTIME: surgery panel assets (only when INCLUDE-SURGERY)

   External-asset variants — for each constant CSS/JS slot a parallel
   href/src kwarg emits a same-origin <link rel=stylesheet> / <script src>
   instead of an inline <style>/<script>, so a strict no-'unsafe-inline'
   CSP holds: BASE-CSS-HREF, COMPONENT-CSS-HREF, HTMX-INDICATOR-CSS-HREF,
   REACTIVE-RUNTIME-SRC, HTMX-RUNTIME-SRC, SURGERY-CSS-HREF,
   SURGERY-RUNTIME-SRC. Each is a URL (relative or http/https). When the
   external variant is non-NIL it takes precedence and the slot is emitted
   external; otherwise the inline payload kwarg is emitted (an opt-in
   escape hatch). Emit position is unchanged either way, preserving
   document order (anti-FOUC). The server-coupled LOL-WEB:PAGE wrapper
   registers the constant payloads and passes these href/src for you;
   HTML-PAGE itself stays decoupled — it only emits the URLs it is given.

   This contract lets the html sub-system build standalone — it depends
   only on :lol-web/escape and cl-who, never on css/htmx/server/devtools.

   Other options:
   - TITLE: Page title
   - LANG: HTML lang attribute (default \"en\")
   - BODY-CLASS: Additional body CSS classes
   - HEAD-EXTRA: Custom head content (string)
   - BODY: Page body content (string)
   - CSS-HREF: Compiled CSS stylesheet path (when provided, suppresses CDN)
   - INCLUDE-TAILWIND: Include Tailwind CDN (default t, ignored when CSS-HREF set)
   - INCLUDE-HTMX: Include HTMX-style runtime (default t)
   - INCLUDE-SURGERY: Include surgery panel runtime (default nil)
   - DESCRIPTION: <meta name=description> for search snippets
   - CANONICAL: <link rel=canonical> URL (always set on public pages)
   - OG-TYPE: og:type (default \"website\")
   - OG-TITLE / OG-DESCRIPTION / OG-URL / OG-IMAGE / OG-IMAGE-ALT / OG-SITE-NAME:
     OpenGraph fields for social cards (LinkedIn, Slack, Discord, iMessage).
     OG-TITLE/OG-DESCRIPTION default to TITLE/DESCRIPTION when nil."
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:html :lang (escape-attribute (princ-to-string lang))
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title (cl-who:str (escape-html (princ-to-string title))))

       (when description
         (cl-who:htm (:meta :name "description" :content (escape-attribute (princ-to-string description)))))
       (when canonical
         (cl-who:htm
          (:link :rel "canonical"
                 :href (%html-page-url canonical :canonical))))

       ;; OpenGraph — emitted when any og-* is set or canonical/description
       ;; give us enough to populate the minimum quad. cl-who does not escape
       ;; runtime attribute values, so each :content is escape-attribute'd at
       ;; the call site; princ-to-string first normalises symbol/number input.
       (let ((og-t  (or og-title title))
             (og-d  (or og-description description))
             (og-u  (or og-url canonical)))
         (when (or og-image og-t og-d og-u og-site-name)
           (cl-who:htm
            (:meta :property "og:type"        :content (escape-attribute (princ-to-string og-type)))
            (when og-t (cl-who:htm (:meta :property "og:title"       :content (escape-attribute (princ-to-string og-t)))))
            (when og-d (cl-who:htm (:meta :property "og:description" :content (escape-attribute (princ-to-string og-d)))))
            (when og-u
              (cl-who:htm
               (:meta :property "og:url"
                      :content (%html-page-url og-u :og-url))))
            (when og-site-name
              (cl-who:htm (:meta :property "og:site_name" :content (escape-attribute (princ-to-string og-site-name)))))
            (when og-image
              (cl-who:htm
               (:meta :property "og:image"
                      :content (%html-page-url og-image :og-image))
               (:meta :property "og:image:width"  :content "1200")
               (:meta :property "og:image:height" :content "630")
               (when og-image-alt
                 (cl-who:htm (:meta :property "og:image:alt" :content (escape-attribute (princ-to-string og-image-alt))))))))))

       ;; Compiled CSS (when provided, replaces CDN)
       (when css-href
         (cl-who:htm
          (:link :rel "stylesheet"
                 :href (%html-page-url css-href :css-href))))

       ;; Tailwind CDN (only when no compiled CSS). Pinned to 3.4.16 with
       ;; subresource integrity so the browser refuses any bundle whose
       ;; SHA-384 does not match. README §"Tailwind CDN" documents the
       ;; pin and the procedure for rotating the version + hash together.
       (when (and include-tailwind (not css-href))
         (cl-who:htm
          (:script :src "https://cdn.tailwindcss.com/3.4.16"
                   :integrity "sha384-mS5Uq7sE90lgbBDN8xgf34ibEgbZo4gB3tfLY40ZRle+M188BQw8onzNHg6GUZaA"
                   :crossorigin "anonymous")
          (when tailwind-script
            (cl-who:htm (:script (cl-who:str (coerce-html-emit tailwind-script)))))))

       ;; CSS variables from tokens — caller pre-computes via :lol-web/css.
       ;; External href takes precedence; inline payload is the escape hatch.
       (cond
         (base-css-href
          (cl-who:htm (:link :rel "stylesheet"
                             :href (%html-page-url base-css-href :base-css-href))))
         (base-css
          (cl-who:htm (:style (cl-who:str (coerce-html-emit base-css))))))

       ;; Registered component CSS — caller pre-computes via :lol-web/css.
       (cond
         (component-css-href
          (cl-who:htm (:link :rel "stylesheet"
                             :href (%html-page-url component-css-href :component-css-href))))
         (component-css
          (cl-who:htm (:style (cl-who:str (coerce-html-emit component-css))))))

       ;; HTMX indicator styles — caller pre-computes via :lol-web/htmx.
       (when include-htmx
         (cond
           (htmx-indicator-css-href
            (cl-who:htm (:link :rel "stylesheet"
                               :href (%html-page-url htmx-indicator-css-href :htmx-indicator-css-href))))
           (htmx-indicator-css
            (cl-who:htm (:style (cl-who:str (coerce-html-emit htmx-indicator-css)))))))

       ;; CSRF meta tag for HTMX runtime — caller passes the session token
       ;; (e.g., (lol-web/server:get-csrf-token)).
       (when (and include-htmx csrf-token)
         (cl-who:htm (:meta :name "csrf-token" :content (escape-attribute (princ-to-string csrf-token)))))

       ;; App-provided head content
       (cl-who:str (coerce-html-emit head-extra)))

      (:body :class (escape-attribute (princ-to-string body-class))
        ;; Main content
        (cl-who:str (coerce-html-emit body))

        ;; Reactive runtime script (Parenscript). External src takes
        ;; precedence; then an inline payload; then the same-package
        ;; reactive-runtime-js fallback (defined below, returns a
        ;; SAFE-HTML-STRING) so a bare html-page call is still functional.
        (cond
          (reactive-runtime-src
           (cl-who:htm (:script :src (%html-page-url reactive-runtime-src :reactive-runtime-src))))
          (reactive-runtime
           (cl-who:htm (:script (cl-who:str (coerce-html-emit reactive-runtime)))))
          (t
           (cl-who:htm (:script (cl-who:str (coerce-html-emit (reactive-runtime-js)))))))

        ;; HTMX runtime — caller pre-computes via :lol-web/htmx.
        (when include-htmx
          (cond
            (htmx-runtime-src
             (cl-who:htm (:script :src (%html-page-url htmx-runtime-src :htmx-runtime-src))))
            (htmx-runtime
             (cl-who:htm (:script (cl-who:str (coerce-html-emit htmx-runtime)))))))

        ;; Surgery panel — caller pre-computes via :lol-web/devtools.
        (when include-surgery
          (cond
            (surgery-css-href
             (cl-who:htm (:link :rel "stylesheet"
                                :href (%html-page-url surgery-css-href :surgery-css-href))))
            (surgery-css
             (cl-who:htm (:style (cl-who:str (coerce-html-emit surgery-css)))))))
        (when include-surgery
          (cond
            (surgery-runtime-src
             (cl-who:htm (:script :src (%html-page-url surgery-runtime-src :surgery-runtime-src))))
            (surgery-runtime
             (cl-who:htm (:script (cl-who:str (coerce-html-emit surgery-runtime)))))))))))

;;; ============================================================================
;;; REACTIVE RUNTIME (Parenscript)
;;;
;;; Client-side reactivity. ALL JavaScript generated via Parenscript.
;;; ============================================================================

(defun reactive-runtime-js ()
  "Generate the client-side reactive runtime via Parenscript.
   NO raw JavaScript strings. Returns a SAFE-HTML-STRING so html-page
   emits the script verbatim instead of escaping it."
  (make-safe-html-string
   (parenscript:ps
    ;; LOL-REACTIVE Runtime
    (defvar *lol-reactive*
      (ps:create
       :components (ps:new (-Map))

       :register (lambda (id handlers)
                   (ps:chain (ps:@ this components) (set id handlers)))

       :dispatch (lambda (component-id action &rest args)
                   (ps:chain
                    (fetch "/api/dispatch"
                           (ps:create :method "POST"
                                      :headers (ps:create "Content-Type" "application/json")
                                      :body (ps:chain -j-s-o-n (stringify
                                             (ps:create :component-id component-id
                                                        :action action
                                                        :args args)))))
                    (then (lambda (r) (ps:chain r (json))))
                    (then (lambda (data)
                            (when (ps:@ data html)
                              (let ((el (ps:chain document (query-selector
                                         (+ "[data-component-id=\"" component-id "\"]")))))
                                (when el
                                  (setf (ps:@ el inner-h-t-m-l) (ps:@ data html)))))))))

       :set-state (lambda (component-id key value)
                    (ps:chain
                     (fetch "/api/set-state"
                            (ps:create :method "POST"
                                       :headers (ps:create "Content-Type" "application/json")
                                       :body (ps:chain -j-s-o-n (stringify
                                              (ps:create :component-id component-id
                                                         :key key
                                                         :value value)))))
                     (then (lambda (r) (ps:chain r (json))))
                     (then (lambda (data)
                             (when (ps:@ data html)
                               (let ((el (ps:chain document (query-selector
                                          (+ "[data-component-id=\"" component-id "\"]")))))
                                 (when el
                                   (setf (ps:@ el inner-h-t-m-l) (ps:@ data html)))))))))))

    ;; Shorthand - only set if LOL-REACTIVE exists
    (defvar dispatch
      (when *lol-reactive*
        (ps:chain (ps:@ *lol-reactive* dispatch) (bind *lol-reactive*))))
    (defvar set-state
      (when *lol-reactive*
        (ps:chain (ps:getprop *lol-reactive* "set-state") (bind *lol-reactive*))))

    (ps:chain console (log "(LOL-REACTIVE :status :loaded)")))))
