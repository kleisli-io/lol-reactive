;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/SERVER; Base: 10 -*-
;;;; Content-addressed external assets.
;;;;
;;;; html-page can serve deploy-constant CSS/JS as same-origin external
;;;; resources (<link>/<script src>) instead of inlining them, so a strict
;;;; Content-Security-Policy with no 'unsafe-inline' holds. register-asset
;;;; memoises a payload under its SHA-256 and returns the URL it is served
;;;; from; asset-middleware serves that URL.
;;;;
;;;; The middleware sits at the :static tier of the make-app stack — OUTSIDE
;;;; the session/csrf middleware — so a publicly-cacheable, immutable asset
;;;; response never carries a Set-Cookie (which a shared cache could leak
;;;; across users). Lookups are pure hash-table reads keyed by URL basename,
;;;; never a filesystem path, so there is no traversal surface.

(in-package :lol-web/server)

;;; ============================================================================
;;; REGISTRY
;;; ============================================================================

(defparameter *asset-route-prefix* "/_lol/a/"
  "URL path prefix under which content-addressed assets are served. A
   request whose path begins here is handled by ASSET-MIDDLEWARE.")

(defparameter +asset-extensions+
  '(("text/css" . "css")
    ("application/javascript" . "js"))
  "Supported asset media-type → URL extension. Only the same-origin static
   stylesheet/script types a page links under a strict CSP are registrable.")

(defvar *asset-registry* (make-hash-table :test 'equal)
  "URL basename \"<sha>.<ext>\" → (CONTENT-STRING . MEDIA-TYPE). Guarded by
   *ASSET-REGISTRY-LOCK*.")

(defvar *asset-registry-lock*
  (bordeaux-threads:make-recursive-lock "lol-web asset registry")
  "Serialises *ASSET-REGISTRY* reads and writes.")

(defun %asset-extension (content-type)
  "URL extension for CONTENT-TYPE, or signal if it is not registrable."
  (or (cdr (assoc content-type +asset-extensions+ :test #'string=))
      (error "register-asset: unsupported content-type ~S (expected one of ~{~S~^, ~})"
             content-type (mapcar #'car +asset-extensions+))))

(defun register-asset (content &key (content-type "text/css"))
  "Memoise CONTENT under its SHA-256 and return the same-origin URL it is
   served from: <*ASSET-ROUTE-PREFIX*><sha>.<ext>. Idempotent — identical
   CONTENT + CONTENT-TYPE yields the same URL, so re-rendering a page does
   not grow the registry. CONTENT is a string or a SAFE-HTML-STRING (its
   value is taken). CONTENT-TYPE must be a key of +ASSET-EXTENSIONS+.

   The URL is a relative same-origin path, so a page references it from a
   <link>/<script src> under script-src/style-src 'self' without inlining."
  (let* ((value (etypecase content
                  (string content)
                  (safe-html-string (safe-html-string-value content))))
         (ext (%asset-extension content-type))
         (name (format nil "~A.~A" (sha256-hex value) ext)))
    (bordeaux-threads:with-recursive-lock-held (*asset-registry-lock*)
      (setf (gethash name *asset-registry*) (cons value content-type)))
    (concatenate 'string *asset-route-prefix* name)))

(defun clear-asset-registry ()
  "Drop every registered asset. Test/REPL affordance — production registers
   assets at page-render time, so a cleared registry refills on next render."
  (bordeaux-threads:with-recursive-lock-held (*asset-registry-lock*)
    (clrhash *asset-registry*))
  (values))

(defun %lookup-asset (name)
  "Return the (CONTENT . MEDIA-TYPE) registered under basename NAME, or NIL."
  (bordeaux-threads:with-recursive-lock-held (*asset-registry-lock*)
    (gethash name *asset-registry*)))

(defun %has-prefix (string prefix)
  "True when STRING begins with PREFIX."
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

;;; ============================================================================
;;; SERVING MIDDLEWARE
;;; ============================================================================

(defun %asset-response (method content content-type)
  "Build the raw Clack response for a registered asset. Runs outside the
   route dispatcher's WITH-RESPONSE-HEADERS scope, so it constructs the
   (status headers body) list directly. Immutable + publicly cacheable;
   nosniff so the browser honours the declared type. HEAD omits the body."
  (list 200
        (list :content-type (format nil "~A; charset=utf-8" content-type)
              :cache-control "public, max-age=31536000, immutable"
              :x-content-type-options "nosniff")
        (if (eq method :head) nil (list content))))

(defun %try-serve-asset (env)
  "Serve a registered asset for a GET/HEAD request on *ASSET-ROUTE-PREFIX*,
   or return NIL so the caller falls through to the rest of the stack. The
   basename is a hash-table key, never a path — an unknown name simply
   misses and 404s downstream."
  (let ((method (getf env :request-method))
        (path (getf env :path-info)))
    (when (and (member method '(:get :head))
               (stringp path)
               (%has-prefix path *asset-route-prefix*))
      (let ((entry (%lookup-asset (subseq path (length *asset-route-prefix*)))))
        (when entry
          (%asset-response method (car entry) (cdr entry)))))))

(defun asset-middleware (app)
  "Lack middleware: serve a registered content-addressed asset, else pass
   through to APP. Installed at the :static tier of MAKE-APP — outside the
   session/csrf stack — so immutable public assets never carry Set-Cookie."
  (lambda (env)
    (or (%try-serve-asset env)
        (funcall app env))))

;;; ============================================================================
;;; PAGE WRAPPER
;;; ============================================================================

(defun %asset-kv (key content content-type)
  "Plist fragment (KEY url) registering CONTENT as an external asset, or NIL
   when CONTENT is NIL — so the slot is simply omitted."
  (when content
    (list key (register-asset content :content-type content-type))))

(defun %strip-plist-keys (plist keys)
  "PLIST without the entries whose key is in KEYS."
  (loop for (k v) on plist by #'cddr
        unless (member k keys)
          append (list k v)))

(defun page (&rest args
             &key base-css component-css htmx-indicator-css
                  reactive-runtime htmx-runtime surgery-css surgery-runtime
                  (include-htmx t) (include-surgery nil)
             &allow-other-keys)
  "Server-coupled HTML-PAGE wrapper that satisfies the strict CSP: register
   each deploy-constant CSS/JS payload as a same-origin external asset and
   have HTML-PAGE emit <link>/<script src> for it instead of inlining. The
   reactive runtime is externalised by default (the HTML-PAGE inline
   fallback is registered too). Every other key — TITLE, BODY, HEAD-EXTRA,
   CSS-HREF, the OG-* / description SEO fields, INCLUDE-* toggles, etc. —
   is forwarded to HTML-PAGE unchanged, so this is a drop-in for callers.

   The payload kwargs accepted here (BASE-CSS, COMPONENT-CSS,
   HTMX-INDICATOR-CSS, REACTIVE-RUNTIME, HTMX-RUNTIME, SURGERY-CSS,
   SURGERY-RUNTIME) take a string or SAFE-HTML-STRING — the same values
   HTML-PAGE would inline."
  (let ((rest (%strip-plist-keys
               args '(:base-css :component-css :htmx-indicator-css
                      :reactive-runtime :htmx-runtime
                      :surgery-css :surgery-runtime))))
    (apply #'html-page
           (append
            (%asset-kv :base-css-href base-css "text/css")
            (%asset-kv :component-css-href component-css "text/css")
            (when include-htmx
              (%asset-kv :htmx-indicator-css-href htmx-indicator-css "text/css"))
            (%asset-kv :reactive-runtime-src
                       (or reactive-runtime (reactive-runtime-js))
                       "application/javascript")
            (when include-htmx
              (%asset-kv :htmx-runtime-src htmx-runtime "application/javascript"))
            (when include-surgery
              (%asset-kv :surgery-css-href surgery-css "text/css"))
            (when include-surgery
              (%asset-kv :surgery-runtime-src surgery-runtime "application/javascript"))
            rest))))

;;; ============================================================================
;;; NON-EXECUTABLE DATA BLOCKS
;;; ============================================================================

(defun embed-json-data (id data)
  "Return a SAFE-HTML-STRING <script type=\"application/json\" id=ID> block
   carrying DATA as JSON. A JSON data block is not gated by script-src, so
   per-request data rides under a strict CSP without inlining executable
   script — an external script reads it via
   JSON.parse(document.getElementById(id).textContent). The payload is
   </script>-neutralised (</ → <\\/, which JSON.parse decodes back), so an
   embedded close tag cannot terminate the element early."
  (make-safe-html-string
   (format nil "<script type=\"application/json\" id=\"~A\">~A</script>"
           (escape-attribute (princ-to-string id))
           (neutralize-script-close (encode-json-string data)))))
