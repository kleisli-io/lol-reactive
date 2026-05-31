;;;; Tests for content-addressed external assets (src/server/assets.lisp).
;;;;
;;;; Covers: register-asset memoisation + content-type→extension; the
;;;; asset-middleware serving exact bytes with immutable/nosniff headers
;;;; and falling through on a miss; html-page emitting external
;;;; <link>/<script src> for the asset slots (with the inline escape hatch
;;;; intact); the lol-web:page wrapper externalising constant assets; and
;;;; embed-json-data emitting a non-executable, </script>-neutralised block.

(in-package :lol-web/server/test)
(in-suite :lol-web/server/test)

(defun %ends-with (string suffix)
  (let ((sl (length string)) (fl (length suffix)))
    (and (>= sl fl) (string= suffix (subseq string (- sl fl))))))

;;; ============================================================================
;;; register-asset — content addressing + memoisation
;;; ============================================================================

(test register-asset-memoises-by-content
  "Identical content yields one URL; distinct content yields distinct URLs."
  (clear-asset-registry)
  (let ((a  (register-asset "body{}" :content-type "text/css"))
        (a2 (register-asset "body{}" :content-type "text/css"))
        (b  (register-asset "div{}"  :content-type "text/css")))
    (is (string= a a2) "same content + type → same href")
    (is (not (string= a b)) "different content → different href")
    (is (eql 0 (search *asset-route-prefix* a)) "href lives under the asset prefix")))

(test register-asset-extension-tracks-content-type
  "URL extension is derived from the media type; unknown types signal."
  (clear-asset-registry)
  (let ((css (register-asset "body{}"  :content-type "text/css"))
        (js  (register-asset "var x=1" :content-type "application/javascript")))
    (is (%ends-with css ".css") "text/css → .css")
    (is (%ends-with js  ".js")  "application/javascript → .js"))
  (signals error (register-asset "x" :content-type "text/plain")))

(test register-asset-accepts-safe-html-string
  "A SAFE-HTML-STRING payload registers by its underlying value."
  (clear-asset-registry)
  (let ((from-string (register-asset ":root{--a:1}" :content-type "text/css"))
        (from-safe (register-asset
                    (lol-web/html:make-safe-html-string ":root{--a:1}")
                    :content-type "text/css")))
    (is (string= from-string from-safe)
        "safe-html-string and its string value hash to the same href")))

;;; ============================================================================
;;; asset-middleware — serving
;;; ============================================================================

(defun %asset-test-app ()
  "asset-middleware wrapping a sentinel downstream app."
  (asset-middleware (lambda (env) (declare (ignore env)) '(404 () ("downstream")))))

(test asset-middleware-serves-registered-bytes
  "A GET on a registered asset returns its exact bytes, the immutable
   cache directive, the declared content-type, and nosniff."
  (clear-asset-registry)
  (let* ((path (register-asset "body{color:red}" :content-type "text/css"))
         (app (%asset-test-app))
         (resp (funcall app (list :request-method :get :path-info path))))
    (destructuring-bind (status headers body) resp
      (is (= 200 status))
      (is (string= "text/css; charset=utf-8" (getf headers :content-type)))
      (let ((cc (getf headers :cache-control)))
        (is (search "public" cc))
        (is (search "max-age=31536000" cc))
        (is (search "immutable" cc)))
      (is (string= "nosniff" (getf headers :x-content-type-options)))
      (is (equal '("body{color:red}") body) "exact registered bytes"))))

(test asset-middleware-head-omits-body
  "HEAD returns the asset headers with no body."
  (clear-asset-registry)
  (let* ((path (register-asset "a{}" :content-type "text/css"))
         (app (%asset-test-app))
         (resp (funcall app (list :request-method :head :path-info path))))
    (destructuring-bind (status headers body) resp
      (declare (ignore headers))
      (is (= 200 status))
      (is (null body) "HEAD body is empty"))))

(test asset-middleware-falls-through-on-miss
  "An unknown asset name, a non-asset path, and a non-GET/HEAD method all
   pass through to the downstream app untouched."
  (clear-asset-registry)
  (let ((app (%asset-test-app)))
    (is (equal '(404 () ("downstream"))
               (funcall app (list :request-method :get
                                  :path-info (concatenate 'string *asset-route-prefix* "deadbeef.css"))))
        "unknown asset → downstream")
    (is (equal '(404 () ("downstream"))
               (funcall app (list :request-method :get :path-info "/elsewhere")))
        "non-asset path → downstream")
    (let ((path (register-asset "a{}" :content-type "text/css")))
      (is (equal '(404 () ("downstream"))
                 (funcall app (list :request-method :post :path-info path)))
          "POST to an asset path → downstream"))))

(test clear-asset-registry-empties-the-store
  "After clear, a previously-registered asset is no longer served."
  (let* ((path (register-asset "z{}" :content-type "text/css"))
         (app (%asset-test-app)))
    (clear-asset-registry)
    (is (equal '(404 () ("downstream"))
               (funcall app (list :request-method :get :path-info path)))
        "cleared asset falls through")))

;;; ============================================================================
;;; html-page — external emit vs inline escape hatch
;;; ============================================================================

(test html-page-emits-external-link-for-css-href
  "A *-css-href emits a same-origin <link rel=stylesheet>, not <style>."
  (let ((html (lol-web/html:html-page :base-css-href "/_lol/a/x.css"
                                      :include-tailwind nil :include-htmx nil)))
    (is (search "<link" html))
    (is (search "stylesheet" html))
    (is (search "/_lol/a/x.css" html))
    (is (null (search "<style" html)) "no inline <style> when an href is given")))

(test html-page-emits-external-script-for-runtime-src
  "A *-runtime-src emits <script src>, and the inline reactive fallback is
   suppressed."
  (let ((html (lol-web/html:html-page :reactive-runtime-src "/_lol/a/r.js"
                                      :include-tailwind nil :include-htmx nil)))
    (is (search "/_lol/a/r.js" html))
    (is (search "<script src=" html))
    (is (null (search "LOL-REACTIVE :status :loaded" html))
        "inline reactive runtime fallback suppressed when src is given")))

(test html-page-inline-escape-hatch-still-emits-style
  "With no href but an inline payload, html-page still inlines (opt-in)."
  (let ((html (lol-web/html:html-page
               :base-css (lol-web/html:make-safe-html-string ":root{--x:1}")
               :include-tailwind nil :include-htmx nil)))
    (is (search "<style" html))
    (is (search ":root{--x:1}" html))))

;;; ============================================================================
;;; page wrapper — register + thread URLs
;;; ============================================================================

(test page-wrapper-externalises-constant-assets
  "lol-web:page registers the constant CSS/JS payloads and threads their
   URLs into html-page so nothing is inlined; the reactive runtime is
   externalised by default."
  (clear-asset-registry)
  (let ((html (page :base-css (lol-web/html:make-safe-html-string ":root{--a:1}")
                    :component-css (lol-web/html:make-safe-html-string ".c{color:red}")
                    :include-tailwind nil :include-htmx nil)))
    (is (null (search "<style" html)) "no inline <style> through the wrapper")
    (is (search "<link" html) "external stylesheet link present")
    (is (search *asset-route-prefix* html) "links point at the asset route")
    (is (null (search "LOL-REACTIVE :status :loaded" html))
        "default reactive runtime externalised, not inlined")
    (is (search "<script src=" html) "external runtime script present")))

;;; ============================================================================
;;; embed-json-data — non-executable, neutralised data block
;;; ============================================================================

(test embed-json-data-emits-non-executable-block
  "embed-json-data returns a SAFE-HTML-STRING application/json data block."
  (let* ((block (embed-json-data "tg" 42))
         (s (lol-web/html:safe-html-string-value block)))
    (is (lol-web/html:safe-html-string-p block))
    (is (search "<script type=\"application/json\" id=\"tg\">" s))
    (is (search "42" s))))

(test embed-json-data-neutralises-script-close
  "An embedded </script> in the payload cannot terminate the element early."
  (let* ((block (embed-json-data "x" "a</script>"))
         (s (lol-web/html:safe-html-string-value block)))
    (is (null (search "a</script>" s)) "payload </ is broken")
    (is (search "a<\\/script>" s) "</ rewritten to <\\/")))

;;; ============================================================================
;;; make-app — assets middleware placement
;;; ============================================================================

(test app-middleware-order-places-assets-at-static-tier
  "use-assets installs :assets immediately after :static and outside session."
  (let ((order (app-middleware-order :use-static t :use-assets t
                                     :use-session t :use-csrf t)))
    (let ((sp (position :static order))
          (ap (position :assets order))
          (sess (position :session order)))
      (is (and sp ap (= ap (1+ sp))) ":assets immediately follows :static")
      (is (and ap sess (< ap sess)) ":assets dispatches outside :session"))))
