;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTMX; Base: 10 -*-
;;;; HTMX server-side helpers for request detection and response generation

(in-package :lol-web/htmx)

;;; ============================================================================
;;; REQUEST DETECTION
;;;
;;; Detect HTMX requests via HX-* headers sent by the client runtime.
;;; ============================================================================

(defun htmx-request-p ()
  "Check if current request is from HTMX client.
   Returns T if HX-Request header is 'true'."
  (string= "true" (request-header "HX-Request")))

(defun htmx-boosted-p ()
  "Check if request is from a hx-boost link.
   Returns T if HX-Boosted header is 'true'."
  (string= "true" (request-header "HX-Boosted")))

(defun htmx-history-restore-request-p ()
  "Check if request is for browser-history restoration.
   Returns T if HX-History-Restore-Request header is 'true'."
  (string= "true" (request-header "HX-History-Restore-Request")))

(defun htmx-target ()
  "Get target element ID from HTMX request.
   Returns the value of HX-Target header, or NIL if not present."
  (request-header "HX-Target"))

(defun htmx-trigger ()
  "Get triggering element ID from HTMX request.
   Returns the value of HX-Trigger header, or NIL if not present."
  (request-header "HX-Trigger"))

(defun htmx-trigger-name ()
  "Get the triggering element's name attribute from HTMX request.
   Returns the value of HX-Trigger-Name header, or NIL if not present."
  (request-header "HX-Trigger-Name"))

(defun htmx-current-url ()
  "Get current browser URL from HTMX request.
   Returns the value of HX-Current-URL header, or NIL if not present."
  (request-header "HX-Current-URL"))

(defun htmx-prompt ()
  "Get the user's response from a hx-prompt dialog.
   Returns the value of HX-Prompt header, or NIL if not present."
  (request-header "HX-Prompt"))

;;; ============================================================================
;;; RESPONSE HELPERS
;;;
;;; Every HX-* response header value passes through validate-header-value so
;;; CR/LF cannot forge a second header; URL-shaped fields additionally pass
;;; through safe-url-allowlist so a javascript:/data:/vbscript: scheme cannot
;;; smuggle a payload into the browser's navigate path.
;;; ============================================================================

(defun %hx-safe-url (field-name url)
  "Return URL when safe-url-allowlist accepts it; signal otherwise."
  (or (safe-url-allowlist url)
      (error "~A: URL scheme not in allowlist: ~S" field-name url)))

(defmacro! with-htmx-response ((&key trigger retarget reswap reselect
                                     push-url replace-url refresh)
                               &body body)
  "Execute BODY with HTMX response headers set.

   Options:
   - TRIGGER: Event name(s) to trigger on client (string or alist; strings
     route through jsonify, non-strings JSON-encode for HX-Trigger).
   - RETARGET / RESWAP / RESELECT: header-value-shaped fields routed
     through validate-header-value.
   - PUSH-URL / REPLACE-URL: URL-shaped fields routed through
     safe-url-allowlist (scheme guard) then validate-header-value (CR/LF).
   - REFRESH: when truthy, emits HX-Refresh: true.

   If BODY returns a string, wraps it in html-response carrying the
   accumulated headers. Non-string BODY values pass through unchanged.

   Example:
   (with-htmx-response (:trigger \"cartUpdated\" :reswap \"innerHTML\")
     (render-cart-items))"
  `(progn
     ,@(when trigger
         ;; stringp / symbolp must dispatch on the runtime value, not the
         ;; macroexpansion form: (let ((evt "x")) (with-htmx-response
         ;; (:trigger evt) ...)) has (stringp 'evt) NIL at expansion and
         ;; would force JSON-encoding.
         `((let ((,g!trigger-val ,trigger))
             (add-response-header
              "HX-Trigger"
              (validate-header-value
               (if (or (stringp ,g!trigger-val) (symbolp ,g!trigger-val))
                   (jsonify ,g!trigger-val)
                   (encode-json-string ,g!trigger-val)))))))
     ,@(when retarget
         `((add-response-header "HX-Retarget" (validate-header-value ,retarget))))
     ,@(when reswap
         `((add-response-header "HX-Reswap" (validate-header-value ,reswap))))
     ,@(when reselect
         `((add-response-header "HX-Reselect" (validate-header-value ,reselect))))
     ,@(when push-url
         `((add-response-header
            "HX-Push-Url"
            (validate-header-value (%hx-safe-url "HX-Push-Url" ,push-url)))))
     ,@(when replace-url
         `((add-response-header
            "HX-Replace-Url"
            (validate-header-value (%hx-safe-url "HX-Replace-Url" ,replace-url)))))
     ,@(when refresh
         `((add-response-header "HX-Refresh" "true")))
     (let ((,g!body-result (progn ,@body)))
       (if (stringp ,g!body-result)
           (html-response ,g!body-result :headers (get-response-headers))
           ,g!body-result))))

(defun set-htmx-trigger (event-name &optional event-detail)
  "Set HX-Trigger so the client dispatches EVENT-NAME (optionally with
   EVENT-DETAIL). EVENT-NAME routes through jsonify so symbols become
   downcased strings and conses signal. The final header value passes
   through validate-header-value."
  (let ((name (jsonify event-name)))
    (add-response-header
     "HX-Trigger"
     (validate-header-value
      (if event-detail
          (encode-json-string (list (cons name event-detail)))
          name)))))

(defun set-htmx-redirect (url)
  "Set HX-Redirect to URL after safe-url-allowlist (scheme guard) and
   validate-header-value (CR/LF guard)."
  (add-response-header
   "HX-Redirect"
   (validate-header-value (%hx-safe-url "HX-Redirect" url))))

(defun set-htmx-location (url &key target swap)
  "Set HX-Location to URL for client-side navigation. URL routes through
   safe-url-allowlist; the final header value passes through
   validate-header-value. With TARGET or SWAP the value is the WHATWG
   location object carrying path, target, swap."
  (let ((safe-path (%hx-safe-url "HX-Location" url)))
    (add-response-header
     "HX-Location"
     (validate-header-value
      (if (or target swap)
          (encode-json-string
           (remove nil
                   (list (cons "path" safe-path)
                         (when target (cons "target" target))
                         (when swap (cons "swap" swap)))))
          safe-path)))))

;;; ============================================================================
;;; OOB RESPONSE RENDERING
;;;
;;; Combine primary content with out-of-band updates.
;;; ============================================================================

(defun render-with-oob (main-content &rest oob-updates)
  "Render main content with out-of-band updates.

   MAIN-CONTENT: Primary HTML — either NIL, or a SAFE-HTML-STRING the
                 producer asserts is safe to emit verbatim.
   OOB-UPDATES: List of OOB update specifications; each CONTENT must be
                a SAFE-HTML-STRING (enforced by `oob-swap'):
     - (id content) - default outerHTML swap
     - (id content :swap strategy) - specific swap strategy

   Example:
   (render-with-oob
     (render-cart-item product-id) ; returns safe-html-string
     (list \"cart-count\" (make-safe-html-string (format nil \"~a\" (cart-count))))
     (list \"cart-total\" (make-safe-html-string (format nil \"$~,2F\" (cart-total))) :swap \"innerHTML\")
     (list \"cart-dropdown\" (render-cart-dropdown) :swap \"outerHTML\"))"
  (when main-content
    (check-type main-content lol-web/html:safe-html-string))
  (with-output-to-string (s)
    (when main-content
      (write-string (lol-web/html:safe-html-string-value main-content) s))
    (dolist (update oob-updates)
      (destructuring-bind (id content &key (swap "true")) update
        (write-string (oob-swap id content :swap swap) s)))))

(defmacro render-oob-only (&rest oob-updates)
  "Render only out-of-band updates (no primary target content).
   Use with hx-swap='none' on the triggering element.

   Each OOB-UPDATE is: (id content &key swap). CONTENT must be a
   SAFE-HTML-STRING — the OOB sink emits it verbatim and rejects a bare
   string with a TYPE-ERROR, so wrap producer output in MAKE-SAFE-HTML-STRING.

   Example:
   (render-oob-only
     (\"cart-count\" (make-safe-html-string (format nil \"~a\" count)))
     (\"cart-total\" (make-safe-html-string (format nil \"$~,2F\" total)) :swap \"innerHTML\"))"
  `(render-with-oob nil ,@(mapcar (lambda (u) `(list ,@u)) oob-updates)))

;;; ============================================================================
;;; CONDITIONAL RENDERING
;;;
;;; Helpers for responding differently to HTMX vs regular requests.
;;; ============================================================================

(defmacro htmx-or-redirect (htmx-body redirect-url)
  "If HTMX request, evaluate HTMX-BODY. Otherwise, redirect.

   Example:
   (defroute \"/api/cart/add\" :post (product-id)
     (add-to-cart product-id)
     (htmx-or-redirect
       (render-oob-only
         (\"cart-count\" (make-safe-html-string (format nil \"~a\" (cart-count)))))
       \"/cart\"))"
  `(if (htmx-request-p)
       ,htmx-body
       (redirect-response ,redirect-url)))

(defmacro htmx-or-full-page (htmx-body full-page-body)
  "If HTMX request, return partial. Otherwise, render full page.

   Example:
   (defroute \"/products/:id\" :get (id)
     (let ((product (get-product id)))
       (htmx-or-full-page
         (render-product-card product)  ; Partial for HTMX
         (html-page :body (render-product-page product)))))"
  `(if (htmx-request-p)
       ,htmx-body
       ,full-page-body))
