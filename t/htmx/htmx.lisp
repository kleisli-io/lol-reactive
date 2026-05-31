(in-package :lol-web/htmx/test)
(in-suite :lol-web/htmx/test)

;;; ============================================================================
;;; OOB-SWAP
;;; ============================================================================

(test oob-swap-basic
  "oob-swap wraps content with target ID and swap attribute"
  (let ((result (oob-swap "my-target"
                          (lol-web/html:make-safe-html-string "<p>content</p>"))))
    (is (stringp result))
    (is (search "id=\"my-target\"" result))
    (is (search "hx-swap-oob" result))
    (is (search "content" result))))

(test oob-swap-with-strategy
  "oob-swap respects swap strategy"
  (let ((result (oob-swap "target"
                          (lol-web/html:make-safe-html-string "content")
                          :swap "innerHTML")))
    (is (search "innerHTML" result))))

(test oob-swap-default-true
  "oob-swap defaults to true strategy"
  (let ((result (oob-swap "target"
                          (lol-web/html:make-safe-html-string "content"))))
    (is (search "true" result))))

;;; ============================================================================
;;; RENDER-WITH-OOB
;;; ============================================================================

(test render-with-oob-nil-primary
  "render-with-oob handles nil primary content"
  (let ((result (render-with-oob
                  nil
                  (list "count" (lol-web/html:make-safe-html-string "5"))
                  (list "total" (lol-web/html:make-safe-html-string "$10.00")))))
    (is (stringp result))
    (is (search "count" result))
    (is (search "total" result))))

(test render-with-oob-multiple-targets
  "render-with-oob includes all OOB targets"
  (let ((result (render-with-oob
                  (lol-web/html:make-safe-html-string "<main>primary</main>")
                  (list "header" (lol-web/html:make-safe-html-string "Header content"))
                  (list "footer" (lol-web/html:make-safe-html-string "Footer content"))
                  (list "sidebar" (lol-web/html:make-safe-html-string "Sidebar content")))))
    (is (search "primary" result))
    (is (search "header" result))
    (is (search "footer" result))
    (is (search "sidebar" result))))

;;; ============================================================================
;;; HTMX runtime JS — basic shape
;;; ============================================================================

(test htmx-runtime-js-generates-js
  "htmx-runtime-js generates JavaScript code"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (stringp js))
    (is (search "htmx" js :test #'char-equal))
    (is (search "processElement" js))
    (is (search "issueRequest" js))))

;;; ============================================================================
;;; HTMX attribute helpers
;;; ============================================================================

(test hx-get-generates-attr
  "hx-get generates correct attribute string"
  (let ((result (hx-get "/api/data")))
    (is (stringp result))
    (is (search "hx-get" result))
    (is (search "/api/data" result))))

(test hx-post-generates-attr
  "hx-post generates correct attribute string"
  (let ((result (hx-post "/api/submit")))
    (is (stringp result))
    (is (search "hx-post" result))
    (is (search "/api/submit" result))))

(test hx-post-with-target
  "hx-post includes target when specified"
  (let ((result (hx-post "/api/submit" :target "#result")))
    (is (search "hx-target" result))
    (is (search "#result" result))))

;;; ============================================================================
;;; Autocomplete widget
;;; ============================================================================

(test render-autocomplete-generates-html
  "render-autocomplete generates complete autocomplete widget HTML"
  (let ((result (render-autocomplete :id "search" :endpoint "/api/search")))
    (is (stringp result))
    (is (search "type=\"search\"" result))
    (is (search "id=\"search\"" result))
    (is (search "hx-get=\"/api/search\"" result))
    (is (search "hx-sync=\"this:replace\"" result))
    (is (search "role=\"combobox\"" result))
    (is (search "aria-controls=\"search-results\"" result))
    (is (search "aria-expanded=\"false\"" result))
    (is (search "aria-autocomplete=\"list\"" result))
    (is (search "id=\"search-results\"" result))
    (is (search "role=\"listbox\"" result))
    (is (search "autocomplete-loading" result))))

(test render-autocomplete-custom-options
  "render-autocomplete respects custom options"
  (let ((result (render-autocomplete
                  :id "users"
                  :endpoint "/api/users"
                  :placeholder "Find users..."
                  :debounce 500
                  :class "my-class")))
    (is (search "placeholder=\"Find users...\"" result))
    (is (search "delay:500ms" result))
    (is (search "autocomplete-container my-class" result))))

(test render-autocomplete-results-with-items
  "render-autocomplete-results renders list items with ARIA"
  (let ((result (render-autocomplete-results
                  '("Alice" "Bob" "Charlie")
                  :id "search")))
    (is (stringp result))
    (is (search "<ul" result))
    (is (search "role=\"listbox\"" result))
    (is (search "role=\"option\"" result))
    (is (search "aria-selected=\"false\"" result))
    (is (search "id=\"search-option-0\"" result))
    (is (search "id=\"search-option-1\"" result))
    (is (search "id=\"search-option-2\"" result))
    (is (search "Alice" result))
    (is (search "Bob" result))
    (is (search "Charlie" result))))

(test render-autocomplete-results-empty
  "render-autocomplete-results handles empty list"
  (let ((result (render-autocomplete-results nil :id "search")))
    (is (stringp result))
    (is (search "autocomplete-empty" result))
    (is (search "No results found" result))))

(test render-autocomplete-results-custom-empty-message
  "render-autocomplete-results allows custom empty message"
  (let ((result (render-autocomplete-results
                  nil
                  :id "search"
                  :empty-message "Try different keywords")))
    (is (search "Try different keywords" result))))

(test render-autocomplete-results-custom-render-item
  "render-autocomplete-results uses custom render function"
  (let ((result (render-autocomplete-results
                  '((:name "Alice" :role "Admin")
                    (:name "Bob" :role "User"))
                  :id "users"
                  :render-item (lambda (user)
                                 (format nil "~a (~a)"
                                         (getf user :name)
                                         (getf user :role))))))
    (is (search "Alice (Admin)" result))
    (is (search "Bob (User)" result))))

(test render-autocomplete-results-escapes-bare-string-render
  "When render-item returns a bare string, the render boundary routes it
   through coerce-html-emit; attacker-controlled markup is escaped, not
   emitted verbatim. Producers that want raw markup must return a
   safe-html-string."
  (let ((result (render-autocomplete-results
                  (list "innocuous" "<script>alert(1)</script>")
                  :id "search")))
    (is (search "innocuous" result))
    (is (search "&lt;script&gt;" result)
        "raw <script> must render escaped at the autocomplete render boundary")
    (is (null (search "<script>alert(1)" result))
        "unescaped payload must not appear in the result list")))

(test render-autocomplete-results-empty-message-escapes-bare-string
  "An empty-message bare string is escaped at the render boundary.
   safe-html-string wrapping is the opt-in for raw emission."
  (let ((result (render-autocomplete-results
                  nil
                  :id "search"
                  :empty-message "<img src=x onerror=alert(1)>")))
    (is (search "&lt;img" result)
        "raw < must be escaped in the empty-message slot")
    (is (null (search "<img src=x" result))
        "unescaped img tag must not reach the DOM")))

(test render-autocomplete-results-accepts-safe-html-string
  "A safe-html-string from render-item bypasses the escape — the
   producer's safety claim is honoured at the render boundary."
  (let ((result (render-autocomplete-results
                  (list "marker-x")
                  :id "search"
                  :render-item (lambda (item)
                                 (lol-web/html:make-safe-html-string
                                  (format nil "<em>~A</em>" item))))))
    (is (search "<em>marker-x</em>" result)
        "safe-html-string passes through coerce-html-emit unchanged")))

(test render-autocomplete-results-with-oob
  "render-autocomplete-results works with render-with-oob"
  (let ((result (render-with-oob
                  (lol-web/html:make-safe-html-string
                   (render-autocomplete-results
                     '("Result 1" "Result 2")
                     :id "search"))
                  (list "count" (lol-web/html:make-safe-html-string "2 results")))))
    (is (search "Result 1" result))
    (is (search "Result 2" result))
    (is (search "hx-swap-oob" result))
    (is (search "2 results" result))))

(test autocomplete-css-uses-design-tokens
  "autocomplete-css uses CSS variables from design system"
  (let ((css (autocomplete-css)))
    (is (stringp css))
    (is (search "var(--color-surface)" css))
    (is (search "var(--color-surface-alt)" css))
    (is (search "var(--color-text)" css))
    (is (search "var(--color-muted)" css))
    (is (search "var(--color-primary)" css))
    (is (search "var(--space-1)" css))
    (is (search "var(--space-2)" css))
    (is (search "var(--space-3)" css))
    (is (search "var(--effect-shadow-md)" css))
    (is (search "var(--effect-z-modal)" css))))

(test htmx-runtime-includes-sync-support
  "htmx-runtime-js includes hx-sync AbortController support"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "abortControllers" js))
    (is (search "AbortController" js))
    (is (search "syncStrategy" js))
    (is (search "AbortError" js))))

(test htmx-runtime-includes-keyboard-nav
  "htmx-runtime-js includes keyboard navigation for autocomplete"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "setupAutocomplete" js))
    (is (search "ArrowDown" js))
    (is (search "ArrowUp" js))
    (is (search "highlightOption" js))
    (is (search "clearHighlights" js))
    (is (search "MutationObserver" js))))

;;; ============================================================================
;;; IntersectionObserver triggers
;;; ============================================================================

(test htmx-runtime-includes-intersection-observer
  "htmx-runtime-js includes IntersectionObserver infrastructure"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "observers" js))
    (is (search "IntersectionObserver" js))
    (is (search "setupIntersectionObserver" js))
    (is (search "disconnectObserver" js))))

(test htmx-runtime-handles-revealed-trigger
  "htmx-runtime-js routes revealed trigger to IntersectionObserver"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "revealed" js))
    (is (search "intersect" js))
    (is (search "setupIntersectionObserver" js))))

(test htmx-runtime-handles-load-trigger
  "htmx-runtime-js handles load trigger for immediate execution"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "'load'" js))))

;;; ============================================================================
;;; Keepalive
;;; ============================================================================

(test htmx-runtime-includes-keepalive-support
  "htmx-runtime-js reads hx-keepalive attribute and passes to fetch"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "hx-keepalive" js))
    (is (search "keepalive" js))))

(test htmx-runtime-version-0-3-1
  "htmx-runtime-js reports version 0.3.1"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "0.3.1" js))))

;;; ============================================================================
;;; Event lifecycle
;;; ============================================================================

(test htmx-runtime-dispatches-config-request
  "htmx:configRequest event dispatched with headers and verb"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:configRequest" js))
    (is (search "'headers'" js))
    (is (search "'verb'" js))))

(test htmx-runtime-dispatches-before-request
  "htmx:beforeRequest is cancelable via preventDefault"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:beforeRequest" js))
    (is (search "'cancelable' : true" js))))

(test htmx-runtime-dispatches-response-error
  "htmx:responseError dispatched on non-ok HTTP responses"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:responseError" js))
    (is (search "response.status" js))))

(test htmx-runtime-dispatches-before-swap
  "htmx:beforeSwap is cancelable with shouldSwap flag"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:beforeSwap" js))
    (is (search "should-swap" js))
    (is (search "server-response" js))))

(test htmx-runtime-dispatches-after-settle
  "htmx:afterSettle dispatched after DOM settling phase"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:afterSettle" js))))

(test htmx-runtime-dispatches-after-request
  "htmx:afterRequest dispatched in finally with success tracking"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:afterRequest" js))
    (is (search "requestSucceeded" js))
    (is (search "'successful'" js))
    (is (search "'failed'" js))))

(test htmx-runtime-dispatches-send-error
  "htmx:sendError dispatched for network errors"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:sendError" js))))

(test htmx-runtime-dispatches-load-event
  "htmx:load dispatched on new content in both swap and OOB paths"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (let ((first-pos (search "htmx:load" js)))
      (is (not (null first-pos)))
      (is (not (null (search "htmx:load" js :start2 (1+ first-pos))))))))

(test htmx-runtime-event-ordering
  "Events are dispatched in correct lifecycle order"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (let ((config-pos (search "htmx:configRequest" js))
          (before-pos (search "htmx:beforeRequest" js))
          (error-pos (search "htmx:responseError" js))
          (swap-pos (search "htmx:beforeSwap" js))
          (after-swap-pos (search "htmx:afterSwap" js))
          (settle-pos (search "htmx:afterSettle" js))
          (after-req-pos (search "htmx:afterRequest" js)))
      (is (< config-pos before-pos))
      (is (< before-pos error-pos))
      (is (< error-pos swap-pos))
      (is (< swap-pos after-swap-pos))
      (is (< after-swap-pos settle-pos))
      (is (< settle-pos after-req-pos)))))

;;; ============================================================================
;;; HX-ON-* init
;;; ============================================================================

(test htmx-runtime-scans-all-hx-on-attributes
  "Init scans all elements for hx-on-* not just hardcoded selectors"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "startsWith('hx-on')" js))
    (is (not (search "[hx-on-htmx-after-swap]" js)))))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(test htmx-runtime-exposes-process-method
  "htmx.process() exposed for dynamic content initialization"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "'process'" js))
    (is (search "processElement" js))
    (is (search "processHxOn" js))))

(test htmx-runtime-exposes-ajax-method
  "htmx.ajax() exposed for programmatic requests"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "'ajax'" js))
    (is (search "issueRequest" js))))

(test htmx-runtime-exposes-trigger-method
  "htmx.trigger() exposed for custom event dispatch"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "'trigger'" js))))

(test htmx-runtime-exposes-on-off-methods
  "htmx.on() and htmx.off() exposed for event handling"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "'on'" js))
    (is (search "'off'" js))
    (is (search "addEventListener" js))
    (is (search "removeEventListener" js))))

(test htmx-runtime-exposes-on-load-method
  "htmx.onLoad() registers callbacks for htmx:load events"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "'onLoad'" js))
    (is (search "addEventListener('htmx:load'" js))))

;;; ============================================================================
;;; Timeout
;;; ============================================================================

(test htmx-runtime-supports-timeout
  "Request timeout via config.timeout with AbortController"
  (let ((js (lol-web/html:safe-html-string-value (htmx-runtime-js))))
    (is (search "htmx:timeout" js))
    (is (search "clearTimeout" js))
    (is (search "HTMX.config.timeout" js))))
