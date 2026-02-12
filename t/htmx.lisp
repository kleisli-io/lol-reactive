;;;; LOL-REACTIVE Test Suite - HTMX Integration
;;;;
;;;; Tests for HTMX runtime, OOB swaps, and request handling.

(in-package :lol-reactive.tests)
(in-suite :htmx)

;;; ============================================================================
;;; OOB-SWAP TESTS
;;; ============================================================================

(test oob-swap-basic
  "oob-swap wraps content with target ID and swap attribute"
  (let ((result (lol-reactive:oob-swap "my-target" "<p>content</p>")))
    (is (stringp result))
    (is (search "id=\"my-target\"" result))
    (is (search "hx-swap-oob" result))
    (is (search "content" result))))

(test oob-swap-with-strategy
  "oob-swap respects swap strategy"
  (let ((result (lol-reactive:oob-swap "target" "content" :swap "innerHTML")))
    (is (search "innerHTML" result))))

(test oob-swap-default-true
  "oob-swap defaults to true strategy"
  (let ((result (lol-reactive:oob-swap "target" "content")))
    (is (search "true" result))))

;;; ============================================================================
;;; RENDER-WITH-OOB TESTS
;;; ============================================================================

(test render-with-oob-nil-primary
  "render-with-oob handles nil primary content"
  (let ((result (lol-reactive:render-with-oob
                  nil
                  (list "count" "5")
                  (list "total" "$10.00"))))
    (is (stringp result))
    (is (search "count" result))
    (is (search "total" result))))

(test render-with-oob-multiple-targets
  "render-with-oob includes all OOB targets"
  (let ((result (lol-reactive:render-with-oob
                  "<main>primary</main>"
                  (list "header" "Header content")
                  (list "footer" "Footer content")
                  (list "sidebar" "Sidebar content"))))
    (is (search "primary" result))
    (is (search "header" result))
    (is (search "footer" result))
    (is (search "sidebar" result))))

;;; ============================================================================
;;; HTMX RUNTIME JS TESTS
;;; ============================================================================

(test htmx-runtime-js-generates-js
  "htmx-runtime-js generates JavaScript code"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (stringp js))
    ;; Should contain HTMX global
    (is (search "htmx" js :test #'char-equal))
    ;; Should contain key functions
    (is (search "processElement" js))
    (is (search "issueRequest" js))))

;;; ============================================================================
;;; HTMX ATTRIBUTE HELPERS
;;; ============================================================================

(test hx-get-generates-attr
  "hx-get generates correct attribute string"
  (let ((result (lol-reactive:hx-get "/api/data")))
    (is (stringp result))
    (is (search "hx-get" result))
    (is (search "/api/data" result))))

(test hx-post-generates-attr
  "hx-post generates correct attribute string"
  (let ((result (lol-reactive:hx-post "/api/submit")))
    (is (stringp result))
    (is (search "hx-post" result))
    (is (search "/api/submit" result))))

(test hx-post-with-target
  "hx-post includes target when specified"
  (let ((result (lol-reactive:hx-post "/api/submit" :target "#result")))
    (is (search "hx-target" result))
    (is (search "#result" result))))

;;; ============================================================================
;;; AUTOCOMPLETE TESTS
;;; ============================================================================

(test render-autocomplete-generates-html
  "render-autocomplete generates complete autocomplete widget HTML"
  (let ((result (lol-reactive:render-autocomplete :id "search" :endpoint "/api/search")))
    (is (stringp result))
    ;; Contains input with correct attributes
    (is (search "type=\"search\"" result))
    (is (search "id=\"search\"" result))
    (is (search "hx-get=\"/api/search\"" result))
    ;; Contains hx-sync for race condition prevention
    (is (search "hx-sync=\"this:replace\"" result))
    ;; Contains ARIA attributes
    (is (search "role=\"combobox\"" result))
    (is (search "aria-controls=\"search-results\"" result))
    (is (search "aria-expanded=\"false\"" result))
    (is (search "aria-autocomplete=\"list\"" result))
    ;; Contains results container
    (is (search "id=\"search-results\"" result))
    (is (search "role=\"listbox\"" result))
    ;; Contains loading indicator
    (is (search "autocomplete-loading" result))))

(test render-autocomplete-custom-options
  "render-autocomplete respects custom options"
  (let ((result (lol-reactive:render-autocomplete
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
  (let ((result (lol-reactive:render-autocomplete-results
                  '("Alice" "Bob" "Charlie")
                  :id "search")))
    (is (stringp result))
    ;; Contains list structure
    (is (search "<ul" result))
    (is (search "role=\"listbox\"" result))
    ;; Contains items with correct attributes
    (is (search "role=\"option\"" result))
    (is (search "aria-selected=\"false\"" result))
    (is (search "id=\"search-option-0\"" result))
    (is (search "id=\"search-option-1\"" result))
    (is (search "id=\"search-option-2\"" result))
    ;; Contains actual content
    (is (search "Alice" result))
    (is (search "Bob" result))
    (is (search "Charlie" result))))

(test render-autocomplete-results-empty
  "render-autocomplete-results handles empty list"
  (let ((result (lol-reactive:render-autocomplete-results nil :id "search")))
    (is (stringp result))
    (is (search "autocomplete-empty" result))
    (is (search "No results found" result))))

(test render-autocomplete-results-custom-empty-message
  "render-autocomplete-results allows custom empty message"
  (let ((result (lol-reactive:render-autocomplete-results
                  nil
                  :id "search"
                  :empty-message "Try different keywords")))
    (is (search "Try different keywords" result))))

(test render-autocomplete-results-custom-render-item
  "render-autocomplete-results uses custom render function"
  (let ((result (lol-reactive:render-autocomplete-results
                  '((:name "Alice" :role "Admin")
                    (:name "Bob" :role "User"))
                  :id "users"
                  :render-item (lambda (user)
                                 (format nil "~a (~a)"
                                         (getf user :name)
                                         (getf user :role))))))
    (is (search "Alice (Admin)" result))
    (is (search "Bob (User)" result))))

(test render-autocomplete-results-with-oob
  "render-autocomplete-results works with render-with-oob"
  (let ((result (lol-reactive:render-with-oob
                  (lol-reactive:render-autocomplete-results
                    '("Result 1" "Result 2")
                    :id "search")
                  (list "count" "2 results"))))
    ;; Main content
    (is (search "Result 1" result))
    (is (search "Result 2" result))
    ;; OOB update
    (is (search "hx-swap-oob" result))
    (is (search "2 results" result))))

(test autocomplete-css-uses-design-tokens
  "autocomplete-css uses CSS variables from design system"
  (let ((css (lol-reactive:autocomplete-css)))
    (is (stringp css))
    ;; Uses color tokens
    (is (search "var(--color-surface)" css))
    (is (search "var(--color-surface-alt)" css))
    (is (search "var(--color-text)" css))
    (is (search "var(--color-muted)" css))
    (is (search "var(--color-primary)" css))
    ;; Uses spacing tokens
    (is (search "var(--space-1)" css))
    (is (search "var(--space-2)" css))
    (is (search "var(--space-3)" css))
    ;; Uses effect tokens
    (is (search "var(--effect-shadow-md)" css))
    (is (search "var(--effect-z-modal)" css))))

(test htmx-runtime-includes-sync-support
  "htmx-runtime-js includes hx-sync AbortController support"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; Contains AbortController infrastructure
    (is (search "abortControllers" js))
    (is (search "AbortController" js))
    ;; Contains sync strategy handling
    (is (search "syncStrategy" js))
    ;; Handles abort errors gracefully
    (is (search "AbortError" js))))

(test htmx-runtime-includes-keyboard-nav
  "htmx-runtime-js includes keyboard navigation for autocomplete"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; Contains setupAutocomplete function
    (is (search "setupAutocomplete" js))
    ;; Contains keyboard handlers
    (is (search "ArrowDown" js))
    (is (search "ArrowUp" js))
    ;; Contains highlight functions
    (is (search "highlightOption" js))
    (is (search "clearHighlights" js))
    ;; Contains MutationObserver for result updates
    (is (search "MutationObserver" js))))

;;; ============================================================================
;;; INTERSECTIONOBSERVER TRIGGER TESTS
;;; ============================================================================

(test htmx-runtime-includes-intersection-observer
  "htmx-runtime-js includes IntersectionObserver infrastructure"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; Contains observers storage
    (is (search "observers" js))
    ;; Contains IntersectionObserver API usage
    (is (search "IntersectionObserver" js))
    ;; Contains setup function
    (is (search "setupIntersectionObserver" js))
    ;; Contains cleanup function
    (is (search "disconnectObserver" js))))

(test htmx-runtime-handles-revealed-trigger
  "htmx-runtime-js routes revealed trigger to IntersectionObserver"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; addTriggerHandler checks for revealed
    (is (search "revealed" js))
    ;; Also supports intersect alias
    (is (search "intersect" js))
    ;; Routes to setupIntersectionObserver
    (is (search "setupIntersectionObserver" js))))

(test htmx-runtime-handles-load-trigger
  "htmx-runtime-js handles load trigger for immediate execution"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; addTriggerHandler checks for load event
    (is (search "'load'" js))))

;;; ============================================================================
;;; KEEPALIVE SUPPORT TESTS
;;; ============================================================================

(test htmx-runtime-includes-keepalive-support
  "htmx-runtime-js reads hx-keepalive attribute and passes to fetch"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; Reads hx-keepalive from element
    (is (search "hx-keepalive" js))
    ;; Passes keepalive boolean to fetch options
    (is (search "keepalive" js))))

(test htmx-runtime-version-0-3-1
  "htmx-runtime-js reports version 0.3.1"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "0.3.1" js))))

;;; ============================================================================
;;; EVENT LIFECYCLE TESTS
;;; ============================================================================

(test htmx-runtime-dispatches-config-request
  "htmx:configRequest event dispatched with headers and verb"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:configRequest" js))
    (is (search "'headers'" js))
    (is (search "'verb'" js))))

(test htmx-runtime-dispatches-before-request
  "htmx:beforeRequest is cancelable via preventDefault"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:beforeRequest" js))
    (is (search "'cancelable' : true" js))))

(test htmx-runtime-dispatches-response-error
  "htmx:responseError dispatched on non-ok HTTP responses"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:responseError" js))
    (is (search "response.status" js))))

(test htmx-runtime-dispatches-before-swap
  "htmx:beforeSwap is cancelable with shouldSwap flag"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:beforeSwap" js))
    (is (search "should-swap" js))
    (is (search "server-response" js))))

(test htmx-runtime-dispatches-after-settle
  "htmx:afterSettle dispatched after DOM settling phase"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:afterSettle" js))))

(test htmx-runtime-dispatches-after-request
  "htmx:afterRequest dispatched in finally with success tracking"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:afterRequest" js))
    (is (search "requestSucceeded" js))
    (is (search "'successful'" js))
    (is (search "'failed'" js))))

(test htmx-runtime-dispatches-send-error
  "htmx:sendError dispatched for network errors"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:sendError" js))))

(test htmx-runtime-dispatches-load-event
  "htmx:load dispatched on new content in both swap and OOB paths"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; Should appear at least twice (issueRequest + processOobSwaps)
    (let ((first-pos (search "htmx:load" js)))
      (is (not (null first-pos)))
      (is (not (null (search "htmx:load" js :start2 (1+ first-pos))))))))

(test htmx-runtime-event-ordering
  "Events are dispatched in correct lifecycle order"
  (let ((js (lol-reactive:htmx-runtime-js)))
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
;;; HX-ON-* INIT FIX TESTS
;;; ============================================================================

(test htmx-runtime-scans-all-hx-on-attributes
  "Init scans all elements for hx-on-* not just hardcoded selectors"
  (let ((js (lol-reactive:htmx-runtime-js)))
    ;; General attribute prefix scan
    (is (search "startsWith('hx-on')" js))
    ;; Old hardcoded selectors should be gone
    (is (not (search "[hx-on-htmx-after-swap]" js)))))

;;; ============================================================================
;;; PUBLIC API TESTS
;;; ============================================================================

(test htmx-runtime-exposes-process-method
  "htmx.process() exposed for dynamic content initialization"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "'process'" js))
    ;; process should handle both verb elements and hx-on-*
    (is (search "processElement" js))
    (is (search "processHxOn" js))))

(test htmx-runtime-exposes-ajax-method
  "htmx.ajax() exposed for programmatic requests"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "'ajax'" js))
    ;; Should call issueRequest
    (is (search "issueRequest" js))))

(test htmx-runtime-exposes-trigger-method
  "htmx.trigger() exposed for custom event dispatch"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "'trigger'" js))))

(test htmx-runtime-exposes-on-off-methods
  "htmx.on() and htmx.off() exposed for event handling"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "'on'" js))
    (is (search "'off'" js))
    (is (search "addEventListener" js))
    (is (search "removeEventListener" js))))

(test htmx-runtime-exposes-on-load-method
  "htmx.onLoad() registers callbacks for htmx:load events"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "'onLoad'" js))
    (is (search "addEventListener('htmx:load'" js))))

;;; ============================================================================
;;; TIMEOUT TESTS
;;; ============================================================================

(test htmx-runtime-supports-timeout
  "Request timeout via config.timeout with AbortController"
  (let ((js (lol-reactive:htmx-runtime-js)))
    (is (search "htmx:timeout" js))
    (is (search "clearTimeout" js))
    (is (search "HTMX.config.timeout" js))))
