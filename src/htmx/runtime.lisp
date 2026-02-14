;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; HTMX-style client runtime for declarative DOM updates
;;;;
;;;; Provides AJAX request handling, swap strategies, and OOB updates
;;;; via Parenscript-generated JavaScript.

(in-package :lol-reactive)

;;; ============================================================================
;;; HTMX RUNTIME (Parenscript)
;;;
;;; Client-side runtime that processes hx-* attributes and performs
;;; partial page updates. All JavaScript generated via Parenscript.
;;; ============================================================================

(defun htmx-runtime-js ()
  "Generate the HTMX-style client runtime via Parenscript.
   Processes hx-get, hx-post, hx-swap, hx-target, hx-trigger, hx-sync attributes.
   Supports 9 swap strategies, out-of-band updates, request cancellation, and autocomplete."
  (parenscript:ps
    ;; HTMX Runtime Object
    (defvar *htmx*
      (ps:create
       "version" "0.3.1"
       "config" (ps:create
                "defaultSwapStyle" "innerHTML"
                "defaultSettleDelay" 20
                "withCredentials" nil
                "timeout" 0)

       ;; AbortController storage for hx-sync support
       "abortControllers" (ps:create)

       ;; IntersectionObserver storage for revealed/intersect triggers
       "observers" (ps:create)

       ;; Swap Strategies
       "swap" (lambda (target html swap-style)
               (let ((style (or swap-style (ps:@ *htmx* config default-swap-style))))
                 (cond
                   ((= style "innerHTML")
                    (setf (ps:@ target inner-h-t-m-l) html))
                   ((= style "outerHTML")
                    (setf (ps:@ target outer-h-t-m-l) html))
                   ((= style "beforebegin")
                    (ps:chain target (insert-adjacent-h-t-m-l "beforebegin" html)))
                   ((= style "afterbegin")
                    (ps:chain target (insert-adjacent-h-t-m-l "afterbegin" html)))
                   ((= style "beforeend")
                    (ps:chain target (insert-adjacent-h-t-m-l "beforeend" html)))
                   ((= style "afterend")
                    (ps:chain target (insert-adjacent-h-t-m-l "afterend" html)))
                   ((= style "textContent")
                    (setf (ps:@ target text-content) html))
                   ((= style "delete")
                    (ps:chain target (remove)))
                   ((= style "none")
                    nil)
                   (t
                    (setf (ps:@ target inner-h-t-m-l) html)))))

       ;; Out-of-Band Swap Processing
       "processOobSwaps" (lambda (response-html)
                            (let ((temp (ps:chain document (create-element "div"))))
                              (setf (ps:@ temp inner-h-t-m-l) response-html)
                              (let ((oob-elements (ps:chain temp (query-selector-all "[hx-swap-oob]"))))
                                (ps:chain oob-elements (for-each
                                                        (lambda (el)
                                                          (let* ((oob-value (ps:chain el (get-attribute "hx-swap-oob")))
                                                                 (target-id (ps:@ el id))
                                                                 (target (ps:chain document (get-element-by-id target-id))))
                                                            (when target
                                                              (ps:chain el (remove-attribute "hx-swap-oob"))
                                                              (let ((strategy (if (or (= oob-value "true")
                                                                                      (= oob-value ""))
                                                                                  "outerHTML"
                                                                                  oob-value)))
                                                                ;; For outerHTML, preserve dynamic classes from target
                                                                (when (= strategy "outerHTML")
                                                                  ;; Copy dynamic state classes (e.g., "open") to new element
                                                                  (when (ps:chain target class-list (contains "open"))
                                                                    (ps:chain el class-list (add "open"))))
                                                                ;; Perform the swap
                                                                (if (= strategy "outerHTML")
                                                                    ((ps:@ *htmx* swap) target (ps:@ el outer-h-t-m-l) strategy)
                                                                    ((ps:@ *htmx* swap) target (ps:@ el inner-h-t-m-l) strategy))
                                                                ;; Re-initialize HTMX on the new element
                                                                (let ((new-el (ps:chain document (get-element-by-id target-id))))
                                                                  (when new-el
                                                                    ;; Process the element itself
                                                                    ((ps:@ *htmx* process-element) new-el)
                                                                    ;; Process any children with hx-* attributes
                                                                    (let ((htmx-children (ps:chain new-el
                                                                                           (query-selector-all
                                                                                            "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                                                                      (ps:chain htmx-children
                                                                                (for-each (ps:@ *htmx* process-element))))
                                                                    ;; Dispatch htmx:load on new OOB content
                                                                    ((ps:@ *htmx* dispatch-event) "htmx:load" new-el
                                                                     (ps:create :elt new-el))))))
                                                            (ps:chain el (remove)))))))
                              (ps:@ temp inner-h-t-m-l)))

       ;; Request Handling with hx-sync support and form serialization
       "issueRequest" (lambda (element method url)
                        (let* ((target-selector (ps:chain element (get-attribute "hx-target")))
                               (target (if target-selector
                                           (if (= (ps:chain target-selector (char-at 0)) "#")
                                               (ps:chain document (get-element-by-id
                                                                   (ps:chain target-selector (substring 1))))
                                               (ps:chain document (query-selector target-selector)))
                                           element))
                               (swap-style (or (ps:chain element (get-attribute "hx-swap"))
                                               (ps:@ *htmx* config default-swap-style)))
                               (headers (ps:create
                                         "HX-Request" "true"
                                         "HX-Trigger" (or (ps:@ element id) "")
                                         "HX-Target" (or (and target (ps:@ target id)) "")
                                         "HX-Current-URL" (ps:@ window location href)))
                               ;; Form serialization: find form element
                               ;; hx-include: pull values from another element/form
                               (include-selector (ps:chain element (get-attribute "hx-include")))
                               (include-el (when include-selector
                                             (ps:chain document (query-selector include-selector))))
                               (form (cond
                                       ;; If hx-include points to a form, use that
                                       ((and include-el
                                             (= (ps:chain (ps:@ include-el tag-name) (to-lower-case)) "form"))
                                        include-el)
                                       ;; Element is itself a form
                                       ((= (ps:chain (ps:@ element tag-name) (to-lower-case)) "form")
                                        element)
                                       ;; Nearest ancestor form
                                       (t (ps:chain element (closest "form")))))
                               ;; For GET: append element values as query parameters
                               (get-url (when (= method "GET")
                                          (if (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "form")
                                              ;; Form GET: serialize all form inputs
                                              (let ((params (ps:new (-U-R-L-Search-Params
                                                                     (ps:new (-Form-Data element))))))
                                                (let ((qs (ps:chain params (to-string))))
                                                  (if (> (ps:@ qs length) 0)
                                                      (+ url "?" qs)
                                                      url)))
                                              ;; Input GET: include element's own name=value
                                              (let ((input-name (ps:chain element (get-attribute "name")))
                                                    (input-value (ps:@ element value)))
                                                (if (and input-name input-value
                                                         (> (ps:@ input-value length) 0))
                                                    (+ url "?"
                                                       (encode-u-r-i-component input-name) "="
                                                       (encode-u-r-i-component input-value))
                                                    url)))))
                               ;; Create FormData for POST/PUT/DELETE
                               (body (when (not (= method "GET"))
                                       (let ((fd (if form
                                                     (ps:new (-Form-Data form))
                                                     ;; Standalone input: include own name=value
                                                     (let ((input-name (ps:chain element (get-attribute "name")))
                                                           (input-value (ps:@ element value)))
                                                       (when (and input-name input-value)
                                                         (let ((fd (ps:new (-Form-Data))))
                                                           (ps:chain fd (append input-name input-value))
                                                           fd))))))
                                         ;; Append CSRF token from meta tag (if present)
                                         (when fd
                                           (let ((csrf-meta (ps:chain document
                                                              (query-selector "meta[name=csrf-token]"))))
                                             (when csrf-meta
                                               (let ((token (ps:chain csrf-meta (get-attribute "content"))))
                                                 (when token
                                                   (ps:chain fd (append "csrf-token" token)))))))
                                         fd)))
                               ;; hx-sync support: parse "this:replace" or "this:drop" etc.
                               (sync-attr (ps:chain element (get-attribute "hx-sync")))
                               (sync-strategy (when sync-attr
                                                (let ((parts (ps:chain sync-attr (split ":"))))
                                                  (if (> (ps:@ parts length) 1)
                                                      (aref parts 1)
                                                      "replace"))))
                               (element-id (or (ps:@ element id) "htmx-default"))
                               (existing-controller (ps:getprop (ps:@ *htmx* abort-controllers) element-id))
                               ;; Determine if we should proceed with request
                               (should-proceed t)
                               ;; hx-keepalive: survive page navigation (e.g. blur-save)
                               (keepalive (ps:chain element (get-attribute "hx-keepalive")))
                               ;; Track request outcome for htmx:afterRequest
                               (request-succeeded false))
                          ;; Handle sync strategies
                          (when (and existing-controller sync-strategy)
                            (cond
                              ;; replace/abort: cancel existing, start new
                              ((or (= sync-strategy "replace")
                                   (= sync-strategy "abort"))
                               (ps:chain existing-controller (abort)))
                              ;; drop: don't start new request if one in flight
                              ((= sync-strategy "drop")
                               (setf should-proceed nil))))
                          ;; Only proceed if not dropped
                          (when should-proceed
                            ;; Create new AbortController for this request
                            (let ((controller (ps:new (-Abort-Controller)))
                                  (timeout-id nil))
                              (setf (ps:getprop (ps:@ *htmx* abort-controllers) element-id) controller)
                              ;; Set up request timeout if configured
                              (when (> (ps:@ *htmx* config timeout) 0)
                                (setf timeout-id
                                      (set-timeout
                                       (lambda ()
                                         (ps:chain controller (abort))
                                         ((ps:@ *htmx* dispatch-event) "htmx:timeout" element
                                          (ps:create :elt element :target target)))
                                       (ps:@ *htmx* config timeout))))
                              (ps:chain element class-list (add "htmx-request"))
                              ;; Dispatch htmx:configRequest - listeners can modify headers/params
                              ((ps:@ *htmx* dispatch-event) "htmx:configRequest" element
                               (ps:create :headers headers :elt element :target target :verb method))
                              ;; Dispatch htmx:beforeRequest (cancelable) - last chance to cancel
                              (let ((before-event (ps:new (-Custom-Event "htmx:beforeRequest"
                                                           (ps:create :bubbles t :cancelable t
                                                                      :detail (ps:create :elt element :target target))))))
                                (if (ps:chain element (dispatch-event before-event))
                                    ;; Not cancelled - proceed with request
                                    (progn
                              ;; Build fetch options - include body for non-GET with form data
                              ;; Note: Don't set Content-Type for FormData; browser sets it with boundary
                              (ps:chain
                               (fetch (or get-url url) (ps:create
                                           :method method
                                           :headers headers
                                           :body body
                                           :credentials (if (ps:@ *htmx* config with-credentials)
                                                            "include"
                                                            "same-origin")
                                           :signal (ps:@ controller signal)
                                           :keepalive (if keepalive true false)))
                               (then (lambda (response)
                                       (if (ps:@ response ok)
                                           (progn
                                             (setf request-succeeded t)
                                             (ps:chain response (text)))
                                           (progn
                                             ;; Dispatch htmx:responseError for HTTP errors
                                             ((ps:@ *htmx* dispatch-event) "htmx:responseError" element
                                              (ps:create :elt element :target target
                                                         :status (ps:@ response status)
                                                         :status-text (ps:@ response status-text)))
                                             (ps:chain -promise (reject (ps:@ response status-text)))))))
                               (then (lambda (html)
                                       (let ((remaining-html ((ps:@ *htmx* process-oob-swaps) html)))
                                         ;; Dispatch htmx:beforeSwap (cancelable, with shouldSwap flag)
                                         (let ((before-swap-event
                                                 (ps:new (-Custom-Event "htmx:beforeSwap"
                                                           (ps:create :bubbles t :cancelable t
                                                                      :detail (ps:create
                                                                               :elt element :target target
                                                                               :server-response remaining-html
                                                                               :should-swap t))))))
                                           (let ((not-cancelled (ps:chain element (dispatch-event before-swap-event))))
                                             (when (and not-cancelled
                                                        (aref (ps:@ before-swap-event detail) "should-swap")
                                                        target
                                                        (not (= swap-style "none")))
                                               (set-timeout
                                                (lambda ()
                                                  ;; For outerHTML, target is replaced - capture parent + id first
                                                  (let ((parent (ps:@ target parent-node))
                                                        (target-id (ps:@ target id)))
                                                    ((ps:@ *htmx* swap) target remaining-html swap-style)
                                                    ;; Re-initialize HTMX on new elements
                                                    (let ((scope (if (and (= swap-style "outerHTML") target-id)
                                                                     ;; Try to find new element with same ID
                                                                     (or (ps:chain document (get-element-by-id target-id))
                                                                         parent)
                                                                     target)))
                                                      (when scope
                                                        ;; Process the scope element itself if it has hx-* attrs
                                                        (when (ps:chain scope (get-attribute "hx-get"))
                                                          ((ps:@ *htmx* process-element) scope))
                                                        ;; Process any children with hx-* attributes
                                                        (let ((htmx-children (ps:chain scope
                                                                               (query-selector-all
                                                                                "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                                                          (ps:chain htmx-children
                                                                    (for-each (ps:@ *htmx* process-element))))))
                                                    ;; Dispatch htmx:afterSwap on the requesting element
                                                    ((ps:@ *htmx* dispatch-event) "htmx:afterSwap" element
                                                     (ps:create :elt element :target target))
                                                    ;; Dispatch htmx:load on new content (for library initialization)
                                                    ((ps:@ *htmx* dispatch-event) "htmx:load" scope
                                                     (ps:create :elt scope))
                                                    ;; Dispatch htmx:afterSettle after DOM has settled
                                                    ((ps:@ *htmx* dispatch-event) "htmx:afterSettle" element
                                                     (ps:create :elt element :target target))))
                                                (ps:@ *htmx* config default-settle-delay))))))))
                               (catch (lambda (err)
                                        ;; Don't log abort errors (they're intentional)
                                        (unless (= (ps:@ err name) "AbortError")
                                          ;; Dispatch htmx:sendError for network failures
                                          ((ps:@ *htmx* dispatch-event) "htmx:sendError" element
                                           (ps:create :elt element :target target :error err))
                                          (ps:chain console (error "HTMX error:" err)))))
                               (finally (lambda ()
                                          ;; Clear request timeout
                                          (when timeout-id
                                            (clear-timeout timeout-id))
                                          ;; Dispatch htmx:afterRequest (always fires, success or failure)
                                          ((ps:@ *htmx* dispatch-event) "htmx:afterRequest" element
                                           (ps:create :elt element :target target
                                                      :successful request-succeeded
                                                      :failed (not request-succeeded)))
                                          (ps:chain element class-list (remove "htmx-request"))
                                          ;; Clean up controller if this is the current one
                                          (when (= (ps:getprop (ps:@ *htmx* abort-controllers) element-id)
                                                   controller)
                                            (delete (ps:getprop (ps:@ *htmx* abort-controllers) element-id))))))) ;; close: delete+when+finally-lambda+finally+catch-lambda+catch+chain
                                    ) ;; close progn (if true branch)
                                    ;; Cancelled by htmx:beforeRequest - clean up
                                    (progn
                                      (ps:chain element class-list (remove "htmx-request"))
                                      (delete (ps:getprop (ps:@ *htmx* abort-controllers) element-id))))) ;; close: progn+if+let(before-event)
                              )))  ;; close: let(controller)+when(should-proceed)+let*(issueRequest lambda)

       ;; Utility: convert dash-case to camelCase
       "dashToCamel" (lambda (str)
                       (ps:chain str
                         (replace (ps:regex "/-([a-z])/g")
                                  (lambda (match letter)
                                    (ps:chain letter (to-upper-case))))))

       ;; hx-on-* attribute processing
       ;; Supports: hx-on-click, hx-on-htmx-after-swap, hx-on--after-swap
       "processHxOn" (lambda (element)
                       (let ((attrs (ps:chain -array prototype slice
                                              (call (ps:chain element attributes)))))
                         (ps:chain attrs
                           (for-each
                             (lambda (attr)
                               (let ((name (ps:@ attr name)))
                                 (when (ps:chain name (starts-with "hx-on"))
                                   (let* ((suffix (ps:chain name (substring 5)))
                                          (event-name
                                            (cond
                                              ;; hx-on--after-swap → htmx:afterSwap
                                              ((ps:chain suffix (starts-with "--"))
                                               (+ "htmx:" ((ps:@ *htmx* dash-to-camel)
                                                            (ps:chain suffix (substring 2)))))
                                              ;; hx-on-htmx-after-swap → htmx:afterSwap
                                              ((ps:chain suffix (starts-with "-htmx-"))
                                               (+ "htmx:" ((ps:@ *htmx* dash-to-camel)
                                                            (ps:chain suffix (substring 6)))))
                                              ;; hx-on-click → click
                                              (t (ps:chain suffix (substring 1)))))
                                          (code (ps:@ attr value)))
                                     (ps:chain element
                                       (add-event-listener event-name
                                         (ps:new (-Function "event" code))))))))))))

       ;; Dispatch a CustomEvent on an element (bubbles up)
       "dispatchEvent" (lambda (name element detail)
                         (let ((event (ps:new (-Custom-Event name
                                               (ps:create :bubbles t
                                                          :cancelable t
                                                          :detail detail)))))
                           (ps:chain element (dispatch-event event))))

       ;; Element Processing
       "processElement" (lambda (element)
                          ;; Process hx-on-* event handler attributes
                          ((ps:@ *htmx* process-hx-on) element)
                          (let ((get-url (ps:chain element (get-attribute "hx-get"))))
                            (when get-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "GET" get-url))))))
                          (let ((post-url (ps:chain element (get-attribute "hx-post"))))
                            (when post-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "POST" post-url))))))
                          (let ((put-url (ps:chain element (get-attribute "hx-put"))))
                            (when put-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "PUT" put-url))))))
                          (let ((delete-url (ps:chain element (get-attribute "hx-delete"))))
                            (when delete-url
                              (let ((trigger ((ps:@ *htmx* parse-trigger)
                                              (or (ps:chain element (get-attribute "hx-trigger")) "click"))))
                                ((ps:@ *htmx* add-trigger-handler) element trigger
                                 (lambda () ((ps:@ *htmx* issue-request) element "DELETE" delete-url)))))))

       ;; Trigger Parsing
       "parseInterval" (lambda (str)
                         (cond
                           ((ps:chain str (ends-with "ms"))
                            (parse-int (ps:chain str (slice 0 -2))))
                           ((ps:chain str (ends-with "s"))
                            (* 1000 (parse-int (ps:chain str (slice 0 -1)))))
                           (t
                            (parse-int str))))

       "parseTrigger" (lambda (trigger-string)
                        (let ((parts (ps:chain trigger-string (split " ")))
                              (spec (ps:create
                                     :event "click"
                                     :delay nil
                                     :throttle nil
                                     :changed nil
                                     :once nil
                                     :from nil)))
                          (when (> (ps:@ parts length) 0)
                            (let ((first-part (aref parts 0)))
                              (if (or (ps:chain first-part (starts-with "delay:"))
                                      (ps:chain first-part (starts-with "throttle:"))
                                      (ps:chain first-part (starts-with "from:"))
                                      (= first-part "changed")
                                      (= first-part "once"))
                                  (setf (ps:@ spec event) "click")
                                  (setf (ps:@ spec event) first-part))))
                          (ps:chain parts (for-each
                                           (lambda (part)
                                             (cond
                                               ((ps:chain part (starts-with "delay:"))
                                                (setf (ps:@ spec delay)
                                                      ((ps:@ *htmx* parse-interval)
                                                       (ps:chain part (substring 6)))))
                                               ((ps:chain part (starts-with "throttle:"))
                                                (setf (ps:@ spec throttle)
                                                      ((ps:@ *htmx* parse-interval)
                                                       (ps:chain part (substring 9)))))
                                               ((ps:chain part (starts-with "from:"))
                                                (setf (ps:@ spec from)
                                                      (ps:chain part (substring 5))))
                                               ((= part "changed")
                                                (setf (ps:@ spec changed) t))
                                               ((= part "once")
                                                (setf (ps:@ spec once) t))))))
                          spec))

       "addTriggerHandler" (lambda (element trigger-spec handler)
                              (let ((event-name (ps:@ trigger-spec event))
                                    ;; from: modifier — listen on a different element
                                    (listen-on (if (ps:@ trigger-spec from)
                                                   (ps:chain document
                                                     (query-selector (ps:@ trigger-spec from)))
                                                   element)))
                                (unless listen-on
                                  (setf listen-on element))
                                (cond
                                  ;; IntersectionObserver triggers: revealed, intersect
                                  ((or (= event-name "revealed")
                                       (= event-name "intersect"))
                                   ((ps:@ *htmx* setup-intersection-observer)
                                    element
                                    (lambda (entry)
                                      (declare (ignore entry))
                                      ;; Respect delay modifier if specified
                                      (if (ps:@ trigger-spec delay)
                                          (set-timeout handler (ps:@ trigger-spec delay))
                                          (funcall handler)))
                                    (ps:create "threshold" 0.1
                                               "once" (ps:@ trigger-spec once))))

                                  ;; Load trigger: fire immediately (or after delay)
                                  ((= event-name "load")
                                   (if (ps:@ trigger-spec delay)
                                       (set-timeout handler (ps:@ trigger-spec delay))
                                       (funcall handler)))

                                  ;; Standard DOM events: use addEventListener
                                  (t
                                   (let ((timer nil)
                                         (last-value nil)
                                         (throttled nil)
                                         (fired nil))
                                     (ps:chain listen-on
                                               (add-event-listener
                                                event-name
                                                (lambda (event)
                                                  (when (or (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "a")
                                                            (= (ps:chain (ps:@ element tag-name) (to-lower-case)) "form"))
                                                    (ps:chain event (prevent-default)))
                                                  (let ((should-fire t))
                                                    (when (and (ps:@ trigger-spec once) fired)
                                                      (setf should-fire nil))
                                                    (when (and should-fire (ps:@ trigger-spec changed))
                                                      (let ((current-value (or (ps:@ element value)
                                                                               (ps:@ element inner-h-t-m-l))))
                                                        (if (= current-value last-value)
                                                            (setf should-fire nil)
                                                            (setf last-value current-value))))
                                                    (when (and should-fire (ps:@ trigger-spec throttle) throttled)
                                                      (setf should-fire nil))
                                                    (when should-fire
                                                      (if (ps:@ trigger-spec delay)
                                                          (progn
                                                            (clear-timeout timer)
                                                            (setf timer (set-timeout
                                                                         (lambda ()
                                                                           (setf fired t)
                                                                           (funcall handler))
                                                                         (ps:@ trigger-spec delay))))
                                                          (progn
                                                            (when (ps:@ trigger-spec throttle)
                                                              (setf throttled t)
                                                              (set-timeout
                                                               (lambda () (setf throttled nil))
                                                               (ps:@ trigger-spec throttle)))
                                                            (setf fired t)
                                                            (funcall handler)))))))))))))

       ;; ============================================================
       ;; Autocomplete / Keyboard Navigation Support
       ;; ============================================================

       ;; Setup autocomplete keyboard navigation for an input
       "setupAutocomplete" (lambda (input-id results-selector &optional options)
                             (let ((input (ps:chain document (get-element-by-id input-id)))
                                   (selected-index -1)
                                   (on-select (and options (ps:@ options on-select))))
                               (when input
                                 ;; Keyboard navigation handler
                                 (ps:chain input (add-event-listener "keydown"
                                   (lambda (event)
                                     (let* ((results-container (ps:chain document (query-selector results-selector)))
                                            (opts (if results-container
                                                      (ps:chain results-container (query-selector-all "[role=option]"))
                                                      (array)))
                                            (len (ps:@ opts length)))
                                       (when (> len 0)
                                         (cond
                                           ;; Arrow Down - navigate to next option
                                           ((= (ps:@ event key) "ArrowDown")
                                            (ps:chain event (prevent-default))
                                            (setf selected-index (if (>= selected-index (1- len))
                                                                     0
                                                                     (1+ selected-index)))
                                            ((ps:@ *htmx* highlight-option) opts selected-index input))
                                           ;; Arrow Up - navigate to previous option
                                           ((= (ps:@ event key) "ArrowUp")
                                            (ps:chain event (prevent-default))
                                            (setf selected-index (if (<= selected-index 0)
                                                                     (1- len)
                                                                     (1- selected-index)))
                                            ((ps:@ *htmx* highlight-option) opts selected-index input))
                                           ;; Enter - select current option
                                           ((= (ps:@ event key) "Enter")
                                            (when (>= selected-index 0)
                                              (ps:chain event (prevent-default))
                                              (let ((opt (aref opts selected-index)))
                                                (if on-select
                                                    (funcall on-select opt)
                                                    (ps:chain opt (click))))))
                                           ;; Escape - clear selection and close
                                           ((= (ps:@ event key) "Escape")
                                            (setf selected-index -1)
                                            ((ps:@ *htmx* clear-highlights) opts)
                                            (setf (ps:@ input aria-expanded) "false"))))))))
                                 ;; Reset selection when results update via MutationObserver
                                 (let ((results-el (ps:chain document (query-selector results-selector))))
                                   (when results-el
                                     (let ((observer (ps:new (-Mutation-Observer
                                                             (lambda ()
                                                               (setf selected-index -1)
                                                               ;; Update aria-expanded based on results presence
                                                               (let ((has-results (> (ps:@ (ps:chain results-el
                                                                                             (query-selector-all "[role=option]")) length) 0)))
                                                                 (setf (ps:@ input aria-expanded)
                                                                       (if has-results "true" "false"))))))))
                                       (ps:chain observer (observe results-el
                                                                   (ps:create "childList" t "subtree" t)))))))))

       ;; Highlight a specific option in the autocomplete list
       "highlightOption" (lambda (options index input)
                           (ps:chain options (for-each
                             (lambda (opt i)
                               (if (= i index)
                                   (progn
                                     (ps:chain opt class-list (add "selected"))
                                     (ps:chain opt (set-attribute "aria-selected" "true"))
                                     (ps:chain opt (scroll-into-view (ps:create :block "nearest")))
                                     ;; Update aria-activedescendant on input
                                     (when (and input (ps:@ opt id))
                                       (ps:chain input (set-attribute "aria-activedescendant" (ps:@ opt id)))))
                                   (progn
                                     (ps:chain opt class-list (remove "selected"))
                                     (ps:chain opt (set-attribute "aria-selected" "false"))))))))

       ;; Clear all highlights from autocomplete options
       "clearHighlights" (lambda (options)
                           (ps:chain options (for-each
                             (lambda (opt)
                               (ps:chain opt class-list (remove "selected"))
                               (ps:chain opt (set-attribute "aria-selected" "false"))))))

       ;; ============================================================
       ;; IntersectionObserver Support (revealed/intersect triggers)
       ;; ============================================================

       ;; Setup IntersectionObserver for an element
       ;; Options: threshold (0.1 default), once (disconnect after fire), root, rootMargin
       "setupIntersectionObserver" (lambda (element handler &optional options)
                                     (let* ((threshold (or (and options (ps:getprop options "threshold")) 0.1))
                                            (once (and options (ps:getprop options "once")))
                                            (root (and options (ps:getprop options "root")))
                                            (root-margin (or (and options (ps:getprop options "rootMargin")) "0px"))
                                            ;; Ensure element has ID for storage
                                            (element-id (or (ps:@ element id)
                                                            (progn
                                                              (setf (ps:@ element id)
                                                                    (+ "htmx-obs-" (ps:chain -math (random) (to-string 36) (substr 2 9))))
                                                              (ps:@ element id))))
                                            ;; Pre-declare observer so callback can reference it
                                            (observer nil))
                                       ;; Create and assign observer
                                       (setf observer
                                             (ps:new (-Intersection-Observer
                                                     (lambda (entries)
                                                       (ps:chain entries (for-each
                                                         (lambda (entry)
                                                           (when (ps:@ entry is-intersecting)
                                                             (funcall handler entry)
                                                             (when once
                                                               ;; Disconnect and clean up
                                                               (ps:chain observer (unobserve (ps:@ entry target)))
                                                               (delete (ps:getprop (ps:@ *htmx* observers) element-id))))))))
                                                     (ps:create "threshold" threshold
                                                                "root" root
                                                                "rootMargin" root-margin))))
                                       ;; Store observer for cleanup
                                       (setf (ps:getprop (ps:@ *htmx* observers) element-id) observer)
                                       ;; Start observing
                                       (ps:chain observer (observe element))
                                       ;; Return observer for manual control
                                       observer))

       ;; Disconnect observer for an element
       "disconnectObserver" (lambda (element)
                              (let* ((element-id (ps:@ element id))
                                     (observer (and element-id
                                                    (ps:getprop (ps:@ *htmx* observers) element-id))))
                                (when observer
                                  (ps:chain observer (disconnect))
                                  (delete (ps:getprop (ps:@ *htmx* observers) element-id)))))

       ;; ============================================================
       ;; Public API (htmx.org compatible)
       ;; ============================================================

       ;; htmx.process(elt) - initialize htmx behavior on dynamically added content
       "process" (lambda (elt)
                   ;; Process the element itself if it has verb attributes
                   (when (or (ps:chain elt (get-attribute "hx-get"))
                             (ps:chain elt (get-attribute "hx-post"))
                             (ps:chain elt (get-attribute "hx-put"))
                             (ps:chain elt (get-attribute "hx-delete")))
                     ((ps:@ *htmx* process-element) elt))
                   ;; Process children with verb attributes
                   (let ((children (ps:chain elt (query-selector-all
                                                  "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                     (ps:chain children (for-each (ps:@ *htmx* process-element))))
                   ;; Process hx-on-* on this element and all children
                   ((ps:@ *htmx* process-hx-on) elt)
                   (let ((all-children (ps:chain -array (from (ps:chain elt (get-elements-by-tag-name "*"))))))
                     (ps:chain all-children (for-each
                       (lambda (child)
                         (let ((attrs (ps:chain -array prototype slice (call (ps:chain child attributes)))))
                           (when (ps:chain attrs (some (lambda (attr)
                                                         (ps:chain (ps:@ attr name) (starts-with "hx-on")))))
                             ((ps:@ *htmx* process-hx-on) child))))))))

       ;; htmx.ajax(verb, path, target) - issue programmatic AJAX request
       "ajax" (lambda (verb path target)
                (let ((target-el (if (stringp target)
                                     (ps:chain document (query-selector target))
                                     target)))
                  (when target-el
                    ((ps:@ *htmx* issue-request) target-el verb path))))

       ;; htmx.trigger(elt, name, detail) - dispatch custom event on element
       "trigger" (lambda (elt name detail)
                   ((ps:@ *htmx* dispatch-event) name elt (or detail (ps:create))))

       ;; htmx.on(elt, event, fn) - add event listener, returns listener
       "on" (lambda (elt event-name listener)
              (ps:chain elt (add-event-listener event-name listener))
              listener)

       ;; htmx.off(elt, event, fn) - remove event listener
       "off" (lambda (elt event-name listener)
               (ps:chain elt (remove-event-listener event-name listener)))

       ;; htmx.onLoad(fn) - register callback for htmx:load events
       "onLoad" (lambda (callback)
                  (ps:chain document (add-event-listener "htmx:load"
                    (lambda (evt)
                      (callback (ps:@ evt detail elt))))))

       ;; ============================================================
       ;; Initialization
       ;; ============================================================

       "init" (lambda ()
               ;; Process hx-* elements (request verbs + hx-on-* handlers)
               (let ((htmx-elements (ps:chain document (query-selector-all
                                                        "[hx-get], [hx-post], [hx-put], [hx-delete]"))))
                 (ps:chain htmx-elements (for-each (lambda (el)
                                                     (ps:chain *htmx* (process-element el))))))
               ;; Process hx-on-* on elements without request verbs
               ;; Scan all elements for any hx-on-* attribute (not just a hardcoded list)
               (let ((all-elements (ps:chain -array (from (ps:chain document (get-elements-by-tag-name "*"))))))
                 (ps:chain all-elements (for-each
                   (lambda (el)
                     ;; Skip if already processed via processElement (has verb attributes)
                     (unless (or (ps:chain el (get-attribute "hx-get"))
                                 (ps:chain el (get-attribute "hx-post"))
                                 (ps:chain el (get-attribute "hx-put"))
                                 (ps:chain el (get-attribute "hx-delete")))
                       ;; Check if any attribute starts with hx-on
                       (let ((attrs (ps:chain -array prototype slice (call (ps:chain el attributes)))))
                         (when (ps:chain attrs (some (lambda (attr)
                                                       (ps:chain (ps:@ attr name) (starts-with "hx-on")))))
                           ((ps:@ *htmx* process-hx-on) el))))))))
               ;; Initialize autocomplete keyboard navigation
               (let ((ac-elements (ps:chain document (query-selector-all "[aria-autocomplete]"))))
                 (ps:chain ac-elements (for-each
                   (lambda (el)
                     (let ((el-id (ps:@ el id)))
                       (when el-id
                         ((ps:@ *htmx* setup-autocomplete)
                          el-id (+ "#" el-id "-results"))))))))
               (ps:chain console (log "(HTMX :status :loaded :version" (ps:@ *htmx* version) ")")))))

    ;; Auto-initialize on DOMContentLoaded
    (if (= (ps:@ document ready-state) "loading")
        (ps:chain document (add-event-listener "DOMContentLoaded"
                                               (ps:@ *htmx* init)))
        ((ps:@ *htmx* init)))

    ;; Lowercase alias for compatibility with standard htmx naming
    (setf (ps:@ window htmx) *htmx*)))

;;; ============================================================================
;;; HTMX ATTRIBUTE HELPERS
;;; ============================================================================

(defun hx-get (url &key target swap trigger)
  "Generate hx-get attribute string for cl-who."
  (format nil "~@[hx-get=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

(defun hx-post (url &key target swap trigger)
  "Generate hx-post attribute string for cl-who."
  (format nil "~@[hx-post=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

(defun hx-put (url &key target swap trigger)
  "Generate hx-put attribute string for cl-who."
  (format nil "~@[hx-put=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

(defun hx-delete (url &key target swap trigger)
  "Generate hx-delete attribute string for cl-who."
  (format nil "~@[hx-delete=\"~a\"~]~@[ hx-target=\"~a\"~]~@[ hx-swap=\"~a\"~]~@[ hx-trigger=\"~a\"~]"
          url target swap trigger))

