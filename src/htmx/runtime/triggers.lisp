;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTMX; Base: 10 -*-
;;;; HTMX runtime — triggers + per-element discovery cluster
;;;;
;;;; - dashToCamel utility
;;;; - hx-on-* event handler attribute processing
;;;; - dispatchEvent helper
;;;; - processElement (per-element verb binding)
;;;; - parseInterval / parseTrigger (hx-trigger spec parsing)
;;;; - addTriggerHandler (delay/throttle/once/changed/from/load/revealed/intersect)
;;;; - setupAutocomplete + highlightOption + clearHighlights
;;;;   (autocomplete keyboard navigation: ArrowUp/ArrowDown/Enter/Escape)
;;;; - setupIntersectionObserver / disconnectObserver (revealed/intersect triggers)

(in-package :lol-web/htmx)

(defun htmx-runtime-triggers-pairs ()
  "Property-value pairs for the trigger and per-element discovery clusters."
  (list
   ;; Utility: convert dash-case to camelCase
   "dashToCamel" '(lambda (str)
                    (ps:chain str
                      (replace (ps:regex "/-([a-z])/g")
                               (lambda (match letter)
                                 (ps:chain letter (to-upper-case))))))

   ;; hx-on-* attribute processing
   ;; Supports: hx-on-click, hx-on-htmx-after-swap, hx-on--after-swap
   ;;
   ;; Native DOM events (click/submit/change/...) are bound by writing the
   ;; native `on<event>' attribute via setAttribute — the browser then
   ;; compiles the handler under `script-src 'unsafe-inline'' (the same rule
   ;; that governs `<button onclick="...">' in source HTML). htmx custom
   ;; events (htmx:afterSwap, etc.) have no native attribute, so they fall
   ;; back to addEventListener + Function — that path needs `'unsafe-eval''
   ;; in CSP. We wrap it in try/catch so a strict CSP fails one handler
   ;; instead of aborting the whole runtime init.
   ;; The prefix that gates handler lifting is spliced from the canonical
   ;; lol-web/escape:*hx-on-attribute-prefix*, the same source the server-side
   ;; sanitize-hx-on-attrs strips on — so the lift set and the scrub set cannot
   ;; drift (the colon-form gap was exactly such a drift).
   "processHxOn" `(lambda (element)
                    (let ((attrs (ps:chain -array prototype slice
                                           (call (ps:chain element attributes)))))
                      (ps:chain attrs
                        (for-each
                          (lambda (attr)
                            (let ((name (ps:@ attr name)))
                              (when (ps:chain name (starts-with ,lol-web/escape:*hx-on-attribute-prefix*))
                                (let* ((suffix (ps:chain name (substring 5)))
                                       (code (ps:@ attr value))
                                       (is-htmx-event
                                         (or (ps:chain suffix (starts-with "--"))
                                             (ps:chain suffix (starts-with "-htmx-")))))
                                  (if is-htmx-event
                                      (let ((event-name
                                              (if (ps:chain suffix (starts-with "--"))
                                                  (+ "htmx:" ((ps:@ *htmx* dash-to-camel)
                                                              (ps:chain suffix (substring 2))))
                                                  (+ "htmx:" ((ps:@ *htmx* dash-to-camel)
                                                              (ps:chain suffix (substring 6)))))))
                                        (ps:try
                                          (ps:chain element
                                            (add-event-listener event-name
                                              (ps:new (-Function "event" code))))
                                          (:catch (e)
                                            (ps:chain console
                                              (warn "[HTMX] hx-on handler skipped (CSP unsafe-eval needed):"
                                                    name e)))))
                                      ;; Native DOM event — let the browser compile
                                      ;; under inline-attribute CSP. suffix is e.g.
                                      ;; "-click"; native attribute name is "onclick".
                                      (let ((dom-attr (+ "on" (ps:chain suffix (substring 1)))))
                                        (ps:chain element
                                          (set-attribute dom-attr code))))))))))))

   ;; Dispatch a CustomEvent on an element (bubbles up)
   "dispatchEvent" '(lambda (name element detail)
                      (let ((event (ps:new (-Custom-Event name
                                            (ps:create :bubbles t
                                                       :cancelable t
                                                       :detail detail)))))
                        (ps:chain element (dispatch-event event))))

   ;; Element Processing
   "processElement" '(lambda (element)
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
   "parseInterval" '(lambda (str)
                      (cond
                        ((ps:chain str (ends-with "ms"))
                         (parse-int (ps:chain str (slice 0 -2))))
                        ((ps:chain str (ends-with "s"))
                         (* 1000 (parse-int (ps:chain str (slice 0 -1)))))
                        (t
                         (parse-int str))))

   "parseTrigger" '(lambda (trigger-string)
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

   "addTriggerHandler" '(lambda (element trigger-spec handler)
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

   ;; Setup autocomplete keyboard navigation for an input
   "setupAutocomplete" '(lambda (input-id results-selector &optional options)
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
   "highlightOption" '(lambda (options index input)
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
   "clearHighlights" '(lambda (options)
                        (ps:chain options (for-each
                          (lambda (opt)
                            (ps:chain opt class-list (remove "selected"))
                            (ps:chain opt (set-attribute "aria-selected" "false"))))))

   ;; Setup IntersectionObserver for an element
   ;; Options: threshold (0.1 default), once (disconnect after fire), root, rootMargin
   "setupIntersectionObserver" '(lambda (element handler &optional options)
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
   "disconnectObserver" '(lambda (element)
                           (let* ((element-id (ps:@ element id))
                                  (observer (and element-id
                                                 (ps:getprop (ps:@ *htmx* observers) element-id))))
                             (when observer
                               (ps:chain observer (disconnect))
                               (delete (ps:getprop (ps:@ *htmx* observers) element-id)))))))
