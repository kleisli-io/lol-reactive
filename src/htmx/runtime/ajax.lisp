;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/HTMX; Base: 10 -*-
;;;; HTMX runtime — AJAX cluster
;;;;
;;;; The fetch lifecycle: header construction, form serialization, hx-include,
;;;; hx-vals merging, CSRF token attachment, hx-sync abort behaviour, request
;;;; cancellation, htmx:beforeRequest/configRequest/beforeSwap/afterSwap/afterSettle/
;;;; afterRequest/sendError event dispatch.

(in-package :lol-web/htmx)

(defun htmx-runtime-ajax-pairs ()
  "Property-value pairs for the *htmx* issueRequest cluster."
  (list
       ;; Request Handling with hx-sync support and form serialization
       "issueRequest" `(lambda (element method url)
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
                                         ;; hx-include: serialize included non-form elements
                                         (when (and include-el (not form))
                                           (let ((inputs
                                                   (if (or (= (ps:chain (ps:@ include-el tag-name) (to-lower-case)) "input")
                                                           (= (ps:chain (ps:@ include-el tag-name) (to-lower-case)) "select")
                                                           (= (ps:chain (ps:@ include-el tag-name) (to-lower-case)) "textarea"))
                                                       (array include-el)
                                                       (ps:chain -array (from
                                                         (ps:chain include-el (query-selector-all "input, select, textarea")))))))
                                             (when (> (ps:@ inputs length) 0)
                                               (unless fd
                                                 (setf fd (ps:new (-Form-Data))))
                                               (ps:chain inputs (for-each
                                                 (lambda (inp)
                                                   (let ((n (ps:chain inp (get-attribute "name")))
                                                         (v (ps:@ inp value)))
                                                     (when (and n v)
                                                       (ps:chain fd (append n v))))))))))
                                         ;; Merge hx-vals JSON into request body
                                         (let ((hx-vals-attr (ps:chain element (get-attribute "hx-vals"))))
                                           (when hx-vals-attr
                                             (unless fd
                                               (setf fd (ps:new (-Form-Data))))
                                             (let ((vals (ps:chain -j-s-o-n (parse hx-vals-attr))))
                                               (ps:chain -object (keys vals)
                                                 (for-each (lambda (key)
                                                             (ps:chain fd (append key (ps:getprop vals key)))))))))
                                         ;; Append CSRF token from meta tag (if present)
                                         (when fd
                                           (let ((csrf-meta (ps:chain document
                                                              (query-selector "meta[name=csrf-token]"))))
                                             (when csrf-meta
                                               (let ((token (ps:chain csrf-meta (get-attribute "content"))))
                                                 (when token
                                                   (ps:chain fd (append "csrf-token" token)))))))
                                         (when fd
                                           (let ((has-file false))
                                             (ps:chain fd (for-each
                                               (lambda (v)
                                                 (when (or (ps:instanceof v -File)
                                                           (ps:instanceof v -Blob))
                                                   (setf has-file true)))))
                                             (unless has-file
                                               (let ((params (ps:new (-U-R-L-Search-Params))))
                                                 (ps:chain fd (for-each
                                                   (lambda (v k)
                                                     (ps:chain params (append k v)))))
                                                 (setf fd params)))))
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
    )) ;; list, defun
