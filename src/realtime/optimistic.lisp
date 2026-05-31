;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/REALTIME-HTMX; Base: 10 -*-
;;;; Optimistic update client runtime (Parenscript)
;;;;
;;;; Provides instant UI feedback before server response with automatic rollback.

(in-package :lol-web/realtime-htmx)

;;; ============================================================================
;;; OPTIMISTIC UPDATE — SERVER-SIDE GUARDS
;;; ============================================================================
;;;
;;; The Parenscript runtime below renders client-side JS; the server-side
;;; helpers in this section validate what the server is willing to push at
;;; an optimistic-apply call site. Two invariants:
;;;
;;;   1. HTML payloads must be SAFE-HTML-STRING — bare strings are refused
;;;      at the boundary so an attacker-supplied payload cannot reach
;;;      innerHTML without the producer's explicit safety claim.
;;;
;;;   2. The per-component originals registry is bounded by
;;;      *optimistic-originals-cap*. Apply-side bookkeeping rides on
;;;      lol-web/core:component-originals so unregistering the component
;;;      releases the entire history with no leak.

(defparameter *optimistic-originals-cap* 64
  "Maximum simultaneous in-flight optimistic snapshots per component
   instance. When the cap is reached, optimistic-record-original returns
   :cap-reached and the caller must refuse the apply at its call site.
   NIL disables the cap.")

(defparameter *optimistic-originals-global-cap* 4096
  "Maximum simultaneous in-flight optimistic snapshots across every
   registered component. Complements the per-component cap so a swarm of
   components each holding < per-cap snapshots cannot collectively pin
   arbitrary memory. NIL disables the global cap.")

(defun %total-optimistic-originals ()
  "Sum the component-originals length across every registered component.
   Caller need not hold any lock — component-originals reads are
   linearised by the core registry's own discipline."
  (let ((total 0))
    (maphash (lambda (id entry)
               (declare (ignore entry))
               (incf total (length (lol-web/core:component-originals id))))
             lol-web/core:*components*)
    total))

(defun %coerce-html-payload (value)
  "Return the wire string for a SAFE-HTML-STRING value; signal on any
   other type. The boundary here is the only point at which the optimistic
   layer accepts an HTML payload — every dispatcher above it must hand
   over a value the producer has already tagged."
  (unless (lol-web/html:safe-html-string-p value)
    (error 'type-error :datum value
                       :expected-type 'lol-web/html:safe-html-string))
  (lol-web/html:safe-html-string-value value))

(defun optimistic-apply-payload (component-id config)
  "Build the wire payload an optimistic-apply emit consumes. CONFIG is a
   plist of (:text :html :class :add-class :remove-class :disabled). Any
   :html value must be a SAFE-HTML-STRING; bare strings signal
   TYPE-ERROR. Returns a plist suitable for encode-json-string.

   COMPONENT-ID is the lol-web/core component-entry id whose originals
   slot tracks this apply. The cap is enforced via
   optimistic-record-original — when it returns :cap-reached the apply
   signals so the call site does not silently drop bookkeeping."
  (check-type component-id string)
  (let ((wire (loop for (k v) on config by #'cddr
                    collect k
                    collect (if (eq k :html)
                                (%coerce-html-payload v)
                                v))))
    (let ((rec (optimistic-record-original component-id config)))
      (case rec
        (:cap-reached
         (error "optimistic-apply-payload: component ~S originals cap reached (~S)"
                component-id *optimistic-originals-cap*))
        (:global-cap-reached
         (error "optimistic-apply-payload: global originals cap reached (~S)"
                *optimistic-originals-global-cap*))))
    wire))

(defun optimistic-record-original (component-id snapshot)
  "Record SNAPSHOT in the component's originals registry. Returns :ok on
   record, :no-component when COMPONENT-ID is not registered (silent
   no-op — mirrors component-snapshots' lifecycle contract), :cap-reached
   when *optimistic-originals-cap* would be exceeded for this component,
   or :global-cap-reached when *optimistic-originals-global-cap* would
   be exceeded across every registered component."
  (cond
    ((null (lol-web/core:find-component component-id))
     :no-component)
    (t
     (let ((existing (lol-web/core:component-originals component-id)))
       (cond
         ((and *optimistic-originals-cap*
               (>= (length existing) *optimistic-originals-cap*))
          :cap-reached)
         ((and *optimistic-originals-global-cap*
               (>= (%total-optimistic-originals)
                   *optimistic-originals-global-cap*))
          :global-cap-reached)
         (t
          (setf (lol-web/core:component-originals component-id)
                (cons (cons (get-universal-time) snapshot) existing))
          :ok))))))

(defun optimistic-clear-originals (component-id)
  "Drop every recorded original for COMPONENT-ID.
   Returns NIL when COMPONENT-ID is unregistered."
  (when (lol-web/core:find-component component-id)
    (setf (lol-web/core:component-originals component-id) nil)
    t))

;;; ============================================================================
;;; OPTIMISTIC UPDATE RUNTIME (Parenscript)
;;; ============================================================================

(defun optimistic-js ()
  "Generate optimistic update client code via Parenscript.
   Provides instant UI feedback before server response with automatic rollback."
  (parenscript:ps
    (defvar *optimistic*
      (ps:create
       ;; Store original states for rollback
       "originals" (ps:create)

       ;; Apply optimistic state to element
       "apply" (lambda (element config)
                 (let ((id (or (ps:@ element id)
                               (ps:chain -math (random) (to-string 36) (substr 2 9)))))
                   ;; Ensure element has ID for tracking
                   (unless (ps:@ element id)
                     (setf (ps:@ element id) id))
                   ;; Save original state
                   (setf (ps:getprop (ps:@ *optimistic* originals) id)
                         (ps:create
                          :text-content (ps:@ element text-content)
                          :inner-h-t-m-l (ps:@ element inner-h-t-m-l)
                          :class-name (ps:@ element class-name)
                          :disabled (ps:@ element disabled)
                          :value (ps:@ element value)))
                   ;; Apply optimistic changes
                   (when (ps:@ config text)
                     (setf (ps:@ element text-content) (ps:@ config text)))
                   (when (ps:@ config html)
                     (setf (ps:@ element inner-h-t-m-l) (ps:@ config html)))
                   (when (ps:@ config class)
                     (setf (ps:@ element class-name) (ps:@ config class)))
                   (when (ps:@ config add-class)
                     (ps:chain element class-list (add (ps:@ config add-class))))
                   (when (ps:@ config remove-class)
                     (ps:chain element class-list (remove (ps:@ config remove-class))))
                   (when (not (ps:=== undefined (ps:@ config disabled)))
                     (setf (ps:@ element disabled) (ps:@ config disabled)))
                   id))

       ;; Rollback to original state
       "rollback" (lambda (element-or-id)
                    (let* ((id (if (stringp element-or-id)
                                   element-or-id
                                   (ps:@ element-or-id id)))
                           (element (if (stringp element-or-id)
                                        (ps:chain document (get-element-by-id element-or-id))
                                        element-or-id))
                           (original (ps:getprop (ps:@ *optimistic* originals) id)))
                      (when (and element original)
                        (setf (ps:@ element text-content) (ps:@ original text-content))
                        (setf (ps:@ element inner-h-t-m-l) (ps:@ original inner-h-t-m-l))
                        (setf (ps:@ element class-name) (ps:@ original class-name))
                        (setf (ps:@ element disabled) (ps:@ original disabled))
                        (when (ps:@ original value)
                          (setf (ps:@ element value) (ps:@ original value)))
                        ;; Clean up stored state
                        (delete (ps:getprop (ps:@ *optimistic* originals) id)))))

       ;; Confirm optimistic change (clear stored original)
       "confirm" (lambda (element-or-id)
                   (let ((id (if (stringp element-or-id)
                                 element-or-id
                                 (ps:@ element-or-id id))))
                     (delete (ps:getprop (ps:@ *optimistic* originals) id))))

       ;; Wrap HTMX request with optimistic update
       "wrap" (lambda (element config)
                (let ((id ((ps:@ *optimistic* apply) element config)))
                  ;; Listen for HTMX events to confirm or rollback
                  (ps:chain element (add-event-listener "htmx:afterRequest"
                    (lambda (event)
                      (if (ps:@ event detail successful)
                          ((ps:@ *optimistic* confirm) id)
                          ((ps:@ *optimistic* rollback) id)))
                    (ps:create :once t)))))))))
