;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Optimistic update client runtime (Parenscript)
;;;;
;;;; Provides instant UI feedback before server response with automatic rollback.

(in-package :lol-reactive)

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
