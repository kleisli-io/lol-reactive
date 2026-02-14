;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Out-of-Band (OOB) response helpers for HTMX-style partial updates
;;;;
;;;; Server-side utilities for generating hx-swap-oob responses.

(in-package :lol-reactive)

;;; ============================================================================
;;; OOB RESPONSE HELPERS
;;; ============================================================================

(defun find-tag-end (html)
  "Find the position of > that ends the opening tag, skipping > inside quotes.
   Returns the position of the closing > or NIL if not found."
  (let ((in-quote nil)
        (len (length html)))
    (loop for i from 0 below len
          for char = (char html i)
          do (cond
               ((char= char #\")
                (setf in-quote (not in-quote)))
               ((and (char= char #\>) (not in-quote))
                (return i)))
          finally (return nil))))

(defun content-starts-with-id-p (html target-id)
  "Check if HTML starts with an element that has the specified ID.
   Returns T if the first element's opening tag contains id=\"TARGET-ID\".
   Correctly handles > characters inside quoted attribute values."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) html)))
    (when (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\<))
      ;; Find end of opening tag (skip > inside quotes)
      (let ((tag-end (find-tag-end trimmed)))
        (when tag-end
          ;; Look for id="target-id" within the opening tag
          (let ((tag-content (subseq trimmed 0 tag-end))
                (id-pattern (format nil "id=\"~a\"" target-id)))
            (search id-pattern tag-content :test #'char-equal)))))))

(defun inject-oob-attribute (html swap-value)
  "Inject hx-swap-oob attribute into the first element's opening tag.
   Handles both regular tags and self-closing tags (e.g., <input />).
   Correctly handles > characters inside quoted attribute values.
   Returns the modified HTML string."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) html)))
    (let ((first-gt (find-tag-end trimmed)))
      (if first-gt
          ;; Check for self-closing tag: look for / before >
          (let* ((before-gt (subseq trimmed 0 first-gt))
                 (slash-pos (position #\/ before-gt :from-end t))
                 ;; Is it a self-closing tag? (/ appears near end, only whitespace between / and >)
                 (self-closing-p (and slash-pos
                                      (every (lambda (c) (member c '(#\Space #\Tab)))
                                             (subseq before-gt (1+ slash-pos)))))
                 ;; Insert position: before the / for self-closing, before > otherwise
                 (insert-pos (if self-closing-p slash-pos first-gt)))
            (concatenate 'string
                         (subseq trimmed 0 insert-pos)
                         (format nil " hx-swap-oob=\"~a\"" swap-value)
                         (subseq trimmed insert-pos)))
          ;; Fallback if no > found (shouldn't happen with valid HTML)
          html))))

(defun oob-swap (id content &key (swap "true"))
  "Generate an OOB swap element.
   SWAP can be: true (outerHTML), innerHTML, beforebegin, afterbegin, etc.

   Smart behavior for outerHTML swaps: if content already contains an element
   with the target ID, injects hx-swap-oob attribute directly instead of
   wrapping (which would create duplicate IDs)."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) content)))
    (if (and (string= swap "outerHTML")
             (content-starts-with-id-p trimmed id))
        ;; Content already has the ID - inject attribute directly
        (inject-oob-attribute trimmed swap)
        ;; Standard wrapping for innerHTML, other strategies, or content without ID
        (cl-who:with-html-output-to-string (s)
          (:div :id id :hx-swap-oob swap
                (cl-who:str content))))))

(defmacro with-oob-swaps ((&rest swaps) &body body)
  "Execute BODY and append OOB swap elements.
   SWAPS is a list of (id content &key swap) specifications."
  `(concatenate 'string
                (progn ,@body)
                ,@(mapcar (lambda (swap-spec)
                            `(oob-swap ,@swap-spec))
                          swaps)))

(defun oob-content (id content)
  "Generate an OOB innerHTML swap that preserves target element attributes.

   Unlike oob-swap which replaces the entire element (including class, hx-*, etc),
   this only replaces the innerHTML of the target element, preserving all attributes.

   Use this when the target element has attributes you want to keep, such as:
   - CSS classes for styling
   - hx-trigger for polling
   - data-* attributes

   Example:
     ;; Target: <div id=\"counter\" class=\"big\" hx-trigger=\"every 1s\">0</div>
     (oob-content \"counter\" \"42\")
     ;; Result: <span style=\"display:none\" hx-swap-oob=\"innerHTML:#counter\">42</span>
     ;; Target becomes: <div id=\"counter\" class=\"big\" hx-trigger=\"every 1s\">42</div>"
  (htm-str
    (:span :style "display:none" :hx-swap-oob (format nil "innerHTML:#~A" id)
      (cl-who:str content))))
