;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; HTMX autocomplete component and indicator CSS
;;;;
;;;; Server-side rendering for autocomplete inputs and CSS for HTMX indicators.

(in-package :lol-reactive)

;;; ============================================================================
;;; HTMX CSS
;;; ============================================================================

(defun htmx-indicator-css ()
  "Generate CSS for HTMX request indicators using lol-reactive CSS utilities."
  (concatenate 'string
               "/* HTMX Request Indicator Styles */"
               (css-rules ".htmx-request"
                          :opacity "0.7"
                          :cursor "wait")
               (css-rules ".htmx-request.htmx-indicator"
                          :opacity "1")
               (css-rules ".htmx-indicator"
                          :display "none")
               (css-rules ".htmx-request .htmx-indicator, .htmx-request.htmx-indicator"
                          :display "inline-block")))

;;; ============================================================================
;;; AUTOCOMPLETE SUPPORT
;;; ============================================================================

(defun render-autocomplete (&key id endpoint
                                 (placeholder "Search...")
                                 (debounce 300)
                                 (min-chars 1)
                                 class)
  "Render a search input with autocomplete behavior.

   ID: Unique identifier for this autocomplete (required)
   ENDPOINT: Server endpoint to fetch results from (required)
   PLACEHOLDER: Input placeholder text
   DEBOUNCE: Milliseconds to wait before firing request (default 300)
   MIN-CHARS: Minimum characters before searching (default 1, reserved for future use)
   CLASS: Additional CSS classes for the container"
  (declare (ignore min-chars)) ; Reserved for future use
  (htm-str
    (:div :class (format nil "autocomplete-container~@[ ~a~]" class)
      (:input :type "search"
              :id id
              :name "q"
              :placeholder placeholder
              :autocomplete "off"
              :hx-get endpoint
              :hx-trigger (format nil "input changed delay:~ams" debounce)
              :hx-target (format nil "#~a-results" id)
              :hx-sync "this:replace"
              :hx-indicator (format nil "#~a-loading" id)
              :role "combobox"
              :aria-controls (format nil "~a-results" id)
              :aria-expanded "false"
              :aria-autocomplete "list")
      (:span :id (format nil "~a-loading" id)
             :class "htmx-indicator autocomplete-loading"
             "Searching...")
      (:div :id (format nil "~a-results" id)
            :role "listbox"
            :class "autocomplete-results"
            :aria-label "Search results"))))

(defun render-autocomplete-results (items &key id render-item (empty-message "No results found"))
  "Render search results for autocomplete.

   ITEMS: List of items to render
   ID: Autocomplete ID (must match render-autocomplete)
   RENDER-ITEM: Function to render each item (default: identity)
   EMPTY-MESSAGE: Message to show when no results"
  (let ((render-fn (or render-item #'identity)))
    (htm-str
      (if items
          (cl-who:htm
            (:ul :id (format nil "~a-results" id)
                 :role "listbox"
                 :class "autocomplete-results"
              (loop for item in items
                    for i from 0
                    do (cl-who:htm
                        (:li :role "option"
                             :id (format nil "~a-option-~a" id i)
                             :class "autocomplete-result"
                             :tabindex "-1"
                             :aria-selected "false"
                             (cl-who:str (funcall render-fn item)))))))
          (cl-who:htm
            (:div :id (format nil "~a-results" id)
                  :role "listbox"
                  :class "autocomplete-results autocomplete-empty"
              (:span :class "autocomplete-no-results"
                     (cl-who:str empty-message))))))))

(defun autocomplete-css ()
  "CSS for autocomplete component using design system tokens.
   Uses CSS variables for theming support."
  (concatenate 'string
    "/* Autocomplete Component Styles */"
    (css-rules ".autocomplete-container"
               "position" "relative")
    (css-rules ".autocomplete-results"
               "position" "absolute"
               "width" "100%"
               "max-height" "300px"
               "overflow-y" "auto"
               "background" (css-var "color-surface")
               "border" (format nil "~a solid ~a"
                                (css-var "effect-border-thin")
                                (css-var "color-muted"))
               "border-radius" (css-var "space-1")
               "box-shadow" (css-var "effect-shadow-md")
               "z-index" (css-var "effect-z-modal")
               "margin" "0"
               "padding" "0")
    (css-rules ".autocomplete-results:empty"
               "display" "none")
    (css-rules ".autocomplete-result"
               "padding" (format nil "~a ~a"
                                 (css-var "space-2")
                                 (css-var "space-3"))
               "cursor" "pointer"
               "list-style" "none"
               "color" (css-var "color-text")
               "transition" (css-var "effect-transition-fast"))
    (css-rules ".autocomplete-result:hover, .autocomplete-result.selected"
               "background" (css-var "color-surface-alt"))
    (css-rules ".autocomplete-result.selected"
               "outline" (format nil "2px solid ~a" (css-var "color-primary"))
               "outline-offset" "-2px")
    (css-rules ".autocomplete-loading"
               "display" "none"
               "position" "absolute"
               "right" (css-var "space-2")
               "top" "50%"
               "transform" "translateY(-50%)"
               "color" (css-var "color-muted")
               "font-size" (css-var "font-small"))
    (css-rules ".htmx-request .autocomplete-loading"
               "display" "inline")
    (css-rules ".autocomplete-no-results"
               "display" "block"
               "padding" (css-var "space-3")
               "color" (css-var "color-muted")
               "font-style" "italic")
    (css-rules ".autocomplete-empty"
               "border" "none"
               "box-shadow" "none")))
