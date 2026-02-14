;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; HTML page template using cl-who
;;;;
;;;; GENERIC INFRASTRUCTURE - NO hardcoded colors, fonts, or theme styles.
;;;; Apps provide their own visual identity by setting *colors*, *typography*, etc.
;;;;
;;;; Shared macros (htm, htm-str), component rendering (render-component,
;;;; component->html), highlight-sexp, and cl-who config live in html/elements.lisp.
;;;; This file provides only the full-page template and client-side runtime.

(in-package :lol-reactive)

;;; ============================================================================
;;; PAGE TEMPLATE (GENERIC)
;;;
;;; Provides infrastructure only. Apps define aesthetics via:
;;; - Setting *colors*, *typography*, etc. before rendering
;;; - Registering CSS modules via defcss
;;; - Passing custom head-extra content
;;; ============================================================================

(defun html-page (&key (title "LOL-REACTIVE")
                       (lang "en")
                       (body-class "")
                       head-extra
                       body
                       css-href
                       (include-tailwind t)
                       (include-htmx t)
                       (include-surgery nil))
  "Generate a complete HTML page with token-driven CSS variables.

   NO hardcoded colors, fonts, or styles - apps provide their own theme.

   Options:
   - TITLE: Page title
   - LANG: HTML lang attribute (default \"en\")
   - BODY-CLASS: Additional body CSS classes
   - HEAD-EXTRA: Custom head content (string)
   - BODY: Page body content (string)
   - CSS-HREF: Compiled CSS stylesheet path (when provided, suppresses CDN)
   - INCLUDE-TAILWIND: Include Tailwind CDN (default t, ignored when CSS-HREF set)
   - INCLUDE-HTMX: Include HTMX-style runtime (default t)
   - INCLUDE-SURGERY: Include surgery panel runtime (default nil)"
  (cl-who:with-html-output-to-string (s nil :prologue t)
    (:html :lang lang
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title (cl-who:str title))

       ;; Compiled CSS (when provided, replaces CDN)
       (when css-href
         (cl-who:htm
          (:link :rel "stylesheet" :href css-href)))

       ;; Tailwind CDN (only when no compiled CSS)
       (when (and include-tailwind (not css-href))
         (cl-who:htm
          (:script :src "https://cdn.tailwindcss.com")
          (:script (cl-who:str (tailwind-config)))))

       ;; CSS variables from tokens
       (:style (cl-who:str (generate-css-variables)))

       ;; Registered component CSS
       (:style (cl-who:str (generate-all-component-css)))

       ;; HTMX indicator styles
       (when include-htmx
         (cl-who:htm
          (:style (cl-who:str (htmx-indicator-css)))))

       ;; CSRF meta tag for HTMX runtime (reads token from session if available)
       (when include-htmx
         (handler-case
             (let ((token (get-csrf-token)))
               (when token
                 (cl-who:htm
                  (:meta :name "csrf-token" :content token))))
           ;; No session available (e.g., no session middleware) — skip
           (error () nil)))

       ;; App-provided head content
       (cl-who:str (or head-extra "")))

      (:body :class body-class
        ;; Main content
        (cl-who:str (or body ""))

        ;; Reactive runtime script (Parenscript)
        (:script (cl-who:str (reactive-runtime-js)))

        ;; HTMX runtime (optional, default on)
        (when include-htmx
          (cl-who:htm
           (:script (cl-who:str (htmx-runtime-js)))))

        ;; Surgery panel (optional)
        (when include-surgery
          (cl-who:htm
           (:style (cl-who:str (surgery-css)))
           (:script (cl-who:str (surgery-runtime-js)))))))))

;;; ============================================================================
;;; REACTIVE RUNTIME (Parenscript)
;;;
;;; Client-side reactivity. ALL JavaScript generated via Parenscript.
;;; ============================================================================

(defun reactive-runtime-js ()
  "Generate the client-side reactive runtime via Parenscript.
   NO raw JavaScript strings."
  (parenscript:ps
    ;; LOL-REACTIVE Runtime
    (defvar *lol-reactive*
      (ps:create
       :components (ps:new (-Map))

       :register (lambda (id handlers)
                   (ps:chain (ps:@ this components) (set id handlers)))

       :dispatch (lambda (component-id action &rest args)
                   (ps:chain
                    (fetch "/api/dispatch"
                           (ps:create :method "POST"
                                      :headers (ps:create "Content-Type" "application/json")
                                      :body (ps:chain -j-s-o-n (stringify
                                             (ps:create :component-id component-id
                                                        :action action
                                                        :args args)))))
                    (then (lambda (r) (ps:chain r (json))))
                    (then (lambda (data)
                            (when (ps:@ data html)
                              (let ((el (ps:chain document (query-selector
                                         (+ "[data-component-id=\"" component-id "\"]")))))
                                (when el
                                  (setf (ps:@ el inner-h-t-m-l) (ps:@ data html)))))))))

       :set-state (lambda (component-id key value)
                    (ps:chain
                     (fetch "/api/set-state"
                            (ps:create :method "POST"
                                       :headers (ps:create "Content-Type" "application/json")
                                       :body (ps:chain -j-s-o-n (stringify
                                              (ps:create :component-id component-id
                                                         :key key
                                                         :value value)))))
                     (then (lambda (r) (ps:chain r (json))))
                     (then (lambda (data)
                             (when (ps:@ data html)
                               (let ((el (ps:chain document (query-selector
                                          (+ "[data-component-id=\"" component-id "\"]")))))
                                 (when el
                                   (setf (ps:@ el inner-h-t-m-l) (ps:@ data html)))))))))))

    ;; Shorthand - only set if LOL-REACTIVE exists
    (defvar dispatch
      (when *lol-reactive*
        (ps:chain (ps:@ *lol-reactive* dispatch) (bind *lol-reactive*))))
    (defvar set-state
      (when *lol-reactive*
        (ps:chain (ps:getprop *lol-reactive* "set-state") (bind *lol-reactive*))))

    (ps:chain console (log "(LOL-REACTIVE :status :loaded)"))))
