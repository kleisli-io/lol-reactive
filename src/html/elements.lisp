;;;; LOL-REACTIVE HTML Elements
;;;; cl-who shorthand macros and component rendering utilities
;;;;
;;;; GENERIC INFRASTRUCTURE - NO hardcoded colors, fonts, or theme styles.

(in-package :lol-web/html)

;;; ============================================================================
;;; CL-WHO CONFIGURATION
;;; ============================================================================

(setf cl-who:*attribute-quote-char* #\"
      cl-who:*html-empty-tag-aware-p* t)

;;; ============================================================================
;;; CL-WHO SHORTHAND MACROS
;;; ============================================================================

(defmacro htm (&body body)
  "Shorthand for cl-who output to *standard-output*."
  `(cl-who:with-html-output (*standard-output*) ,@body))

(defmacro htm-str (&body body)
  "Generate HTML and return as string."
  `(cl-who:with-html-output-to-string (s) ,@body))

(defun safe-attribute-name-p (name)
  "Return T if NAME is safe to emit as an HTML attribute name.

   Contract: the name must start with an ASCII letter and contain only
   ASCII letters, digits, dash, underscore, or colon. This forbids
   whitespace, quotes, angle brackets, slashes, and `=`, blocking
   injection of attribute-context escape sequences. Event-handler
   attributes are refused because HTML attribute escaping does not make
   JavaScript bodies safe — both the classic `on*` names and the htmx
   inline-handler `hx-on*` names (HX-ON-ATTRIBUTE-NAME-P, in either the
   `hx-on:click` or `hx-on-click` form) the client runtime lifts to a
   handler. A dynamic attribute *name* must not be able to emit an htmx
   handler any more than an `onclick`.

   Accepts strings and symbols (uses SYMBOL-NAME for the latter)."
  (let ((str (etypecase name
               (string name)
               (symbol (symbol-name name)))))
    (and (plusp (length str))
         (not (and (>= (length str) 2)
                   (char-equal (char str 0) #\o)
                   (char-equal (char str 1) #\n)))
         (not (hx-on-attribute-name-p str))
         (let ((c0 (char str 0)))
           (or (and (char<= #\a c0) (char<= c0 #\z))
               (and (char<= #\A c0) (char<= c0 #\Z))))
         (every (lambda (c)
                  (or (and (char<= #\a c) (char<= c #\z))
                      (and (char<= #\A c) (char<= c #\Z))
                      (and (char<= #\0 c) (char<= c #\9))
                      (char= c #\-)
                      (char= c #\_)
                      (char= c #\:)))
                str))))

(define-condition unsafe-attribute-name (error)
  ((name :initarg :name :reader unsafe-attribute-name-name))
  (:report (lambda (c stream)
             (format stream "Unsafe HTML attribute name: ~S (allowed: ASCII letter start, then letters/digits/-/_/:)"
                     (unsafe-attribute-name-name c)))))

(defun html-attrs (&rest pairs)
  "Build an HTML attribute fragment from key-value pairs.
   NIL values are omitted entirely; T values become boolean attributes
   (no `=value` suffix). All other values are coerced to strings and
   passed through `escape-attribute` so embedded quotes and angle
   brackets cannot escape the attribute context.

   Attribute *names* are validated against SAFE-ATTRIBUTE-NAME-P; an
   unsafe name signals UNSAFE-ATTRIBUTE-NAME. The compiler macro
   defined below catches literal names at macro-expansion time so the
   error surfaces before runtime.

   Returns a string with a leading space when non-empty so it can be
   spliced directly after a tag name: `(format nil \"<input~A/>\"
   (html-attrs ...))`."
  (with-output-to-string (s)
    (loop for (name value) on pairs by #'cddr
          when value do
          (unless (safe-attribute-name-p name)
            (error 'unsafe-attribute-name :name name))
          (if (eq value t)
              (format s " ~A" name)
              (format s " ~A=\"~A\"" name (escape-attribute (princ-to-string value)))))))

(define-compiler-macro html-attrs (&whole form &rest pairs)
  "Compile-time check: reject literal attribute names that fail
   SAFE-ATTRIBUTE-NAME-P before they reach the runtime call."
  (loop for (name nil) on pairs by #'cddr
        for literal = (cond
                        ((or (keywordp name) (stringp name)) name)
                        ((and (consp name) (eq (car name) 'quote)
                              (symbolp (cadr name)))
                         (cadr name)))
        when literal
          do (unless (safe-attribute-name-p literal)
               (error 'unsafe-attribute-name :name literal)))
  form)

;;; ============================================================================
;;; COMPONENT RENDERING
;;; ============================================================================

(defun render-component (component)
  "Render a component to HTML string."
  (funcall component :render))

(defparameter *component-render-hook* nil
  "Optional function (function (component) string) that replaces the
   default component-wrapper div around a component's rendered HTML.
   :lol-web/devtools installs an x-ray wrapper here when surgery mode
   is enabled. Held in :lol-web/html so the renderer never has to know
   about devtools (which depends on html, so the reverse edge would
   create a cycle).")

(defun component->html (component &key (wrapper t))
  "Convert a component to HTML, optionally wrapping in a container.
   When *component-render-hook* is bound to a function and WRAPPER is
   true, the hook produces the wrapper instead of the default div."
  (cond
    ((and wrapper *component-render-hook*)
     (funcall *component-render-hook* component))
    (wrapper
     (let ((html (render-component component))
           (id (funcall component :id)))
       (cl-who:with-html-output-to-string (s)
         (:div :id (safe-attr id)
               :class "component-wrapper"
               :data-component-id (safe-attr id)
           (cl-who:str html)))))
    (t
     (render-component component))))

;;; ============================================================================
;;; S-EXPRESSION HIGHLIGHTING (generic utility)
;;; ============================================================================

(defparameter *highlight-sexp-max-length* 65536
  "Upper bound on the PRIN1 length HIGHLIGHT-SEXP processes. Longer forms
   are truncated before escaping and the regex passes, so a pathological
   input cannot drive quadratic backtracking on the `&quot;...&quot;` scan.")

(defparameter *highlight-sexp-print-level* 32
  "*print-level* bound while HIGHLIGHT-SEXP prints a form: a deeply nested
   value is elided past this depth rather than driving PRIN1 unbounded
   before the length cap applies.")

(defparameter *highlight-sexp-print-length* 4096
  "*print-length* bound while HIGHLIGHT-SEXP prints a form: the element
   count printed at each level is capped so a very wide form cannot run PRIN1
   unbounded before the length cap applies.")

(defmacro with-cycle-safe-printer (&body body)
  "Run BODY with the printer bound so even a cyclic or pathologically
   deep/wide form prints in bounded time: *print-circle* labels shared and
   circular structure (no infinite walk), and *print-level* / *print-length*
   bound the depth and breadth emitted."
  `(let ((*print-circle* t)
         (*print-level* *highlight-sexp-print-level*)
         (*print-length* *highlight-sexp-print-length*)
         (*print-readably* nil)
         (*print-pretty* nil))
     ,@body))

(defun highlight-sexp (form)
  "Convert a Lisp form to syntax-highlighted HTML.

   Uses CSS classes that apps can style as needed. The printed form is
   HTML-escaped before the regex passes run so any `<`, `>`, `&`, `'`,
   or `\"` characters appearing inside string-valued positions become
   inert entities. The string-tag regex therefore matches `&quot;...&quot;`
   rather than raw `\"...\"`. The printed form is capped at
   *HIGHLIGHT-SEXP-MAX-LENGTH* before escaping (truncating the raw text,
   never a half-emitted entity). The print runs under WITH-CYCLE-SAFE-PRINTER
   so a cyclic or pathologically deep/wide FORM cannot hang PRIN1 before the
   cap applies."
  (let* ((raw (with-cycle-safe-printer (prin1-to-string form)))
         (capped (if (> (length raw) *highlight-sexp-max-length*)
                     (subseq raw 0 *highlight-sexp-max-length*)
                     raw))
         (str (escape-html capped)))
    (setf str (cl-ppcre:regex-replace-all
               ":(\\w+)"
               str
               "<span class=\"sexp-keyword\">:\\1</span>"))
    (setf str (cl-ppcre:regex-replace-all
               "&quot;(.*?)&quot;"
               str
               "<span class=\"sexp-string\">&quot;\\1&quot;</span>"))
    (setf str (cl-ppcre:regex-replace-all
               "\\b(\\d+)\\b"
               str
               "<span class=\"sexp-number\">\\1</span>"))
    str))
