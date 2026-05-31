;;;; css/registry.lisp - CSS module system
;;;;
;;;; CSS modules are message-dispatch closures (`(:render :rules :add-rule
;;;; :remove-rule :inspect)`). A global registry collects them in
;;;; registration order, then `generate-all-component-css` concatenates
;;;; their rendered output in load order.
;;;;
;;;; Concurrency: registration and generation can happen from any thread
;;;; (e.g., concurrent component module loads under SLY/swank, or runtime
;;;; `defcss` evaluation). `*css-registry-lock*` serialises every read and
;;;; write of `*component-css-registry*` and `*css-load-order*`. Recursive
;;;; because public entry points compose (e.g., `get-component-css` calls
;;;; `get-css-module`).

(in-package :lol-web/css)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Global Registry State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *component-css-registry* (make-hash-table :test 'eq)
  "Hash table mapping module names (keywords) to CSS module closures.
   Mutated only under `*css-registry-lock*`.")

(defvar *css-load-order* nil
  "List of module names in registration order (most recent first).
   Reversed during generation to maintain load order in output.
   Mutated only under `*css-registry-lock*`.")

(defvar *css-registry-lock*
  (bordeaux-threads:make-recursive-lock "lol-web/css registry")
  "Serialises every read and write of `*component-css-registry*` and
   `*css-load-order*`. Recursive so composing entry points (e.g.,
   `get-component-css` → `get-css-module`) don't self-deadlock.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Module Factory (Let Over Lambda Pattern)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-css-module (name &rest initial-rules)
  "Create a CSS module with pandoric introspection.

   NAME: Keyword identifying the module (e.g., :buttons)
   INITIAL-RULES: List of (selector . properties-alist) pairs. Each
   selector is validated against `safe-css-selector-p`; an unsafe
   selector signals UNSAFE-CSS-SELECTOR at registration time so a
   malformed rule never enters the registry.

   Returns a dlambda responding to:
     :name      - Get module name
     :rules     - Get all rules
     :add-rule  - Add (selector . properties) rule
     :render    - Generate CSS string
     :inspect   - Full state dump

   Example:
   (make-css-module :buttons
     '(\".btn\" . ((\"padding\" . \"1rem\")))
     '(\".btn:hover\" . ((\"opacity\" . \"0.8\"))))"
  (dolist (rule initial-rules)
    (let ((selector (car rule)))
      ;; @-rules (@media, @keyframes, @supports) carry their own selector
      ;; on inner blocks; the top-level `@media (...)` string is gated by
      ;; the same predicate so attacker-supplied media queries cannot
      ;; close the rule.
      (unless (safe-css-selector-p selector)
        (error 'unsafe-css-selector :selector selector))))
  (let ((module-name name)
        (rules (copy-list initial-rules))
        (rules-lock (bordeaux-threads:make-recursive-lock
                     (format nil "lol-web/css module ~A" name)))
        (created-at (get-universal-time)))
    (bordeaux-threads:with-recursive-lock-held (*css-registry-lock*)
      ;; Auto-register in global registry. The closure body runs later under
      ;; callers' threads, so the per-module rules list has its own lock.
      (setf (gethash name *component-css-registry*)
            (lambda (message &rest args)
              (case message
                (:name module-name)

                (:rules
                 (bordeaux-threads:with-recursive-lock-held (rules-lock)
                   (copy-list rules)))

                (:add-rule
                 (destructuring-bind (selector properties) args
                   (unless (safe-css-selector-p selector)
                     (error 'unsafe-css-selector :selector selector))
                   (bordeaux-threads:with-recursive-lock-held (rules-lock)
                     (push (cons selector properties) rules))
                   selector))

                (:remove-rule
                 (let ((selector (first args)))
                   (bordeaux-threads:with-recursive-lock-held (rules-lock)
                     (setf rules
                           (remove-if (lambda (r) (string= (car r) selector))
                                      rules)))
                   selector))

                (:render
                 (let ((snapshot
                         (bordeaux-threads:with-recursive-lock-held (rules-lock)
                           (copy-list rules))))
                   ;; Each branch delegates to css-rule / css-keyframes from
                   ;; generation.lisp so symbol/keyword property keys are
                   ;; normalised the same way regardless of entry point.
                   (with-output-to-string (out)
                     (dolist (rule snapshot)
                       (let ((selector (car rule))
                             (props (cdr rule)))
                         (cond
                           ;; "@keyframes <name>" — strip the 11-char prefix
                           ;; and pass the frame list to css-keyframes.
                           ((and (stringp selector)
                                 (>= (length selector) 11)
                                 (string= (subseq selector 0 11) "@keyframes "))
                            (format out "~A~%"
                                    (apply #'css-keyframes
                                           (subseq selector 11)
                                           props)))
                           ;; "@media ..." — props is a list of (sel props)
                           ;; pairs; render each nested rule via css-rule.
                           ((and (stringp selector)
                                 (>= (length selector) 6)
                                 (string= (subseq selector 0 6) "@media"))
                            (format out "~A {~%" selector)
                            (dolist (inner-rule props)
                              (let ((inner-sel (first inner-rule))
                                    (inner-props (second inner-rule)))
                                (format out "  ~A~%" (css-rule inner-sel inner-props))))
                            (format out "}~%"))
                           (t
                            (format out "~A~%" (css-rule selector props)))))))))

                (:inspect
                 (bordeaux-threads:with-recursive-lock-held (rules-lock)
                   (list :name module-name
                         :created-at created-at
                         :rule-count (length rules)
                         :rules (copy-list rules))))

                (otherwise
                 (error "Unknown CSS module message: ~S" message)))))

      ;; Track load order
      (unless (member name *css-load-order*)
        (push name *css-load-order*))

      ;; Return the module
      (gethash name *component-css-registry*))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Registry Functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-css-module (name)
  "Get a CSS module by name."
  (bordeaux-threads:with-recursive-lock-held (*css-registry-lock*)
    (gethash name *component-css-registry*)))

(defun get-component-css (name)
  "Get rendered CSS for a single module.
   The `:render` call happens outside the registry lock — the module's
   own state is independent of the registry once we hold the closure."
  (let ((module (get-css-module name)))
    (when module
      (funcall module :render))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CSS Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun generate-all-component-css ()
  "Generate CSS from all registered modules in load order.

   Returns concatenated CSS string with section headers for each module.
   Snapshots the load order under the registry lock, then renders each
   module without holding the lock — `:render` calls touch only the
   module's own closure state."
  (let ((names-in-load-order
          (bordeaux-threads:with-recursive-lock-held (*css-registry-lock*)
            (reverse *css-load-order*))))
    (with-output-to-string (out)
      (dolist (name names-in-load-order)
        (let ((module (get-css-module name)))
          (when module
            (let ((css (funcall module :render)))
              (when (and css (> (length css) 0))
                (format out "~&/* ═══ ~A ═══ */~%" name)
                (format out "~A~%" css)))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Convenience Macro
;;; ─────────────────────────────────────────────────────────────────────────────

(defmacro defcss (name &body rules)
  "Define a CSS module at load time.

   NAME: Keyword identifying the module
   RULES: (selector properties-alist) pairs

   Example:
   (defcss :buttons
     (\".btn\" ((\"padding\" . \"1rem\")))
     (\".btn:hover\" ((\"opacity\" . \"0.8\"))))"
  (let ((rule-forms (mapcar (lambda (rule)
                              (let ((sel (first rule)))
                                (if (and (stringp sel)
                                         (plusp (length sel))
                                         (char= (char sel 0) #\@)
                                         (not (search "@keyframes" sel)))
                                    ;; @media / @supports: normalize both syntaxes
                                    ;; Wrapped:   ("@media ..." ((sel1 props1) (sel2 props2)))
                                    ;; Unwrapped: ("@media ..." (sel1 props1) (sel2 props2))
                                    (let ((inner-rules (if (stringp (car (second rule)))
                                                           (rest rule)
                                                           (second rule))))
                                      `(cons ,sel ',inner-rules))
                                    ;; Regular rule or @keyframes: selector + properties alist
                                    `(cons ,sel ',(second rule)))))
                            rules)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (make-css-module ,name ,@rule-forms))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Testing & Introspection
;;; ─────────────────────────────────────────────────────────────────────────────

(defun clear-css-registry ()
  "Clear the CSS registry. Used in tests."
  (bordeaux-threads:with-recursive-lock-held (*css-registry-lock*)
    (clrhash *component-css-registry*)
    (setf *css-load-order* nil)
    t))

(defun list-registered-css-components ()
  "List all registered module names in load order."
  (bordeaux-threads:with-recursive-lock-held (*css-registry-lock*)
    (reverse *css-load-order*)))

(defun inspect-css-registry ()
  "Inspect all registered CSS modules."
  (let ((names-in-load-order
          (bordeaux-threads:with-recursive-lock-held (*css-registry-lock*)
            (reverse *css-load-order*))))
    (mapcar (lambda (name)
              (let ((module (get-css-module name)))
                (when module
                  (funcall module :inspect))))
            names-in-load-order)))
