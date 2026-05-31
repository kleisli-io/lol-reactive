;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/RESOURCES; Base: 10 -*-
;;;; async/resources.lisp - Async Data Resources with Loading States
;;;;
;;;; PURPOSE:
;;;;   Define async data resources with loading, error, and success states.
;;;;   Declarative data fetching with caching support.
;;;;
;;;; KEY MACROS:
;;;;   DEFRESOURCE - Define an async data resource
;;;;   WITH-RESOURCE - Use a resource with automatic state handling
;;;;
;;;; FEATURES:
;;;;   - Loading state rendering
;;;;   - Error state handling
;;;;   - Memory and session caching
;;;;   - Automatic retry support

(in-package :lol-web/resources)

;;; ============================================================================
;;; RESOURCE REGISTRY
;;; ============================================================================

(defvar *resources* (make-hash-table :test 'eq)
  "Registry of defined resources.")

(defparameter *resource-cache-max-entries* 1024
  "Element-count cap on *RESOURCE-CACHE*. Request-derived params can cycle
   distinct cache keys, so the bound turns unbounded growth into LRU
   eviction rather than OOM.")

(defvar *resource-cache*
  (lol-web/core:make-bounded-cache :max-entries *resource-cache-max-entries*
                                   :test 'equal)
  "Bounded LRU cache for resource data. Each value is (TIMESTAMP . DATA) so
   the fetch time travels with the datum and eviction is atomic — no parallel
   timestamp table to fall out of sync with the data table.")

(defun register-resource (name spec)
  "Register a resource specification."
  (setf (gethash name *resources*) spec))

(defun get-resource-spec (name)
  "Get resource specification by name."
  (gethash name *resources*))

(defun list-resources ()
  "List all registered resources."
  (let (resources)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k resources))
             *resources*)
    (nreverse resources)))

;;; ============================================================================
;;; RESOURCE STATES
;;; ============================================================================

(defstruct (resource-state (:constructor make-resource-state))
  "State of a resource fetch operation."
  (status :idle :type keyword)    ; :idle :loading :success :error
  (data nil)                      ; Fetched data on success
  (error nil)                     ; Error object on failure
  (timestamp nil)                 ; When data was fetched
  (params nil))                   ; Parameters used for fetch

(defun resource-loading-p (state)
  "Check if resource is currently loading."
  (eq (resource-state-status state) :loading))

(defun resource-success-p (state)
  "Check if resource fetch succeeded."
  (eq (resource-state-status state) :success))

(defun resource-error-p (state)
  "Check if resource fetch failed."
  (eq (resource-state-status state) :error))

(defun resource-idle-p (state)
  "Check if resource hasn't been fetched yet."
  (eq (resource-state-status state) :idle))

;;; ============================================================================
;;; CACHING
;;; ============================================================================

(defun make-cache-key (resource-name params)
  "Create a cache key from resource name and parameters."
  (format nil "~A:~S" resource-name params))

(defun get-cached-data (resource-name params &key (max-age nil))
  "Get cached data if available and not expired.
   MAX-AGE: Maximum age in seconds (nil = no expiry)"
  (let ((key (make-cache-key resource-name params)))
    (multiple-value-bind (entry present-p)
        (lol-web/core:bounded-cache-get *resource-cache* key)
      (when present-p
        (destructuring-bind (timestamp . data) entry
          (if (or (null max-age)
                  (<= (- (get-universal-time) timestamp) max-age))
              data
              ;; Cache expired, remove it
              (progn
                (lol-web/core:bounded-cache-remove *resource-cache* key)
                nil)))))))

(defun set-cached-data (resource-name params data)
  "Store data in cache, stamping the current time alongside it."
  (let ((key (make-cache-key resource-name params)))
    (lol-web/core:bounded-cache-set *resource-cache* key
                                    (cons (get-universal-time) data))
    data))

(defun clear-resource-cache (&optional resource-name)
  "Clear cached data, optionally for specific resource."
  (if resource-name
      ;; Clear specific resource
      (let ((prefix (format nil "~A:" resource-name)))
        (dolist (k (lol-web/core:bounded-cache-keys *resource-cache*))
          (when (and (stringp k) (>= (length k) (length prefix))
                     (string= prefix (subseq k 0 (length prefix))))
            (lol-web/core:bounded-cache-remove *resource-cache* k))))
      ;; Clear all
      (lol-web/core:bounded-cache-clear *resource-cache*)))

;;; ============================================================================
;;; RESOURCE FETCHING
;;; ============================================================================

(defun fetch-resource (resource-name &rest params)
  "Fetch a resource synchronously.
   Returns a resource-state struct."
  (let* ((spec (get-resource-spec resource-name))
         (fetcher (getf spec :fetcher))
         (cache-strategy (getf spec :cache :none))
         (cache-max-age (getf spec :cache-max-age)))

    (unless spec
      (return-from fetch-resource
        (make-resource-state :status :error
                             :error (format nil "Resource ~A not found" resource-name)
                             :params params)))

    ;; Check cache first
    (when (and (member cache-strategy '(:memory :session))
               (not (eq cache-strategy :none)))
      (let ((cached (get-cached-data resource-name params :max-age cache-max-age)))
        (when cached
          (return-from fetch-resource
            (make-resource-state :status :success
                                 :data cached
                                 :timestamp (get-universal-time)
                                 :params params)))))

    ;; Fetch data
    (handler-case
        (let ((data (apply fetcher params)))
          ;; Cache result
          (when (member cache-strategy '(:memory :session))
            (set-cached-data resource-name params data))
          (make-resource-state :status :success
                               :data data
                               :timestamp (get-universal-time)
                               :params params))
      (error (e)
        (make-resource-state :status :error
                             :error (princ-to-string e)
                             :params params)))))

;;; ============================================================================
;;; RESOURCE RENDERING
;;; ============================================================================

(defun render-resource-loading (resource-name)
  "Render loading state for a resource using Tailwind classes.
   Uses custom :loading from spec, or default Tailwind-styled spinner."
  (let* ((spec (get-resource-spec resource-name))
         (loading (getf spec :loading)))
    (if loading
        (if (functionp loading)
            (funcall loading)
            loading)
        ;; Default loading UI with Tailwind classes via htm-str
        (htm-str
          (:div :class (classes "p-4" "text-center" "text-muted")
            (:span :class (classes "inline-block" "w-5" "h-5" "border-2"
                                   "border-muted" "border-t-primary"
                                   "rounded-full" "animate-spin" "mr-2" "align-middle"))
            "Loading...")))))

(defun render-resource-error (resource-name error)
  "Render error state for a resource using Tailwind classes.
   Uses custom :error from spec, or default Tailwind-styled error box."
  (let* ((spec (get-resource-spec resource-name))
         (error-handler (getf spec :error)))
    (if error-handler
        (if (functionp error-handler)
            (funcall error-handler error)
            error-handler)
        ;; Default error UI with Tailwind classes via htm-str
        (htm-str
          (:div :class (classes "p-4" "bg-error/10" "border" "border-error/30"
                                "rounded-md" "text-error")
            (:strong "Error:") " " (cl-who:esc (princ-to-string error)))))))

;;; ============================================================================
;;; DEFRESOURCE MACRO
;;; ============================================================================

(defmacro defresource (name (&rest params) &key fetcher loading error cache cache-max-age)
  "Define an async data resource with loading states.

   NAME: Resource identifier
   PARAMS: Parameter list for the fetcher function
   FETCHER: Function that fetches the data (receives params)
   LOADING: Loading state component (string or function)
   ERROR: Error state component (function receiving error object)
   CACHE: Cache strategy (:none :memory :session)
   CACHE-MAX-AGE: Maximum cache age in seconds

   Creates:
   - (fetch-NAME params...) - Fetch resource, returns resource-state
   - (NAME-loading) - Render loading state
   - (NAME-error err) - Render error state
   - (with-resource (data (NAME params...)) body) - Use with automatic state handling

   Example:
     (defresource user-data (user-id)
       :fetcher (lambda (id) (get-user-from-db id))
       :loading \"<div class='spinner'>Loading user...</div>\"
       :error (lambda (e) (format nil \"<div class='error'>~A</div>\" e))
       :cache :memory
       :cache-max-age 300)"
  (let ((fetch-fn-name (symb "FETCH-" name))
        (loading-fn-name (symb name "-LOADING"))
        (error-fn-name (symb name "-ERROR")))
    `(progn
       ;; Register resource spec
       (register-resource ',name
                          (list :fetcher ,fetcher
                                :loading ,loading
                                :error ,error
                                :cache ,(or cache :none)
                                :cache-max-age ,cache-max-age
                                :params ',params))

       ;; Define fetch function
       (defun ,fetch-fn-name (,@params)
         ,(format nil "Fetch ~A resource." name)
         (fetch-resource ',name ,@params))

       ;; Define loading renderer
       (defun ,loading-fn-name ()
         ,(format nil "Render loading state for ~A." name)
         (render-resource-loading ',name))

       ;; Define error renderer
       (defun ,error-fn-name (error)
         ,(format nil "Render error state for ~A." name)
         (render-resource-error ',name error))

       ',name)))

;;; ============================================================================
;;; WITH-RESOURCE MACRO
;;; ============================================================================

(defmacro with-resource ((data-var resource-call) &body body)
  "Use a resource in component, handling loading/error states automatically.

   DATA-VAR: Variable to bind the fetched data
   RESOURCE-CALL: Form like (resource-name params...)
   BODY: Code to execute when data is successfully loaded

   Automatically renders:
   - Loading component while fetching
   - Error component on failure
   - BODY with data bound on success

   Example:
     (with-resource (user (user-data user-id))
       (htm-str (:h1 \"Welcome, \" (cl-who:esc (getf user :name)))))"
  (let* ((resource-name (car resource-call))
         (resource-params (cdr resource-call))
         (fetch-fn (symb "FETCH-" resource-name))
         (state-var (gensym "STATE")))
    `(let ((,state-var (,fetch-fn ,@resource-params)))
       (cond
         ((resource-loading-p ,state-var)
          (render-resource-loading ',resource-name))
         ((resource-error-p ,state-var)
          (render-resource-error ',resource-name (resource-state-error ,state-var)))
         ((resource-success-p ,state-var)
          (let ((,data-var (resource-state-data ,state-var)))
            ,@body))
         (t
          (render-resource-loading ',resource-name))))))

;;; ============================================================================
;;; RESOURCE STYLES
;;; ============================================================================

(defun resource-styles-css ()
  "OPTIONAL: CSS for projects NOT using Tailwind.
   The default render functions use Tailwind classes. This function provides
   fallback CSS with CSS variables for non-Tailwind projects."
  (flet ((p (s) (make-safe-css-payload-string s)))
    (concatenate 'string
      (css-section (p "Resource Loading")
        (p (css-rule ".resource-loading"
                     `(("padding" . ,(css-var "spacing-4"))
                       ("text-align" . "center")
                       ("color" . ,(css-var "color-muted")))))
        (p (css-rule ".resource-loading .spinner"
                     `(("display" . "inline-block")
                       ("width" . "20px")
                       ("height" . "20px")
                       ("border" . ,(format nil "2px solid ~A" (css-var "color-muted")))
                       ("border-top-color" . ,(css-var "color-primary"))
                       ("border-radius" . "50%")
                       ("animation" . "lol-spin 1s linear infinite")
                       ("margin-right" . ,(css-var "spacing-2"))
                       ("vertical-align" . "middle")))))
      (format nil "~%")
      (css-keyframes "lol-spin"
        '("to" . (("transform" . "rotate(360deg)"))))
      (format nil "~%")
      (css-section (p "Resource Error")
        (p (css-rule ".resource-error"
                     `(("padding" . ,(css-var "spacing-4"))
                       ("background" . ,(format nil "color-mix(in srgb, ~A 10%, ~A)"
                                                (css-var "color-error")
                                                (css-var "color-surface")))
                       ("border" . ,(format nil "1px solid color-mix(in srgb, ~A 30%, ~A)"
                                            (css-var "color-error")
                                            (css-var "color-surface")))
                       ("border-radius" . ,(css-var "radius-md"))
                       ("color" . ,(css-var "color-error")))))))))

;;; ============================================================================
;;; RESOURCE INTROSPECTION
;;; ============================================================================

(defun inspect-resource (name)
  "Return introspection data for a resource."
  (let ((spec (get-resource-spec name)))
    (when spec
      (list :name name
            :params (getf spec :params)
            :cache (getf spec :cache)
            :cache-max-age (getf spec :cache-max-age)
            :has-loading (not (null (getf spec :loading)))
            :has-error-handler (not (null (getf spec :error)))))))

(defun resource-cache-stats ()
  "Get statistics about the resource cache."
  (let ((count 0)
        (total-age 0)
        (now (get-universal-time)))
    (dolist (k (lol-web/core:bounded-cache-keys *resource-cache*))
      (multiple-value-bind (entry present-p)
          (lol-web/core:bounded-cache-get *resource-cache* k)
        (when present-p
          (incf count)
          (incf total-age (- now (car entry))))))
    (list :cached-items count
          :average-age-seconds (if (> count 0) (/ total-age count) 0))))
