;;;; LOL-REACTIVE Error Handling
;;;; Production-grade error handling with logging and safe error pages
;;;;
;;;; Provides:
;;;; - with-error-handling: Macro for consistent error handling
;;;; - log-error: Structured error logging
;;;; - render-error-page: User-friendly error pages with fallbacks

(in-package :lol-web/server)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Configuration
;;; ═══════════════════════════════════════════════════════════════════════════

(defvar *debug-mode* nil
  "When T, show full backtraces in error responses and log detailed info.
   Image-global by design — error rendering is a process-wide policy.
   Mutated through SET-DEBUG-MODE / ENABLE-DEBUG-MODE / DISABLE-DEBUG-MODE;
   freeze the value at boot with LOCK-DEBUG-MODE so later writes signal.")

(defvar *debug-mode-locked-p* nil
  "When T, subsequent writes through the SET-DEBUG-MODE gate signal
   DEBUG-MODE-LOCKED-ERROR. One-way: cannot be unlocked. Engaged by
   LOCK-DEBUG-MODE or by make-app's :LOCK-DEBUG-MODE-P kwarg.")

(define-condition debug-mode-locked-error (error)
  ((value :initarg :value :reader debug-mode-locked-error-value))
  (:report
   (lambda (c s)
     (format s "*debug-mode* is locked; refusing to set to ~S."
             (debug-mode-locked-error-value c)))))

(defun %default-error-log-path ()
  (let* ((cache-root (or (uiop:getenv "XDG_CACHE_HOME")
                         (let ((home (user-homedir-pathname)))
                           (when home
                             (namestring (merge-pathnames ".cache/" home)))))))
    (when cache-root
      (namestring
       (merge-pathnames "lol-web/error.log"
                        (uiop:ensure-directory-pathname cache-root))))))

(defvar *error-log-path* (%default-error-log-path)
  "Path to error log file. Set to NIL to disable file logging.")

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Error Logging
;;; ═══════════════════════════════════════════════════════════════════════════

(defun log-error (context error &key (stream *error-output*) (backtrace-count 30))
  "Log error with context, type, and optional backtrace.
   CONTEXT is a string describing where the error occurred (e.g., \"GET /home\").
   ERROR is the condition object.
   :stream - where to write (default *error-output*)
   :backtrace-count - number of stack frames to show (default 30)"
  ;; Console logging
  (format stream "~&========================================~%")
  (format stream "LOL-REACTIVE ERROR~%")
  (format stream "========================================~%")
  (format stream "Context: ~A~%" context)
  (format stream "Error: ~A~%" error)
  (format stream "Type: ~A~%" (type-of error))
  (format stream "Time: ~A~%" (get-universal-time))

  ;; Always include backtrace in server logs - this is not user-facing
  #+sbcl
  (handler-case
      (progn
        (format stream "~%BACKTRACE:~%")
        (sb-debug:print-backtrace :stream stream :count backtrace-count))
    (error () nil))

  (format stream "========================================~%~%")
  (force-output stream)

  ;; File logging (always, if path configured)
  (when *error-log-path*
    (ignore-errors
      (ensure-directories-exist *error-log-path*)
      (with-open-file (log-stream *error-log-path*
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
        (format log-stream "~%[~A] ~A: ~A~%"
                (get-universal-time)
                context
                error)
        #+sbcl
        (handler-case
            (sb-debug:print-backtrace :stream log-stream :count backtrace-count)
          (error () nil))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Error Pages
;;; ═══════════════════════════════════════════════════════════════════════════

(defun minimal-error-html (title heading message &optional debug-info)
  "Generate minimal error page HTML with no dependencies on app state.
   Uses inline neutral CSS values rather than design tokens — error pages
   must render even if *colors* / *typography* are unbound or partially
   initialised. Apps that want themed error pages should compose their
   own renderer rather than override the fallback."
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>~A</title>
<style>
body {
  background: #ffffff;
  color: #1a1a1a;
  font-family: system-ui, -apple-system, sans-serif;
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 100vh;
  margin: 0;
}
.container { text-align: center; max-width: 600px; padding: 2rem; }
h1 { color: #000000; font-size: 3rem; margin: 0 0 1rem 0; }
p { color: #666666; margin: 1rem 0; line-height: 1.6; }
a { color: #0066cc; text-decoration: none; }
a:hover { text-decoration: underline; }
pre {
  background: #f5f5f5;
  padding: 1rem;
  border-radius: 4px;
  overflow-x: auto;
  text-align: left;
  font-size: 0.85rem;
  color: #666666;
  margin-top: 2rem;
}
</style>
</head>
<body>
<div class=\"container\">
<h1>~A</h1>
<p>~A</p>
<a href=\"/\">Return to home</a>
~A
</div>
</body>
</html>"
          title
          heading
          message
          (if debug-info
              (format nil "<pre>~A</pre>" debug-info)
              "")))

(defun render-error-page (error &key (context "Unknown"))
  "Render a user-friendly error page.
   In debug mode, shows error details. Otherwise, shows generic message.
   Uses minimal HTML as fallback if complex rendering fails.

   CONTEXT and the printed form of ERROR can carry attacker-influenced
   text (request paths, condition bodies, extractor coercion-error
   messages); both are routed through escape-html before format inserts
   them into the page so a `<script>` payload renders as literal text."
  (if *debug-mode*
      (minimal-error-html
       "Error"
       "Something went wrong"
       (format nil "Context: ~A" (escape-html (princ-to-string context)))
       (format nil "~A~%~%Type: ~A"
               (escape-html (princ-to-string error))
               (escape-html (princ-to-string (type-of error)))))
      (minimal-error-html
       "Error"
       "Something went wrong"
       "We're working on it. Please try again later.")))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Error Handling Macro
;;; ═══════════════════════════════════════════════════════════════════════════

(defmacro with-error-handling (context &body body)
  "Wrap handler with consistent error handling, logging, and safe error pages.
   CONTEXT is a string describing the operation (e.g., \"GET /home\" or \"API /users\").

   Example:
     (with-error-handling \"GET /home\"
       (render-home-page))

   Behavior:
   1. Executes body
   2. On error:
      - Logs to console and file (with backtrace)
      - Returns 500 error response with styled error page
      - If error page rendering fails, returns minimal safe HTML

   Note: Security headers are NOT added here - that is the route's
   responsibility (via defroute :secure t).  Error handling and security
   are separate concerns."
  (let ((context-var (gensym "CONTEXT")))
    `(let ((,context-var ,context))
       (handler-case
           (progn ,@body)
         (http-error (e)
           ;; Application-signalled status — short-circuit without a backtrace.
           (error-response (http-error-status e)
                           :message (or (http-error-body e)
                                        (http-status-text (http-error-status e)))))
         (error (e)
           ;; Log the error
           (log-error ,context-var e)

           ;; Render error page with fallback, return as Clack response
           (error-response 500
                           :content-type "text/html; charset=utf-8"
                           :message (handler-case
                                        (render-error-page e :context ,context-var)
                                      (error (secondary-error)
                                        ;; If error page rendering fails, use absolute minimal HTML
                                        (format *error-output* "~%Secondary error rendering error page: ~A~%" secondary-error)
                                        (minimal-error-html
                                         "Error"
                                         "Error"
                                         (format nil "~A (error page also failed: ~A)" ,context-var secondary-error))))))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; HTTP Status Error Pages (for custom Hunchentoot acceptor)
;;; ═══════════════════════════════════════════════════════════════════════════

(defun render-404-page (&optional path)
  "Render custom 404 Not Found page. PATH is request-controlled and
   routed through escape-html before format inserts it into the page."
  (minimal-error-html
   "404 - Not Found"
   "404"
   (if path
       (format nil "The page '~A' doesn't exist."
               (escape-html (princ-to-string path)))
       "The page you're looking for doesn't exist.")))

(defun render-500-page (&optional error)
  "Render custom 500 Internal Server Error page. ERROR's printed form
   can carry attacker-influenced text; it is escape-html'd before
   format inserts it into the debug pane."
  (if (and *debug-mode* error)
      (minimal-error-html
       "500 - Server Error"
       "500"
       "Something went wrong on our end."
       (escape-html (princ-to-string error)))
      (minimal-error-html
       "500 - Server Error"
       "500"
       "Something went wrong on our end.")))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Debug Mode Control
;;; ═══════════════════════════════════════════════════════════════════════════

(defun set-debug-mode (value)
  "Set *debug-mode* to VALUE through the production-lock gate. Signals
   DEBUG-MODE-LOCKED-ERROR when *debug-mode-locked-p* is T."
  (when *debug-mode-locked-p*
    (error 'debug-mode-locked-error :value value))
  (setf *debug-mode* value))

(defun lock-debug-mode ()
  "Engage the one-way lock on *debug-mode*. Subsequent SET-DEBUG-MODE
   (including ENABLE/DISABLE-DEBUG-MODE) signal DEBUG-MODE-LOCKED-ERROR."
  (setf *debug-mode-locked-p* t))

(defun enable-debug-mode ()
  "Enable debug mode - shows detailed errors, backtraces in responses."
  (set-debug-mode t)
  (format t "LOL-REACTIVE debug mode enabled~%"))

(defun disable-debug-mode ()
  "Disable debug mode - shows minimal error info to users."
  (set-debug-mode nil)
  (format t "LOL-REACTIVE debug mode disabled~%"))
