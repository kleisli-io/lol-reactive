;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/CLIENT-RUNTIME; Base: 10 -*-
;;;; Combined client runtime aggregator
;;;;
;;;; Bundles all client-side JavaScript runtimes into a single output.

(in-package :lol-web/client-runtime)

;;; ============================================================================
;;; COMBINED RUNTIME
;;; ============================================================================

(defun lol-reactive-runtime-js ()
  "Generate the complete lol-reactive client runtime.
   Includes HTMX runtime, WebSocket client, SSE client, and optimistic updates.

   HTMX runtime returns a SAFE-HTML-STRING (its content is consumed at
   html-page sinks that consult the type); the bundler unwraps it to
   concatenate, then re-tags the composite for downstream sinks."
  (make-safe-html-string
   (concatenate 'string
                (safe-html-string-value (htmx-runtime-js))
                ";"
                (ws-client-js)
                ";"
                (sse-client-js)
                ";"
                (optimistic-js))))
