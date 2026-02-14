;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-REACTIVE; Base: 10 -*-
;;;; Combined client runtime aggregator
;;;;
;;;; Bundles all client-side JavaScript runtimes into a single output.

(in-package :lol-reactive)

;;; ============================================================================
;;; COMBINED RUNTIME
;;; ============================================================================

(defun lol-reactive-runtime-js ()
  "Generate the complete lol-reactive client runtime.
   Includes HTMX runtime, WebSocket client, SSE client, and optimistic updates."
  (concatenate 'string
               (htmx-runtime-js)
               ";"
               (ws-client-js)
               ";"
               (sse-client-js)
               ";"
               (optimistic-js)))
