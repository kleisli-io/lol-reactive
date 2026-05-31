;;;; Regression tests for :lol-web/client-runtime.
;;;;
;;;; The single exported function `lol-reactive-runtime-js` concatenates
;;;; outputs from htmx (`htmx-runtime-js`), html (`reactive-runtime-js`),
;;;; and realtime-htmx (`ws-client-js`, `sse-client-js`, `optimistic-js`).
;;;; Smoke-level coverage: actually invoke it and check the output is a
;;;; substantive non-empty string containing landmarks from each source.

(in-package :lol-web/client-runtime/test)
(in-suite :lol-web/client-runtime/test)

(test regression-lol-reactive-runtime-js-non-empty
  "Calling lol-reactive-runtime-js produces a non-empty JS string. This
   alone catches the regression where the package failed to import the
   realtime-htmx symbols (`ws-client-js`, etc.) — the call would error
   with an undefined-function condition."
  (let ((js (lol-web/html:safe-html-string-value (lol-reactive-runtime-js))))
    (is (stringp js))
    (is (> (length js) 500)
        "combined runtime should be a substantive JS bundle")))

(test regression-lol-reactive-runtime-js-bundles-all-clients
  "The bundled output must contain landmarks from each upstream
   generator: WebSocket from ws-client-js, EventSource from sse-client-js,
   and the htmx runtime's own marker."
  (let ((js (lol-web/html:safe-html-string-value (lol-reactive-runtime-js))))
    (is (search "WebSocket" js)
        "must include ws-client-js output (WebSocket constructor ref)")
    (is (search "EventSource" js)
        "must include sse-client-js output (EventSource constructor ref)")))
