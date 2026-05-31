;;;; Regression tests for :lol-web/realtime.
;;;;
;;;; Smoke-level coverage of the SSE message formatter and connection-
;;;; registry shape. The handler creators (`make-ws-handler`,
;;;; `make-sse-handler`) and broadcast functions need a live Hunchentoot
;;;; environment to exercise meaningfully and are not covered here.

(in-package :lol-web/realtime/test)
(in-suite :lol-web/realtime/test)

;;; ============================================================================
;;; format-sse-event — SSE wire-format spec compliance
;;; ============================================================================

(test regression-format-sse-event-basic-shape
  "An event with type and string data emits `event:` and `data:` lines
   terminated by an empty line per the SSE spec."
  (let ((s (format-sse-event "update" "hello")))
    (is (search "event: update" s))
    (is (search "data: hello" s))
    (is (search (format nil "~%~%") s)
        "event must terminate with a blank line")))

(test regression-format-sse-event-multiline-data
  "Multi-line data is split and each line gets its own `data:` prefix
   per SSE spec (otherwise the client treats embedded newlines as event
   terminators)."
  (let ((s (format-sse-event "msg" (format nil "line1~%line2~%line3"))))
    (is (search "data: line1" s))
    (is (search "data: line2" s))
    (is (search "data: line3" s))))

(test regression-format-sse-event-id-and-retry
  "Optional `:id` and `:retry` keys produce `id:` and `retry:` prefix
   lines used by the client for resume and backoff."
  (let ((s (format-sse-event "tick" "1" :id "evt-7" :retry 5000)))
    (is (search "id: evt-7" s))
    (is (search "retry: 5000" s))))

(test regression-format-sse-event-non-string-data-json-encoded
  "Non-string data is JSON-encoded by `encode-json-string` so plists
   and lists become valid JSON payloads on the wire."
  (let ((s (format-sse-event "obj" '(:k 1 :v "two"))))
    ;; Don't pin the exact JSON shape — encode-json-string's output is
    ;; library-controlled. Just assert non-string data didn't get
    ;; passed through verbatim (which would emit Lisp `:K` keywords).
    (is (search "data: " s))
    (is (not (search "data: (:K 1" s))
        "non-string data must be JSON-encoded, not princ'd")))

;;; ============================================================================
;;; Connection registry — defaults are empty hash-tables, counts are 0
;;; ============================================================================

(test regression-connection-registries-default-empty
  "The connection registries are initialised to empty hash-tables and
   the count helpers return 0 with no live connections."
  (is (hash-table-p *ws-connections*))
  (is (hash-table-p *sse-connections*))
  (is (= 0 (ws-connection-count "channel-with-no-clients")))
  (is (= 0 (sse-connection-count "channel-with-no-clients"))))

(test regression-channels-listing-shape
  "`ws-channels` and `sse-channels` return a list (possibly empty) of
   active channel ids."
  (is (listp (ws-channels)))
  (is (listp (sse-channels))))

;;; ============================================================================
;;; format-sse-event — WHATWG §9.2.6 line-terminator hygiene
;;; ============================================================================

(defun %signals-error-p (thunk)
  "Run THUNK; return T if it signals an error, NIL on normal return."
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(test regression-format-sse-event-id-rejects-cr-lf
  "An :id containing CR or LF would terminate the id field on the wire
   and forge whatever followed as a new event — the formatter signals."
  (is (%signals-error-p
       (lambda () (format-sse-event "tick" "1" :id (format nil "evt~C7" #\Return)))))
  (is (%signals-error-p
       (lambda () (format-sse-event "tick" "1" :id (format nil "evt~C7" #\Linefeed)))))
  (is (%signals-error-p
       (lambda () (format-sse-event "tick" "1"
                                    :id (format nil "evt-7~C~Cevent: pwned"
                                                #\Return #\Linefeed))))))

(test regression-format-sse-event-retry-rejects-cr-lf
  "A string :retry carrying CR or LF must signal — same forge hazard."
  (is (%signals-error-p
       (lambda () (format-sse-event "tick" "1"
                                    :retry (format nil "5000~Cevent: pwned" #\Return)))))
  (is (%signals-error-p
       (lambda () (format-sse-event "tick" "1"
                                    :retry (format nil "5000~Cevent: pwned"
                                                    #\Linefeed)))))
  (is (stringp (format-sse-event "tick" "1" :retry 5000))
      "numeric :retry has no terminator and is accepted unchanged"))

(test regression-format-sse-event-event-type-rejects-cr-lf
  "EVENT-TYPE is a single-line field; CR or LF embedded in it could
   terminate the event: prefix and forge a sibling field."
  (is (%signals-error-p
       (lambda () (format-sse-event (format nil "ok~Cdata: pwned" #\Return) "x"))))
  (is (%signals-error-p
       (lambda () (format-sse-event (format nil "ok~Cdata: pwned" #\Linefeed) "x")))))

(test regression-format-sse-event-data-splits-on-bare-cr
  "A bare CR inside data terminates a line per WHATWG; the formatter
   re-prefixes every produced line with `data: ` so the browser cannot
   interpret the second half as a new field."
  (let ((s (format-sse-event "msg" (format nil "alpha~Cbeta" #\Return))))
    (is (search "data: alpha" s))
    (is (search "data: beta" s))
    (is (not (search (format nil "alpha~Cbeta" #\Return) s))
        "raw CR-joined string must not appear inside any single line")))

(test regression-format-sse-event-data-treats-crlf-as-one-terminator
  "CRLF is one line terminator, not two — the formatter must not emit
   a spurious empty `data:` line between alpha and beta."
  (let* ((s (format-sse-event "msg" (format nil "alpha~C~Cbeta"
                                             #\Return #\Linefeed)))
         (lines (uiop:split-string s :separator '(#\Newline)))
         (data-lines (remove-if-not (lambda (l)
                                      (and (>= (length l) 6)
                                           (string= "data: " l :end2 6)))
                                    lines)))
    (is (equal '("data: alpha" "data: beta") data-lines))))

(test regression-format-sse-event-data-splits-on-mixed-terminators
  "Mixed CR / LF / CRLF terminators in a single data string each
   produce one and only one line break in the output."
  (let* ((mixed (format nil "a~Cb~Cc~C~Cd" #\Return #\Linefeed #\Return #\Linefeed))
         (s (format-sse-event "msg" mixed))
         (lines (uiop:split-string s :separator '(#\Newline)))
         (data-lines (remove-if-not (lambda (l)
                                      (and (>= (length l) 6)
                                           (string= "data: " l :end2 6)))
                                    lines)))
    (is (equal '("data: a" "data: b" "data: c" "data: d") data-lines))))

(test regression-split-sse-data-lines-treats-crlf-as-one-terminator
  "The splitter handles CRLF, bare CR, and bare LF without a canonicalizing
   intermediate string."
  (is (equal '("a" "b" "c" "d")
             (lol-web/realtime::%split-sse-data-lines
              (format nil "a~C~Cb~Cc~Cd" #\Return #\Linefeed
                      #\Return #\Linefeed)))))

(test regression-sse-send-drops-oversized-data-before-format
  "Oversized data is rejected before format-sse-event builds the wire string."
  (let ((*sse-max-event-bytes* 4)
        (called nil))
    (let ((conn (lol-web/realtime::make-sse-connection
                 :stream (lambda (wire)
                           (setf called wire))
                 :channel "test")))
      (is (null (sse-send conn "msg" "12345")))
      (is (null called)))))

(test regression-format-sse-event-data-injection-cannot-forge-event
  "An attacker-controlled data string carrying `\\nevent: ` cannot
   forge a sibling event — every output line carries a `data: ` prefix."
  (let* ((payload (format nil "innocuous~Cevent: pwned~Cdata: leaked"
                          #\Linefeed #\Linefeed))
         (s (format-sse-event "msg" payload)))
    (is (search "data: innocuous" s))
    (is (search "data: event: pwned" s)
        "the forged `event: ` line must be quoted under a data: prefix")
    (is (not (search (format nil "~Cevent: pwned" #\Linefeed) s))
        "bare `event: pwned` line must never appear outside a data: prefix")))

;;; ============================================================================
;;; make-ws-handler / make-sse-handler — required :auth + :origin
;;; ============================================================================

(test regression-make-ws-handler-requires-auth-and-origin
  "make-ws-handler signals when :auth or :origin is missing; a streaming
   endpoint that ships without an explicit per-route policy is a
   construction error, not a runtime denial."
  (is (%signals-error-p
       (lambda () (make-ws-handler "chan"))))
  (is (%signals-error-p
       (lambda () (make-ws-handler
                   "chan"
                   :origin '("https://app.example.com")))))
  (is (%signals-error-p
       (lambda () (make-ws-handler
                   "chan"
                   :auth (lambda (env) (declare (ignore env)) t))))))

(test regression-make-ws-handler-returns-streaming-route-entry
  "On success make-ws-handler returns a streaming-route-entry whose
   policy fields read back the kwargs and whose body is a callable."
  (let* ((origin '("https://app.example.com"))
         (auth (lambda (env) (declare (ignore env)) t))
         (entry (make-ws-handler "chan" :auth auth :origin origin)))
    (is (lol-web/server:streaming-route-entry-p entry))
    (is (eq auth (lol-web/server:streaming-route-entry-auth entry)))
    (is (equal origin (lol-web/server:streaming-route-entry-origin entry)))
    (is (functionp (lol-web/server:streaming-route-entry-body entry)))))

(test regression-make-sse-handler-requires-auth-and-origin
  "make-sse-handler matches make-ws-handler's fail-closed contract:
   missing :auth or :origin signals at construction."
  (is (%signals-error-p
       (lambda () (make-sse-handler "chan"))))
  (is (%signals-error-p
       (lambda () (make-sse-handler
                   "chan"
                   :origin '("https://app.example.com")))))
  (is (%signals-error-p
       (lambda () (make-sse-handler
                   "chan"
                   :auth (lambda (env) (declare (ignore env)) t))))))

(test regression-make-sse-handler-returns-streaming-route-entry
  "make-sse-handler returns a streaming-route-entry; the body is a
   one-arg function ready for streaming-gate dispatch."
  (let* ((origin '("https://app.example.com"))
         (auth (lambda (env) (declare (ignore env)) t))
         (entry (make-sse-handler "chan" :auth auth :origin origin)))
    (is (lol-web/server:streaming-route-entry-p entry))
    (is (eq auth (lol-web/server:streaming-route-entry-auth entry)))
    (is (equal origin (lol-web/server:streaming-route-entry-origin entry)))
    (is (functionp (lol-web/server:streaming-route-entry-body entry)))))

;;; ============================================================================
;;; Per-IP and global connection caps — slot acquire / release shape
;;; ============================================================================

(test regression-ws-per-conn-rate-limit-evicts
  "The 5th simultaneous WebSocket from one IP is refused while the first
   four occupy slots; releasing one re-opens the slot for the next acquire.
   The check operates directly on the per-IP counter so it is independent
   of the websocket-driver runtime."
  (let ((lol-web/realtime::*ws-per-ip-counts* (make-hash-table :test 'equal))
        (lol-web/realtime::*ws-connections* (make-hash-table :test 'equal))
        (lol-web/realtime:*ws-per-ip-conn-cap* 4)
        (lol-web/realtime:*ws-global-conn-cap* 1024))
    (dotimes (i 4)
      (is (eq :ok (lol-web/realtime::%ws-acquire-slot "10.0.0.1"))
          "first four acquires succeed"))
    (is (eq :per-ip-full (lol-web/realtime::%ws-acquire-slot "10.0.0.1"))
        "fifth acquire from same IP is refused")
    (is (= 4 (lol-web/realtime:ws-per-ip-count "10.0.0.1"))
        "counter shows the cap is held")
    (lol-web/realtime::%ws-release-slot "10.0.0.1")
    (is (= 3 (lol-web/realtime:ws-per-ip-count "10.0.0.1"))
        "release drops the counter")
    (is (eq :ok (lol-web/realtime::%ws-acquire-slot "10.0.0.1"))
        "freed slot is reclaimed by the next acquire")))

(test regression-ws-global-cap-rejects
  "Global cap denies even when no single IP is near its per-IP cap. The
   global accounting uses the *ws-connections* table length; the test
   pre-populates with a sibling IP and trips the cap from a fresh peer."
  (let ((lol-web/realtime::*ws-per-ip-counts* (make-hash-table :test 'equal))
        (lol-web/realtime::*ws-connections* (make-hash-table :test 'equal))
        (lol-web/realtime:*ws-per-ip-conn-cap* 4)
        (lol-web/realtime:*ws-global-conn-cap* 2))
    (setf (gethash "ch" lol-web/realtime::*ws-connections*) (list :a :b))
    (is (eq :global-full (lol-web/realtime::%ws-acquire-slot "10.0.0.2"))
        "global cap denies a fresh peer once 2 sockets are open")))

(test regression-ws-frame-size-cap
  "make-ws-handler accepts :max-frame-size and forwards it to the
   websocket-driver server's :max-length keyword. The test asserts that
   the handler is constructed without error for a sub-megabyte cap."
  (let ((entry (make-ws-handler
                "chan"
                :auth (lambda (env) (declare (ignore env)) t)
                :origin '("https://app.example.com")
                :max-frame-size (* 16 1024))))
    (is (lol-web/server:streaming-route-entry-p entry))
    (is (functionp (lol-web/server:streaming-route-entry-body entry)))))

;;; ============================================================================
;;; Broadcast safety — text auto-escapes, safe-html refuses raw strings
;;; ============================================================================

(test regression-broadcast-text-escapes
  "ws-broadcast-text wraps the payload in JSON whose :text field has the
   attacker-controlled markup HTML-escaped. The test installs a stub
   ws-broadcast-json that captures its data argument so the assertion
   does not need a live websocket-driver runtime."
  (let ((captured nil)
        (lol-web/realtime::*ws-connections* (make-hash-table :test 'equal)))
    (let ((orig (symbol-function 'lol-web/realtime:ws-broadcast-json)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:ws-broadcast-json)
                  (lambda (channel data)
                    (declare (ignore channel))
                    (setf captured data)
                    1))
            (lol-web/realtime:ws-broadcast-text
             "ch" "#target" "<script>alert(1)</script>")
            (let ((text (cdr (assoc :text captured))))
              (is (stringp text))
              (is (not (search "<script>" text))
                  "raw <script> must not appear in the broadcast payload")
              (is (search "&lt;script&gt;" text)
                  "escaped form must appear")))
        (setf (symbol-function 'lol-web/realtime:ws-broadcast-json) orig)))))

(test regression-broadcast-safe-html-rejects-raw-string
  "ws-broadcast-safe-html signals when the caller passes a bare string;
   only safe-html-string values are accepted at the boundary."
  (is (%signals-error-p
       (lambda ()
         (lol-web/realtime:ws-broadcast-safe-html "ch" "#t" "<b>x</b>"))))
  ;; Positive path: a tagged safe-html-string is accepted. Stub broadcast-json
  ;; so the call does not require a live connection.
  (let ((captured nil)
        (lol-web/realtime::*ws-connections* (make-hash-table :test 'equal)))
    (let ((orig (symbol-function 'lol-web/realtime:ws-broadcast-json)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:ws-broadcast-json)
                  (lambda (channel data)
                    (declare (ignore channel))
                    (setf captured data)
                    1))
            (lol-web/realtime:ws-broadcast-safe-html
             "ch" "#t" (lol-web/html:make-safe-html-string "<b>x</b>"))
            (is (string= "<b>x</b>" (cdr (assoc :html captured))))
            (is (string= "html" (cdr (assoc :type captured)))))
        (setf (symbol-function 'lol-web/realtime:ws-broadcast-json) orig)))))

(test regression-sse-broadcast-text-escapes
  "sse-broadcast-text passes its payload to sse-broadcast with the
   attacker-controlled markup HTML-escaped under the :text key."
  (let ((captured nil))
    (let ((orig (symbol-function 'lol-web/realtime:sse-broadcast)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:sse-broadcast)
                  (lambda (channel event-type data &key id)
                    (declare (ignore channel event-type id))
                    (setf captured data)
                    1))
            (lol-web/realtime:sse-broadcast-text
             "ch" "#target" "<img src=x onerror=1>")
            (let ((text (cdr (assoc :text captured))))
              (is (stringp text))
              (is (not (search "<img" text))
                  "the raw < that opens an img tag must be escaped")
              (is (search "&lt;img" text)
                  "escaped form must appear")))
        (setf (symbol-function 'lol-web/realtime:sse-broadcast) orig)))))

(test regression-sse-broadcast-safe-html-rejects-raw-string
  "sse-broadcast-safe-html signals when the caller passes a bare string."
  (is (%signals-error-p
       (lambda ()
         (lol-web/realtime:sse-broadcast-safe-html "ch" "#t" "<b>x</b>")))))

;;; ============================================================================
;;; Broadcast-OOB safety — raw HTML rejected, hx-on stripped on the wire
;;; ============================================================================

(test regression-ws-broadcast-oob-refuses-raw-string
  "ws-broadcast-oob signals when a per-update HTML field is a bare string.
   The single-target ws-broadcast-safe-html sibling already enforces this
   contract; the OOB variant must enforce it identically — a bare string
   accepted at the boundary becomes an unescaped payload delivered to
   every connected peer."
  (is (%signals-error-p
       (lambda ()
         (lol-web/realtime:ws-broadcast-oob
          "ch"
          (list (list "#t" "<b>raw</b>" :swap "outerHTML")))))))

(test regression-sse-broadcast-oob-refuses-raw-string
  "sse-broadcast-oob mirrors ws-broadcast-oob's per-update type discipline."
  (is (%signals-error-p
       (lambda ()
         (lol-web/realtime:sse-broadcast-oob
          "ch"
          (list (list "#t" "<b>raw</b>" :swap "outerHTML")))))))

(test regression-ws-broadcast-oob-strips-hx-on-handlers
  "ws-broadcast-oob unwraps the safe-html-string then strips hx-on-*
   attributes before emitting on the wire.  A producer's safety claim
   covers HTML emission; broadcast amplification still needs the strip
   because hx-on-* arriving via a swap lifts to a native handler in the
   peer DOM."
  (let ((captured nil)
        (lol-web/realtime::*ws-connections* (make-hash-table :test 'equal)))
    (let ((orig (symbol-function 'lol-web/realtime:ws-broadcast-json)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:ws-broadcast-json)
                  (lambda (channel data)
                    (declare (ignore channel))
                    (setf captured data)
                    1))
            (lol-web/realtime:ws-broadcast-oob
             "ch"
             (list (list "#t"
                         (lol-web/html:make-safe-html-string
                          "<button hx-on-click=\"alert(1)\">x</button>")
                         :swap "outerHTML")))
            (let* ((updates (cdr (assoc :updates captured)))
                   (first   (first updates))
                   (html    (cdr (assoc :html first))))
              (is (stringp html))
              (is (not (search "hx-on-click" html))
                  "hx-on-* must not appear on the wire")
              (is (not (search "alert(1)" html))
                  "the JS payload must not survive the strip")
              (is (search "<button" html)
                  "non-attack content must survive the strip")
              (is (search "x</button>" html)
                  "tag body must survive the strip")))
        (setf (symbol-function 'lol-web/realtime:ws-broadcast-json) orig)))))

(test regression-sse-broadcast-oob-strips-hx-on-handlers
  "sse-broadcast-oob mirrors ws-broadcast-oob's hx-on strip."
  (let ((captured nil))
    (let ((orig (symbol-function 'lol-web/realtime:sse-broadcast)))
      (unwind-protect
          (progn
            (setf (symbol-function 'lol-web/realtime:sse-broadcast)
                  (lambda (channel event-type data &key id)
                    (declare (ignore channel event-type id))
                    (setf captured data)
                    1))
            (lol-web/realtime:sse-broadcast-oob
             "ch"
             (list (list "#t"
                         (lol-web/html:make-safe-html-string
                          "<a hx-on-click='evil()' href=\"/x\">link</a>")
                         :swap "outerHTML")))
            (let* ((updates (cdr (assoc :updates captured)))
                   (html    (cdr (assoc :html (first updates)))))
              (is (stringp html))
              (is (not (search "hx-on-click" html))
                  "hx-on-* must not appear on the wire")
              (is (not (search "evil()" html))
                  "the JS payload must not survive the strip")
              (is (search "href=\"/x\"" html)
                  "non-hx-on attributes must survive the strip")))
        (setf (symbol-function 'lol-web/realtime:sse-broadcast) orig)))))

(test regression-make-oob-update-refuses-raw-string
  "make-oob-update signals when HTML is a bare string — failing at the
   construction site is preferable to failing inside the broadcast loop."
  (is (%signals-error-p
       (lambda ()
         (lol-web/realtime:make-oob-update :target "#t" :html "<b>x</b>")))))

(test regression-make-oob-update-accepts-safe-html
  "make-oob-update returns the per-update spec when HTML is safe-html-string."
  (let ((u (lol-web/realtime:make-oob-update
            :target "#t"
            :html (lol-web/html:make-safe-html-string "<b>x</b>")
            :swap "innerHTML")))
    (is (string= "#t" (first u)))
    (is (lol-web/html:safe-html-string-p (second u)))
    (is (string= "innerHTML" (getf (cddr u) :swap)))))

;;; ============================================================================
;;; WS slot acquire+register atomic — counter never leaks past a fast disconnect
;;; ============================================================================

(test regression-ws-slot-acquire-and-register-atomic
  "The atomic helper bumps the per-IP counter AND pushes onto the channel
   registry under a single lock cycle. The close-path 'was-registered'
   reconciliation can therefore always find the connection and release."
  (let ((lol-web/realtime::*ws-per-ip-counts* (make-hash-table :test 'equal))
        (lol-web/realtime::*ws-connections* (make-hash-table :test 'equal))
        (lol-web/realtime:*ws-per-ip-conn-cap* 4)
        (lol-web/realtime:*ws-global-conn-cap* 1024))
    (let ((stub-ws :stub-ws))
      (is (eq :ok (lol-web/realtime::%ws-acquire-and-register
                   "10.0.0.5" "chan" stub-ws))
          "atomic acquire returns :ok")
      (is (= 1 (lol-web/realtime:ws-per-ip-count "10.0.0.5"))
          "per-IP counter bumped")
      (is (member stub-ws (gethash "chan" lol-web/realtime::*ws-connections*))
          "registry contains the ws — push happened under the same lock")
      ;; Cap fires correctly through the atomic helper.
      (dotimes (i 3)
        (declare (ignore i))
        (lol-web/realtime::%ws-acquire-and-register
         "10.0.0.5" "chan" (gensym)))
      (is (eq :per-ip-full (lol-web/realtime::%ws-acquire-and-register
                            "10.0.0.5" "chan" (gensym)))
          "cap denial works through atomic helper"))))

;;; ============================================================================
;;; broadcast-all releases lock before send — snapshot pattern
;;; ============================================================================

(test regression-ws-broadcast-all-releases-lock-before-send
  "ws-broadcast-all snapshots connections under *ws-lock*, then sends
   outside it. A slow consumer must not block other WS operations.
   The probe ws-send blocks on a barrier; we assert that ws-connection-count
   completes immediately while a broadcast is mid-send."
  (let ((lol-web/realtime::*ws-connections* (make-hash-table :test 'equal))
        (in-send (bordeaux-threads:make-semaphore))
        (release-send (bordeaux-threads:make-semaphore))
        (orig (symbol-function 'lol-web/realtime:ws-send)))
    (setf (gethash "ch" lol-web/realtime::*ws-connections*) (list :a))
    (unwind-protect
        (progn
          (setf (symbol-function 'lol-web/realtime:ws-send)
                (lambda (ws msg)
                  (declare (ignore ws msg))
                  (bordeaux-threads:signal-semaphore in-send)
                  (bordeaux-threads:wait-on-semaphore release-send)))
          ;; bordeaux-threads:make-thread does not inherit the parent's
          ;; dynamic environment; rebind the registry inside the child so
          ;; the broadcaster iterates the test's per-test hash-table.
          (let* ((shared-conns lol-web/realtime::*ws-connections*)
                 (broadcaster
                  (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((lol-web/realtime::*ws-connections* shared-conns))
                        (lol-web/realtime:ws-broadcast-all "msg")))
                    :name "broadcast-all-probe")))
            (bordeaux-threads:wait-on-semaphore in-send)
            ;; The broadcast is mid-send. Lock-bound reads must not block.
            (let ((counted (lol-web/realtime:ws-connection-count "ch")))
              (is (= 1 counted)
                  "ws-connection-count returned without waiting for ws-send"))
            (bordeaux-threads:signal-semaphore release-send)
            (bordeaux-threads:join-thread broadcaster)))
      (setf (symbol-function 'lol-web/realtime:ws-send) orig))))

(test regression-sse-broadcast-all-releases-lock-before-send
  "sse-broadcast-all snapshots under *sse-lock*, sends outside.
   Same shape as ws-broadcast-all — proven via the same blocking probe."
  (let ((lol-web/realtime::*sse-connections* (make-hash-table :test 'equal))
        (in-send (bordeaux-threads:make-semaphore))
        (release-send (bordeaux-threads:make-semaphore))
        (orig (symbol-function 'lol-web/realtime:sse-send))
        (conn (lol-web/realtime:make-sse-connection
                :stream (lambda (s) (declare (ignore s)))
                :channel "ch"
                :ip "10.0.0.9")))
    (setf (gethash "ch" lol-web/realtime::*sse-connections*) (list conn))
    (unwind-protect
        (progn
          (setf (symbol-function 'lol-web/realtime:sse-send)
                (lambda (c event-type data &key id)
                  (declare (ignore c event-type data id))
                  (bordeaux-threads:signal-semaphore in-send)
                  (bordeaux-threads:wait-on-semaphore release-send)
                  t))
          (let* ((shared-conns lol-web/realtime::*sse-connections*)
                 (broadcaster
                  (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((lol-web/realtime::*sse-connections* shared-conns))
                        (lol-web/realtime:sse-broadcast-all "msg" "data")))
                    :name "sse-broadcast-all-probe")))
            (bordeaux-threads:wait-on-semaphore in-send)
            (let ((counted (lol-web/realtime:sse-connection-count "ch")))
              (is (= 1 counted)
                  "sse-connection-count returned without waiting for sse-send"))
            (bordeaux-threads:signal-semaphore release-send)
            (bordeaux-threads:join-thread broadcaster)))
      (setf (symbol-function 'lol-web/realtime:sse-send) orig))))

;;; ============================================================================
;;; SSE global cap defaults below the Hunchentoot worker pool
;;; ============================================================================

(test regression-sse-global-cap-defaults-below-worker-pool
  "*sse-global-conn-cap* default must be <= the reference worker pool
   so a coordinated SSE flood cannot exhaust ordinary HTTP capacity."
  (is (and lol-web/realtime:*sse-global-conn-cap*
           lol-web/realtime:*sse-default-worker-pool-size*
           (<= lol-web/realtime:*sse-global-conn-cap*
               lol-web/realtime:*sse-default-worker-pool-size*))
      "default cap ~S must not exceed reference pool ~S"
      lol-web/realtime:*sse-global-conn-cap*
      lol-web/realtime:*sse-default-worker-pool-size*))

(test regression-defws-requires-auth-and-origin-keywords
  "DEFWS signals at macro-expansion when required streaming policy keywords are omitted."
  (is (%signals-error-p
       (lambda ()
         (macroexpand-1
          '(lol-web/realtime:defws "/ws/missing-auth" "ch"
            :origin '("https://app.example.com"))))))
  (is (%signals-error-p
       (lambda ()
         (macroexpand-1
          '(lol-web/realtime:defws "/ws/missing-origin" "ch"
            :auth (lambda (env) (declare (ignore env)) t)))))))

(test regression-defsse-requires-auth-and-origin-keywords
  "DEFSSE signals at macro-expansion when required streaming policy keywords
   are omitted; a form supplying both expands cleanly."
  (is (%signals-error-p
       (lambda ()
         (macroexpand-1
          '(lol-web/realtime:defsse "/sse/missing-auth" "ch"
            :origin '("https://app.example.com"))))))
  (is (%signals-error-p
       (lambda ()
         (macroexpand-1
          '(lol-web/realtime:defsse "/sse/missing-origin" "ch"
            :auth (lambda (env) (declare (ignore env)) t))))))
  (is (not (%signals-error-p
            (lambda ()
              (macroexpand-1
               '(lol-web/realtime:defsse "/sse/ok" "ch"
                 :auth (lambda (env) (declare (ignore env)) t)
                 :origin '("https://app.example.com"))))))
      "a defsse supplying both :auth and :origin must expand without error"))

(test regression-make-sse-handler-warns-when-cap-exceeds-pool
  "make-sse-handler emits a WARNING when *sse-global-conn-cap* exceeds
   the reference worker pool — operators see the misconfiguration at
   route construction, not at the first denied connect."
  (let ((lol-web/realtime:*sse-global-conn-cap* 999)
        (lol-web/realtime:*sse-default-worker-pool-size* 100)
        (warned nil))
    (handler-bind ((warning (lambda (w) (declare (ignore w))
                              (setf warned t)
                              (muffle-warning))))
      (lol-web/realtime:make-sse-handler
       "chan"
       :auth (lambda (env) (declare (ignore env)) t)
       :origin '("https://app.example.com")))
    (is (not (null warned))
        "expected a warning when cap exceeds reference pool")))

;;; ============================================================================
;;; sse-remove-connection uses opaque identity, not eq on the struct
;;; ============================================================================

(test regression-sse-remove-connection-uses-opaque-id
  "Removal matches on sse-connection-id, so even a fresh struct copy
   sharing the id removes the live registration. The previous eq-on-plist
   shape would silently leak in this scenario."
  (let* ((lol-web/realtime::*sse-connections*
           (make-hash-table :test 'equal))
         (conn (lol-web/realtime:make-sse-connection
                 :stream (lambda (s) (declare (ignore s)))
                 :channel "ch"
                 :ip "10.0.0.8")))
    (setf (gethash "ch" lol-web/realtime::*sse-connections*) (list conn))
    ;; Build a sibling struct that carries the same id but is not eq.
    (let ((sibling (lol-web/realtime::%make-sse-connection
                     :id (lol-web/realtime:sse-connection-id conn)
                     :stream (lambda (s) (declare (ignore s)))
                     :channel "ch"
                     :ip "10.0.0.8"
                     :created-at 0
                     :alive-p t
                     :on-disconnect nil)))
      (is (not (eq conn sibling))
          "siblings are not eq")
      (is (= (lol-web/realtime:sse-connection-id conn)
             (lol-web/realtime:sse-connection-id sibling))
          "siblings share id")
      (lol-web/realtime:sse-remove-connection "ch" sibling)
      (is (null (gethash "ch" lol-web/realtime::*sse-connections*))
          "removal by sibling-with-same-id drops the live entry"))))

;;; ============================================================================
;;; SSE effective cap is bounded by worker-pool headroom
;;; ============================================================================

(test regression-sse-effective-cap-bounded-by-worker-pool
  "The enforced SSE ceiling is the smaller of *sse-global-conn-cap* and the
   worker-pool headroom (*sse-default-worker-pool-size* minus
   *sse-worker-pool-reserve*): even a global cap far above the pool cannot let
   long-lived SSE streams starve ordinary HTTP traffic. %sse-acquire-slot
   enforces the effective cap, not the raw global cap."
  (let ((lol-web/realtime:*sse-global-conn-cap* 1000)
        (lol-web/realtime:*sse-default-worker-pool-size* 20)
        (lol-web/realtime:*sse-worker-pool-reserve* 16)
        (lol-web/realtime::*sse-connections* (make-hash-table :test 'equal))
        (lol-web/realtime::*sse-per-ip-counts* (make-hash-table :test 'equal)))
    (is (= 4 (lol-web/realtime::%sse-effective-global-cap))
        "effective cap must be min(global 1000, pool 20 - reserve 16 = 4), got ~S"
        (lol-web/realtime::%sse-effective-global-cap))
    (dotimes (i 4)
      (is (eq :ok (lol-web/realtime::%sse-acquire-slot nil))
          "acquire ~D within the effective cap must be :ok" i)
      (push i (gethash "ch" lol-web/realtime::*sse-connections*)))
    (is (eq :global-full (lol-web/realtime::%sse-acquire-slot nil))
        "the fifth acquire (total 4 >= effective cap 4) must be :global-full")))

;;; ============================================================================
;;; SSE per-IP slot released symmetrically — acquire/release pair never leaks
;;; ============================================================================

(test regression-sse-slot-released-across-connect-disconnect
  "Each connect/disconnect cycle releases the per-IP slot it took, so repeated
   cycles from one IP never exhaust the cap. sse-remove-connection is the
   confirmed-release counterpart to %sse-acquire-slot — were the release
   missing, the fifth cycle would trip :per-ip-full."
  (let ((lol-web/realtime::*sse-connections* (make-hash-table :test 'equal))
        (lol-web/realtime::*sse-per-ip-counts* (make-hash-table :test 'equal))
        (lol-web/realtime:*sse-per-ip-conn-cap* 4)
        (ip "10.0.0.20"))
    (dotimes (i 6)
      (is (eq :ok (lol-web/realtime::%sse-acquire-slot ip))
          "acquire ~D is :ok because the prior cycle released its slot" i)
      (let ((conn (lol-web/realtime:make-sse-connection
                    :stream (lambda (s) (declare (ignore s)))
                    :channel "ch"
                    :ip ip)))
        (push conn (gethash "ch" lol-web/realtime::*sse-connections*))
        (is (= 1 (lol-web/realtime:sse-per-ip-count ip))
            "slot held while the connection is registered")
        (lol-web/realtime:sse-remove-connection "ch" conn)
        (is (= 0 (lol-web/realtime:sse-per-ip-count ip))
            "removal released the per-IP slot")))
    (is (eq :ok (lol-web/realtime::%sse-acquire-slot ip))
        "after 6 connect/disconnect cycles a fresh acquire is still :ok")))

(test regression-sse-slot-released-via-reaper-path
  "sse-broadcast's dead-connection sweep routes through sse-remove-connection,
   so a connection whose writer throws is dropped from the registry AND has its
   per-IP slot released and on-disconnect fired exactly once. The raw
   set-difference cleanup it replaced did neither."
  (let ((lol-web/realtime::*sse-connections* (make-hash-table :test 'equal))
        (lol-web/realtime::*sse-per-ip-counts* (make-hash-table :test 'equal))
        (lol-web/realtime:*sse-per-ip-conn-cap* 4)
        (ip "10.0.0.21")
        (disconnected nil))
    (is (eq :ok (lol-web/realtime::%sse-acquire-slot ip))
        "slot acquired for the soon-to-be-dead connection")
    (let ((conn (lol-web/realtime:make-sse-connection
                  :stream (lambda (wire)
                            (declare (ignore wire))
                            (error "writer is dead"))
                  :channel "ch"
                  :ip ip
                  :on-disconnect (lambda (c)
                                   (declare (ignore c))
                                   (setf disconnected t)))))
      (push conn (gethash "ch" lol-web/realtime::*sse-connections*))
      (is (= 1 (lol-web/realtime:sse-per-ip-count ip))
          "slot held before the broadcast reap")
      (is (= 0 (lol-web/realtime:sse-broadcast "ch" "msg" "payload"))
          "the only connection's writer errors, so zero deliveries")
      (is (null (gethash "ch" lol-web/realtime::*sse-connections*))
          "the dead connection was removed from the registry")
      (is (= 0 (lol-web/realtime:sse-per-ip-count ip))
          "the reaper released the per-IP slot via sse-remove-connection")
      (is (not (null disconnected))
          "on-disconnect fired on the reaped connection"))))
