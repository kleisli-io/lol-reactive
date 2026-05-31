;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/REALTIME; Base: 10 -*-
;;;; Server-Sent Events (SSE) support for lol-reactive
;;;;
;;;; Provides SSE connection management and message broadcasting
;;;; for simpler server-to-client push scenarios.

(in-package :lol-web/realtime)

;;; ============================================================================
;;; CONNECTION REGISTRY
;;; ============================================================================

(defvar *sse-connections* (make-hash-table :test 'equal)
  "Active SSE connections indexed by channel ID.
   Each channel maps to a list of connection plists with :stream and :alive-p keys.")

(defvar *sse-per-ip-counts* (make-hash-table :test 'equal)
  "Open-SSE count per client IP. Slot occupancy is bumped on connect and
   released on disconnect so the cap enforces concurrent streams per peer,
   not lifetime connects.")

(defvar *sse-lock* (bordeaux-threads:make-lock "sse-connections-lock")
  "Lock for thread-safe access to *sse-connections* and *sse-per-ip-counts*.")

;;; ============================================================================
;;; SSE CONNECTION — opaque identity type
;;;
;;; Connection bookkeeping was previously a plist mutated in place; the
;;; registry compared with :test #'eq, which works only because the same
;;; cons identity was always handed back. A copy or shared-structure
;;; mutation would silently break removal. The struct gives each
;;; connection a strong id slot and a typed accessor surface; the
;;; registry matches on the id, not on the plist identity.
;;; ============================================================================

(defvar *sse-connection-id-counter* 0
  "Monotonic counter for assigning sse-connection ids. Protected by
   *sse-lock*; the id is opaque, only equality matters.")

(defstruct (sse-connection
            (:constructor %make-sse-connection)
            (:predicate sse-connection-p)
            (:copier nil))
  "Opaque per-connection record. The ID slot is the registry key; STREAM
   is the Clack writer closure; CHANNEL and IP are bookkeeping read by
   the cleanup paths; ALIVE-P is mutated by the worker loop and by
   sse-send to flag a dead peer; ON-DISCONNECT runs at remove time."
  (id 0 :type integer :read-only t)
  (stream nil :read-only t)
  (channel "" :type string :read-only t)
  (ip nil :read-only t)
  (created-at 0 :type integer :read-only t)
  (alive-p t :type boolean)
  (on-disconnect nil :read-only t))

(defun %next-sse-connection-id ()
  "Fresh integer id, incremented under *sse-lock*."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (incf *sse-connection-id-counter*)))

(defun make-sse-connection (&key stream channel ip on-disconnect)
  "Construct an sse-connection with a fresh id and the current universal
   time. The id assignment is the only side effect — registration onto
   *sse-connections* is the caller's responsibility."
  (%make-sse-connection :id (%next-sse-connection-id)
                        :stream stream
                        :channel channel
                        :ip ip
                        :created-at (get-universal-time)
                        :on-disconnect on-disconnect))

(defparameter *sse-per-ip-conn-cap* 4
  "Maximum simultaneous SSE connections from one client IP. A peer reaching
   the cap has further connects refused with 503 before the long-lived
   stream is opened. NIL disables the cap.")

(defparameter *sse-global-conn-cap* 64
  "Maximum simultaneous SSE connections across all IPs. Beyond this bound
   new connects are refused with 503 so the worker pool cannot be saturated
   by a coordinated flood. NIL disables the cap.

   The default sits well under typical Hunchentoot worker-pool sizes (100)
   because each live SSE connection holds a worker for its lifetime; a cap
   above the pool drives ordinary HTTP traffic to starvation. Raising this
   value requires correspondingly enlarging the Hunchentoot worker pool
   or moving SSE onto a non-thread-per-conn host.")

(defparameter *sse-default-worker-pool-size* 100
  "Reference value for the warning emitted by %warn-if-sse-cap-exceeds-workers.
   Read only by the warning helper; the SSE cap itself is enforced via
   *sse-global-conn-cap*. Override per-deployment when the Hunchentoot
   worker pool is sized differently.")

(defparameter *sse-worker-pool-reserve* 16
  "Workers kept free for ordinary HTTP traffic: the enforced SSE ceiling is
   *sse-default-worker-pool-size* minus this reserve, so long-lived streams
   cannot starve request/response traffic even if *sse-global-conn-cap* is
   raised above the pool. NIL reserves nothing.")

(defun %sse-effective-global-cap ()
  "Enforced ceiling on concurrent SSE connections: the smaller of the
   explicit *sse-global-conn-cap* and the worker-pool headroom
   (*sse-default-worker-pool-size* minus *sse-worker-pool-reserve*). Either
   bound may be NIL; the result is NIL only when both are."
  (let ((pool-bound (when *sse-default-worker-pool-size*
                      (max 0 (- *sse-default-worker-pool-size*
                                (or *sse-worker-pool-reserve* 0))))))
    (cond
      ((and *sse-global-conn-cap* pool-bound)
       (min *sse-global-conn-cap* pool-bound))
      (t (or *sse-global-conn-cap* pool-bound)))))

(defun %warn-if-sse-cap-exceeds-workers ()
  "Emit a runtime warning when *sse-global-conn-cap* exceeds the
   reference worker pool size. Called from make-sse-handler so the
   warning lands at registration, not at first connect."
  (when (and *sse-global-conn-cap*
             *sse-default-worker-pool-size*
             (> *sse-global-conn-cap* *sse-default-worker-pool-size*))
    (warn "sse: global cap ~S exceeds reference worker pool ~S; SSE clients ~
           past the pool size will starve ordinary HTTP traffic"
          *sse-global-conn-cap* *sse-default-worker-pool-size*)))

(defparameter *sse-max-event-bytes* (* 64 1024)
  "Maximum byte length of a single formatted SSE event payload (the wire
   text emitted by format-sse-event). Events past this cap are dropped at
   the broadcast site rather than written; NIL disables the cap.")

(define-condition sse-cap-exceeded ()
  ((scope :initarg :scope :reader sse-cap-exceeded-scope)
   (ip    :initarg :ip    :reader sse-cap-exceeded-ip))
  (:documentation
   "Signalled (not errored) when an SSE connect is refused because the
    per-IP or global cap was reached. SCOPE is :per-ip or :global; IP is
    the peer string or NIL. Observers handler-bind to count drops.")
  (:report
   (lambda (c s)
     (format s "sse-cap-exceeded: scope ~S ip ~S"
             (sse-cap-exceeded-scope c) (sse-cap-exceeded-ip c)))))

(defun sse-connection-count (&optional channel)
  "Return count of SSE connections.
   If CHANNEL is provided, count connections for that channel.
   Otherwise return total connection count."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (if channel
        (length (gethash channel *sse-connections*))
        (loop for conns being the hash-values of *sse-connections*
              sum (length conns)))))

(defun sse-channels ()
  "Return list of active SSE channel IDs."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (loop for channel being the hash-keys of *sse-connections*
          collect channel)))

(defun sse-per-ip-count (ip)
  "Current open SSE count for IP. 0 when IP is absent or NIL."
  (if (null ip)
      0
      (bordeaux-threads:with-lock-held (*sse-lock*)
        (gethash ip *sse-per-ip-counts* 0))))

(defun %sse-total-count-unlocked ()
  "Caller holds *sse-lock*. Total open SSE count across all channels."
  (loop for conns being the hash-values of *sse-connections*
        sum (length conns)))

(defun %sse-acquire-slot (ip)
  "Take one slot for IP. Returns :ok when the connect may proceed,
   :per-ip-full / :global-full when the cap is reached."
  (bordeaux-threads:with-lock-held (*sse-lock*)
    (let ((total (%sse-total-count-unlocked))
          (per-ip (gethash ip *sse-per-ip-counts* 0))
          (global-cap (%sse-effective-global-cap)))
      (cond
        ((and global-cap (>= total global-cap))
         :global-full)
        ((and *sse-per-ip-conn-cap* ip (>= per-ip *sse-per-ip-conn-cap*))
         :per-ip-full)
        (t
         (when ip
           (setf (gethash ip *sse-per-ip-counts*) (1+ per-ip)))
         :ok)))))

(defun %sse-release-slot-unlocked (ip)
  "Drop one slot for IP. Caller holds *SSE-LOCK* (which is non-recursive, so
   the confirmed-removal path in SSE-REMOVE-CONNECTION releases through this
   variant rather than re-entering the lock)."
  (when ip
    (let ((n (gethash ip *sse-per-ip-counts* 0)))
      (if (<= n 1)
          (remhash ip *sse-per-ip-counts*)
          (setf (gethash ip *sse-per-ip-counts*) (1- n))))))

(defun %sse-release-slot (ip)
  "Drop one slot for IP. Caller holds no lock."
  (when ip
    (bordeaux-threads:with-lock-held (*sse-lock*)
      (%sse-release-slot-unlocked ip))))

;;; ============================================================================
;;; SSE MESSAGE FORMATTING
;;; ============================================================================

(defun %sse-reject-line-terminators (field-name value)
  "Refuse VALUE for FIELD-NAME when its printed form carries CR or LF;
   either character terminates an SSE field and would forge a new event."
  (let ((s (princ-to-string value)))
    (when (or (find #\Return s) (find #\Newline s))
      (error "format-sse-event: ~A field contains a line terminator: ~S"
             field-name value))
    s))

(defun %split-sse-data-lines (s)
  "Split S on CR | LF | CRLF per WHATWG HTML §9.2.6; a CRLF pair counts
   as one terminator, bare CR and bare LF each count as one."
  (let ((n (length s))
        (start 0)
        (i 0)
        (lines nil))
    (loop while (< i n)
          for ch = (char s i)
          do (cond
               ((or (char= ch #\Return) (char= ch #\Newline))
                (push (subseq s start i) lines)
                (if (and (char= ch #\Return)
                         (< (1+ i) n)
                         (char= (char s (1+ i)) #\Newline))
                    (setf i (+ i 2)
                          start i)
                    (setf i (1+ i)
                          start i)))
               (t
                (incf i))))
    (nreverse (cons (subseq s start) lines))))

(defun format-sse-event (event-type data &key id retry)
  "Format an SSE event per WHATWG HTML §9.2.6. EVENT-TYPE, ID, and
   RETRY are single-line: embedded CR or LF signals. DATA splits on
   CR / LF / CRLF; non-string DATA is JSON-encoded."
  (with-output-to-string (s)
    (when id
      (format s "id: ~A~%" (%sse-reject-line-terminators "id" id)))
    (when retry
      (format s "retry: ~A~%" (%sse-reject-line-terminators "retry" retry)))
    (when event-type
      (format s "event: ~A~%" (%sse-reject-line-terminators "event" event-type)))
    (let ((data-str (if (stringp data)
                        data
                        (encode-json-string data))))
      (dolist (line (%split-sse-data-lines data-str))
        (format s "data: ~A~%" line)))
    (format s "~%")))

;;; ============================================================================
;;; SSE HANDLER CREATION
;;; ============================================================================

(defun make-sse-handler (channel &key on-connect on-disconnect
                                      (auth nil auth-supplied-p)
                                      (origin nil origin-supplied-p))
  "Build a streaming-route-entry for an SSE channel. The entry's body is
   a Clack application that opens the long-lived event stream; the policy
   fields are read by streaming-gate to deny the connection before any
   bytes leave the writer.

   CHANNEL: String identifying the channel (e.g., \"updates\", \"notifications\")
   ON-CONNECT: Called with (conn) when connection opens
   ON-DISCONNECT: Called with (conn) when connection closes
   :AUTH    REQUIRED. One-arg callable (env -> generalised boolean);
            streaming-gate denies with 401 when it returns NIL.
   :ORIGIN  REQUIRED. List of allowed origin strings matched verbatim by
            validate-origin (RFC 6454 §5). Empty list denies every request.

   Per-IP and global connection caps (*sse-per-ip-conn-cap*,
   *sse-global-conn-cap*) are enforced at handler entry: when either is
   reached the connect is refused with 503 \"capacity\" before the
   long-lived writer is opened, and an sse-cap-exceeded signal fires
   for observers.

   Returns a streaming-route-entry suitable for storing in
   lol-web/server::*streaming-routes* (defsse does that automatically)."
  (unless auth-supplied-p
    (error "make-sse-handler: :auth is required (no default)."))
  (unless origin-supplied-p
    (error "make-sse-handler: :origin is required (no default)."))
  (lol-web/server::%require-streaming-policy "make-sse-handler" auth origin)
  (%warn-if-sse-cap-exceeds-workers)
  (lol-web/server:make-streaming-route-entry
   :auth auth
   :origin origin
   :body
   (lambda (env)
    (declare (ignore env))
    (let* ((ip (lol-web/server:client-ip))
           (acquired (%sse-acquire-slot ip)))
      (cond
        ((not (eq acquired :ok))
         (signal 'sse-cap-exceeded
                 :scope (ecase acquired
                          (:per-ip-full :per-ip)
                          (:global-full :global))
                 :ip ip)
         (list 503
               (list :content-type "text/plain; charset=utf-8")
               (list "capacity")))
        (t
         ;; Return delayed/streaming response
         (lambda (responder)
           (block sse-handler
             (let ((writer nil)
                   (conn nil))
               ;; The slot was taken by %sse-acquire-slot above. One
               ;; unwind-protect owns its release: a confirmed
               ;; sse-remove-connection once CONN is registered, else a bare
               ;; %sse-release-slot if the responder threw before register.
               (unwind-protect
                   (progn
                     (setf writer (funcall responder
                                           '(200 (:content-type "text/event-stream"
                                                  :cache-control "no-cache"
                                                  :connection "keep-alive"
                                                  :x-accel-buffering "no"))))
                     (setf conn (make-sse-connection :stream writer
                                                     :channel channel
                                                     :ip ip
                                                     :on-disconnect on-disconnect))
                     (bordeaux-threads:with-lock-held (*sse-lock*)
                       (push conn (gethash channel *sse-connections*)))
                     (handler-case
                         (progn
                           (funcall writer (format-sse-event "connected"
                                                             `((:channel . ,channel)
                                                               (:timestamp . ,(get-universal-time)))
                                                             :retry 3000))
                           (when on-connect
                             (funcall on-connect conn)))
                       (error (e)
                         (declare (ignore e))
                         ;; Initial event failed; unwind-protect releases.
                         (return-from sse-handler nil)))

          ;;; ─────────────────────────────────────────────────────────────────
          ;;; CONSTRAINT: thread-per-connection (Hunchentoot)
          ;;;
          ;;; Server-Sent Events under Hunchentoot occupy one worker thread
          ;;; per active connection for the lifetime of that connection. The
          ;;; loop below blocks the worker (alternating writes + sleep 30) so
          ;;; the underlying TCP stream stays open and the writer closure
          ;;; remains valid; returning would let the worker tear the response
          ;;; down. Nothing here is async — every connection is a held thread.
          ;;;
          ;;; Implications:
          ;;;   - Total concurrent SSE clients is bounded by the Hunchentoot
          ;;;     worker pool (default 100). Beyond that, new SSE requests
          ;;;     queue or are rejected; ordinary HTTP traffic competes for
          ;;;     the same workers.
          ;;;   - Long-lived dashboards / push streams should run on a
          ;;;     dedicated server instance (or a non-thread-per-conn host
          ;;;     like Woo) when client counts grow into the hundreds.
          ;;;   - The 30-second cadence is a heartbeat, not a poll interval.
          ;;;     Real events flow through sse-send / sse-broadcast, which
          ;;;     write to the held :stream from any thread.
          ;;; ─────────────────────────────────────────────────────────────────
                     (loop while (sse-connection-alive-p conn)
                           do (handler-case
                                  (progn
                                    ;; Heartbeat keeps proxies / NAT entries
                                    ;; from treating the connection as idle.
                                    (funcall writer (format nil ": keepalive~%~%"))
                                    (sleep 30))
                                (error (e)
                                  (declare (ignore e))
                                  (setf (sse-connection-alive-p conn) nil)))))
                 ;; Release the acquired slot exactly once.
                 (if conn
                     (sse-remove-connection channel conn)
                     (%sse-release-slot ip))))))))))))

(defun sse-remove-connection (channel conn)
  "Remove an SSE connection from the registry by its opaque id, release its
   per-IP slot, and call its disconnect handler. Matching by id (not by EQ on
   the struct) makes the cleanup robust against future copies, logging
   wrappers, or any other path that may reconstitute the connection record
   without preserving cons identity.

   The slot release and the disconnect callback fire only when THIS call is
   the one that removes the connection from the registry — so a connection
   reaped by more than one path (the handler unwind, the broadcast dead-sweep,
   the ping reaper) releases its slot and notifies exactly once. Mirrors the
   WebSocket close-path was-registered confirmed release."
  (let ((target-id (sse-connection-id conn))
        (removed nil))
    (bordeaux-threads:with-lock-held (*sse-lock*)
      (let* ((current (gethash channel *sse-connections*))
             (next (remove target-id current :key #'sse-connection-id)))
        (when (< (length next) (length current))
          (setf (gethash channel *sse-connections*) next
                removed t)
          (%sse-release-slot-unlocked (sse-connection-ip conn)))))
    (when removed
      (let ((on-disconnect (sse-connection-on-disconnect conn)))
        (when on-disconnect
          (handler-case
              (funcall on-disconnect conn)
            (error () nil)))))))

;;; ============================================================================
;;; MESSAGE SENDING
;;; ============================================================================

(defun sse-send (conn event-type data &key id)
  "Send an SSE event to a specific connection.
   Returns T on success, NIL if connection is dead or the formatted event
   exceeds *sse-max-event-bytes*."
  (let* ((stream (sse-connection-stream conn))
         (data-str (if (stringp data) data (encode-json-string data))))
    (cond
      ((and *sse-max-event-bytes*
            (> (babel:string-size-in-octets data-str :encoding :utf-8)
               *sse-max-event-bytes*))
       nil)
      (t
       (let ((wire (format-sse-event event-type data-str :id id)))
         (cond
           ((and *sse-max-event-bytes*
                 (> (babel:string-size-in-octets wire :encoding :utf-8)
                    *sse-max-event-bytes*))
            nil)
           (t
            (handler-case
                (progn
                  (funcall stream wire)
                  t)
              (error (e)
                (declare (ignore e))
                (setf (sse-connection-alive-p conn) nil)
                nil)))))))))

(defun sse-send-comment (conn comment)
  "Send an SSE comment (keep-alive ping).
   Comments start with colon and are ignored by EventSource."
  (let ((stream (sse-connection-stream conn)))
    (handler-case
        (progn
          (funcall stream (format nil ": ~A~%~%" comment))
          t)
      (error ()
        (setf (sse-connection-alive-p conn) nil)
        nil))))

;;; ============================================================================
;;; BROADCASTING
;;; ============================================================================

(defun sse-broadcast (channel event-type data &key id)
  "Broadcast an SSE event to all connections on a channel.
   Returns count of connections that received the message.
   Dead connections are automatically removed."
  (let ((connections (bordeaux-threads:with-lock-held (*sse-lock*)
                       (copy-list (gethash channel *sse-connections*))))
        (sent 0)
        (dead nil))
    (dolist (conn connections)
      (if (sse-send conn event-type data :id id)
          (incf sent)
          (push conn dead)))
    ;; Route dead connections through the confirmed-release path so each
    ;; releases its per-IP slot and fires on-disconnect exactly once,
    ;; rather than dropping them from the registry with a raw set-difference.
    (dolist (conn dead)
      (sse-remove-connection channel conn))
    sent))

(defun sse-broadcast-all (event-type data)
  "Broadcast an SSE event to ALL connections across all channels.
   Snapshots the flat connection list under *sse-lock*, then sends
   without the lock so a slow consumer cannot stall every other SSE
   operation — mirrors sse-broadcast's discipline."
  (let ((connections (bordeaux-threads:with-lock-held (*sse-lock*)
                       (loop for conns being the hash-values of *sse-connections*
                             append (copy-list conns))))
        (total 0))
    (dolist (conn connections total)
      (when (sse-send conn event-type data)
        (incf total)))))

(defun sse-ping-all ()
  "Send keep-alive ping to all SSE connections. Returns count of live
   connections. Snapshots channel + connection pairs under the lock,
   then sends and removes outside the lock — same shape as
   sse-broadcast-all."
  (let ((snapshot (bordeaux-threads:with-lock-held (*sse-lock*)
                    (let ((acc nil))
                      (maphash (lambda (channel connections)
                                 (dolist (conn connections)
                                   (push (cons channel conn) acc)))
                               *sse-connections*)
                      acc)))
        (alive 0)
        (dead nil))
    (dolist (entry snapshot)
      (let ((conn (cdr entry)))
        (if (sse-send-comment conn "ping")
            (incf alive)
            (push entry dead))))
    (dolist (entry dead)
      (sse-remove-connection (car entry) (cdr entry)))
    alive))

(defun sse-broadcast-text (channel target-id text &key id)
  "Broadcast TEXT to a channel as a text-content update. TEXT is escaped
   so it is safe to render into the DOM's textContent; callers who already
   hold trusted markup must use sse-broadcast-safe-html with a
   safe-html-string wrapper."
  (sse-broadcast channel "text"
    `((:target . ,target-id)
      (:text . ,(lol-web/escape:escape-html
                 (if (stringp text) text (princ-to-string text)))))
    :id id))

;;; ============================================================================
;;; HTMX INTEGRATION
;;; ============================================================================

(defun sse-broadcast-safe-html (channel target-id html &key (swap "innerHTML") id)
  "Broadcast a SAFE-HTML-STRING update to all SSE connections on a channel.
   HTML must be a safe-html-string the producer asserted is safe to emit
   verbatim; a bare string signals at the broadcast site rather than
   delivering an unescaped attacker payload to every connected peer."
  (check-type html lol-web/html:safe-html-string)
  (sse-broadcast channel "update"
    `((:target . ,target-id)
      (:html . ,(lol-web/html:safe-html-string-value html))
      (:swap . ,swap))
    :id id))

(defun sse-broadcast-oob (channel updates &key id)
  "Broadcast out-of-band updates to all SSE connections on a channel.
   UPDATES is a list of (target-id html &key swap) specifications.

   Each HTML must be a SAFE-HTML-STRING. A bare string signals at the
   broadcast boundary rather than amplifying an unescaped payload to
   every connected peer. `hx-on-*' attributes are stripped from the
   wire-emitted markup — see `ws-broadcast-oob' for rationale."
  (sse-broadcast channel "oob"
    `((:updates . ,(mapcar (lambda (u)
                             (destructuring-bind (target-id html &key (swap "outerHTML")) u
                               (check-type html lol-web/html:safe-html-string)
                               `((:target . ,target-id)
                                 (:html . ,(lol-web/escape:sanitize-hx-on-attrs
                                            (lol-web/html:safe-html-string-value html)))
                                 (:swap . ,swap))))
                           updates)))
    :id id))

(defun sse-broadcast-trigger (channel event &optional detail)
  "Broadcast an event trigger to all SSE connections on a channel.
   CLIENT-SIDE: Will dispatch a CustomEvent with the given name and detail."
  (sse-broadcast channel "trigger"
    `((:event . ,event)
      ,@(when detail `((:detail . ,detail))))))

;;; ============================================================================
;;; ROUTE REGISTRATION HELPER
;;; ============================================================================

(defmacro defsse (path channel &key on-connect on-disconnect
                                    ((:auth auth) nil auth-supplied-p)
                                    ((:origin origin) nil origin-supplied-p))
  "Define an SSE route.

   PATH: URL path for SSE endpoint (e.g., \"/sse/updates\")
   CHANNEL: Channel name for connection grouping
   ON-CONNECT: Handler called when connection opens (conn)
   ON-DISCONNECT: Handler called on disconnect (conn)
   :AUTH    REQUIRED. One-arg callable (env -> generalised boolean) read by
            streaming-gate; NIL denies the connection with 401.
   :ORIGIN  REQUIRED. List of allowed origin strings (verbatim RFC 6454);
            empty list denies every request with 403.

   Example:
     (defsse \"/sse/notifications\" \"notifications\"
       :auth   (lambda (env) (let ((*env* env)) (current-principal)))
       :origin '(\"https://app.example.com\")
       :on-connect (lambda (conn)
                     (log:info \"Client connected to notifications\")))"
  (unless auth-supplied-p
    (error "DEFSSE requires an explicit :AUTH argument"))
  (unless origin-supplied-p
    (error "DEFSSE requires an explicit :ORIGIN argument"))
  `(bordeaux-threads:with-recursive-lock-held (lol-web/server::*routes-lock*)
     (setf (gethash (cons :get ,path) lol-web/server::*streaming-routes*)
           (make-sse-handler ,channel
                             :on-connect ,on-connect
                             :on-disconnect ,on-disconnect
                             :auth ,auth
                             :origin ,origin))))
