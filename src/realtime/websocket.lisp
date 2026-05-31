;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/REALTIME; Base: 10 -*-
;;;; WebSocket support for lol-reactive via websocket-driver
;;;;
;;;; Provides WebSocket connection management and message broadcasting
;;;; for real-time bidirectional communication.

(in-package :lol-web/realtime)

;;; ============================================================================
;;; CONNECTION REGISTRY
;;; ============================================================================

(defvar *ws-connections* (make-hash-table :test 'equal)
  "Active WebSocket connections indexed by channel ID.
   Each channel maps to a list of websocket-driver ws objects.")

(defvar *ws-per-ip-counts* (make-hash-table :test 'equal)
  "Open-WebSocket count per client IP. Slot occupancy is bumped on
   :open and released on :close so the cap enforces concurrent connections
   per peer, not lifetime upgrades.")

(defvar *ws-lock* (bordeaux-threads:make-lock "ws-connections-lock")
  "Lock for thread-safe access to *ws-connections* and *ws-per-ip-counts*.")

(defparameter *ws-per-ip-conn-cap* 4
  "Maximum simultaneous WebSocket connections from one client IP. A peer
   reaching the cap has further upgrades closed immediately after the
   handshake completes; existing connections are preserved. NIL disables
   the cap. Bound at make-app time via :ws-per-ip-conn-cap.")

(defparameter *ws-global-conn-cap* 1024
  "Maximum simultaneous WebSocket connections across all IPs. Beyond this
   bound the server closes new upgrades after handshake so the worker
   pool cannot be saturated by a coordinated flood. NIL disables the cap.")

(defparameter *ws-max-frame-size* (* 64 1024)
  "Default per-frame payload byte cap for inbound WebSocket messages.
   Passed to websocket-driver-server's :max-length. Per-route override
   via make-ws-handler :max-frame-size. NIL disables the cap.")

(define-condition ws-cap-exceeded ()
  ((scope :initarg :scope :reader ws-cap-exceeded-scope)
   (ip    :initarg :ip    :reader ws-cap-exceeded-ip))
  (:documentation
   "Signalled (not errored) when a WebSocket upgrade is closed after
    handshake because the per-IP or global cap was reached. SCOPE is
    :per-ip or :global; IP is the connecting peer or NIL when unknown.
    Observers handler-bind to count drops; unhandled signals are no-ops.")
  (:report
   (lambda (c s)
     (format s "ws-cap-exceeded: scope ~S ip ~S"
             (ws-cap-exceeded-scope c) (ws-cap-exceeded-ip c)))))

(defun ws-connection-count (&optional channel)
  "Return count of WebSocket connections.
   If CHANNEL is provided, count connections for that channel.
   Otherwise return total connection count."
  (bordeaux-threads:with-lock-held (*ws-lock*)
    (if channel
        (length (gethash channel *ws-connections*))
        (loop for conns being the hash-values of *ws-connections*
              sum (length conns)))))

(defun ws-channels ()
  "Return list of active channel IDs."
  (bordeaux-threads:with-lock-held (*ws-lock*)
    (loop for channel being the hash-keys of *ws-connections*
          collect channel)))

(defun ws-per-ip-count (ip)
  "Current open WebSocket count for IP. 0 when IP is absent or NIL."
  (if (null ip)
      0
      (bordeaux-threads:with-lock-held (*ws-lock*)
        (gethash ip *ws-per-ip-counts* 0))))

(defun %ws-total-count-unlocked ()
  "Caller holds *ws-lock*. Total open WebSocket count across all channels."
  (loop for conns being the hash-values of *ws-connections*
        sum (length conns)))

(defun %ws-acquire-slot (ip)
  "Take one slot for IP. Returns :ok when the upgrade may proceed,
   :per-ip-full / :global-full when the cap is reached. Updates
   *ws-per-ip-counts* only on :ok; the registry push is the caller's
   responsibility once :open fires."
  (bordeaux-threads:with-lock-held (*ws-lock*)
    (let ((total (%ws-total-count-unlocked))
          (per-ip (gethash ip *ws-per-ip-counts* 0)))
      (cond
        ((and *ws-global-conn-cap* (>= total *ws-global-conn-cap*))
         :global-full)
        ((and *ws-per-ip-conn-cap* ip (>= per-ip *ws-per-ip-conn-cap*))
         :per-ip-full)
        (t
         (when ip
           (setf (gethash ip *ws-per-ip-counts*) (1+ per-ip)))
         :ok)))))

(defun %ws-release-slot (ip)
  "Drop one slot for IP. Caller holds no lock. Hash entry is removed when
   the count falls to zero so the table size tracks the active IP set."
  (when ip
    (bordeaux-threads:with-lock-held (*ws-lock*)
      (let ((n (gethash ip *ws-per-ip-counts* 0)))
        (if (<= n 1)
            (remhash ip *ws-per-ip-counts*)
            (setf (gethash ip *ws-per-ip-counts*) (1- n)))))))

(defun %ws-acquire-and-register (ip channel ws)
  "Atomically bump the per-IP / global counters AND push WS onto the
   channel registry under a single *ws-lock* cycle. Returns :ok when
   the upgrade may proceed, :per-ip-full / :global-full otherwise.

   The push happens inside the same lock that bumps the counter so the
   close-path 'was-registered' reconciliation can never observe a
   bumped counter alongside an empty registry: either both happened or
   neither did. A peer that abandons the handshake between :open and
   :close fires :close with WS in the registry, the registry-remove
   succeeds, and the slot is released. No leaked counter."
  (bordeaux-threads:with-lock-held (*ws-lock*)
    (let ((total (%ws-total-count-unlocked))
          (per-ip (gethash ip *ws-per-ip-counts* 0)))
      (cond
        ((and *ws-global-conn-cap* (>= total *ws-global-conn-cap*))
         :global-full)
        ((and *ws-per-ip-conn-cap* ip (>= per-ip *ws-per-ip-conn-cap*))
         :per-ip-full)
        (t
         (when ip
           (setf (gethash ip *ws-per-ip-counts*) (1+ per-ip)))
         (push ws (gethash channel *ws-connections*))
         :ok)))))

;;; ============================================================================
;;; WEBSOCKET HANDLER CREATION
;;; ============================================================================

(defun make-ws-handler (channel &key on-open on-message on-close on-error
                                     (auth nil auth-supplied-p)
                                     (origin nil origin-supplied-p)
                                     (max-frame-size *ws-max-frame-size*))
  "Build a streaming-route-entry for a WebSocket channel. The entry's body
   is a Clack application performing the upgrade and dispatching event
   handlers; the policy fields are read by streaming-gate to deny the
   upgrade before any WebSocket frame is read.

   CHANNEL: String identifying the channel (e.g., \"chat\", \"notifications\")
   ON-OPEN: Called with (ws) when connection opens
   ON-MESSAGE: Called with (ws message) when message received
   ON-CLOSE: Called with (ws &key code reason) when connection closes
   ON-ERROR: Called with (ws error) on protocol error
   :AUTH    REQUIRED. One-arg callable (env -> generalised boolean);
            streaming-gate denies with 401 when it returns NIL.
   :ORIGIN  REQUIRED. List of allowed origin strings matched verbatim by
            validate-origin (RFC 6454 §5). Empty list denies every request.
   :MAX-FRAME-SIZE  Per-frame payload byte cap passed to the websocket-driver
            server as :max-length. Defaults to *ws-max-frame-size*; NIL
            disables. Oversize frames terminate the connection at the driver.

   Per-IP and global connection caps (*ws-per-ip-conn-cap*,
   *ws-global-conn-cap*) are enforced after the upgrade handshake: when
   either is reached the new socket is closed with code 1013 (\"try again
   later\") before any payload is sent or registered, and a ws-cap-exceeded
   signal fires for observers.

   Returns a streaming-route-entry suitable for storing in
   lol-web/server::*streaming-routes* (defws does that automatically)."
  (unless auth-supplied-p
    (error "make-ws-handler: :auth is required (no default)."))
  (unless origin-supplied-p
    (error "make-ws-handler: :origin is required (no default)."))
  (lol-web/server::%require-streaming-policy "make-ws-handler" auth origin)
  (lol-web/server:make-streaming-route-entry
   :auth auth
   :origin origin
   :body
   (lambda (env)
     (let* ((ip (lol-web/server:client-ip))
            (ws (apply #'websocket-driver.server:make-server env
                       (when max-frame-size
                         (list :max-length max-frame-size)))))
       (event-emitter:on :open ws
         (lambda ()
           (let ((acquired (%ws-acquire-and-register ip channel ws)))
             (case acquired
               (:ok
                (when on-open
                  (funcall on-open ws)))
               (t
                (signal 'ws-cap-exceeded
                        :scope (ecase acquired
                                 (:per-ip-full :per-ip)
                                 (:global-full :global))
                        :ip ip)
                (handler-case
                    (websocket-driver.ws.base:close-connection
                     ws :code 1013 :reason "capacity")
                  (error () nil)))))))

       (event-emitter:on :message ws
         (lambda (message)
           (when on-message
             (funcall on-message ws message))))

       (event-emitter:on :close ws
         (lambda (&key code reason)
           (let ((was-registered
                  (bordeaux-threads:with-lock-held (*ws-lock*)
                    (let ((conns (gethash channel *ws-connections*)))
                      (cond
                        ((member ws conns)
                         (setf (gethash channel *ws-connections*)
                               (remove ws conns))
                         t)
                        (t nil))))))
             (when was-registered
               (%ws-release-slot ip)))
           (when on-close
             (funcall on-close ws :code code :reason reason))))

       (event-emitter:on :error ws
         (lambda (error)
           (when on-error
             (funcall on-error ws error))))

       (lambda (responder)
         (declare (ignore responder))
         ;; start-connection sends 101 Switching Protocols directly to socket
         ;; and enters blocking read loop until connection closes.
         (websocket-driver.ws.base:start-connection ws)
         ;; Mark headers as sent so Hunchentoot does not try to emit a second
         ;; response after start-connection returns on close.
         (setf hunchentoot::*headers-sent* t))))))

;;; ============================================================================
;;; MESSAGE SENDING
;;; ============================================================================

(defun ws-send (ws message)
  "Send a message to a WebSocket connection.
   MESSAGE can be a string (sent as text) or byte vector (sent as binary)."
  (websocket-driver.ws.base:send ws message))

(defun ws-send-text (ws text)
  "Send a text message to a WebSocket connection."
  (websocket-driver.ws.base:send-text ws text))

(defun ws-send-binary (ws data)
  "Send binary data to a WebSocket connection."
  (websocket-driver.ws.base:send-binary ws data))

(defun ws-send-json (ws data)
  "Send data as JSON to a WebSocket connection."
  (ws-send-text ws (encode-json-string data)))

(defun ws-close (ws &key code reason)
  "Close a WebSocket connection."
  (websocket-driver.ws.base:close-connection ws :code code :reason reason))

;;; ============================================================================
;;; BROADCASTING
;;; ============================================================================

(defun ws-broadcast (channel message)
  "Broadcast a message to all connections on a channel.
   Returns count of connections that received the message."
  (let ((connections (bordeaux-threads:with-lock-held (*ws-lock*)
                       (copy-list (gethash channel *ws-connections*))))
        (sent 0))
    (dolist (ws connections sent)
      (handler-case
          (progn
            (ws-send ws message)
            (incf sent))
        (error (e)
          (declare (ignore e))
          ;; Connection probably dead, will be cleaned up on close event
          nil)))))

(defun ws-broadcast-json (channel data)
  "Broadcast data as JSON to all connections on a channel."
  (ws-broadcast channel (encode-json-string data)))

(defun ws-broadcast-all (message)
  "Broadcast a message to ALL WebSocket connections across all channels.
   Snapshots the flat connection list under *ws-lock*, then releases the
   lock before sending. A slow or stuck consumer cannot stall every
   other WS operation — same shape as ws-broadcast."
  (let ((connections (bordeaux-threads:with-lock-held (*ws-lock*)
                       (loop for conns being the hash-values of *ws-connections*
                             append (copy-list conns))))
        (total 0))
    (dolist (ws connections total)
      (handler-case
          (progn
            (ws-send ws message)
            (incf total))
        (error () nil)))))

(defun ws-broadcast-text (channel target-id text)
  "Broadcast TEXT to a channel as a text-content update. TEXT is escaped
   so it is safe to render into the DOM's textContent; callers who already
   hold trusted markup must use ws-broadcast-safe-html with a
   safe-html-string wrapper."
  (ws-broadcast-json channel
    `((:type . "text")
      (:target . ,target-id)
      (:text . ,(lol-web/escape:escape-html
                 (if (stringp text) text (princ-to-string text)))))))

;;; ============================================================================
;;; HTMX INTEGRATION
;;; ============================================================================

(defun ws-broadcast-safe-html (channel target-id html &key (swap "innerHTML"))
  "Broadcast a SAFE-HTML-STRING update to all connections on a channel.
   HTML must be a safe-html-string the producer has asserted is safe to
   emit verbatim; a bare string signals at the broadcast site rather than
   delivering an unescaped attacker payload to every connected peer."
  (check-type html lol-web/html:safe-html-string)
  (ws-broadcast-json channel
    `((:type . "html")
      (:target . ,target-id)
      (:html . ,(lol-web/html:safe-html-string-value html))
      (:swap . ,swap))))

(defun ws-broadcast-oob (channel updates)
  "Broadcast out-of-band updates to all connections on a channel.
   UPDATES is a list of (target-id html &key swap) specifications.

   Each HTML must be a SAFE-HTML-STRING. A bare string signals at the
   broadcast boundary rather than amplifying an unescaped payload to
   every connected peer. `hx-on-*' attributes are stripped from the
   wire-emitted markup — broadcast-sourced HTML cannot lift handlers
   into the peer DOM, where it would acquire native-handler privilege."
  (ws-broadcast-json channel
    `((:type . "oob")
      (:updates . ,(mapcar (lambda (u)
                             (destructuring-bind (target-id html &key (swap "outerHTML")) u
                               (check-type html lol-web/html:safe-html-string)
                               `((:target . ,target-id)
                                 (:html . ,(lol-web/escape:sanitize-hx-on-attrs
                                            (lol-web/html:safe-html-string-value html)))
                                 (:swap . ,swap))))
                           updates)))))

(defun make-oob-update (&key target html (swap "outerHTML"))
  "Build a per-update spec for `ws-broadcast-oob' / `sse-broadcast-oob'.
   HTML must be a SAFE-HTML-STRING; the constructor asserts the type
   up-front so producers fail at the construction site rather than at
   the broadcast site."
  (check-type target string)
  (check-type html lol-web/html:safe-html-string)
  (list target html :swap swap))

(defun ws-broadcast-trigger (channel event &optional detail)
  "Broadcast an event trigger to all connections on a channel.
   CLIENT-SIDE: Will dispatch a CustomEvent with the given name and detail."
  (ws-broadcast-json channel
    `((:type . "trigger")
      (:event . ,event)
      ,@(when detail `((:detail . ,detail))))))

;;; ============================================================================
;;; ROUTE REGISTRATION HELPER
;;; ============================================================================

(defmacro defws (path channel &key on-open on-message on-close on-error
                                    ((:auth auth) nil auth-supplied-p)
                                    ((:origin origin) nil origin-supplied-p))
  "Define a WebSocket route.

   PATH: URL path for WebSocket endpoint (e.g., \"/ws/chat\")
   CHANNEL: Channel name for connection grouping
   ON-OPEN: Handler called when connection opens (ws)
   ON-MESSAGE: Handler called on message (ws message)
   ON-CLOSE: Handler called on close (ws &key code reason)
   ON-ERROR: Handler called on error (ws error)
   :AUTH    REQUIRED. One-arg callable (env -> generalised boolean) read by
            streaming-gate; NIL denies the upgrade with 401.
   :ORIGIN  REQUIRED. List of allowed origin strings (verbatim RFC 6454);
            empty list denies every request with 403.

   Example:
     (defws \"/ws/notifications\" \"notifications\"
       :auth   (lambda (env) (let ((*env* env)) (current-principal)))
       :origin '(\"https://app.example.com\")
       :on-message (lambda (ws msg)
                     (let ((data (decode-json-string msg)))
                       (handle-notification ws data))))"
  (unless auth-supplied-p
    (error "DEFWS requires an explicit :AUTH argument"))
  (unless origin-supplied-p
    (error "DEFWS requires an explicit :ORIGIN argument"))
  `(bordeaux-threads:with-recursive-lock-held (lol-web/server::*routes-lock*)
     (setf (gethash (cons :get ,path) lol-web/server::*streaming-routes*)
           (make-ws-handler ,channel
                            :on-open ,on-open
                            :on-message ,on-message
                            :on-close ,on-close
                            :on-error ,on-error
                            :auth ,auth
                            :origin ,origin))))
