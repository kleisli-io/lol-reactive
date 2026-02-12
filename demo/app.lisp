;;;; LOL-REACTIVE Demo - Realtime Dashboard
;;;;
;;;; Stress tests WebSocket, SSE, and Optimistic Update features with:
;;;; - Chat room (WebSocket) with typing indicators
;;;; - Shared counter (SSE) with event log
;;;; - Todo list (Optimistic) with rollback
;;;; - Chaos mode for random failures and delays

(in-package :lol-reactive-demo)

;;; Forward declaration for variable defined in main.lisp
(defvar *shared-counter*)

;;; ============================================================================
;;; THEME STATE
;;;
;;; Dark/light mode synced across all clients via SSE.
;;; ============================================================================

(defvar *dark-mode* t
  "When non-nil, dashboard uses dark theme. Synced via SSE to all clients.")

(defun broadcast-theme-change ()
  "Send theme state to all connected SSE clients."
  ;; Use explicit JSON to avoid cl-json case conversion issues
  (sse-broadcast "counter" nil
    (format nil "{\"type\":\"theme\",\"darkMode\":~A}"
            (if *dark-mode* "true" "false"))))

;;; ============================================================================
;;; CHAOS MODE INFRASTRUCTURE
;;;
;;; Enables random failures and delays to stress test error handling.
;;; Toggle via /api/chaos/toggle endpoint.
;;; ============================================================================

(defvar *chaos-mode* nil
  "When non-nil, handlers may randomly delay or fail.")

(defvar *chaos-delay-range* '(100 . 3000)
  "Random delay range in milliseconds (min . max).")

(defvar *chaos-failure-rate* 0.2
  "Probability of random failure (0.0-1.0).")

(defun chaos-delay ()
  "Apply random delay if chaos mode enabled.
   Returns the delay applied in milliseconds, or 0 if not in chaos mode."
  (if *chaos-mode*
      (let ((delay (+ (car *chaos-delay-range*)
                      (random (- (cdr *chaos-delay-range*)
                                 (car *chaos-delay-range*))))))
        (sleep (/ delay 1000.0))
        delay)
      0))

(defun chaos-maybe-fail ()
  "Check if should randomly fail. Returns T if chaos mode triggers failure."
  (and *chaos-mode*
       (< (random 1.0) *chaos-failure-rate*)))

(defmacro with-chaos (() &body body)
  "Wrap handler with chaos mode effects.
   Applies random delay, then either fails or executes body."
  `(progn
     (chaos-delay)
     (if (chaos-maybe-fail)
         (error-response 500 :message "Chaos mode: Random failure!")
         (progn ,@body))))

;;; ============================================================================
;;; CHAT STATE AND WEBSOCKET HANDLER
;;; ============================================================================

(defvar *chat-history* '()
  "Recent chat messages (max 100). Each is (:username :text :time).")

(defvar *typing-users* (make-hash-table :test 'equal)
  "Maps username -> last typing timestamp.")

(defvar *ws-usernames* (make-hash-table :test 'eq)
  "Maps WebSocket connection -> username.")

(defun generate-username ()
  "Generate a random username for new connections."
  (format nil "User-~A" (random 9999)))

(defun ws-username (ws)
  "Get username for a WebSocket connection."
  (gethash ws *ws-usernames*))

(defun (setf ws-username) (username ws)
  "Set username for a WebSocket connection."
  (setf (gethash ws *ws-usernames*) username))

(defun push-chat-message (username text)
  "Add message to history, keeping max 100."
  (push `((:username . ,username) (:text . ,text) (:time . ,(get-universal-time)))
        *chat-history*)
  (when (> (length *chat-history*) 100)
    (setf *chat-history* (subseq *chat-history* 0 100))))

(defun get-typing-users ()
  "Get list of users currently typing (within last 3 seconds)."
  (let ((now (get-universal-time))
        (typing '()))
    (maphash (lambda (user timestamp)
               (if (< (- now timestamp) 3)
                   (push user typing)
                   (remhash user *typing-users*)))
             *typing-users*)
    typing))

(defun broadcast-typing-status ()
  "Send typing users list to all chat clients."
  (ws-broadcast-json "chat"
    `((:type . "typing")
      (:users . ,(get-typing-users)))))

(defun get-connected-users ()
  "Get list of all connected usernames."
  (let ((users '()))
    (maphash (lambda (ws username)
               (declare (ignore ws))
               (push username users))
             *ws-usernames*)
    (sort users #'string<)))

(defun broadcast-user-list ()
  "Send connected users list to all chat clients."
  (ws-broadcast-json "chat"
    `((:type . "users")
      (:users . ,(coerce (get-connected-users) 'vector)))))

;; WebSocket handler for chat
(defws "/ws/chat" "chat"
  :on-open (lambda (ws)
             (let ((username (generate-username)))
               ;; Store username for this connection
               (setf (ws-username ws) username)
               ;; Announce join to all
               (ws-broadcast-json "chat"
                 `((:type . "join")
                   (:username . ,username)
                   (:count . ,(ws-connection-count "chat"))))
               ;; Broadcast updated user list
               (broadcast-user-list)
               ;; Send history to new user
               (lol-reactive:ws-send-json ws
                 `((:type . "history")
                   (:messages . ,(reverse (subseq *chat-history*
                                                   0 (min 20 (length *chat-history*)))))))))

  :on-message (lambda (ws message)
                (let* ((data (cl-json:decode-json-from-string message))
                       (msg-type (cdr (assoc :type data)))
                       (username (ws-username ws)))
                  (cond
                    ;; Typing indicator
                    ((string= msg-type "typing")
                     (setf (gethash username *typing-users*) (get-universal-time))
                     (broadcast-typing-status))
                    ;; Chat message
                    ((string= msg-type "message")
                     (let ((text (cdr (assoc :text data))))
                       (when (and text (> (length text) 0))
                         ;; Remove from typing and broadcast updated status
                         (remhash username *typing-users*)
                         (broadcast-typing-status)
                         ;; Save and broadcast message
                         (push-chat-message username text)
                         (ws-broadcast-json "chat"
                           `((:type . "message")
                             (:username . ,username)
                             (:text . ,text)
                             (:time . ,(get-universal-time))))))))))

  :on-close (lambda (ws &key code reason)
              (declare (ignore code reason))
              (let ((username (ws-username ws)))
                (when username
                  ;; Remove from typing and broadcast updated status
                  (remhash username *typing-users*)
                  (broadcast-typing-status)
                  ;; Clean up username mapping
                  (remhash ws *ws-usernames*)
                  ;; Announce leave
                  (ws-broadcast-json "chat"
                    `((:type . "leave")
                      (:username . ,username)
                      (:count . ,(ws-connection-count "chat"))))
                  ;; Broadcast updated user list
                  (broadcast-user-list)))))

;;; ============================================================================
;;; SSE COUNTER
;;; ============================================================================

(defvar *counter-log* '()
  "Recent counter events (max 50). Each is (:type :delta :time).")

(defun log-counter-event (event-type delta)
  "Log a counter event."
  (push (list :type event-type :delta delta :time (get-universal-time))
        *counter-log*)
  (when (> (length *counter-log*) 50)
    (setf *counter-log* (subseq *counter-log* 0 50))))

;; SSE endpoint for counter updates
(defsse "/sse/counter" "counter"
  :on-connect (lambda (conn)
                (declare (ignore conn))
                ;; Initial state sent via broadcast after connect
                nil)
  :on-disconnect (lambda (conn)
                   (declare (ignore conn))
                   nil))

;;; ============================================================================
;;; TODO LIST STATE
;;; ============================================================================

(defvar *todos* '()
  "List of todo items.")

(defvar *todo-id-counter* 0
  "Auto-incrementing ID for todos.")

(defstruct todo
  "A todo item."
  id text completed created-at)

(defun todo-to-alist (todo)
  "Convert todo struct to alist for JSON."
  `((:id . ,(todo-id todo))
    (:text . ,(todo-text todo))
    (:completed . ,(if (todo-completed todo) t :false))))

;;; ============================================================================
;;; DASHBOARD STYLES (using defcss)
;;; ============================================================================

(defcss :dashboard-layout
  (".dashboard" (("min-height" . "100vh")
                 ("background" . "#0f172a")
                 ("color" . "#e2e8f0")))
  (".dashboard-header" (("display" . "flex")
                        ("justify-content" . "space-between")
                        ("align-items" . "center")
                        ("padding" . "1rem 2rem")
                        ("background" . "#1e293b")
                        ("border-bottom" . "1px solid #334155")))
  (".header-left .logo" (("margin" . "0")
                         ("font-size" . "1.5rem")
                         ("color" . "#f59e0b")))
  (".header-right" (("display" . "flex")
                    ("align-items" . "center")
                    ("gap" . "1rem")))
  (".chaos-toggle" (("display" . "flex")
                    ("align-items" . "center")
                    ("gap" . "0.5rem")
                    ("cursor" . "pointer")))
  (".chaos-toggle input" (("width" . "1.25rem")
                          ("height" . "1.25rem")))
  (".chaos-status" (("padding" . "0.25rem 0.75rem")
                    ("background" . "#ef4444")
                    ("color" . "white")
                    ("border-radius" . "0.25rem")
                    ("font-size" . "0.75rem")
                    ("font-weight" . "bold")
                    ("animation" . "pulse 1s infinite")))
  (".chaos-status:empty" (("display" . "none")))
  ("@keyframes pulse" (("0%, 100%" . (("opacity" . "1")))
                       ("50%" . (("opacity" . "0.5")))))
  (".connection-status" (("padding" . "0.25rem 0.75rem")
                         ("background" . "#475569")
                         ("border-radius" . "0.25rem")
                         ("font-size" . "0.875rem"))))

(defcss :dashboard-grid
  (".dashboard-grid" (("display" . "grid")
                      ("grid-template-columns" . "repeat(2, 1fr)")
                      ("gap" . "1rem")
                      ("padding" . "1rem 2rem")
                      ("max-width" . "1400px")
                      ("margin" . "0 auto")))
  ("@media (max-width: 900px)" ((".dashboard-grid" . (("grid-template-columns" . "1fr"))))))

(defcss :dashboard-panel
  (".panel" (("background" . "#1e293b")
             ("border-radius" . "0.5rem")
             ("padding" . "1rem")
             ("border" . "1px solid #334155")))
  (".panel h2" (("margin" . "0 0 1rem")
                ("font-size" . "1.125rem")
                ("color" . "#94a3b8"))))

(defcss :dashboard-chat
  (".chat-messages" (("height" . "200px")
                     ("overflow-y" . "auto")
                     ("background" . "#0f172a")
                     ("border-radius" . "0.25rem")
                     ("padding" . "0.5rem")
                     ("margin-bottom" . "0.5rem")))
  (".chat-msg" (("padding" . "0.25rem 0")))
  (".chat-msg .username" (("font-weight" . "bold")
                          ("color" . "#60a5fa")))
  (".chat-msg .text" (("margin-left" . "0.5rem")))
  (".typing-indicator" (("height" . "1.25rem")
                        ("font-size" . "0.75rem")
                        ("color" . "#94a3b8")
                        ("font-style" . "italic")))
  (".chat-input-row" (("display" . "flex")
                      ("gap" . "0.5rem")))
  (".chat-input-row input" (("flex" . "1")
                            ("padding" . "0.5rem")
                            ("background" . "#0f172a")
                            ("border" . "1px solid #334155")
                            ("border-radius" . "0.25rem")
                            ("color" . "#e2e8f0")))
  (".online-count" (("margin-top" . "0.5rem")
                    ("font-size" . "0.875rem")
                    ("color" . "#94a3b8")))
  ;; User list styles
  (".user-list" (("display" . "flex")
                 ("flex-wrap" . "wrap")
                 ("gap" . "0.25rem")
                 ("margin-top" . "0.5rem")
                 ("padding" . "0.5rem")
                 ("background" . "var(--bg-tertiary)")
                 ("border-radius" . "0.25rem")
                 ("min-height" . "2rem")))
  (".user-chip" (("display" . "inline-flex")
                 ("align-items" . "center")
                 ("gap" . "0.25rem")
                 ("padding" . "0.125rem 0.5rem")
                 ("background" . "#3b82f6")
                 ("color" . "white")
                 ("border-radius" . "1rem")
                 ("font-size" . "0.75rem")))
  (".user-chip .dot" (("width" . "0.5rem")
                      ("height" . "0.5rem")
                      ("background" . "#22c55e")
                      ("border-radius" . "50%"))))

(defcss :dashboard-counter
  (".counter-display" (("text-align" . "center")
                       ("margin" . "1rem 0")))
  (".counter-value" (("font-size" . "3rem")
                     ("font-weight" . "bold")
                     ("color" . "#22c55e")))
  (".counter-controls" (("display" . "flex")
                        ("justify-content" . "center")
                        ("gap" . "0.5rem")
                        ("flex-wrap" . "wrap")))
  (".event-log" (("margin-top" . "1rem")
                 ("max-height" . "100px")
                 ("overflow-y" . "auto")))
  (".event-log h4" (("margin" . "0 0 0.5rem")
                    ("font-size" . "0.875rem")
                    ("color" . "#94a3b8")))
  (".event-log ul" (("list-style" . "none")
                    ("padding" . "0")
                    ("margin" . "0")
                    ("font-size" . "0.75rem")
                    ("font-family" . "monospace"))))

(defcss :dashboard-todos
  (".todo-input-row" (("display" . "flex")
                      ("gap" . "0.5rem")
                      ("margin-bottom" . "0.75rem")))
  (".todo-input-row input" (("flex" . "1")
                            ("padding" . "0.5rem")
                            ("background" . "#0f172a")
                            ("border" . "1px solid #334155")
                            ("border-radius" . "0.25rem")
                            ("color" . "#e2e8f0")))
  (".todo-list" (("list-style" . "none")
                 ("padding" . "0")
                 ("margin" . "0")
                 ("max-height" . "200px")
                 ("overflow-y" . "auto")))
  (".todo-item" (("display" . "flex")
                 ("align-items" . "center")
                 ("gap" . "0.5rem")
                 ("padding" . "0.5rem")
                 ("border-bottom" . "1px solid #334155")))
  (".todo-item.completed .todo-text" (("text-decoration" . "line-through")
                                       ("opacity" . "0.6")))
  (".todo-item.optimistic" (("opacity" . "0.7")))
  (".delete-btn" (("margin-left" . "auto")
                  ("background" . "none")
                  ("border" . "none")
                  ("color" . "#ef4444")
                  ("cursor" . "pointer")
                  ("font-size" . "1.25rem")))
  (".optimistic-hint" (("font-size" . "0.75rem")
                       ("color" . "#94a3b8")
                       ("margin-top" . "0.5rem"))))

(defcss :dashboard-status
  (".status-grid" (("display" . "grid")
                   ("gap" . "0.5rem")))
  (".status-item" (("display" . "flex")
                   ("justify-content" . "space-between")
                   ("padding" . "0.5rem")
                   ("background" . "#0f172a")
                   ("border-radius" . "0.25rem")))
  (".status-label" (("color" . "#94a3b8")))
  (".status-value" (("font-weight" . "bold")))
  (".status-value.connected" (("color" . "#22c55e")))
  (".status-value.disconnected" (("color" . "#ef4444")))
  (".error-log" (("margin-top" . "1rem")))
  (".error-log h4" (("margin" . "0 0 0.5rem")
                    ("font-size" . "0.875rem")
                    ("color" . "#94a3b8")))
  (".error-log ul" (("list-style" . "none")
                    ("padding" . "0")
                    ("margin" . "0")
                    ("font-size" . "0.75rem")
                    ("max-height" . "80px")
                    ("overflow-y" . "auto")))
  (".error-log li" (("padding" . "0.25rem")
                    ("color" . "#fca5a5"))))

(defcss :dashboard-buttons
  (".btn" (("padding" . "0.5rem 1rem")
           ("background" . "#3b82f6")
           ("color" . "white")
           ("border" . "none")
           ("border-radius" . "0.25rem")
           ("cursor" . "pointer")
           ("font-size" . "0.875rem")
           ("transition" . "background 0.2s")))
  (".btn:hover" (("background" . "#2563eb")))
  (".btn-secondary" (("background" . "#475569")))
  (".btn-secondary:hover" (("background" . "#64748b")))
  (".btn-warning" (("background" . "#f59e0b")))
  (".btn-warning:hover" (("background" . "#d97706"))))

(defcss :dashboard-toast
  (".error-toast" (("position" . "fixed")
                   ("bottom" . "1rem")
                   ("right" . "1rem")
                   ("padding" . "0.75rem 1.5rem")
                   ("background" . "#ef4444")
                   ("color" . "white")
                   ("border-radius" . "0.25rem")
                   ("animation" . "slideIn 0.3s ease")
                   ("z-index" . "1000")))
  ("@keyframes slideIn" (("from" . (("transform" . "translateX(100%)")
                                    ("opacity" . "0")))
                         ("to" . (("transform" . "translateX(0)")
                                  ("opacity" . "1")))))
  (".placeholder" (("color" . "#64748b")
                   ("font-style" . "italic"))))

(defcss :dashboard-theme
  ;; CSS custom properties for theming
  (":root" (("--bg-primary" . "#0f172a")
            ("--bg-secondary" . "#1e293b")
            ("--bg-tertiary" . "#0f172a")
            ("--text-primary" . "#e2e8f0")
            ("--text-secondary" . "#94a3b8")
            ("--border-color" . "#334155")))
  ;; Light mode overrides
  (":root.light-mode" (("--bg-primary" . "#f8fafc")
                       ("--bg-secondary" . "#ffffff")
                       ("--bg-tertiary" . "#f1f5f9")
                       ("--text-primary" . "#1e293b")
                       ("--text-secondary" . "#64748b")
                       ("--border-color" . "#e2e8f0")))
  ;; Apply theme variables
  (".dashboard" (("background" . "var(--bg-primary)")
                 ("color" . "var(--text-primary)")))
  (".dashboard-header" (("background" . "var(--bg-secondary)")
                        ("border-color" . "var(--border-color)")))
  (".panel" (("background" . "var(--bg-secondary)")
             ("border-color" . "var(--border-color)")))
  (".panel h2" (("color" . "var(--text-secondary)")))
  (".chat-messages" (("background" . "var(--bg-tertiary)")))
  (".chat-input-row input" (("background" . "var(--bg-tertiary)")
                            ("border-color" . "var(--border-color)")
                            ("color" . "var(--text-primary)")))
  (".todo-input-row input" (("background" . "var(--bg-tertiary)")
                            ("border-color" . "var(--border-color)")
                            ("color" . "var(--text-primary)")))
  (".status-item" (("background" . "var(--bg-tertiary)")))
  (".status-label" (("color" . "var(--text-secondary)")))
  (".typing-indicator" (("color" . "var(--text-secondary)")))
  (".online-count" (("color" . "var(--text-secondary)")))
  (".event-log h4" (("color" . "var(--text-secondary)")))
  (".error-log h4" (("color" . "var(--text-secondary)")))
  (".optimistic-hint" (("color" . "var(--text-secondary)")))
  ;; Theme toggle button
  (".theme-toggle" (("display" . "flex")
                    ("align-items" . "center")
                    ("gap" . "0.25rem"))))

;;; ============================================================================
;;; BASE LAYOUT
;;;
;;; Dashboard grid with panels for chat, counter, todos, and status.
;;; ============================================================================

(defun render-dashboard-header ()
  "Render dashboard header with theme toggle, chaos toggle and connection status."
  (htm-str
    (:header :class "dashboard-header"
      (:div :class "header-left"
        (:h1 :class "logo" "⚡ lol-reactive Dashboard"))
      (:div :class "header-right"
        ;; Theme toggle button (uses fetch instead of HTMX to avoid OOB swap loop)
        (:button :class "btn btn-secondary theme-toggle" :id "theme-toggle"
                 :onclick "toggleTheme()"
          (cl-who:str (if *dark-mode* "☀️ Light" "🌙 Dark")))
        (:label :class "chaos-toggle"
          (:input :type "checkbox" :id "chaos-checkbox"
                  :hx-post "/api/chaos/toggle"
                  :hx-swap "none"
                  (when *chaos-mode* (cl-who:htm :checked "checked")))
          (:span :class "chaos-label" "Chaos Mode 🔥"))
        (:span :class "chaos-status" :id "chaos-status"
          (if *chaos-mode* "CHAOS ON" ""))
        (:span :class "connection-status" :id "connection-status"
          "Connecting...")))))

(defun render-chat-panel ()
  "Render the WebSocket chat panel."
  (htm-str
    (:section :id "chat-section" :class "panel chat-panel"
      (:h2 "💬 Chat")
      (:div :class "chat-messages" :id "chat-messages"
        (:p :class "placeholder" "Chat loading..."))
      (:div :class "typing-indicator" :id "typing-indicator")
      (:div :class "chat-input-row"
        (:input :type "text" :id "chat-input"
                :placeholder "Type a message..." :autocomplete "off")
        (:button :class "btn" :id "chat-send" "Send"))
      (:div :class "online-count"
        "Online: " (:span :id "online-count" "0"))
      (:div :class "user-list" :id "user-list"
        (:span :class "placeholder" "Loading users...")))))

(defun render-counter-panel ()
  "Render the SSE counter panel."
  (htm-str
    (:section :id "counter-section" :class "panel counter-panel"
      (:h2 "📊 Shared Counter (SSE)")
      (:div :class "counter-display"
        (:span :class "counter-value" :id "counter-value"
               (cl-who:str (or *shared-counter* 0))))
      (:div :class "counter-controls"
        (:button :class "btn" :hx-post "/api/counter/decrement"
                 :hx-swap "none" "-1")
        (:button :class "btn" :hx-post "/api/counter/increment"
                 :hx-swap "none" "+1")
        (:button :class "btn btn-secondary" :hx-post "/api/counter/add"
                 :hx-vals "{\"amount\": 10}" :hx-swap "none" "+10")
        (:button :class "btn btn-warning" :hx-post "/api/counter/burst"
                 :hx-swap "none" "Burst 50"))
      (:div :class "event-log"
        (:h4 "Event Log")
        (:ul :id "counter-events")))))

(defun render-todos-panel ()
  "Render the optimistic todo panel."
  (htm-str
    (:section :id "todos-section" :class "panel todos-panel"
      (:h2 "✅ Todos (Optimistic)")
      (:div :class "todo-input-row"
        (:input :type "text" :id "todo-input"
                :placeholder "Add a todo..." :autocomplete "off")
        (:button :class "btn" :id "todo-add" "Add"))
      (:ul :class "todo-list" :id "todo-list")
      (:p :class "optimistic-hint"
        "⚡ Updates appear instantly and rollback on error"))))

(defun render-status-panel ()
  "Render connection status and debug info panel."
  (htm-str
    (:section :id "status-section" :class "panel status-panel"
      (:h2 "🔌 Connections")
      (:div :class "status-grid"
        (:div :class "status-item"
          (:span :class "status-label" "WebSocket")
          (:span :class "status-value" :id "ws-status" "Disconnected"))
        (:div :class "status-item"
          (:span :class "status-label" "SSE")
          (:span :class "status-value" :id "sse-status" "Disconnected"))
        (:div :class "status-item"
          (:span :class "status-label" "Chaos Mode")
          (:span :class "status-value" :id "chaos-mode-status"
            (if *chaos-mode* "Enabled" "Disabled"))))
      (:div :class "error-log"
        (:h4 "Error Log")
        (:ul :id "error-log")))))

(defun render-app ()
  "Render the complete dashboard layout."
  (htm-str
    (:div :class "dashboard"
      (cl-who:str (render-dashboard-header))
      (:main :class "dashboard-grid"
        (cl-who:str (render-chat-panel))
        (cl-who:str (render-counter-panel))
        (cl-who:str (render-todos-panel))
        (cl-who:str (render-status-panel))))))
