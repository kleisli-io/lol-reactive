;;;; Demo Main - Routes and entry point for Realtime Dashboard

(in-package :lol-reactive-demo)

;;; ============================================================================
;;; DASHBOARD ROUTES
;;; ============================================================================

(defroute "/" (:method :get)
  "Serve the realtime dashboard."
  (html-page
   :title "lol-reactive Realtime Dashboard"
   :include-tailwind nil
   :include-htmx nil  ; Using lol-reactive-runtime-js which includes HTMX
   :body (concatenate 'string
           ;; Set initial theme class immediately to prevent flash
           (format nil "<script>~A</script>"
                   (if *dark-mode*
                       ""
                       "document.documentElement.classList.add('light-mode');"))
           (render-app)
           (format nil "<style>~A</style>"
                   (generate-all-component-css))
           (format nil "<script>~A</script>"
                   (lol-reactive-runtime-js))
           (format nil "<script>~A</script>"
                   (dashboard-client-js)))))

;;; ============================================================================
;;; THEME API
;;; ============================================================================

(defroute "/api/theme/toggle" (:method :post :content-type "text/plain")
  "Toggle dark/light mode and broadcast to all SSE clients.
   SSE broadcast updates the theme on all clients - no OOB swap needed."
  (setf *dark-mode* (not *dark-mode*))
  (broadcast-theme-change)
  "ok")

;;; ============================================================================
;;; CHAOS MODE API
;;; ============================================================================

(defroute "/api/chaos/toggle" (:method :post :content-type "text/html")
  "Toggle chaos mode on/off. Returns OOB updates for status indicators."
  (setf *chaos-mode* (not *chaos-mode*))
  (with-htmx-response (:trigger "chaosToggled")
    (render-with-oob
      nil
      (list "chaos-status" (if *chaos-mode* "CHAOS ON" ""))
      (list "chaos-mode-status" (if *chaos-mode* "Enabled" "Disabled")))))

(defapi "/api/chaos/status" (:method :get)
  "Get current chaos mode status."
  `((:enabled . ,*chaos-mode*)
    (:delay-range . ,*chaos-delay-range*)
    (:failure-rate . ,*chaos-failure-rate*)))

;;; ============================================================================
;;; COUNTER API
;;; ============================================================================

(defvar *shared-counter* 0 "Shared counter value for SSE demo.")

(defun broadcast-counter-update (delta source)
  "Broadcast counter update via SSE to all connected clients."
  (log-counter-event source delta)
  (sse-broadcast "counter" nil
    (cl-json:encode-json-to-string
      `((:value . ,*shared-counter*)
        (:delta . ,delta)
        (:source . ,source)))))

(defroute "/api/counter/increment" (:method :post :content-type "text/html")
  "Increment counter with chaos mode effects."
  (with-chaos ()
    (incf *shared-counter*)
    (broadcast-counter-update 1 "increment")
    (with-htmx-response (:trigger "counterUpdated")
      (render-with-oob
        nil
        (list "counter-value" (format nil "~A" *shared-counter*))))))

(defroute "/api/counter/decrement" (:method :post :content-type "text/html")
  "Decrement counter with chaos mode effects."
  (with-chaos ()
    (decf *shared-counter*)
    (broadcast-counter-update -1 "decrement")
    (with-htmx-response (:trigger "counterUpdated")
      (render-with-oob
        nil
        (list "counter-value" (format nil "~A" *shared-counter*))))))

(defroute "/api/counter/add" (:method :post :content-type "text/html")
  "Add amount to counter with chaos mode effects."
  (with-chaos ()
    (let ((amount (or (ignore-errors
                        (parse-integer (or (query-param "amount")
                                           (post-param "amount")
                                           "10")))
                      10)))
      (incf *shared-counter* amount)
      (broadcast-counter-update amount "add")
      (with-htmx-response (:trigger "counterUpdated")
        (render-with-oob
          nil
          (list "counter-value" (format nil "~A" *shared-counter*)))))))

(defroute "/api/counter/burst" (:method :post :content-type "text/html")
  "Burst 50 increments to stress test."
  (dotimes (i 50)
    (incf *shared-counter*)
    (sse-broadcast "counter" nil
      (cl-json:encode-json-to-string
        `((:value . ,*shared-counter*)
          (:delta . 1)
          (:source . "burst")))))
  (with-htmx-response (:trigger "counterBurst")
    (render-with-oob
      nil
      (list "counter-value" (format nil "~A" *shared-counter*)))))

;;; ============================================================================
;;; TODO API
;;; ============================================================================

(defapi "/api/todos" (:method :get)
  "Get all todos."
  `((:todos . ,(mapcar #'todo-to-alist *todos*))))

(defapi "/api/todos" (:method :post)
  "Create a new todo."
  (with-chaos ()
    (let* ((text (or (post-param "text") ""))
           (todo (make-todo :id (incf *todo-id-counter*)
                            :text text
                            :completed nil
                            :created-at (get-universal-time))))
      (push todo *todos*)
      (todo-to-alist todo))))

(defapi "/api/todos/toggle" (:method :post)
  "Toggle todo completion status."
  (with-chaos ()
    (let* ((id-str (or (post-param "id") (query-param "id")))
           (id (ignore-errors (parse-integer id-str)))
           (todo (find id *todos* :key #'todo-id)))
      (if todo
          (progn
            (setf (todo-completed todo) (not (todo-completed todo)))
            (todo-to-alist todo))
          (error-response 404 :message "Todo not found")))))

(defapi "/api/todos/delete" (:method :post)
  "Delete a todo."
  (with-chaos ()
    (let* ((id-str (or (post-param "id") (query-param "id")))
           (id (ignore-errors (parse-integer id-str))))
      (setf *todos* (remove id *todos* :key #'todo-id))
      `((:deleted . ,id)))))

;;; ============================================================================
;;; CLIENT-SIDE JAVASCRIPT
;;; ============================================================================

(defun dashboard-client-js ()
  "Generate client-side JavaScript for dashboard."
  (ps:ps
    ;; === State ===
    (defvar *chat-ws* nil)
    (defvar *typing-timeout* nil)

    ;; === Connection Status ===
    (defun update-connection-status (status)
      (let ((el (ps:chain document (get-element-by-id "connection-status"))))
        (when el
          (setf (ps:@ el text-content) status))))

    (defun update-ws-status (status)
      (let ((el (ps:chain document (get-element-by-id "ws-status"))))
        (when el
          (setf (ps:@ el text-content) status)
          (setf (ps:@ el class-name)
                (+ "status-value " (if (= status "Connected") "connected" "disconnected"))))))

    ;; === Error Display ===
    (defun show-error (msg)
      (let ((toast (ps:chain document (create-element "div"))))
        (setf (ps:@ toast class-name) "error-toast")
        (setf (ps:@ toast text-content) msg)
        (ps:chain document body (append-child toast))
        (set-timeout (lambda () (ps:chain toast (remove))) 3000))
      ;; Also log to error panel
      (let ((log (ps:chain document (get-element-by-id "error-log"))))
        (when log
          (let ((li (ps:chain document (create-element "li"))))
            (setf (ps:@ li text-content)
                  (+ (ps:chain (ps:new (-Date)) (to-locale-time-string)) " - " msg))
            (ps:chain log (insert-before li (ps:@ log first-child)))
            ;; Keep only last 10 errors
            (loop while (> (ps:@ log children length) 10)
                  do (ps:chain log (remove-child (ps:@ log last-child))))))))

    ;; === Chat Functions ===
    (defun add-chat-message (username text &optional is-system)
      (let ((messages (ps:chain document (get-element-by-id "chat-messages")))
            (div (ps:chain document (create-element "div"))))
        (setf (ps:@ div class-name) (if is-system "chat-msg system" "chat-msg"))
        (if is-system
            (setf (ps:@ div inner-h-t-m-l)
                  (+ "<span class='text'>" text "</span>"))
            (setf (ps:@ div inner-h-t-m-l)
                  (+ "<span class='username'>" username ":</span>"
                     "<span class='text'>" text "</span>")))
        (ps:chain messages (append-child div))
        ;; Scroll to bottom
        (setf (ps:@ messages scroll-top) (ps:@ messages scroll-height))))

    (defun update-online-count (count)
      (let ((el (ps:chain document (get-element-by-id "online-count"))))
        (when el
          (setf (ps:@ el text-content) count))))

    (defun update-typing-indicator (users)
      (let ((el (ps:chain document (get-element-by-id "typing-indicator"))))
        (when el
          (if (and users (> (ps:@ users length) 0))
              (setf (ps:@ el text-content)
                    (+ (ps:chain users (join ", ")) " typing..."))
              (setf (ps:@ el text-content) "")))))

    (defun update-user-list (users)
      "Update the user list display with connected users."
      (let ((el (ps:chain document (get-element-by-id "user-list"))))
        (when el
          (if (and users (> (ps:@ users length) 0))
              (setf (ps:@ el inner-h-t-m-l)
                    (ps:chain users (map (lambda (user)
                      (+ "<span class='user-chip'><span class='dot'></span>" user "</span>")))
                      (join "")))
              (setf (ps:@ el inner-h-t-m-l)
                    "<span class='placeholder'>No users</span>")))))

    (defun send-chat-message ()
      (let* ((input (ps:chain document (get-element-by-id "chat-input")))
             (text (ps:@ input value)))
        (when (and *chat-ws* (> (ps:@ text length) 0))
          ;; Cancel any pending typing indicator
          (when *typing-timeout*
            (clear-timeout *typing-timeout*)
            (setf *typing-timeout* nil))
          (ps:chain *chat-ws* (send (ps:chain -j-s-o-n (stringify
            (ps:create :type "message" :text text)))))
          (setf (ps:@ input value) ""))))

    (defun send-typing ()
      (when *chat-ws*
        (ps:chain *chat-ws* (send (ps:chain -j-s-o-n (stringify
          (ps:create :type "typing")))))))

    (defun init-chat ()
      ;; Create WebSocket connection
      (let* ((protocol (if (= (ps:@ window location protocol) "https:") "wss:" "ws:"))
             (url (+ protocol "//" (ps:@ window location host) "/ws/chat")))
        (setf *chat-ws* (ps:new (-Web-Socket url))))

      (setf (ps:@ *chat-ws* onopen) (lambda ()
        (update-ws-status "Connected")
        (update-connection-status "Connected")
        ;; Clear placeholder
        (let ((messages (ps:chain document (get-element-by-id "chat-messages"))))
          (setf (ps:@ messages inner-h-t-m-l) ""))))

      (setf (ps:@ *chat-ws* onclose) (lambda ()
        (update-ws-status "Disconnected")
        (update-connection-status "Disconnected")
        ;; Reconnect after 2 seconds
        (set-timeout init-chat 2000)))

      (setf (ps:@ *chat-ws* onerror) (lambda (e)
        (show-error "WebSocket error")))

      (setf (ps:@ *chat-ws* onmessage) (lambda (e)
        (let ((data (ps:chain -j-s-o-n (parse (ps:@ e data)))))
          (ps:chain console (log "WS message:" data))
          (case (ps:@ data type)
            ("message"
             (add-chat-message (ps:@ data username) (ps:@ data text)))
            ("join"
             (add-chat-message nil (+ (ps:@ data username) " joined") t)
             (update-online-count (ps:@ data count)))
            ("leave"
             (add-chat-message nil (+ (ps:@ data username) " left") t)
             (update-online-count (ps:@ data count)))
            ("typing"
             (update-typing-indicator (ps:@ data users)))
            ("users"
             (update-user-list (ps:@ data users)))
            ("history"
             (let ((messages (ps:@ data messages)))
               (when messages
                 (ps:chain messages (for-each (lambda (msg)
                   (add-chat-message (ps:@ msg username) (ps:@ msg text))))))))))))

      ;; Set up input handlers
      (let ((input (ps:chain document (get-element-by-id "chat-input")))
            (btn (ps:chain document (get-element-by-id "chat-send"))))
        (when input
          (ps:chain input (add-event-listener "keypress" (lambda (e)
            ;; Send typing indicator (debounced)
            (when *typing-timeout*
              (clear-timeout *typing-timeout*))
            (setf *typing-timeout* (set-timeout send-typing 100))
            ;; Send on Enter
            (when (= (ps:@ e key) "Enter")
              (send-chat-message))))))
        (when btn
          (ps:chain btn (add-event-listener "click" send-chat-message)))))

    ;; === SSE Counter ===
    (defun update-sse-status (status)
      (let ((el (ps:chain document (get-element-by-id "sse-status"))))
        (when el
          (setf (ps:@ el text-content) status)
          (setf (ps:@ el class-name)
                (+ "status-value " (if (= status "Connected") "connected" "disconnected"))))))

    (defun update-counter-display (value)
      (let ((el (ps:chain document (get-element-by-id "counter-value"))))
        (when el
          (setf (ps:@ el text-content) value))))

    (defun add-counter-event (data)
      (let ((log (ps:chain document (get-element-by-id "counter-events"))))
        (when log
          (let ((li (ps:chain document (create-element "li"))))
            (setf (ps:@ li text-content)
                  (+ (ps:@ data source) ": "
                     (if (> (ps:@ data delta) 0) "+" "")
                     (ps:@ data delta) " → " (ps:@ data value)))
            (ps:chain log (insert-before li (ps:@ log first-child)))
            (loop while (> (ps:@ log children length) 20)
                  do (ps:chain log (remove-child (ps:@ log last-child))))))))

    ;; === Theme Functions ===
    (defun apply-theme (dark-mode)
      "Apply dark or light theme."
      (let ((root (ps:@ document document-element))
            (btn (ps:chain document (get-element-by-id "theme-toggle"))))
        (if dark-mode
            (progn
              (ps:chain root class-list (remove "light-mode"))
              (when btn (setf (ps:@ btn text-content) "☀️ Light")))
            (progn
              (ps:chain root class-list (add "light-mode"))
              (when btn (setf (ps:@ btn text-content) "🌙 Dark"))))))

    (defun toggle-theme ()
      "Toggle theme via fetch. SSE broadcast updates all clients."
      (ps:chain (fetch "/api/theme/toggle" (ps:create :method "POST"))
        (catch (lambda (err)
                 (show-error "Failed to toggle theme")))))

    ;; Make toggleTheme available globally for onclick
    (setf (ps:@ window toggle-theme) toggle-theme)

    (defun init-counter ()
      (ps:chain -s-s-e-m-a-n-a-g-e-r (connect "counter"
        (ps:create
          :on-open (lambda (source)
            (declare (ignore source))
            (update-sse-status "Connected"))
          :on-error (lambda (e source)
            (declare (ignore e source))
            (update-sse-status "Disconnected"))
          :on-message (lambda (data source)
            (declare (ignore source))
            (ps:chain console (log "SSE update:" data))
            ;; Handle different message types
            (cond
              ;; Theme change message
              ((= (ps:@ data type) "theme")
               (apply-theme (ps:@ data dark-mode)))
              ;; Counter update message (default)
              ((ps:@ data value)
               (update-counter-display (ps:@ data value))
               (when (ps:@ data delta)
                 (add-counter-event data)))))))))

    ;; === Optimistic Todos ===
    (defun create-todo-element (id text &optional is-optimistic)
      (let ((li (ps:chain document (create-element "li"))))
        (setf (ps:@ li class-name)
              (+ "todo-item" (if is-optimistic " optimistic" "")))
        (setf (ps:@ li dataset id) id)
        (setf (ps:@ li inner-h-t-m-l)
              (+ "<input type='checkbox' onclick='toggleTodo(" id ", this)'>"
                 "<span class='todo-text'>" text "</span>"
                 "<button class='delete-btn' onclick='deleteTodo(" id ", this.closest(\"li\"))'>×</button>"))
        li))

    (defun add-todo ()
      (let* ((input (ps:chain document (get-element-by-id "todo-input")))
             (text (ps:@ input value))
             (temp-id (+ "temp-" (ps:chain -date (now))))
             (list-el (ps:chain document (get-element-by-id "todo-list"))))
        (when (and text (> (ps:@ text length) 0))
          (let ((li (create-todo-element temp-id text t)))
            (ps:chain list-el (prepend li)))
          (setf (ps:@ input value) "")
          (ps:chain (fetch "/api/todos"
            (ps:create :method "POST"
                       :headers (ps:create "Content-Type" "application/x-www-form-urlencoded")
                       :body (+ "text=" (encode-u-r-i-component text))))
            (then (lambda (r)
                    (if (ps:@ r ok)
                        (ps:chain r (json))
                        (throw (ps:new (-Error "Failed"))))))
            (then (lambda (data)
                    (let ((el (ps:chain document (query-selector (+ "[data-id='" temp-id "']")))))
                      (when el
                        (setf (ps:@ el dataset id) (ps:@ data id))
                        (ps:chain el class-list (remove "optimistic"))
                        (setf (ps:@ el inner-h-t-m-l)
                              (+ "<input type='checkbox' onclick='toggleTodo(" (ps:@ data id) ", this)'>"
                                 "<span class='todo-text'>" text "</span>"
                                 "<button class='delete-btn' onclick='deleteTodo(" (ps:@ data id) ", this.closest(\"li\"))'>×</button>"))))))
            (catch (lambda (err)
                     (let ((el (ps:chain document (query-selector (+ "[data-id='" temp-id "']")))))
                       (when el (ps:chain el (remove))))
                     (show-error "Failed to add todo")))))))

    (setf (ps:@ window toggle-todo) (lambda (id checkbox)
      (let ((item (ps:chain checkbox (closest "li"))))
        (ps:chain item class-list (toggle "completed"))
        (ps:chain (fetch "/api/todos/toggle"
          (ps:create :method "POST"
                     :headers (ps:create "Content-Type" "application/x-www-form-urlencoded")
                     :body (+ "id=" id)))
          (catch (lambda (err)
                   (ps:chain item class-list (toggle "completed"))
                   (setf (ps:@ checkbox checked) (not (ps:@ checkbox checked)))
                   (show-error "Failed to toggle todo")))))))

    (setf (ps:@ window delete-todo) (lambda (id item)
      (setf (ps:@ item style opacity) "0.5")
      (ps:chain (fetch "/api/todos/delete"
        (ps:create :method "POST"
                   :headers (ps:create "Content-Type" "application/x-www-form-urlencoded")
                   :body (+ "id=" id)))
        (then (lambda (r)
                (if (ps:@ r ok)
                    (ps:chain item (remove))
                    (throw (ps:new (-Error "Failed"))))))
        (catch (lambda (err)
                 (setf (ps:@ item style opacity) "1")
                 (show-error "Failed to delete todo"))))))

    (defun init-todos ()
      (let ((input (ps:chain document (get-element-by-id "todo-input")))
            (btn (ps:chain document (get-element-by-id "todo-add"))))
        (when input
          (ps:chain input (add-event-listener "keypress" (lambda (e)
            (when (= (ps:@ e key) "Enter")
              (add-todo))))))
        (when btn
          (ps:chain btn (add-event-listener "click" add-todo)))))

    ;; === HTMX Event Handlers ===
    (ps:chain document body (add-event-listener "htmx:responseError"
      (lambda (e)
        (show-error (or (ps:@ e detail xhr response-text)
                        "Request failed")))))


    ;; === Initialize ===
    (defun init-dashboard ()
      (update-connection-status "Connecting...")
      (init-chat)
      (init-counter)
      (init-todos)
      (ps:chain console (log "Dashboard initialized")))

    (setf (ps:@ window onload) init-dashboard)))

;;; ============================================================================
;;; ENTRY POINTS
;;; ============================================================================

(defun start (&key (port 8080))
  "Start the demo application."
  (format t "~&; Starting Realtime Dashboard on port ~A...~%" port)
  ;; Disable session/CSRF for demo - keeps middleware simple
  (start-server :port port :use-session nil :use-csrf nil)
  (format t "~&; Dashboard running at http://localhost:~A~%" port))

(defun stop ()
  "Stop the demo application."
  (stop-server)
  (format t "~&; Dashboard stopped.~%"))

(defun main ()
  "Entry point for standalone program."
  (format t "~&;; LOL-REACTIVE Realtime Dashboard~%")
  (format t "~&;; Stress testing: WebSocket, SSE, Optimistic Updates~%")
  (format t "~&;; Starting server on port 8080...~%")
  ;; Disable session/CSRF for demo - keeps middleware simple
  (start-server :port 8080 :use-session nil :use-csrf nil)
  (format t "~&;; Visit http://localhost:8080~%")
  (format t "~&;; Press Ctrl+C to stop~%")
  ;; Keep running
  (loop (sleep 3600)))
