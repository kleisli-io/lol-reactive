;;;; LOL-REACTIVE Feature Showcase
;;;;
;;;; Interactive demonstration of all lol-reactive features:
;;;; - Reactive Signals (make-signal, make-computed, make-effect)
;;;; - dlambda Pattern (message-passing closures)
;;;; - Surgery/X-Ray Mode (live component inspection)
;;;; - DOM Diffing (with-diffing macro)
;;;; - Keyed Lists (efficient reconciliation)
;;;; - Multi-Step Wizards (defwizard)
;;;; - HTMX Integration (OOB swaps)
;;;; - State Stores (make-store)
;;;; - Form Validation (reactive feedback)

(in-package :lol-reactive-demo)

;;; ============================================================================
;;; SHOWCASE UTILITIES
;;; ============================================================================

(defun code-block (code &key (language "lisp"))
  "Render a code block with syntax highlighting placeholder."
  (htm-str
    (:pre :class (format nil "code-block language-~a" language)
      (:code (cl-who:esc code)))))

(defun feature-section (id title description code-example demo-html)
  "Render a feature showcase section with code + live demo."
  (htm-str
    (:section :class "feature-section" :id id
      (:div :class "feature-header"
        (:h2 :class "feature-title" (cl-who:esc title))
        (:p :class "feature-description" (cl-who:esc description)))
      (:div :class "feature-content"
        (:div :class "feature-code"
          (:h3 "Code")
          (cl-who:str (code-block code-example)))
        (:div :class "feature-demo"
          (:h3 "Live Demo")
          (:div :class "demo-container"
            (cl-who:str demo-html)))))))

;;; ============================================================================
;;; SECTION 1: REACTIVE SIGNALS
;;; ============================================================================

;; Demo state for signals section
(defvar *showcase-counter* nil)
(defvar *showcase-counter-setter* nil)

(defun init-signals-demo ()
  "Initialize the signals demo state."
  (multiple-value-bind (getter setter) (lol-reactive:make-signal 0)
    (setf *showcase-counter* getter
          *showcase-counter-setter* setter)))

(defun render-signals-section ()
  "Demonstrate make-signal, make-computed, make-effect."
  (unless *showcase-counter*
    (init-signals-demo))
  (let ((count (funcall *showcase-counter*))
        (doubled (lol-reactive:make-computed
                   (lambda () (* 2 (funcall *showcase-counter*))))))
    (feature-section
      "signals"
      "Reactive Signals"
      "Signals are reactive primitives that automatically track dependencies. When a signal changes, all computed values and effects that depend on it are automatically updated."
      "(multiple-value-bind (count set-count) (make-signal 0)
  ;; Computed values auto-track dependencies
  (let ((doubled (make-computed
                   (lambda () (* 2 (funcall count))))))
    ;; Effects run when dependencies change
    (make-effect
      (lambda ()
        (format t \"Count is now: ~A\" (funcall count))))
    ;; Update signal
    (funcall set-count 5)  ; Effect runs, doubled = 10))"
      (htm-str
        (:div :class "signal-demo"
          (:div :class "signal-value"
            (:span :class "label" "Count: ")
            (:span :class "value" :id "signal-count" (cl-who:fmt "~A" count)))
          (:div :class "signal-value"
            (:span :class "label" "Doubled: ")
            (:span :class "value" :id "signal-doubled" (cl-who:fmt "~A" (funcall doubled))))
          (:div :class "signal-controls"
            (:button :class "btn"
                     :hx-post "/showcase/signal/decrement"
                     :hx-swap "none"
              "-")
            (:button :class "btn"
                     :hx-post "/showcase/signal/increment"
                     :hx-swap "none"
              "+")
            (:button :class "btn btn-secondary"
                     :hx-post "/showcase/signal/reset"
                     :hx-swap "none"
              "Reset")))))))

;;; ============================================================================
;;; SECTION 2: DLAMBDA PATTERN
;;; ============================================================================

;; Demo dlambda for message-passing
(defvar *showcase-bank-account* nil)

(defun init-dlambda-demo ()
  "Initialize the dlambda demo - a simple bank account."
  (setf *showcase-bank-account*
        (let ((balance 100))
          (dlambda
            (:balance () balance)
            (:deposit (amount)
              (incf balance amount)
              balance)
            (:withdraw (amount)
              (if (>= balance amount)
                  (progn (decf balance amount) balance)
                  (error "Insufficient funds")))
            (:inspect ()
              (list :balance balance
                    :type 'checking))))))

(defun render-dlambda-section ()
  "Demonstrate the dlambda message-passing pattern."
  (unless *showcase-bank-account*
    (init-dlambda-demo))
  (let ((balance (funcall *showcase-bank-account* :balance)))
    (feature-section
      "dlambda"
      "dlambda Pattern"
      "dlambda creates message-passing closures - functions that respond to different messages with different behaviors. This is the foundation of lol-reactive's component system."
      "(defvar *account*
  (let ((balance 100))
    (dlambda
      (:balance () balance)
      (:deposit (amount) (incf balance amount))
      (:withdraw (amount)
        (when (>= balance amount)
          (decf balance amount)))
      (:inspect ()
        (list :balance balance :type 'checking)))))

;; Usage:
(funcall *account* :balance)      ; => 100
(funcall *account* :deposit 50)   ; => 150
(funcall *account* :withdraw 30)  ; => 120"
      (htm-str
        (:div :class "dlambda-demo"
          (:div :class "account-display"
            (:span :class "currency" "$")
            (:span :class "balance" :id "account-balance"
              (cl-who:fmt "~,2F" balance)))
          (:div :class "account-controls"
            (:button :class "btn btn-success"
                     :hx-post "/showcase/account/deposit"
                     :hx-swap "none"
              "Deposit $10")
            (:button :class "btn btn-warning"
                     :hx-post "/showcase/account/withdraw"
                     :hx-swap "none"
              "Withdraw $10"))
          (:div :class "account-inspect"
            (:button :class "btn btn-secondary"
                     :hx-get "/showcase/account/inspect"
                     :hx-target "#inspect-result"
              "Inspect State")
            (:pre :class "inspect-result" :id "inspect-result"
              "(click Inspect to see internal state)")))))))

;;; ============================================================================
;;; SECTION 3: SURGERY/X-RAY MODE
;;; ============================================================================

(defun render-surgery-section ()
  "Demonstrate live component inspection and state manipulation."
  (feature-section
    "surgery"
    "Surgery / X-Ray Mode"
    "Surgery mode allows live inspection and modification of component state. Enable X-ray mode to see component boundaries and internal state trees."
    "(enable-surgery-mode)  ; Turn on globally

;; Components automatically get X-ray overlay
(defcomponent counter (count)
  ...
  ;; With surgery mode, hovering shows:
  ;; - Component ID
  ;; - Current state values
  ;; - Message handlers
  ;; - Snapshot history)

;; Live state editing:
(surgery-set-state 'counter-1 :count 42)
(surgery-dispatch 'counter-1 :reset)"
    (htm-str
      (:div :class "surgery-demo"
        (:div :class "surgery-controls"
          (:button :class "btn"
                   :id "toggle-surgery"
                   :hx-post "/showcase/surgery/toggle"
                   :hx-swap "none"
            (if (lol-reactive:surgery-mode-p)
                "Disable X-Ray"
                "Enable X-Ray"))
          (:span :class "surgery-status"
            :id "surgery-status"
            (if (lol-reactive:surgery-mode-p)
                "X-Ray Mode: ON"
                "X-Ray Mode: OFF")))
        (:div :class "surgery-target"
              :id "surgery-component"
          (:div :class "demo-component"
            (:h4 "Sample Component")
            (:p "This component can be inspected in X-ray mode")
            (:div :class "component-state"
              (:code "state: {:clicks 0, :name \"demo\"}"))))))))

;;; ============================================================================
;;; SECTION 4: DOM DIFFING
;;; ============================================================================

(defun render-diffing-section ()
  "Demonstrate the DOM diffing algorithm."
  (let* ((old-html '(:ul :class "list"
                     (:li :id "a" "Apple")
                     (:li :id "b" "Banana")))
         (new-html '(:ul :class "list updated"
                     (:li :id "a" "Apple")
                     (:li :id "b" "Blueberry")
                     (:li :id "c" "Cherry")))
         ;; Use internal function directly for demo
         (patches (lol-reactive::diff-html-sexp old-html new-html)))
    (feature-section
      "diffing"
      "DOM Diffing"
      "The diff algorithm compares HTML s-expressions and generates minimal DOM patches. Only changed elements are updated, preserving focus and scroll state."
      "(let ((old '(:ul :class \"list\"
              (:li :id \"a\" \"Apple\")
              (:li :id \"b\" \"Banana\")))
      (new '(:ul :class \"list updated\"
              (:li :id \"a\" \"Apple\")
              (:li :id \"b\" \"Blueberry\")
              (:li :id \"c\" \"Cherry\"))))
  (diff-html-sexp old new))
;; => ((:SET-ATTR (0) :class \"list updated\")
;;     (:SET-TEXT (0 1) \"Blueberry\")
;;     (:INSERT (0 2) \"<li id='c'>Cherry</li>\"))"
      (htm-str
        (:div :class "diff-demo"
          (:div :class "diff-columns"
            (:div :class "diff-column"
              (:h4 "Old HTML")
              (:pre :class "diff-code"
                (cl-who:esc (format nil "~S" old-html))))
            (:div :class "diff-column"
              (:h4 "New HTML")
              (:pre :class "diff-code"
                (cl-who:esc (format nil "~S" new-html))))
            (:div :class "diff-column"
              (:h4 "Generated Patches")
              (:pre :class "diff-code"
                (cl-who:esc (format nil "~{~S~%~}" patches))))))))))

;;; ============================================================================
;;; SECTION 5: KEYED LISTS
;;; ============================================================================

(defvar *showcase-items* '((:id 1 :name "First")
                           (:id 2 :name "Second")
                           (:id 3 :name "Third")))

(defun render-keyed-list-section ()
  "Demonstrate keyed list reconciliation."
  (feature-section
    "keyed-lists"
    "Keyed Lists"
    "Keyed lists use stable identifiers to minimize DOM operations when items are added, removed, or reordered. The reconciliation algorithm determines the minimal set of moves."
    "(keyed-render \"my-list\"
  items
  (lambda (item) (getf item :id))  ; key function
  (lambda (item)                    ; render function
    (htm-str (:li (esc (getf item :name))))))

;; When items change order:
;; - Items with same key are reused (not recreated)
;; - Only position changes are applied
;; - Focus/state in items is preserved"
    (htm-str
      (:div :class "keyed-demo"
        (:ul :class "keyed-list" :id "keyed-list"
          (dolist (item *showcase-items*)
            (cl-who:htm
              (:li :class "keyed-item"
                   :data-id (getf item :id)
                (:span :class "item-name" (cl-who:esc (getf item :name)))
                (:span :class "item-controls"
                  (:button :class "move-btn"
                           :hx-post (format nil "/showcase/list/move-up?id=~A" (getf item :id))
                           :hx-swap "none"
                    "↑")
                  (:button :class "move-btn"
                           :hx-post (format nil "/showcase/list/move-down?id=~A" (getf item :id))
                           :hx-swap "none"
                    "↓"))))))
        (:div :class "list-controls"
          (:button :class "btn"
                   :hx-post "/showcase/list/add"
                   :hx-swap "none"
            "Add Item")
          (:button :class "btn btn-secondary"
                   :hx-post "/showcase/list/shuffle"
                   :hx-swap "none"
            "Shuffle"))))))

;;; ============================================================================
;;; SECTION 6: WIZARDS
;;; ============================================================================

(defun render-wizard-section ()
  "Demonstrate multi-step wizard forms."
  (feature-section
    "wizards"
    "Multi-Step Wizards"
    "defwizard creates multi-step form flows with automatic navigation, data persistence across steps, and customizable completion handlers."
    "(defwizard signup ()
  :steps ((:name :account :title \"Account Info\"
           :form (lambda (data) (render-account-form data)))
          (:name :profile :title \"Profile\"
           :form (lambda (data) (render-profile-form data)))
          (:name :confirm :title \"Confirm\"
           :form (lambda (data) (render-confirmation data))))
  :on-complete (lambda (data) (create-user data)))

;; Automatically provides:
;; - Step progress indicator
;; - Back/Next navigation
;; - Data persistence across steps
;; - Validation at each step"
    (htm-str
      (:div :class "wizard-demo"
        (:div :class "wizard-preview"
          (:div :class "wizard-steps"
            (:div :class "wizard-step completed"
              (:span :class "step-number" "1")
              (:span :class "step-title" "Account"))
            (:div :class "wizard-step active"
              (:span :class "step-number" "2")
              (:span :class "step-title" "Profile"))
            (:div :class "wizard-step"
              (:span :class "step-number" "3")
              (:span :class "step-title" "Confirm")))
          (:div :class "wizard-content"
            (:p "Step 2: Enter your profile information")
            (:div :class "wizard-form-preview"
              (:input :type "text" :placeholder "Display Name" :disabled "disabled")
              (:input :type "text" :placeholder "Bio" :disabled "disabled")))
          (:div :class "wizard-nav"
            (:button :class "btn btn-secondary" :disabled "disabled" "← Back")
            (:button :class "btn" :disabled "disabled" "Next →")))
        (:p :class "wizard-note"
          "See the checkout page for a full working wizard example")))))

;;; ============================================================================
;;; SECTION 7: HTMX INTEGRATION
;;; ============================================================================

(defun render-htmx-section ()
  "Demonstrate HTMX-style declarative updates."
  (feature-section
    "htmx"
    "HTMX Integration"
    "lol-reactive provides an HTMX-compatible runtime for declarative partial page updates. Single server responses can update multiple DOM regions via Out-of-Band (OOB) swaps."
    ";; Client: button with hx-* attributes
(:button :hx-post \"/api/cart/add\"
         :hx-swap \"none\"  ; OOB handles updates
  \"Add to Cart\")

;; Server: return OOB updates for multiple elements
(with-htmx-response (:trigger \"cartUpdated\")
  (render-with-oob
    nil  ; no primary target
    (list \"cart-count\" (cart-count))
    (list \"cart-total\" (format nil \"$~,2F\" (cart-total)))
    (list \"cart-dropdown\" (render-dropdown) :swap \"outerHTML\")))"
    (htm-str
      (:div :class "htmx-demo"
        (:div :class "htmx-example"
          (:div :class "target-elements"
            (:span "Count: " (:span :id "htmx-count" "0"))
            (:span "Total: " (:span :id "htmx-total" "$0.00")))
          (:button :class "btn"
                   :hx-post "/showcase/htmx/update"
                   :hx-swap "none"
            "Trigger OOB Update"))
        (:p :class "htmx-note"
          "Click the button - both count and total update from a single response")))))

;;; ============================================================================
;;; SECTION 8: STATE STORES
;;; ============================================================================

(defvar *showcase-store* nil)

(defun init-store-demo ()
  "Initialize the store demo."
  (setf *showcase-store*
        (make-store
          '((todos . ((:id 1 :text "Learn lol-reactive" :done t)
                      (:id 2 :text "Build something cool" :done nil)))
            (filter . :all)))))

(defun render-store-section ()
  "Demonstrate Redux-style state stores."
  (unless *showcase-store*
    (init-store-demo))
  (let ((state (funcall *showcase-store* :state)))
    (feature-section
      "stores"
      "State Stores"
      "make-store creates Redux-style state containers with immutable updates, action dispatching, and subscriber notifications."
      "(defvar *store*
  (make-store
    '(:todos ((:id 1 :text \"Learn\" :done nil))
      :filter :all)))

;; Get state
(funcall *store* :get-state)

;; Dispatch actions
(funcall *store* :dispatch :add-todo \"New task\")
(funcall *store* :dispatch :toggle-todo 1)
(funcall *store* :dispatch :set-filter :completed)

;; Subscribe to changes
(funcall *store* :subscribe
  (lambda (state) (render-todos state)))"
      (htm-str
        (:div :class "store-demo"
          (:div :class "store-state"
            (:h4 "Current State")
            (:pre :class "state-display" :id "store-state"
              (cl-who:esc (format nil "~S" state))))
          (:div :class "store-actions"
            (:button :class "btn"
                     :hx-post "/showcase/store/add-todo"
                     :hx-swap "none"
              "Add Todo")
            (:button :class "btn btn-secondary"
                     :hx-post "/showcase/store/toggle-first"
                     :hx-swap "none"
              "Toggle First")))))))

;;; ============================================================================
;;; SECTION 9: FORM VALIDATION
;;; ============================================================================

(defun render-forms-section ()
  "Demonstrate form validation with reactive feedback."
  (feature-section
    "forms"
    "Form Validation"
    "lol-reactive provides a Form DSL with compile-time validation rules and reactive error feedback."
    ";; Define form with validation
(defform signup-form ()
  (:email :type :email :required t
          :validate (lambda (v)
                      (when (search \"@\" v) t)))
  (:password :type :password :required t
             :min-length 8)
  (:confirm :type :password
            :must-match :password))

;; Renders with:
;; - Real-time validation as user types
;; - Error messages below fields
;; - Submit disabled until valid"
    (htm-str
      (:div :class "form-demo"
        (:form :class "demo-form"
          (:div :class "form-field"
            (:label "Email")
            (:input :type "email"
                    :name "email"
                    :placeholder "you@example.com"
                    :hx-post "/showcase/form/validate-email"
                    :hx-trigger "input changed delay:300ms"
                    :hx-target "#email-error")
            (:span :class "field-error" :id "email-error"))
          (:div :class "form-field"
            (:label "Password")
            (:input :type "password"
                    :name "password"
                    :placeholder "8+ characters"
                    :hx-post "/showcase/form/validate-password"
                    :hx-trigger "input changed delay:300ms"
                    :hx-target "#password-error")
            (:span :class "field-error" :id "password-error"))
          (:button :class "btn" :type "button" :disabled "disabled"
            "Submit"))))))

;;; ============================================================================
;;; SHOWCASE PAGE LAYOUT
;;; ============================================================================

(defun render-showcase-nav ()
  "Render the showcase navigation sidebar."
  (htm-str
    (:nav :class "showcase-nav"
      (:div :class "nav-header"
        (:a :href "/" :class "nav-logo" "← TechShop")
        (:h2 "Feature Showcase"))
      (:ul :class "nav-links"
        (:li (:a :href "#signals" "Reactive Signals"))
        (:li (:a :href "#dlambda" "dlambda Pattern"))
        (:li (:a :href "#surgery" "Surgery/X-Ray"))
        (:li (:a :href "#diffing" "DOM Diffing"))
        (:li (:a :href "#keyed-lists" "Keyed Lists"))
        (:li (:a :href "#wizards" "Wizards"))
        (:li (:a :href "#htmx" "HTMX Integration"))
        (:li (:a :href "#stores" "State Stores"))
        (:li (:a :href "#forms" "Form Validation"))))))

(defun render-showcase-page ()
  "Render the complete showcase page."
  (htm-str
    (:div :class "showcase-layout"
      (cl-who:str (render-showcase-nav))
      (:main :class "showcase-content"
        (:header :class "showcase-header"
          (:h1 "lol-reactive Features")
          (:p :class "subtitle" "Interactive demonstrations of all framework capabilities"))
        (cl-who:str (render-signals-section))
        (cl-who:str (render-dlambda-section))
        (cl-who:str (render-surgery-section))
        (cl-who:str (render-diffing-section))
        (cl-who:str (render-keyed-list-section))
        (cl-who:str (render-wizard-section))
        (cl-who:str (render-htmx-section))
        (cl-who:str (render-store-section))
        (cl-who:str (render-forms-section))))))

;;; ============================================================================
;;; SHOWCASE CSS
;;; ============================================================================

(defcss :showcase-styles
  ;; Layout
  (".showcase-layout" (("display" . "flex")
                       ("min-height" . "100vh")
                       ("overflow-x" . "hidden")))
  (".showcase-nav" (("min-width" . "220px")
                    ("width" . "220px")
                    ("flex-shrink" . "0")
                    ("background" . "var(--color-surface)")
                    ("padding" . "1.5rem")
                    ("border-right" . "1px solid rgba(255,255,255,0.1)")
                    ("position" . "sticky")
                    ("top" . "0")
                    ("height" . "100vh")
                    ("overflow-y" . "auto")))
  (".nav-header" (("margin-bottom" . "1.5rem")))
  (".nav-logo" (("color" . "var(--color-muted)")
                ("font-size" . "0.875rem")
                ("display" . "block")
                ("margin-bottom" . "0.5rem")))
  (".nav-links" (("list-style" . "none")
                 ("padding" . "0")
                 ("margin" . "0")))
  (".nav-links li" (("margin-bottom" . "0.5rem")))
  (".nav-links a" (("color" . "var(--color-text)")
                   ("padding" . "0.5rem 0.75rem")
                   ("display" . "block")
                   ("border-radius" . "6px")
                   ("transition" . "background 0.2s")))
  (".nav-links a:hover" (("background" . "rgba(255,255,255,0.05)")
                         ("text-decoration" . "none")))

  ;; Content
  (".showcase-content" (("flex" . "1")
                        ("min-width" . "0")
                        ("padding" . "2rem")
                        ("overflow-x" . "hidden")))
  (".showcase-header" (("margin-bottom" . "3rem")
                       ("padding-bottom" . "2rem")
                       ("border-bottom" . "1px solid rgba(255,255,255,0.1)")))
  (".showcase-header h1" (("font-size" . "2.5rem")
                          ("margin-bottom" . "0.5rem")))
  (".subtitle" (("color" . "var(--color-muted)")
                ("font-size" . "1.125rem")))

  ;; Feature sections
  (".feature-section" (("margin-bottom" . "4rem")
                       ("padding" . "2rem")
                       ("background" . "var(--color-surface)")
                       ("border-radius" . "12px")))
  (".feature-title" (("font-size" . "1.5rem")
                     ("margin-bottom" . "0.5rem")))
  (".feature-description" (("color" . "var(--color-muted)")
                           ("margin-bottom" . "1.5rem")))
  (".feature-content" (("display" . "grid")
                       ("grid-template-columns" . "1fr 1fr")
                       ("gap" . "1.5rem")
                       ("min-width" . "0")))
  (".feature-code, .feature-demo" (("min-width" . "0")
                                    ("overflow" . "hidden")))
  (".feature-code h3, .feature-demo h3" (("font-size" . "0.875rem")
                                          ("text-transform" . "uppercase")
                                          ("color" . "var(--color-muted)")
                                          ("margin-bottom" . "0.75rem")))

  ;; Code blocks
  (".code-block" (("background" . "#1e1e2e")
                  ("padding" . "1rem")
                  ("border-radius" . "8px")
                  ("font-family" . "var(--font-mono)")
                  ("font-size" . "0.75rem")
                  ("overflow-x" . "auto")
                  ("line-height" . "1.4")
                  ("max-width" . "100%")))

  ;; Demo containers
  (".demo-container" (("background" . "rgba(0,0,0,0.2)")
                      ("padding" . "1.5rem")
                      ("border-radius" . "8px")
                      ("min-height" . "150px")))

  ;; Buttons
  (".btn" (("background" . "var(--color-primary)")
           ("color" . "white")
           ("border" . "none")
           ("padding" . "0.5rem 1rem")
           ("border-radius" . "6px")
           ("cursor" . "pointer")
           ("font-size" . "0.875rem")
           ("transition" . "opacity 0.2s")))
  (".btn:hover" (("opacity" . "0.9")))
  (".btn:disabled" (("opacity" . "0.5")
                    ("cursor" . "not-allowed")))
  (".btn-secondary" (("background" . "rgba(255,255,255,0.1)")))
  (".btn-success" (("background" . "var(--color-success)")))
  (".btn-warning" (("background" . "var(--color-warning)")
                   ("color" . "black")))

  ;; Signal demo
  (".signal-demo" (("text-align" . "center")))
  (".signal-value" (("font-size" . "1.25rem")
                    ("margin-bottom" . "1rem")))
  (".signal-value .label" (("color" . "var(--color-muted)")))
  (".signal-value .value" (("font-weight" . "bold")
                           ("color" . "var(--color-primary)")))
  (".signal-controls" (("display" . "flex")
                       ("gap" . "0.5rem")
                       ("justify-content" . "center")))

  ;; dlambda demo
  (".account-display" (("font-size" . "2rem")
                       ("text-align" . "center")
                       ("margin-bottom" . "1rem")))
  (".account-display .currency" (("color" . "var(--color-muted)")))
  (".account-display .balance" (("font-weight" . "bold")))
  (".account-controls" (("display" . "flex")
                        ("gap" . "0.5rem")
                        ("justify-content" . "center")
                        ("margin-bottom" . "1rem")))
  (".inspect-result" (("font-size" . "0.75rem")
                      ("color" . "var(--color-muted)")
                      ("margin-top" . "1rem")))

  ;; Surgery demo
  (".surgery-controls" (("display" . "flex")
                        ("align-items" . "center")
                        ("gap" . "1rem")
                        ("margin-bottom" . "1rem")))
  (".surgery-status" (("font-size" . "0.875rem")
                      ("color" . "var(--color-muted)")))
  (".surgery-target" (("border" . "2px dashed rgba(255,255,255,0.2)")
                      ("padding" . "1rem")
                      ("border-radius" . "8px")))

  ;; Diff demo
  (".diff-columns" (("display" . "grid")
                    ("grid-template-columns" . "1fr 1fr 1fr")
                    ("gap" . "1rem")))
  (".diff-column h4" (("font-size" . "0.75rem")
                      ("color" . "var(--color-muted)")
                      ("margin-bottom" . "0.5rem")))
  (".diff-code" (("font-size" . "0.7rem")
                 ("padding" . "0.75rem")))

  ;; Keyed list demo
  (".keyed-list" (("list-style" . "none")
                  ("padding" . "0")
                  ("margin" . "0 0 1rem 0")))
  (".keyed-item" (("display" . "flex")
                  ("justify-content" . "space-between")
                  ("align-items" . "center")
                  ("padding" . "0.5rem 0.75rem")
                  ("background" . "rgba(255,255,255,0.05)")
                  ("margin-bottom" . "0.25rem")
                  ("border-radius" . "4px")))
  (".move-btn" (("background" . "transparent")
                ("border" . "1px solid rgba(255,255,255,0.2)")
                ("color" . "var(--color-text)")
                ("padding" . "0.25rem 0.5rem")
                ("cursor" . "pointer")
                ("border-radius" . "4px")))

  ;; Wizard demo
  (".wizard-preview" (("border" . "1px solid rgba(255,255,255,0.1)")
                      ("border-radius" . "8px")
                      ("overflow" . "hidden")))
  (".wizard-steps" (("display" . "flex")
                    ("background" . "rgba(0,0,0,0.2)")
                    ("padding" . "1rem")))
  (".wizard-step" (("flex" . "1")
                   ("text-align" . "center")
                   ("opacity" . "0.5")))
  (".wizard-step.active, .wizard-step.completed" (("opacity" . "1")))
  (".step-number" (("display" . "inline-block")
                   ("width" . "24px")
                   ("height" . "24px")
                   ("background" . "var(--color-primary)")
                   ("border-radius" . "50%")
                   ("line-height" . "24px")
                   ("font-size" . "0.75rem")
                   ("margin-bottom" . "0.25rem")))
  (".wizard-step.completed .step-number" (("background" . "var(--color-success)")))
  (".step-title" (("display" . "block")
                  ("font-size" . "0.75rem")))
  (".wizard-content" (("padding" . "1.5rem")))
  (".wizard-nav" (("padding" . "1rem")
                  ("border-top" . "1px solid rgba(255,255,255,0.1)")
                  ("display" . "flex")
                  ("justify-content" . "space-between")))
  (".wizard-note" (("font-size" . "0.875rem")
                   ("color" . "var(--color-muted)")
                   ("margin-top" . "1rem")))

  ;; Form demo
  (".demo-form" (("max-width" . "300px")))
  (".form-field" (("margin-bottom" . "1rem")))
  (".form-field label" (("display" . "block")
                        ("font-size" . "0.875rem")
                        ("margin-bottom" . "0.25rem")))
  (".form-field input" (("width" . "100%")
                        ("padding" . "0.5rem")
                        ("border" . "1px solid rgba(255,255,255,0.2)")
                        ("border-radius" . "4px")
                        ("background" . "rgba(0,0,0,0.2)")
                        ("color" . "var(--color-text)")))
  (".field-error" (("font-size" . "0.75rem")
                   ("color" . "var(--color-error)")
                   ("min-height" . "1rem")
                   ("display" . "block"))))
