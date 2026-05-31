;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOL-WEB/PARENSCRIPT; Base: 10 -*-
;;;; Parenscript utilities for reactive client-side code generation

(in-package :lol-web/parenscript)

;;; ============================================================================
;;; PARENSCRIPT HELPERS
;;;
;;; Generate JavaScript from Lisp using Parenscript, maintaining the
;;; "Let Over Lambda" philosophy on the client side.
;;; ============================================================================

(defmacro ps (&body body)
  "Shorthand for parenscript:ps."
  `(parenscript:ps ,@body))

(defmacro ps* (&body body)
  "Shorthand for parenscript:ps*."
  `(parenscript:ps* ,@body))

;;; ============================================================================
;;; REACTIVE SCRIPT GENERATION
;;; ============================================================================

(defmacro reactive-script (&body body)
  "Generate a script tag with Parenscript code.
   Wraps output in a script tag for embedding in cl-who."
  `(cl-who:with-html-output-to-string (s)
     (:script
      (cl-who:str (ps ,@body)))))

(defmacro inline-handler ((&rest args) &body body)
  "Generate an inline JavaScript handler string.
   Useful for onclick, onchange, etc."
  `(ps (lambda ,args ,@body)))

;;; ============================================================================
;;; JS-LITERAL COERCION
;;; ============================================================================

(defun jsonify (val)
  "Coerce VAL to a value parenscript will emit as a JS literal.
   Strings, numbers, T, and NIL pass through unchanged. Symbols are
   downcased to their string name. Conses and any other type signal a
   simple-error rather than reach JS code position."
  (typecase val
    (string  val)
    (integer val)
    (number  val)
    (null    nil)
    ((eql t) t)
    (symbol  (string-downcase (symbol-name val)))
    (cons    (error "jsonify: refusing to emit cons ~S as JS literal" val))
    (t       (error "jsonify: refusing to emit ~S (type ~A) as JS literal"
                    val (type-of val)))))

;;; ============================================================================
;;; COMPONENT EVENT HANDLERS
;;; ============================================================================

(defun on-click (component-id action &rest args)
  "Generate an onclick handler that dispatches to a component.
   COMPONENT-ID must be a string; ACTION must be a string or symbol;
   ARGS each route through JSONIFY so only JS literals reach code position."
  (check-type component-id string)
  (check-type action (or string symbol))
  (parenscript:ps* `(funcall dispatch ,component-id ,(jsonify action)
                             ,@(mapcar #'jsonify args))))

(defun on-change (component-id state-key)
  "Generate an onchange handler that updates component state.
   COMPONENT-ID must be a string; STATE-KEY must be a string or symbol
   and routes through JSONIFY."
  (check-type component-id string)
  (check-type state-key (or string symbol))
  (parenscript:ps* `(funcall set-state ,component-id ,(jsonify state-key)
                             (ps:@ this value))))

(defun on-submit (component-id action)
  "Generate an onsubmit handler.
   COMPONENT-ID must be a string; ACTION must be a string or symbol
   and routes through JSONIFY."
  (check-type component-id string)
  (check-type action (or string symbol))
  (concatenate 'string
    (parenscript:ps* `((ps:@ event prevent-default)))
    " "
    (parenscript:ps* `(funcall dispatch ,component-id ,(jsonify action)
                               (ps:new (-Form-Data this))))))

(defun js-value (val)
  "Convert a Lisp value to a SAFE-JS-STRING-LITERAL whose VALUE field
   holds the JS source representation.

   - NIL    → \"null\"
   - number → its printed form
   - string → single-quoted, with `'`, `\"`, `\\`, `\\n`, `\\r`, `<`,
     U+2028, U+2029 escaped
   - symbol → single-quoted, lowercased, with the same escapes as string
   - SAFE-JS-STRING-LITERAL → returned unchanged
   - other  → TYPE-ERROR (use a string accessor / explicit conversion)"
  (cond
    ((safe-js-string-literal-p val) val)
    ((null val)                     (%make-safe-js-string-literal :value "null"))
    ((numberp val)                  (%make-safe-js-string-literal
                                     :value (princ-to-string val)))
    ((stringp val)                  (make-safe-js-string-literal val))
    ((symbolp val)                  (make-safe-js-string-literal
                                     (string-downcase (symbol-name val))))
    (t (error 'type-error
              :datum val
              :expected-type '(or null number string symbol
                               safe-js-string-literal)))))

;;; ============================================================================
;;; CLIENT-SIDE REACTIVE STATE (Parenscript)
;;;
;;; These generate JavaScript closures that mirror the Let Over Lambda
;;; patterns on the client side.
;;; ============================================================================

(parenscript:defpsmacro make-state (initial-value)
  "Create a reactive state container in JavaScript.
   Returns [getter, setter] like React's useState but with closures."
  `(let ((value ,initial-value)
         (subscribers (array)))
     (array
      ;; Getter
      (lambda () value)
      ;; Setter
      (lambda (new-value)
        (setf value new-value)
        (dolist (sub subscribers)
          (funcall sub new-value))
        value)
      ;; Subscribe
      (lambda (callback)
        ((ps:@ subscribers push) callback)
        ;; Return unsubscribe
        (lambda ()
          (setf subscribers
                ((ps:@ subscribers filter)
                 (lambda (s) (not (= s callback))))))))))

(parenscript:defpsmacro with-state ((getter setter &optional subscriber) init &body body)
  "Destructure state container and execute body."
  `(let* ((state-tuple (make-state ,init))
          (,getter (aref state-tuple 0))
          (,setter (aref state-tuple 1))
          ,@(when subscriber
              `((,subscriber (aref state-tuple 2)))))
     ,@body))

;;; ============================================================================
;;; REACTIVE DOM UPDATES
;;; ============================================================================

(parenscript:defpsmacro bind-element (id state-getter &key (attr "innerText"))
  "Bind a DOM element to reactive state."
  `(let ((el ((ps:@ document get-element-by-id) ,id)))
     (when el
       (setf (ps:@ el ,attr) (funcall ,state-getter)))))

(parenscript:defpsmacro reactive-render (component-id render-fn)
  "Set up reactive rendering for a component."
  `(let ((container ((ps:@ document query-selector)
                     (+ "[data-component-id='" ,component-id "']"))))
     (when container
       (setf (ps:@ container inner-h-t-m-l) (funcall ,render-fn)))))

;;; ============================================================================
;;; WEBSOCKET REACTIVE BRIDGE
;;; ============================================================================

(defun generate-ws-client (component-id)
  "Generate WebSocket client code for real-time updates."
  (ps*
    `(let ((ws (ps:new (-Web-Socket
                        (+ (if (= (ps:@ window location protocol) "https:")
                               "wss://"
                               "ws://")
                           (ps:@ window location host) "/ws/" ,component-id)))))
       (setf (ps:@ ws onmessage)
             (lambda (event)
               (let ((data (ps:chain -j-s-o-n (parse (ps:@ event data)))))
                 (when (ps:@ data html)
                   (let ((el (ps:chain document (query-selector
                              (+ "[data-component-id='" (ps:@ data component-id) "']")))))
                     (when el
                       (setf (ps:@ el inner-h-t-m-l) (ps:@ data html))))))))
       (setf (ps:@ ws onopen)
             (lambda ()
               (ps:chain console (log "(WS :connected)"))))
       (setf (ps:@ ws onclose)
             (lambda ()
               (ps:chain console (log "(WS :disconnected)"))
               (ps:chain window (set-timeout
                (lambda ()
                  ;; Reconnect logic would go here
                  nil)
                1000))))
       ws)))

;;; ============================================================================
;;; ANAPHORIC JS HELPERS
;;; ============================================================================

(parenscript:defpsmacro aif-js (test then &optional else)
  "Anaphoric if for JavaScript - binds 'it' to test result."
  `(let ((it ,test))
     (if it ,then ,else)))

(parenscript:defpsmacro awhen-js (test &body body)
  "Anaphoric when for JavaScript."
  `(aif-js ,test (progn ,@body)))

;;; ============================================================================
;;; COMPONENT CLIENT BEHAVIOR
;;; ============================================================================

(defun component-client-script (component-id &key
                                               (on-mount nil)
                                               (on-unmount nil)
                                               (state-bindings nil))
  "Generate client-side script for a component.
   Uses ps* to interpolate runtime values into Parenscript."
  (parenscript:ps*
    `(let ((component-id ,component-id))
       ;; Register with runtime
       (ps:chain -lol-reactive (register
        component-id
        (ps:create
         :on-mount (lambda () ,@(or on-mount '(nil)))
         :on-unmount (lambda () ,@(or on-unmount '(nil))))))

       ;; Set up state bindings
       ,@(when state-bindings
           (mapcar (lambda (binding)
                     `(bind-element ,(car binding) ,(cadr binding)))
                   state-bindings))

       ;; Call mount
       (funcall (ps:getprop (ps:chain -lol-reactive components (get component-id))
                            "on-mount")))))

;;; ============================================================================
;;; HTMX-STYLE ATTRIBUTES (Alternative to full Parenscript)
;;; ============================================================================

(defun hx-dispatch (component-id action &rest args)
  "Generate data attributes for HTMX-style behavior.

   COMPONENT-ID, ACTION, and every odd-indexed entry of ARGS (the value
   slot of each key/value pair) must be a SAFE-JS-STRING-LITERAL. The
   producer's safety claim is type-enforced at entry; raw strings
   signal a TYPE-ERROR. Each value's underlying form is then HTML-
   attribute-escaped on emit so the JS-quoted body survives parsing
   inside the surrounding double-quoted attribute."
  (check-type component-id safe-js-string-literal)
  (check-type action safe-js-string-literal)
  (format nil "data-dispatch=\"~A\" data-action=\"~A\"~{~A~}"
          (lol-web/escape:escape-attribute
           (safe-js-string-literal-value component-id))
          (lol-web/escape:escape-attribute
           (safe-js-string-literal-value action))
          (iter (for (k v) on args by #'cddr)
            (check-type v safe-js-string-literal)
            ;; The key lands in attribute-NAME position (data-arg-<key>), which
            ;; escape-attribute cannot protect — a name cannot be escaped, only
            ;; rejected. Confine it to letters/digits/-/_ so it cannot break out
            ;; of the attribute or inject a second attribute.
            (let ((key-str (princ-to-string k)))
              (unless (and (plusp (length key-str))
                           (every (lambda (c)
                                    (or (and (char<= #\a c) (char<= c #\z))
                                        (and (char<= #\A c) (char<= c #\Z))
                                        (and (char<= #\0 c) (char<= c #\9))
                                        (char= c #\-)
                                        (char= c #\_)))
                                  key-str))
                (error "hx-dispatch arg key ~S contains characters unsafe for ~
                        the data-arg-<key> attribute name (letters, digits, ~
                        `-`, `_` only)." k)))
            (collect (format nil " data-arg-~A=\"~A\""
                             k
                             (lol-web/escape:escape-attribute
                              (safe-js-string-literal-value v)))))))

(defun hx-bind (component-id state-key)
  "Generate data attributes for two-way binding.

   COMPONENT-ID and STATE-KEY must both be SAFE-JS-STRING-LITERAL; the
   producer's safety claim is type-enforced at entry. Underlying forms
   are HTML-attribute-escaped on emit."
  (check-type component-id safe-js-string-literal)
  (check-type state-key safe-js-string-literal)
  (format nil "data-bind=\"~A\" data-state=\"~A\""
          (lol-web/escape:escape-attribute
           (safe-js-string-literal-value component-id))
          (lol-web/escape:escape-attribute
           (safe-js-string-literal-value state-key))))
