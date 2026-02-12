;;;; LOL-REACTIVE Test Suite - Wizards
;;;;
;;;; Tests for multi-step wizard framework.

(in-package :lol-reactive.tests)
(in-suite :wizards)

;;; ============================================================================
;;; WIZARD REGISTRATION TESTS
;;; ============================================================================

(test register-wizard-exists
  "register-wizard function exists"
  (is (fboundp 'lol-reactive:register-wizard)))

(test list-wizards-exists
  "list-wizards function exists"
  (is (fboundp 'lol-reactive:list-wizards)))

(test get-wizard-spec-exists
  "get-wizard-spec function exists"
  (is (fboundp 'lol-reactive:get-wizard-spec)))

;;; ============================================================================
;;; WIZARD SESSION TESTS
;;; ============================================================================

(test start-wizard-exists
  "start-wizard function exists"
  (is (fboundp 'lol-reactive:start-wizard)))

(test get-wizard-session-exists
  "get-wizard-session function exists"
  (is (fboundp 'lol-reactive:get-wizard-session)))

(test process-wizard-submission-exists
  "process-wizard-submission function exists"
  (is (fboundp 'lol-reactive:process-wizard-submission)))

;;; ============================================================================
;;; WIZARD RENDERING TESTS
;;; ============================================================================

(test render-wizard-step-exists
  "render-wizard-step function exists"
  (is (fboundp 'lol-reactive:render-wizard-step)))

(test render-wizard-complete-exists
  "render-wizard-complete function exists"
  (is (fboundp 'lol-reactive:render-wizard-complete)))

;;; ============================================================================
;;; WIZARD FORM FIELD HELPERS
;;; ============================================================================

(test wizard-text-field-exists
  "wizard-text-field function exists"
  (is (fboundp 'lol-reactive:wizard-text-field)))

(test wizard-select-field-exists
  "wizard-select-field function exists"
  (is (fboundp 'lol-reactive:wizard-select-field)))

(test wizard-radio-group-exists
  "wizard-radio-group function exists"
  (is (fboundp 'lol-reactive:wizard-radio-group)))

;;; ============================================================================
;;; WIZARD FIELD RENDERING
;;; ============================================================================

(test wizard-text-field-renders-html
  "wizard-text-field produces HTML output"
  (let ((html (lol-reactive:wizard-text-field "username" :label "Username")))
    (is (stringp html))
    (is (search "username" html))
    (is (search "input" html :test #'char-equal))))

(test wizard-select-field-renders-options
  "wizard-select-field includes options"
  (let ((html (lol-reactive:wizard-select-field "country"
                '(("us" . "United States") ("uk" . "United Kingdom"))
                :label "Country")))
    (is (stringp html))
    (is (search "select" html :test #'char-equal))
    (is (search "option" html :test #'char-equal))))
