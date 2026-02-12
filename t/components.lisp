;;;; LOL-REACTIVE Test Suite - Components
;;;;
;;;; Tests for defcomponent, component registration, and HOCs.

(in-package :lol-reactive.tests)
(in-suite :components)

;;; ============================================================================
;;; COMPONENT REGISTRATION TESTS
;;; ============================================================================

(test component-register-and-find
  "Components can be registered and found by ID"
  (let ((test-component (lambda () "test")))
    (lol-reactive:register-component "test-comp-1" test-component)
    (is (eq test-component (lol-reactive:find-component "test-comp-1")))
    (lol-reactive::unregister-component "test-comp-1")
    (is (null (lol-reactive:find-component "test-comp-1")))))

(test component-find-nonexistent
  "Finding nonexistent component returns NIL"
  (is (null (lol-reactive:find-component "nonexistent-comp-xyz"))))

;;; ============================================================================
;;; DEFCOMPONENT TESTS (would need to expand the macro for full testing)
;;; ============================================================================

;; Note: Full defcomponent tests would require macro expansion
;; and verification of the generated functions. These are placeholder tests.

(test defcomponent-creates-function
  "defcomponent macro is available"
  (is (fboundp 'lol-reactive:defcomponent)))
