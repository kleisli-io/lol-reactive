;;;; LOL-REACTIVE Test Suite - Suite Definitions
;;;;
;;;; Defines the test suite hierarchy and run functions.

(in-package :lol-reactive.tests)

;;; ============================================================================
;;; SUITE HIERARCHY
;;; ============================================================================

(def-suite :lol-reactive.tests
  :description "Root test suite for lol-reactive framework")

;; Child suites for each module
(def-suite :signals :in :lol-reactive.tests
  :description "Tests for reactive signal primitives")

(def-suite :components :in :lol-reactive.tests
  :description "Tests for component system")

(def-suite :surgery :in :lol-reactive.tests
  :description "Tests for surgery/x-ray mode")

(def-suite :dom-diff :in :lol-reactive.tests
  :description "Tests for DOM diffing algorithm")

(def-suite :keyed-list :in :lol-reactive.tests
  :description "Tests for keyed list reconciliation")

(def-suite :wizards :in :lol-reactive.tests
  :description "Tests for wizard framework")

(def-suite :htmx :in :lol-reactive.tests
  :description "Tests for HTMX integration")

(def-suite :server :in :lol-reactive.tests
  :description "Tests for server routing and security")

(def-suite :parenscript :in :lol-reactive.tests
  :description "Tests for Parenscript utilities")

(def-suite :regression :in :lol-reactive.tests
  :description "Regression tests for fixed bugs")

;;; ============================================================================
;;; RUN FUNCTIONS
;;; ============================================================================

(defun run-all-tests ()
  "Run all lol-reactive tests and return results."
  (run! :lol-reactive.tests))

(defun run-suite (suite-name)
  "Run a specific test suite by keyword name.
   Example: (run-suite :signals)"
  (run! suite-name))

(defun test-summary ()
  "Print a summary of test suites."
  (format t "~&lol-reactive Test Suites:~%")
  (format t "  :lol-reactive.tests  - All tests~%")
  (format t "  :signals             - Reactive signals~%")
  (format t "  :components          - Component system~%")
  (format t "  :surgery             - Surgery/X-ray mode~%")
  (format t "  :dom-diff            - DOM diffing~%")
  (format t "  :keyed-list          - Keyed lists~%")
  (format t "  :wizards             - Wizard framework~%")
  (format t "  :htmx                - HTMX integration~%")
  (format t "  :server              - Server routing~%")
  (format t "  :parenscript         - Parenscript utils~%")
  (format t "  :regression          - Bug regression tests~%")
  (format t "~%Usage: (run-suite :signals)~%"))
