;;;; LOL-REACTIVE Test Suite - Surgery Mode
;;;;
;;;; Tests for surgery/x-ray mode, snapshots, and live inspection.

(in-package :lol-reactive.tests)
(in-suite :surgery)

;;; ============================================================================
;;; SURGERY MODE TOGGLE TESTS
;;; ============================================================================

(test surgery-mode-toggle
  "Surgery mode can be enabled and disabled"
  (let ((initial-state (lol-reactive:surgery-mode-p)))
    ;; Ensure known state
    (lol-reactive:disable-surgery-mode)
    (is (not (lol-reactive:surgery-mode-p)))

    ;; Enable
    (lol-reactive:enable-surgery-mode)
    (is (lol-reactive:surgery-mode-p))

    ;; Disable
    (lol-reactive:disable-surgery-mode)
    (is (not (lol-reactive:surgery-mode-p)))

    ;; Restore initial state
    (if initial-state
        (lol-reactive:enable-surgery-mode)
        (lol-reactive:disable-surgery-mode))))

(test surgery-mode-p-returns-boolean
  "surgery-mode-p returns a boolean"
  (let ((result (lol-reactive:surgery-mode-p)))
    (is (or (eq result t) (null result)))))

;;; ============================================================================
;;; SNAPSHOT TESTS
;;; ============================================================================

(test capture-snapshot-exists
  "capture-snapshot function exists"
  (is (fboundp 'lol-reactive:capture-snapshot)))

(test restore-snapshot-exists
  "restore-snapshot function exists"
  (is (fboundp 'lol-reactive:restore-snapshot)))
