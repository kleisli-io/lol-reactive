(in-package :lol-web/devtools/test)
(in-suite :lol-web/devtools/test)

;;; ============================================================================
;;; Surgery mode toggle
;;; ============================================================================

(test surgery-mode-toggle
  "Surgery mode can be enabled and disabled"
  (let ((initial-state (surgery-mode-p)))
    (disable-surgery-mode)
    (is (not (surgery-mode-p)))

    (let ((lol-web/devtools::*allow-global-surgery-enable* t))
      (enable-surgery-mode))
    (is (surgery-mode-p))

    (disable-surgery-mode)
    (is (not (surgery-mode-p)))

    (if initial-state
        (let ((lol-web/devtools::*allow-global-surgery-enable* t))
          (enable-surgery-mode))
        (disable-surgery-mode))))

(test surgery-mode-p-returns-boolean
  "surgery-mode-p returns a boolean"
  (let ((result (surgery-mode-p)))
    (is (or (eq result t) (null result)))))

;;; ============================================================================
;;; Snapshot helpers exist
;;; ============================================================================

(test capture-snapshot-exists
  "capture-snapshot function exists"
  (is (fboundp 'capture-snapshot)))

(test restore-snapshot-exists
  "restore-snapshot function exists"
  (is (fboundp 'restore-snapshot)))
