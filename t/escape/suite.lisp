(in-package :lol-web/escape/test)

(def-suite :lol-web/escape/test
  :description "Tests for :lol-web/escape (HTML/attribute escape, URL guard with C0 strip, scheme allowlist).")

(defun run-tests ()
  (unless (fiveam:run! :lol-web/escape/test)
    (error "lol-web/escape/test: at least one assertion failed"))
  t)
