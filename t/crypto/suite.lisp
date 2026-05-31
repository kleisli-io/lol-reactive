(in-package :lol-web/crypto/test)

(def-suite :lol-web/crypto/test
  :description "Tests for :lol-web/crypto.")

(defun run-tests ()
  "Run :lol-web/crypto/test suite. Signals an error on any failure so
   the buildLisp test phase fails the derivation."
  (unless (fiveam:run! :lol-web/crypto/test)
    (error "lol-web/crypto/test: at least one assertion failed"))
  t)
