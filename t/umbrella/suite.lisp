(in-package :lol-web/test)

(def-suite :lol-web/test
  :description "Umbrella tests — :lol-reactive shim parity with :lol-web.")

(defun run-all-tests ()
  "Run every per-sub-system suite plus the umbrella suite. Signals an
   error if any suite has at least one failed assertion so the buildLisp
   test phase fails the umbrella derivation."
  (let ((failed-suites '()))
    (dolist (suite '(:lol-web/escape/test
                     :lol-web/core/test
                     :lol-web/css/test
                     :lol-web/rendering/test
                     :lol-web/parenscript/test
                     :lol-web/html/test
                     :lol-web/server/test
                     :lol-web/jschema/test
                     :lol-web/extractors/test
                     :lol-web/openapi/test
                     :lol-web/htmx/test
                     :lol-web/realtime/test
                     :lol-web/realtime-htmx/test
                     :lol-web/resources/test
                     :lol-web/forms/test
                     :lol-web/wizards/test
                     :lol-web/devtools/test
                     :lol-web/fullstack/test
                     :lol-web/optimization/test
                     :lol-web/client-runtime/test
                     :lol-web/test))
      (unless (fiveam:run! suite)
        (push suite failed-suites)))
    (when failed-suites
      (error "lol-web suites with failures: ~{~A~^, ~}"
             (nreverse failed-suites)))
    t))
