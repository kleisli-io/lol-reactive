(in-package :lol-web/html/test)
(in-suite :lol-web/html/test)

(test reactive-runtime-js-exists
  "reactive-runtime-js function exists"
  (is (fboundp 'reactive-runtime-js)))

(test reactive-runtime-js-generates-code
  "reactive-runtime-js generates JavaScript tagged as SAFE-HTML-STRING"
  (let ((js (reactive-runtime-js)))
    (is (safe-html-string-p js))
    (is (> (length (safe-html-string-value js)) 100))))
