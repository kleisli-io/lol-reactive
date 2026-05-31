(defpackage :lol-web/devtools/test
  (:use :cl :lol-web/devtools)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/devtools/test)
