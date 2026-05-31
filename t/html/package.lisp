(defpackage :lol-web/html/test
  (:use :cl :lol-web/html)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/html/test)
