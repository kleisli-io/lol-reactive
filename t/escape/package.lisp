(defpackage :lol-web/escape/test
  (:use :cl :lol-web/escape)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/escape/test)
