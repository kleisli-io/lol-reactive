(defpackage :lol-web/fullstack/test
  (:use :cl :lol-web/fullstack :lol-web/core)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/fullstack/test)
