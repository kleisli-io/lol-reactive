(defpackage :lol-web/forms/test
  (:use :cl :lol-web/forms)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/forms/test)
