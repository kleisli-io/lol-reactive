(defpackage :lol-web/parenscript/test
  (:use :cl :lol-web/parenscript)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/parenscript/test)
