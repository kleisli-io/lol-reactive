(defpackage :lol-web/server/test
  (:use :cl :lol-web/server)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/server/test)
