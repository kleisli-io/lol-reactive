(defpackage :lol-web/crypto/test
  (:use :cl :lol-web/crypto)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals)
  (:export
   #:run-tests))

(in-package :lol-web/crypto/test)
