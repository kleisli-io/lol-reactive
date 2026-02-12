;;;; LOL-REACTIVE Test Suite - Package Definition
;;;;
;;;; FiveAM-based test suite for lol-reactive framework.

(defpackage :lol-reactive.tests
  (:use :cl)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:is-true
                #:is-false
                #:signals
                #:run!
                #:explain!)
  (:export
   #:run-all-tests
   #:run-suite
   #:test-summary))

(in-package :lol-reactive.tests)
