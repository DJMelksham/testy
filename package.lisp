;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testy defines and creates only the testy package 
;;; All functions used from other packages apart from Common Lisp or Testy
;;; are explicity referenced in the source code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported symbols from Testy can be broadly grouped into the following 
;;; categories:
;;;
;;;  - Statistics Functions: For reporting stats about tests
;;;
;;;  - Test Macros and Functions: Responsible for creating tests
;;;
;;;  - Test Runners: Functions that run sequences of tests.
;;;
;;;  - Test Array Generators: Functions that return arrays of tests,
;;;    usually to be used to pass return values to Test Runners or
;;;    Statistics Functions.
;;;
;;;  - Testy Environment, File and Management Functions: Deal with loading,
;;;    serialisation, management of tests, setting active projects,
;;;    and testy objects/variables.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :testy
  (:use #:cl)
  (:export
   #:load-tests
   #:load-test
   #:make-test
   #:delete-all-tests-in-dir
   #:deregister-test
   #:deregister-tests
   #:destroy-test
   #:destroy-tests
   #:set-testy-active-project
   #:get-test
   #:fetch-test
   #:get-tests
   #:fetch-tests
   #:all-tags
   #:fetch-tests-from-tags
   #:get-tests-from-tags
   #:combine-test-sequences
   #:run-test
   #:run-tests
   #:run-tags
   #:tests-if
   #:tests-if-not
   #:failed-tests
   #:failing-tests
   #:passed-tests
   #:passing-tests
   #:high-verbosity
   #:verbosity-high
   #:low-verbosity
   #:verbosity-low
   #:detail-tests
   #:print-results
   #:all-tests
   #:serialise
   #:serialise-tests
   #:stat-number-tests
   #:stat-number-passed
   #:stat-number-failed
   #:stat-perc-passed
   #:stat-ratio-passed
   #:stat-perc-failed
   #:stat-ratio-failed
   #:stat-total-run-time
   #:stat-number-conditions
   #:stat-number-errors
   #:stats
   #:print-stats
   #:test
   #:test-EQ
   #:test=
   #:test-EQL
   #:test-EQUAL
   #:test-EQUALP
   #:test-NULL
   #:test-NOT-NULL
   #:test-condition
   #:test-error
   #:get-tags
   #:add-tags
   #:remove-tags
   #:get-source
   #:set-source
   #:set-sources
   #:get-expectation
   #:set-expectation
   #:set-expectations
   #:get-expected-value
   #:set-expected-value
   #:set-expected-values
   #:get-before-function-source
   #:set-before-function-source
   #:set-before-function-sources
   #:get-after-function-source
   #:set-after-function-source
   #:set-after-function-sources
   #:get-name
   #:set-name
   #:get-names
   #:get-description
   #:set-description
   #:get-run-value
   #:get-result
   #:get-results))
