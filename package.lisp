;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testy itself defines and creates only the testy package and imports 
;;; symbols only from Common-Lisp.
;;; All functions used from other packages are explicity referenced in
;;; the source code.
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
;;;  - Context Macros and Functions: Responsible for creating contexts
;;;    ...that is to say, objects that automate some of the environment,
;;;    setup, repetition and environment destruction when creating tests.
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
;;;  - Everything else: Because I can't categorise everything, you're being
;;;    unreasonable.
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
   #:get-tag
   #:fetch-tag
   #:fetch-tests-from-tags
   #:get-tests-from-tags
   #:combine-test-sequences
   #:load-context
   #:get-context
   #:fetch-context
   #:make-context
   #:with-context
   #:deregister-context
   #:run-test
   #:run-tests
   #:run-tags
   #:tests-if
   #:tests-if-not
   #:map-tests
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
   #:test-error))
