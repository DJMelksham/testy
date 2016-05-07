;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testy defines and creates the TESTY package and assumes one name-space
;;; for each system it is applied to.
;;; 
;;; All functions used from other packages apart from Common Lisp or Testy
;;; are explicity referenced in the source code.
;;;
;;; Exported symbols from Testy are grouped based upon where they are
;;; defined in the system's source code heirachy, viewable in the asdf
;;; system definition.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  INDEX
;;;
;;;  1. Set an active project
;;;
;;;  2. Set the verbosity of printed test objects
;;;
;;;  3. Retrieve individual tests and/or sets of tests
;;;
;;;  4. Manage the registration of tests within Testy's internal register
;;;
;;;  5. The make-test function: Does what it says on the box
;;;
;;;  6. Macros wrap around make-test to allow test production
;;;     to be irrespondibly easy during interactive development
;;;
;;;  7. Control explicit loading/saving of serialised tests as
;;;     well as their deletion
;;;
;;;  8. Run-test: the function responsible for individual test operation
;;;
;;;  9. Run-tests and run-tags: Functions responsible for running sets
;;;     of tests or tests via specific tags
;;;
;;; 10. Statistics functions - report information on sequences of tests
;;;
;;; 11. Report and present test results in specific forms
;;;
;;; 12. Convenient test accessors: programatically access and alter properties
;;;     of defined tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :testy
  (:use #:cl)
  (:export
   #:set-testy-active-project
   #:load-tests
   #:load-test
   #:make-test
   #:delete-all-tests-in-dir
   #:register-test
   #:deregister-test
   #:deregister-tests
   #:destroy-test
   #:destroy-tests
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
