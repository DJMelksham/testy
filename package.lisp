;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testy defines and creates the TESTY package.
;;; 
;;; All functions used from other packages apart from Common Lisp or Testy
;;; are explicity referenced in the source code.
;;;
;;; Exported symbols are grouped based upon where they are
;;; defined in the system's code heirachy, which is viewable in the asdf
;;; system definition.  The astute observer will notice many functions
;;; have synonymous names exported: that is to say, several functions
;;; do identical things.  This is for those users, like me, who have trouble
;;; remembering particular verb/subject orders or labels, but know what they
;;; want.  Since each function name explicitly says what it is doing,
;;; this shouldn't come at any cost as to how expressive or communicable
;;; code actually is.
;;;
;;; Although I have defined naive accessors for every slot in a test object,
;;; I have chosen not to export them. Users looking for that level of access
;;; may study the test object definition code, but the naive accessors
;;; do not provide automatic book-keeping, convenience, validation, updating
;;; and consistency checks provided by the exported functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  INDEX
;;;
;;;  0. Set number of cores for multi-threaded code
;;;
;;;  1. Set the active project
;;;
;;;  2. Set the verbosity of printed test objects
;;;
;;;  3. Retrieve individual tests and/or sets of tests
;;;
;;;  4. Test registration and de-registration
;;;
;;;  5. The make-test function: Does what it says on the box
;;;
;;;  6. Macros to wrap around make-test to allow test production
;;;     to be irresponsibly easy during interactive development
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;; 0. Set number of cores for multi-thread activities ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   #:set-number-of-cores
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;; 1. Set the active project ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   #:set-testy-active-project
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Set the verbosity of printed test objects ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

   #:high-verbosity
   #:verbosity-high
   #:low-verbosity
   #:verbosity-low
   #:verbosity

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Retrieve individual tests and/or sets of tests ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   #:get-test
   #:fetch-test
   #:all-tests
   #:all-tags
   #:get-tests
   #:fetch-tests
   #:fetch-tests-from-tags
   #:get-tests-from-tags
   #:combine-test-sequences
   #:tests-if
   #:tests-if-not
   #:failed-tests
   #:failing-tests
   #:passed-tests
   #:passing-tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Test registration and de-registration ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   #:register-test
   #:deregister-test
   #:deregister-tests
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. The make-test function: Does what it says on the box ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   #:create-test-object
   #:make-test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. Macros responsible for interactive test authorship ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. Load (read from disk, create and register test objects), ;
;;;    serialise (save test objects to disk), and               ;
;;;    delete (remove from disk and remove from Testy registry) ;
;;;    tests.                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   #:serialise
   #:serialise-tests
   #:load-test
   #:load-tests
   #:destroy-test
   #:destroy-tests
   #:delete-all-tests-in-dir
   #:delete-test-from-disk

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7. Run-test: The fundamental test running function ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
   #:run-test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8. Run sets of tests and tests categorised by tags ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   #:run-tests
   #:run-tags

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;; 9. Statistics functions - produce summary information about sets of tests ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10. Additional reporting and presentation functions ;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   #:print-stats
   #:detail-tests
   #:print-results

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 11. Convenient test accessors: programatically access and alter properties ;
;;;     of defined tests.                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
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
