;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run-test is a fundamental function responsible for running functions
;;; contained in a test-object, and then setting the state of that test object
;;; to properly reflect the requisite outcomes of test operation.
;;;
;;; Or, more simply, it runs the tests.
;;;
;;; While Testy is designed for interactive development and definitions
;;; of tests concurrently with operating on code at the REPL, the 
;;; design of the tests and the test-runner is designed to eventually
;;; support automation and, aspirationally, real-time feedback on 
;;; test status.  This means that Testy tests do not drop into the
;;; debugger upon failure of the code or raising of a condition
;;; therein, though they will register that a condition 
;;; was raised.
;;;
;;; The test runner also attempts to be relatively optimised, and
;;; so creates as little overhead as possible in the running of each
;;; individual test, in an attempt to be able to scale testing
;;; as much as possible while retaining total test set feedback as
;;; quickly as possible.
;;;
;;; Thus by default, tests do not recompile their code before each
;;; run, though options exist both within tests, and within the test
;;; runner itself, to turn the option of recompilation on.
;;;
;;; Additionally, time statistics produced by the test runner
;;; attempt to represent the actual time of operating the test
;;; portion of the code, and not the overhead of the test runner.
;;; Hopefully, because the overhead of the test runner is so low,
;;; the wall-clock time of running tests should be practically identical
;;; to the speed reported back for the total run-times of tests
;;; themselves.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun run-test (test &optional (re-evaluate 'auto))
  "Run a Testy test-object"
  
  ;;Establish some local variables to hold the point in time when the test starts
  ;;and the point in time when the test stops.  Will be used later to determine
  ;;the actual run time for the main function of the test object.
  
    (let ((test-time-start 0)
	  (test-time-stop 0))
      (declare (dynamic-extent test-time-start test-time-stop)
	       (type integer test-time-start test-time-stop))
      
      ;; Determine whether we need to re-evaluate the before-function with each run
      ;; and whether the before-function needs to be called at all.
      (if (eq (before-function-compiled-form test) *test-empty-function*)
	  (setf (before-function-run-status test) T)
	  (progn
	    (cond ((eq re-evaluate 'auto) 
		   (if (eq (re-evaluate test) t)
		       (setf (before-function-compiled-form test) (eval (before-function-source test)))))
		  ((eq re-evaluate nil) nil)
		  ((eq re-evaluate T)
		   (setf (before-function-compiled-form test) (eval (before-function-source test))))
		  (t nil))
      
	    ;; Run the before function
	    (multiple-value-bind (value status)
		(ignore-errors (funcall (the function (before-function-compiled-form test))))
	      (if (typep status 'condition)
		  (setf (before-function-run-status test) status)
		  (setf (before-function-run-status test) value)))))
      
      
      ;; Determine whether we need to re-evaluate the actual test function with each run
      
      (cond ((eq re-evaluate 'auto) 
	     (if (eq (re-evaluate test) t)
		 (setf (compiled-form test) (eval (source test)))))
	    ((eq re-evaluate nil) nil)
	    ((eq re-evaluate T)
	     (setf (compiled-form test) (eval (source test))))
	    (t nil))
      
      ;;start the test timer
      (setf test-time-start (get-internal-real-time))

      ;; run the actual test function      
      (multiple-value-bind (value status)
	  (ignore-errors (funcall (the function (compiled-form test))))
	(if (typep status 'condition)
	    (setf (run-value test) status)
	    (setf (run-value test) value)))

      (setf
       ;;set the internal stop time of the test run
       test-time-stop (get-internal-real-time)
       ;;run and set the code that checks the outcome of the test
       (result test) (funcall (the function (expectation-function test))
			      (expected-value test) (run-value test)))
      
      ;; Determine whether we need to re-evaluate the after function with each run
      ;; and whether the before-function needs to be called at all.
      (if (eq (after-function-compiled-form test) *test-empty-function*)
	  (setf (after-function-run-status test) T)
	  (progn
	    (cond ((eq re-evaluate 'auto) 
		   (if (eq (re-evaluate test) t)
		       (setf (after-function-compiled-form test) (eval (after-function-source test)))))
		  ((eq re-evaluate nil) nil)
		  ((eq re-evaluate T)
		   (setf (after-function-compiled-form test) (eval (after-function-source test))))
		  (t nil))
	    ;;Run the after function
	    (multiple-value-bind (value status)
		(ignore-errors (funcall (the function (after-function-compiled-form test))))
	      (if (typep status 'condition)
		  (setf (after-function-run-status test) status)
		  (setf (after-function-run-status test) value)))))

      ;; Set the run-time for the actual test and return the result of the test run
	    (setf (run-time test) (* (- test-time-stop test-time-start)
				     0.001))
      (result test)))
