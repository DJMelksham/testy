;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun run-test (test &optional (re-evaluate 'auto))
    
    (let ((test-time-start 0.0)
	  (test-time-stop 0.0))
      (declare (dynamic-extent test-time-start test-time-stop))
      
      ;; Determine whether we need to re-evaluate the before function with each run
      (if (eq (before-function-compiled-form test) *test-empty-function*)
	  (setf (before-function-run-status test) T)
	  (progn
	    (cond ((eq re-evaluate 'auto) 
		   (if (eq (re-evaluate test) t)
		       (setf (before-function-compiled-form test) (nw-eval? (null (type-of-test test)) (before-function-source test)))))
		  ((eq re-evaluate nil) nil)
		  ((eq re-evaluate T)
		   (setf (before-function-compiled-form test) (nw-eval? (null (type-of-test test)) (before-function-source test))))
		  (t nil))

	    (multiple-value-bind (value status)
		(ignore-errors (apply (before-function-compiled-form test) nil))
	      (if (typep status 'condition)
		  (setf (before-function-run-status test) status)
		  (setf (before-function-run-status test) value)))))
      
      
      ;; Determine whether we need to re-evaluate the actual test function with each run
      
      (cond ((eq re-evaluate 'auto) 
	     (if (eq (re-evaluate test) t)
		 (setf (compiled-form test) (nw-eval? (null (type-of-test test)) (source test)))))
	    ((eq re-evaluate nil) nil)
	    ((eq re-evaluate T)
	     (setf (compiled-form test) (nw-eval? (null (type-of-test test)) (source test))))
	    (t nil))

      (setf test-time-start (get-internal-real-time))
      
      (multiple-value-bind (value status)
	  (ignore-errors (apply (compiled-form test) nil))
	(if (typep status 'condition)
	    (setf (run-value test) status)
	    (setf (run-value test) value)))

      (setf test-time-stop (get-internal-real-time))
      
      (setf (result test)
	    (funcall (gethash (expectation test) *expectation-table*)
		   (expected-value test) (run-value test)))
      
      ;; Determine whether we need to re-evaluate the after function with each run
      (if (eq (after-function-compiled-form test) *test-empty-function*)
	  (setf (after-function-run-status test) T)
	  (progn
	    (cond ((eq re-evaluate 'auto) 
		   (if (eq (re-evaluate test) t)
		       (setf (after-function-compiled-form test) (nw-eval? (null (type-of-test test)) (after-function-source test)))))
		  ((eq re-evaluate nil) nil)
		  ((eq re-evaluate T)
		   (setf (after-function-compiled-form test) (nw-eval? (null (type-of-test test)) (after-function-source test))))
		  (t nil))
	    
	    (multiple-value-bind (value status)
		(ignore-errors (apply (after-function-compiled-form test) nil))
	      (if (typep status 'condition)
		  (setf (after-function-run-status test) status)
		  (setf (after-function-run-status test) value)))))
	    
	    (setf (run-time test) (/ (- test-time-stop test-time-start)
				     1000.0))
      (result test)))