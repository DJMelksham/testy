(in-package :testy)

(defun test-cond (test-identifier)
  (cond ((typep test-identifier 'test)
	 test-identifier)
	((integerp test-identifier) 
	 (gethash test-identifier *test-ids*))
	((or (stringp test-identifier) (symbolp test-identifier)) 
	 (gethash (string-upcase test-identifier) *test-names*))
	(t nil)))

(defun get-test (test-identifier)
  (test-cond test-identifier))

(defun fetch-test (test-identifier)
  (test-cond test-identifier))

(defun run-test (test-identifier &key (re-evaluate 'auto))
 
  (macrolet ((nw-eval? (&rest body)
	       `(if (not (eq (type-of-test test) 'nw))
		    (eval ,@body)
		    (handler-bind
			((style-warning 
			  #'(lambda (w) 
			      (when (undefined-warning-p w)
				(invoke-restart 'muffle-warning)))))
		      (eval ,@body)))))
 
    (let ((test (test-cond test-identifier))
	  (test-time-start 0)
	  (test-time-stop 0))
      
      (setf test-time-start (get-internal-real-time))
      
      ;; Determine whether we need to re-evaluate the before function with each run
      
      (cond ((eq re-evaluate 'auto) 
	     (if (eq (re-evaluate test) t)
		 (setf (before-function-compiled-form test) (nw-eval? (before-function-source test)))))
	    ((eq re-evaluate nil) nil)
	    ((eq re-evaluate T)
	     (setf (before-function-compiled-form test) (nw-eval? (before-function-source test))))
	    (t nil))
      
      (apply (before-function-compiled-form test) nil)
      
      ;; Determine whether we need to re-evaluate the actual test function with each run
      
      (cond ((eq re-evaluate 'auto) 
	     (if (eq (re-evaluate test) t)
		 (setf (compiled-form test) (nw-eval? (source test)))))
	    ((eq re-evaluate nil) nil)
	    ((eq re-evaluate T)
	     (setf (compiled-form test) (nw-eval? (source test))))
	    (t nil))
      
      (multiple-value-bind (value status)
	  (ignore-errors (apply (compiled-form test) nil))
	(if (typep status 'condition)
	    (setf (run-value test) status)
	    (setf (run-value test) value)))
      
      (setf (result test)
	    (apply (gethash (expectation test) (expectation-table test))
		   (list (expected-value test) (run-value test))))
      
      ;; Determine whether we need to re-evaluate the after function with each run
      
      (cond ((eq re-evaluate 'auto) 
	     (if (eq (re-evaluate test) t)
		 (setf (after-function-compiled-form test) (nw-eval? (after-function-source test)))))
	    ((eq re-evaluate nil) nil)
	    ((eq re-evaluate T)
	     (setf (after-function-compiled-form test) (nw-eval? (after-function-source test))))
	    (t nil))
      
      (apply (after-function-compiled-form test) nil)
      
      (setf test-time-stop (get-internal-real-time))
      
      (setf (run-time test) (/ (- test-time-stop test-time-start)
			       1000.0))
      test)))

(defun run-test-re-evaluate (test)
  (run-test test :re-evaluate T))

(defun run-test-no-evaluate (test)
  (run-test test :re-evaluate NIL))
