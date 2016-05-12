;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run-tests and run-tags are two simple functions which map the run-test
;;; function over an array of tests.
;;;
;;; The run-tests function expects to be fed this array.
;;;
;;; The run-tags function, however, takes an array of strings representing tags,
;;; and obtains the test-array itself by generating it from those tests 
;;; identified by those tags.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.  This function runs multiple tests contained in an array ;
;;;     of tests                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-tests (&optional (test-sequence (all-tests)) (re-evaluate 'auto))
  "Run an array of test objects"

  ;; "Should we multi-thread this operation?
  (if (or (not *multi-thread?*)
	  (= *number-of-cores* 1))
      ;; No, we really shouldn't multi-thread this operation...
      (return-from run-tests
	(let ((result t)) ; is returned and gets set to nil if a test fails
	  (loop for tests across test-sequence
	     do (if (not (funcall #'run-test tests re-evaluate))
		    (setf result nil)))
	  result)))

  ;; Yeah, i think we'll multi-thread this
  ;; Start doing some heavy lifing if we want to multi-thread
  ;; in a lockless fashion (which we do).
  
  (let* ((tests-for-threads (make-array *number-of-cores*))
	 (threads nil))
    
    ;; Build test-arrays for all threads
    (loop
       for i from 0 to (- *number-of-cores* 1)
       do (setf (svref tests-for-threads i) (make-array 0 :element-type 'test :adjustable t :fill-pointer 0)))
    
    ;; Populate test-arrays for all threads
    (loop
       for test across test-sequence
       for i = 1 then (incf i)
       for j = (- *number-of-cores* i)
       do (vector-push-extend test (svref tests-for-threads j)) 
       do (if (= i *number-of-cores*)
	      (setf i 0)))
    
    ;; Make threads and run the tests
    
    (loop
       for tests across tests-for-threads
       do (push (sb-thread:make-thread
		 (lambda (tests)
		   (loop
		      for test across tests
		      for j = 0 then (incf j)
		      do (run-test test re-evaluate)))
		 :arguments tests)
		threads))
    
    ;; Ensure all threads have finished work
    
    (loop for thread in threads
       do (sb-thread:join-thread thread))
    
    ;; Return T or NIL depending on whether all tests passed
    (let ((result t))
      (loop for test across test-sequence
	 do (if (not (result test))
		(progn
		  (setf result nil)
		  (return nil))))
      result)))

(defun run-tags (tags &optional (re-evaluate 'auto))
  "Run the tests contained in various tags"
      (run-tests (fetch-tests-from-tags tags) re-evaluate))
