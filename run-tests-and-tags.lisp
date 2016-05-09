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

(defun run-tests (&optional (test-sequence (all-tests)) (re-evaluate 'auto))
  (let ((result t))
	(loop for tests across test-sequence
	   do (if (not (funcall #'run-test tests re-evaluate))
		  (setf result nil)))
	result))

(defun run-tags (tags)
  (run-tests (fetch-tests-from-tags tags)))
