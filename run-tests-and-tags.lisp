;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun run-tests (&optional (test-sequence (all-tests)) (re-evaluate 'auto) (stop-on-fail nil))
  (let ((result t)
	(indv-test-status t))
    (if stop-on-fail
	(loop for tests across test-sequence
	   while (setf indv-test-status (funcall #'run-test tests re-evaluate))
	   finally (setf result indv-test-status))
	(loop for tests across test-sequence
	   do (if (not (setf indv-test-status (funcall #'run-test tests re-evaluate)))
		  (setf result indv-test-status))))
	result))

(defun run-tags (tags)
  (run-test (fetch-tests-from-tags tags))) 
