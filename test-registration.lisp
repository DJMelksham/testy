;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun register-test (test)
  (if (or (not (typep test 'test))
	  (gethash (name test) *test-names*))
	  (return-from register-test nil))

  (setf (gethash (name test) *test-names*) test)
  (loop for tag in (tags test)
     do (hash-ext-array-insert tag test *test-tags*))

  T)

(defun deregister-test (identifier)
  (let ((test (get-test identifier)))
    (if (null test)
	(return-from deregister-test nil))

    (remhash (name test) *test-names*)
    (loop
       for tag in (tags test)
       do (hash-ext-array-remove tag test *test-tags*))
    T))

(defun deregister-tests (&optional (test-sequence (all-tests)))
  (loop
     for test across test-sequence
       for i = 1 then (incf i)
     do (deregister-test test)
       finally (return i)))
