;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun-with-synonyms (low-verbosity verbosity-low) ()
  (setf *print-verbosity* 'low))

(defun-with-synonyms (high-verbosity verbosity-high) ()
  (setf *print-verbosity* 'high))

(defun verbosity (value)
  "Manually set the verbosity level of printed test objects"
  (if (and (not (eq value 'high))
	   (not (eq value 'low)))
      (return-from verbosity nil)
      (setf *print-verbosity* value)))
