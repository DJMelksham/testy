;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The statistic functions report basic information about arrays of tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun stat-number-tests (&optional (test-sequence (all-tests)))
  "Return the number of tests in an array of tests"
  (length test-sequence))

(defun stat-number-passed (&optional (test-sequence (all-tests)))
  "Return the number of passed tests in an array of tests"
  (let ((passed 0)
	(failed 0))
    (loop for test across test-sequence
	 do (if (result test) 
		(incf passed)
		(incf failed)))
    (values passed failed)))

(defun stat-number-failed (&optional (test-sequence (all-tests)))
  "Return the number of failed tests within an array of tests"
  (let ((passed 0)
	(failed 0))
    (loop for test across test-sequence
	 do (if (result test) 
		(incf passed)
		(incf failed)))
    (values failed passed)))

(defun stat-perc-passed (&optional (test-sequence (all-tests)))
  "Return the percentage of passed tests in an array of tests"
  (multiple-value-bind (num-passed num-failed)
    (stat-number-passed test-sequence)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (values 0 0))
	  ((zerop num-passed)
	   (values 0.00 100.00))
	  ((zerop num-failed)
	   (values 100.00 0.00))
	  (t (values (float (* 100 (/ num-passed (+ num-passed num-failed))))
		     (float (* 100 (/ num-failed (+ num-passed num-failed)))))))))

(defun stat-ratio-passed (&optional (test-sequence (all-tests)))
  "Returns the ratio of passed tests in an array of tests"
  (multiple-value-bind (num-passed num-failed)
      (stat-number-passed test-sequence)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (values 0 0))
	  ((zerop num-passed)
	   (values 0 num-failed))
	  ((zerop num-failed)
	   (values num-passed 0))
	  (t (values (/ num-passed (+ num-passed num-failed))
		     (/ num-failed (+ num-passed num-failed)))))))

(defun stat-perc-failed (&optional (test-sequence (all-tests)))
  "Returns the percentage of failed tests in an array of tests"
  (multiple-value-bind (num-passed num-failed)
    (stat-number-passed test-sequence)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (values 0.00 0.00))
	  ((zerop num-passed)
	   (values 100.00 0.00))
	  ((zerop num-failed)
	   (values 0.00 100.00))
	  (t (values (float (* 100 (/ num-failed (+ num-passed num-failed))))
		     (float (* 100 (/ num-passed (+ num-passed num-failed)))))))))

(defun stat-ratio-failed (&optional (test-sequence (all-tests)))
  "Returns the ratio of failed tests in an array of tests"
  (multiple-value-bind (num-passed num-failed)
      (stat-number-passed test-sequence)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (values 0 0))
	  ((zerop num-passed)
	   (values 1/1 0))
	  ((zerop num-failed)
	   (values 0 1/1))
	  (t (values (/ num-failed (+ num-passed num-failed))
		     (/ num-passed (+ num-passed num-failed)))))))

(defun stat-total-run-time (&optional (test-sequence (all-tests)))
  "Approximate total run time of the main test code (not how long it takes to run the tests themselves: attempts to ignore testing overhead)"
  (loop for test across test-sequence
     sum (run-time test)))

(defun stat-number-conditions (&optional (test-sequence (all-tests)))
  "Return the number of conditions in an array of tests"
  (let ((conditions 0)
	(non-conditions 0))
    (loop for test across test-sequence
	 do (if (or (typep (run-value test) 'condition) 
		    (and (or (equal (expectation test) "CONDITION")
			     (equal (expectation test) "ERROR"))
			 (eq (result test) T)))
		(incf conditions)
		(incf non-conditions)))
    (values conditions non-conditions)))

(defun stat-number-errors (&optional (test-sequence (all-tests)))
  "Return the number of error conditions raised by an array of tests"
  (let ((conditions 0)
	(non-conditions 0))
    (loop for test across test-sequence
       do (if (or (typep (run-value test) 'condition) 
		  (and (equal (expectation test) "ERROR")
		       (eq (result test) T)))
		(incf conditions)
		(incf non-conditions)))
    (values conditions non-conditions)))

(defun stats (&optional (test-sequence (all-tests)))
  "Returns a list with multiple statistics defined therein.  Primarily passed to reporter functions."
 (let ((num-tests 0)
       (num-passed 0)
       (num-failed 0)
       (perc-passed 0.0)
       (perc-failed 0.0)
       (ratio-passed 0)
       (ratio-failed 0)
       (num-errors 0)
       (num-conditions 0)
       (total-run-time 0.0))
   (setf num-tests (length test-sequence))
   (multiple-value-setq (num-passed num-failed)
     (stat-number-passed test-sequence))
   (multiple-value-setq (perc-passed perc-failed)
     (stat-perc-passed test-sequence))
   (multiple-value-setq (ratio-passed ratio-failed)
     (stat-ratio-passed test-sequence))
   (setf num-errors (stat-number-errors test-sequence))
   (setf num-conditions (stat-number-conditions test-sequence))
   (setf total-run-time (stat-total-run-time test-sequence))
				     
       (list 
	num-tests 
	 num-passed num-failed 
	 perc-passed perc-failed 
	 ratio-passed ratio-failed 
	 num-errors 
	 num-conditions
	 total-run-time)))
