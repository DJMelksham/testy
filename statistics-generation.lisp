(in-package :testy)

(defun stat-number-tests (test-identifiers)
  (length (fetch-tests test-identifiers)))

(defun stat-number-passed (test-identifiers)
  (let ((passed 0)
	(failed 0))
    (loop for test across (fetch-tests test-identifiers)
	 do (if (result test) 
		(incf passed)
		(incf failed)))
    (values passed failed)))

(defun stat-number-failed (test-identifiers)
  (let ((passed 0)
	(failed 0))
    (loop for test across (fetch-tests test-identifiers)
	 do (if (result test) 
		(incf passed)
		(incf failed)))
    (values failed passed)))


(defun stat-perc-passed (test-identifiers)
  (multiple-value-bind (num-passed num-failed)
    (stat-number-passed test-identifiers)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (error "Somehow, no tests fed into the function via test-identifiers passed or failed."))
	  ((zerop num-passed)
	   (values 0.00 100.00))
	  ((zerop num-failed)
	   (values 100.00 0.00))
	  (t (values (float (* 100 (/ num-passed (+ num-passed num-failed))))
		     (float (* 100 (/ num-failed (+ num-passed num-failed)))))))))

(defun stat-ratio-passed (test-identifiers)
  (multiple-value-bind (num-passed num-failed)
      (stat-number-passed test-identifiers)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (error "Somehow, no tests fed into the function via test-identifiers passed or failed."))
	  ((zerop num-passed)
	   (values 0 num-failed))
	  ((zerop num-failed)
	   (values num-passed 0))
	  (t (values (/ num-passed (+ num-passed num-failed))
		     (/ num-failed (+ num-passed num-failed)))))))

(defun stat-perc-failed (test-identifiers)
  (multiple-value-bind (num-passed num-failed)
    (stat-number-passed test-identifiers)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (error "Somehow, no tests fed into the function via test-identifiers passed or failed."))
	  ((zerop num-passed)
	   (values 0.00 100.00))
	  ((zerop num-failed)
	   (values 100.00 0.00))
	  (t (values (float (* 100 (/ num-failed (+ num-passed num-failed))))
		     (float (* 100 (/ num-passed (+ num-passed num-failed)))))))))

(defun stat-ratio-failed (test-identifiers)
  (multiple-value-bind (num-passed num-failed)
      (stat-number-passed test-identifiers)
    (cond ((and (zerop num-passed)
		(zerop num-failed))
	   (error "Somehow, no tests fed into the function via test-identifiers passed or failed."))
	  ((zerop num-passed)
	   (values 0 num-failed))
	  ((zerop num-failed)
	   (values num-passed 0))
	  (t (values (/ num-failed (+ num-passed num-failed))
		     (/ num-passed (+ num-passed num-failed)))))))

(defun stat-total-run-time (test-identifiers)
  "Approximate total run time of a selection of tests in seconds"
 (reduce #'+ (map-tests (lambda (x) (run-time x)) (fetch-tests test-identifiers))))

(defun stat-number-conditions (test-identifiers)
  (let ((conditions 0)
	(non-conditions 0))
    (loop for test across (fetch-tests test-identifiers)
	 do (if (typep (run-value test) 'condition) 
		(incf conditions)
		(incf non-conditions)))
    (values conditions non-conditions)))


(defun stat-number-errors (test-identifiers)
  (let ((conditions 0)
	(non-conditions 0))
    (loop for test across (fetch-tests test-identifiers)
	 do (if (typep (run-value test) 'error) 
		(incf conditions)
		(incf non-conditions)))
    (values conditions non-conditions)))

(defun stats (test-identifiers)
 (let ((tests (fetch-tests test-identifiers))
       (num-tests 0)
       (num-passed 0)
       (num-failed 0)
       (perc-passed 0.0)
       (perc-failed 0.0)
       (ratio-passed 0)
       (ratio-failed 0)
       (num-errors 0)
       (num-conditions 0)
       (total-run-time 0.0))
   (setf num-tests (length tests))
   (multiple-value-setq (num-passed num-failed)
     (stat-number-passed tests))
   (multiple-value-setq (perc-passed perc-failed)
     (stat-perc-passed tests))
   (multiple-value-setq (ratio-passed ratio-failed)
     (stat-ratio-passed tests))
   (setf num-errors (stat-number-errors tests))
   (setf num-conditions (stat-number-conditions tests))
   (setf total-run-time (stat-total-run-time tests))
				     
       (list 
	num-tests 
	 num-passed num-failed 
	 perc-passed perc-failed 
	 ratio-passed ratio-failed 
	 num-errors 
	 num-conditions
	 total-run-time)))

(defun print-stats (&optional (test-identifiers (all-tests)) (stream t))
  (let* ((stat-results (stats test-identifiers))
	 (seconds (nth 9 stat-results)))

    (format stream "~&***************************************")
    (format stream "~&         TEST STATISTICS")

    (format stream "~&***************************************")
    (format stream "~&      NUMBER OF TESTS: ~a~&         TESTS PASSED: ~a~&         TESTS FAILED: ~a"
	    (nth 0 stat-results)
	    (nth 1 stat-results)
	    (nth 2 stat-results))
    (format stream "~&       PERCENT PASSED: %~,2f~&       PERCENT FAILED: %~,2f"
	    (nth 3 stat-results)
	    (nth 4 stat-results))
    (format stream "~&         RATIO PASSED: ~a~&         RATIO FAILED: ~a" 
	    (nth 5 stat-results)
	    (nth 6 stat-results))
    (format stream "~&---------------------------------------")
    (format stream "~&     NUMBER OF ERRORS: ~a~& NUMBER OF CONDITIONS: ~a"
	    (nth 7 stat-results)
	    (nth 8 stat-results))
    (format stream 
	    "~&***************************************
~&         APPROX. RUN-TIME
~&***************************************
~&              SECONDS: ~,4f
~&              MINUTES: ~,4f
~&                HOURS: ~,4f"
	    seconds
	    (float (/ seconds 60))
	    (float (/ (/ seconds 60) 60)))
    (format stream "~&***************************************")
    T))
