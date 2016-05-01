;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun print-stats (&optional (test-identifiers (all-tests)) (stream t))
  (let* ((stat-results (stats test-identifiers))
	 (seconds (nth 9 stat-results)))

    (format stream "~&***************************************")
    (format stream "~&      TEST STATISTICS: ~a" *testy-active-name*)

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

(defun detail-tests (test-sequence)
  (let ((*print-verbosity* 'high))
    (loop for tests across test-sequence
	 do (print tests))))

(defun print-results (&optional (test-sequence (all-tests)) (stream t))
  
    (loop 
       for test across test-sequence
       for result = (result test) then (result test)
       for name = (name test) then (name test)
       for position = 1 then (incf position)

       do (if (equal result t)
	      (format stream ".")
	      (progn (setf position 0)
		     (format stream "~&~a~&" name)))
       do (if (> position 50)
	      (progn
		(setf position 0)
		(format stream "~&")))))
