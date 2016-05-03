;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following functions return arrays of tests, which can also be thought
;;; of as sets.
;;;
;;; They pass sets of tests to the higher level functions:
;;; primarily the test-running and test-statistic functions, which
;;; then perform all their operations with that particular set.
;;;
;;; Testy may be thought of as having the "test" as the fundamental
;;; unit, but most common operations are applied to "sets of tests".
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index
;;;
;;; 1. All-tests
;;;    Returns an array containing all currently registered tests.
;;;
;;; 2. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun-with-synonyms (get-test fetch-test) (test-identifier)
  (cond ((typep test-identifier 'test)
	 test-identifier)
	((or (stringp test-identifier) (symbolp test-identifier)) 
	 (gethash (string-upcase test-identifier) *test-names*))
	(t nil)))

(defun all-tests ()
  "Return an array of all currently registered tests"
  (let ((result-array (make-array (hash-table-count *test-names*))))
    (loop for tests being the hash-values in *test-names*
       for i = 0 then (incf i)
       do (setf (svref result-array i) tests))
    result-array))

(defun all-tags ()
  "Return a list of all currently registered tags"
  (loop for tags being the hash-keys in *test-tags*
       collect tags))

(defun-with-synonyms (fetch-tests get-tests) (&optional (test-identifier (all-tests)))
  
  (let ((result nil))

    (if (or (typep test-identifier 'string)
	    (typep test-identifier 'symbol))
	(setf result (make-array 1 :initial-element (get-test test-identifier)))
	(setf result (map 'vector #'get-test test-identifier)))
    (remove-duplicates result :test #'eq)))

(defun fetch-tests-from-tags (tag-identifiers)
  (let ((result (loop for tags in (tag-cond tag-identifiers)
		   unless (null (gethash tags *test-tags*))
		   collect (gethash tags *test-tags*))))

    (remove-duplicates (apply #'concatenate 'vector result) :test #'eq)))

(defun get-tests-from-tags (tag-identifiers)
  (fetch-tests-from-tags tag-identifiers))

(defun combine-test-sequences (&rest test-sequences)
 (remove nil 
	 (remove-duplicates 
	  (apply #'concatenate 'vector 
		 (map 'list #'fetch-tests test-sequences)) 
	  :test #'eq)))

(defun tests-if (predicate-func test-sequence)
  (remove-if-not predicate-func (fetch-tests test-sequence)))

(defun tests-if-not (predicate-func test-sequence)
  (remove-if predicate-func (fetch-tests test-sequence)))

(defun failed-tests (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) nil)) test-sequence))

(defun passed-tests (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) t)) test-sequence))

(defun failing-tests (test-sequence)
  (failed-tests test-sequence))

(defun passing-tests (test-sequence)
  (passed-tests test-sequence))
