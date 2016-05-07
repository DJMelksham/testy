;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following functions return tests or arrays of tests, which can also be thought
;;; of as sets.
;;;
;;; Typically in the use of Testy, it is envisioned that you will be working with
;;; too many tests to bother handling them individually too often,
;;;  with the obvious exception of inspecting tests when they
;;; they are in a failing state or you are otherwise constructing or fixing
;;; ammending an individual test.
;;;
;;; Rather, Testy is optimised to run an entire suite of automated tests,
;;; or subsets within that suite, quickly and repetetively.
;;;
;;; Sets of tests may be passed to the higher level functions:
;;; primarily the test-running and test-statistic functions, which
;;; perform all their operations on sets of tests.
;;;
;;; While the "test" is the fundamental unit of the testing framework,
;;; common operations are applicable to "sets of tests".
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index
;;;
;;;  1. Get-test/fetch-test
;;;     Returns an individual test, identified either by its name or by identity
;;;     if being passed an actual test object.
;;;
;;;  2. All-tests
;;;     Returns an array containing all currently registered tests.
;;;
;;;  3. All-tags
;;;     Retuns a list of all currently registered tags.
;;;
;;;  4. Get-tests/fetch-tests
;;;     Returns an array of tests when fed a sequence of names
;;;
;;;  5. Get-tests-from-tags/Fetch-tests-from-tags
;;;     
;;;
;;;  6. Combine-test-sequences
;;;
;;;  7. Tests-if
;;;
;;;  8. Tests-if-not
;;;
;;;  9. Failed-tests/Failing-tests
;;;
;;; 10. Passed-tests/Passing-tests
;;;
;;;
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
    (remove nil (remove-duplicates result :test #'eq) :test #'eq)))

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
