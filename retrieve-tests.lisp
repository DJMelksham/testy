;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following functions return tests or arrays of tests,
;;; which can also be thought of as sets.
;;;
;;; Typically in the use of Testy, it is envisioned that you will be working with
;;; too many tests to bother handling them individually very often,
;;; with the obvious exception of inspecting tests when they
;;; they are in a failing state or you are otherwise constructing, fixing
;;; or ammending an individual test.
;;;
;;; Rather, Testy is optimised to run an entire suite of automated tests
;;; or subsets within that suite, quickly and repetetively.
;;;
;;; Sets of tests may be passed to Testy higher level functions:
;;; primarily the test-running and test-statistic functions, which
;;; perform all their operations on sets of tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index
;;;
;;;  1. Get-test/fetch-test
;;;     Returns an individual test, identified either by its name or by identity
;;;     if being passed an actual test object
;;;
;;;  2. All-tests
;;;     Returns an array containing all currently registered tests
;;;
;;;  3. All-tags
;;;     Retuns an array of all currently registered tags
;;;
;;;  4. Get-tests/fetch-tests
;;;     Returns an array of tests when fed a sequence of names
;;;
;;;  5. Get-tests-from-tags/Fetch-tests-from-tags
;;;     Returns an array of tests based on a sequence of tags
;;;
;;;  6. Combine-test-sequences
;;;     Combines sequences of tests together into one array.
;;;     
;;;  7. Tests-if
;;;     Maps a single argument predicate function over a set of tests,
;;;     returning an array of tests for which the predicate
;;;     returns T.
;;;
;;;  8. Tests-if-not
;;;     Maps a single argument predicate function over a set of tests,
;;;     returning an array of tests for which the predicate returns
;;;     nil.
;;;
;;;  9. Failed-tests/Failing-tests
;;;     Returns an array of all currently registered tests that
;;;     have a result of NIL (a failing test).
;;;
;;; 10. Passed-tests/Passing-tests
;;;     Returns an array of all currently registered tests that
;;;     have a result of T (a passing test).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Get-test & fetch-test ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-synonyms (get-test fetch-test) (test-identifier)
  "Return an individual test object referenced by its name/identity"
  (cond ((typep test-identifier 'test)
	 test-identifier)
	((or (stringp test-identifier) (symbolp test-identifier)) 
	 (gethash (string-upcase test-identifier) *test-names*))
	(t nil)))

;;;;;;;;;;;;;;;;;;
;;; 2. All-tests ;
;;;;;;;;;;;;;;;;;;

(defun all-tests ()
  "Return an array of all currently registered tests"
  (let ((result-array (make-array (hash-table-count *test-names*))))
    (loop for tests being the hash-values in *test-names*
       for i = 0 then (incf i)
       do (setf (svref result-array i) tests))
    result-array))

;;;;;;;;;;;;;;;;;
;;; 3. All-tags ;
;;;;;;;;;;;;;;;;;

(defun all-tags ()
  "Return an array of all currently registered tags"
  (map 'vector #'identity (loop for tags being the hash-keys in *test-tags*
       collect tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Get-tests & fetch-tests ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-synonyms (fetch-tests get-tests)
    (&optional (test-identifier (all-tests)))
 "Return an array of tests referenced by the test name or its identity"
  (let ((result #()))
    (if (or (typep test-identifier 'string)
	    (typep test-identifier 'symbol))
	(setf result (make-array 1 :initial-element (get-test test-identifier)))
	(setf result (map 'vector #'get-test test-identifier)))
    (remove nil (remove-duplicates result :test #'eq) :test #'eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. Get-tests-from-tags & Fetch-tests-from-tags ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-synonyms (fetch-tests-from-tags get-tests-from-tags)
    (tag-identifiers)
  "Return an array of all tests defined by a tag-name or a sequence of tag names"
  (let* ((local-tags (cond ((or (symbolp tag-identifiers)
			       (stringp tag-identifiers))
			   (list (string-upcase tag-identifiers)))
			  ((typep tag-identifiers 'sequence)
			   (map 'list #'string-upcase tag-identifiers))
			  (t nil)))
	(result (loop for tags in local-tags
		   unless (null (gethash tags *test-tags*))
		   collect (gethash tags *test-tags*))))
    (remove-duplicates (apply #'concatenate 'vector result) :test #'eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. Combine-test-sequences ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-test-sequences (&rest test-sequences)
  "Combine several test sequences into one array"
 (remove nil 
	 (remove-duplicates 
	  (concatenate 'vector 
		 (map 'vector #'fetch-tests test-sequences)) 
	  :test #'eq)))

;;;;;;;;;;;;;;;;;
;;; 7. Tests-if ;
;;;;;;;;;;;;;;;;;

(defun tests-if (predicate-func test-sequence)
  "Apply a predicate to a test sequence and return an array of all tests
   where (eq (predicate test) T)"
  (remove-if-not predicate-func (fetch-tests test-sequence)))

;;;;;;;;;;;;;;;;;;;;;
;;; 8. Tests-if-not ;
;;;;;;;;;;;;;;;;;;;;;

(defun tests-if-not (predicate-func test-sequence)
  "Apply a predicate to a test sequence and return an array of all tests
   where (eq (predicate test) nil)"
  (remove-if predicate-func (fetch-tests test-sequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 9. Failed-tests & Failing-tests ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-synonyms (failed-tests failing-tests)
    "Return an array of all tests where the result is eq nil"
    (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) nil)) test-sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10. Passed-tests & Passing-tests ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-synonyms (passed-tests passing-tests)
    "Return an array of all tests where the result is eq T"
    (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) t)) test-sequence))
