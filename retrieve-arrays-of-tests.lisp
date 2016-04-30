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
;;; 3. Simplify insertion and removal of items into hashed extendedable arrays 
;;;    Used to store references to tests in arrays hashed by tag names.
;;;    a) Hashed-extendable-array insert value 
;;;    b) Hashed-extendable-array remove value
;;;
;;; 4. Directory-path-tail
;;;    Return the textual name of the deepest directory in a path
;;;
;;; 5. Defun-with-synonyms
;;;    A convenience macro for synonymous functions.  Assign a function 
;;;    definition to multiple symbols at the same time with
;;;    one (defun) form.  Not robust as of yet, but not designed
;;;    for anything more than our particular use, for which it suffices.
;;;
;;; 6. Defmacro-with-synonyms
;;;    A convenience macro for synonymous macros.  Assign a macro 
;;;    definition to multiple symbols  at the same time with
;;;    one (defmacro) form.  Not robust as of yet, but not designed
;;;    for anything more than our particular use, for which it suffices.
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;; Return an array of all currently registered tests

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

(defun fetch-context (context-identifier)
  (get-context context-identifier))

(defun get-context (context-identifier)
   (cond ((symbolp context-identifier)
	  (gethash (string-upcase context-identifier) *test-contexts*))
	 ((stringp context-identifier) 
	  (gethash (string-upcase context-identifier) *test-contexts*))
	 (t nil)))

(defun tag-cond (tag-identifier)
  (remove-if-not 
   (lambda (x) (gethash x *test-tags*))
   (cond ((symbolp tag-identifier)
	  (list (string-upcase tag-identifier)))
	 ((and (not (listp tag-identifier))
	       (not (stringp tag-identifier))
	       (notevery #'stringp tag-identifier)) 
	  nil)
	 ((stringp tag-identifier) 
	  (list (string-upcase tag-identifier)))
	 ((typep tag-identifier 'sequence) 
	  (remove-duplicates (map 'list #'string-upcase tag-identifier) :test #'equalp))
	 (t nil))))

(defun get-tag (tag-identifier)
  (car (tag-cond tag-identifier)))

(defun fetch-tag (tag-identifier)
  (car (tag-cond tag-identifier)))

(defun-with-synonyms (fetch-tests get-tests) (test-identifier)
  (let ((result nil))

    (if (or (typep test-identifier 'string)
	    (typep test-identifier 'symbol))
	(setf result (make-array 1 :initial-element (test-cond test-identifier)))
	(setf result (map 'vector #'test-cond test-identifier)))
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

(defun map-tests (func test-sequence &key (result-type 'vector))
  (map result-type func (fetch-tests test-sequence)))

(defun failed-tests (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) nil)) test-sequence))

(defun passed-tests (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) t)) test-sequence))

(defun failing-tests (test-sequence)
  (failed-tests test-sequence))

(defun passing-tests (test-sequence)
  (passed-tests test-sequence))
