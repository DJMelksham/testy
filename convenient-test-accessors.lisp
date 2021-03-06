;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These functions provide better access than the CLOS low level defined
;;; accessors: they provide housekeeping, error-checking, and update other
;;; associated state when certain slots of test objects are changed.
;;;
;;; In addition, setter functions that operate on a sequence of tests allow
;;; mapping of certain properties across a multiple tests at a time, allowing
;;; changes to be easily conveyed across several tests without the need
;;; for other explicit macros, fixtures, or factories, keeping the rule of
;;; "Don't repeat yourself", and allowing the proper test designer to maintain
;;; each test as a perfectly self contained unit.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :testy)

;;; Should this get tags from a test, or should it be used to get all currently registered tags
;;; or both

(defun get-tags (&optional (test-identifiers  (all-tests)))
  "Get a list of applicable tags from a test"
  (let ((local-hash (make-hash-table :test #'equalp))
	(local-tests (get-tests test-identifiers)))
    (loop for test across local-tests
       do (loop for tag in (tags test)
	     do (setf (gethash tag local-hash) T)))
    (loop for tags being the hash-keys in local-hash
	 collect tags)))

(defun add-tags (test-identifiers tags)
  "Add tag/tags to tests"
  (loop
     for test across (get-tests test-identifiers)
     with tag-set = (if (stringp tags)
			(list (string-upcase tags))
			(map 'list #'string-upcase tags))
     do (setf (tags test) (union tag-set (tags test) :test #'string=))
     do (loop for tag in (tags test)
	     do (hash-ext-array-insert tag test *test-tags*))))
	     
(defun remove-tags (test-identifiers tags)
  "Remove tag/tags from tests"
  (loop
     for test across (get-tests test-identifiers)
     with tag-set = (if (stringp tags)
			(list (string-upcase tags))
			(map 'list #'string-upcase tags))
     do (setf (tags test) (set-difference (tags test) tag-set :test #'string=))
     do (loop for tag in (tags test)
	     do (hash-ext-array-remove tag test *test-tags*))))

(defun get-source (test)
"Get the source-code of a test"
 (source (get-test test)))

(defun set-source (test source &optional (expected-value nil ev-supplied-p))
  "Set the source code of a test"
  (let* ((local-test (get-test test)))
    (cond  ((and (listp source) (not (equal (car source) 'lambda)))
	    (setf (source local-test) (list 'lambda nil source)))
	   ((and (listp source) (equal (car source) 'lambda))
	    (setf (source local-test) source))
	   (t 
	    (setf (source local-test) (list 'lambda nil source))))

    (if ev-supplied-p
	(setf (expected-value local-test) expected-value))

    (setf (result local-test) nil
	  (run-value local-test) nil)

    (setf (compiled-form local-test) (eval (source local-test)))))

(defun set-sources (test-identifiers source &optional (expected-value nil ev-supplied-p))
  "Set the source code of many test objects referenced in an array of names or identities"
  (loop for test across (get-tests test-identifiers)
     do (if ev-supplied-p
	    (set-source test source expected-value)
	    (set-source test source))))

(defun get-expectation (test)
  "Get the expectation function name from a test object referenced by name or identity"
  (expectation (get-test test)))

(defun set-expectation (test expectation)
  "Set the expectation function name for a test object referenced by name or identity"
  (let ((local-test (get-test test)))
  (if (member (string-upcase expectation)
	      (list "EQ" "=" "EQL" "EQUAL" "EQUALP" "NULL" "NOTNULL" "T" "CONDITION" "ERROR") :test #'equal)
      (progn
	(setf (expectation local-test) expectation)
	t)
      nil)))

(defun set-expectations (test-identifiers expectation)
  "Set the expectation function name for multiple test objects referenced in an array of names or identities"
  (loop for test across (get-tests test-identifiers)
     do (if ev-supplied-p
	    (set-source test source expected-value)
	    (set-source test source))))

(defun get-expected-value (test)
  "Get the expected-value slot of a test object referenced by name or identity"
  (expected-value (get-test test)))

(defun set-expected-value (test value)
  "Set the expected-value slot of a test object referenced by name or identity"
  (setf (expected-value (get-test test)) value))

(defun set-expected-values (test-identifiers value)
  "Set the expected-value slot of multiple test objects referenced in an array of names or identities"
  (loop for test across (get-tests test-identifiers)
       do (set-expected-value test value)))

(defun get-before-function-source (test)
  "Get the before-function source code of a test"
  (before-function-source (get-test test)))

(defun set-before-function-source (test &optional source)
  "Set the before-function source code of a test"
  (let* ((local-test (get-test test)))
    (cond ((null source)
	   (setf (before-function-source local-test)
		 '(lambda () nil)))
	  ((and (listp source) (not (equal (car source) 'lambda)))
	   (setf (before-function-source local-test) (list 'lambda nil source)))
	  ((and (listp source) (equal (car source) 'lambda))
	   (setf (before-function-source local-test) source)))

    (setf (before-function-compiled-form local-test)
	  (if (equalp (before-function-source local-test) (list 'lambda nil nil))
	      *test-empty-function*
	      (eval (before-function-source local-test))))

    (setf (before-function-run-status local-test) nil)

    local-test))

(defun set-before-function-sources (test-identifiers &optional source)
  "Set the before-function source code of multiple tests"
  (loop for test across (get-tests test-identifiers)
       do (set-before-function-source test source)))

(defun get-after-function-source (test)
  "Get the after-function source code of a test"
  (after-function-source (get-test test)))

(defun set-after-function-source (test &optional source)
  "Set the after-function source code of a test"
  (let* ((local-test (get-test test)))
    (cond ((null source)
	   (setf (after-function-source local-test)
		 '(lambda () nil)))
	  ((and (listp source) (not (equal (car source) 'lambda)))
	   (setf (after-function-source local-test) (list 'lambda nil source)))
	  ((and (listp source) (equal (car source) 'lambda))
	   (setf (after-function-source local-test) source)))

    (setf (after-function-compiled-form local-test)
	  (if (equalp (after-function-source local-test) (list 'lambda nil nil))
	      *test-empty-function*
	      (eval (after-function-source local-test))))

    (setf (after-function-run-status local-test) nil)

    local-test))

(defun set-after-function-sources (test-identifiers &optional source)
  "Set the after-function source code of multiple tests"
  (loop for test across (get-tests test-identifiers)
       do (set-after-function-source test source)))

(defun get-name (test)
  "Return the name of a test object"
  (if (get-test test)
      (name (get-test test))
      nil))

(defun set-name (test new-name)
  "Set/change the name of a test"
  (let ((local-test (get-test test)))

    (if (gethash (string-upcase new-name) *test-names*)
	(progn
	  (format t "The name ~a is already taken by another registered test." (string-upcase new-name))
	  (return-from set-name nil))) 
    
    (deregister-test local-test)
    (setf (name local-test) (string-upcase new-name))
    (register-test local-test)

    local-test))

(defun get-names (&optional (test-identifiers (all-tests)))
  "Return the names of multiple test-objects referenced by an array of names or identities"
  (let* ((local-tests (get-tests test-identifiers))
	(local-array (make-array (length local-tests))))

    (loop
       for test across local-tests
       for i = 0 then (incf i)
       do (if (typep test 'test)
	      (setf (svref local-array i) (name test))
	      (setf (svref local-array i) test)))
    
    local-array))

(defun get-description (test)
  "Get the description of a test"
  (description (get-test test)))

(defun set-description (test description)
  "Set/change the description of a test"
  (setf (description (get-test test)) description))

(defun get-run-value (test)
  "Get the latest run-value of a test"
  (run-value (get-test test)))

(defun get-result (test)
  "Get the current status of a test: whether it is T (PASS) or NIL (FAILED)."
  (result (get-test test)))

(defun get-results (&optional (test-identifiers (all-tests)))
  "Get the current status of tests: whether it is T (PASS) or NIL (FAILED)"
  (let* ((local-tests (get-tests test-identifiers))
	 (local-array (make-array (length local-tests))))
    
    (loop for test across local-tests
	 for i = 0 then (incf i)
       do (if (typep test 'test)
	      (setf (svref local-array i) (result test))
	      (setf (svref local-array i) test)))
    local-array))
