(in-package :testy)

(defvar *testy-active-name* nil)
(defvar *testy-active-path* nil)
(defvar *test-names* (make-hash-table :test 'equalp)) 
(defvar *test-tags* (make-hash-table :test 'equalp))
(defvar *test-contexts* (make-hash-table :test 'equalp))
(defvar *print-verbosity* 'high)
(defvar *test-empty-function* (lambda () nil))
(defvar *expectation-table*
  (let ((ex-table (make-hash-table :test #'equal :size 16)))
    (setf (gethash "EQ" ex-table) #'eq
	  (gethash "=" ex-table) #'=
	  (gethash "EQL" ex-table) #'eql
	  (gethash "EQUAL" ex-table) #'equal
	  (gethash "EQUALP" ex-table) #'equalp
	  (gethash "NULL" ex-table) (lambda (x y) (eq x y))
	  (gethash "NOT-NULL" ex-table) (lambda (x y) (not (eq x y)))
	  (gethash "T" ex-table) (lambda (x y) (eq x y))
	  (gethash "CONDITION" ex-table) (lambda (&optional (type 'condition) x)
					   (typep x type))
	  (gethash "ERROR" ex-table) (lambda (&optional (type 'error) x)
				       (typep x type)))
    ex-table))
