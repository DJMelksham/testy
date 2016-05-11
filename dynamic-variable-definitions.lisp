;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file establishes general environment variables for testy.
;;; No dynamic variables are explicitly exported from the testy package,
;;; nor were they designed for direct user interaction.
;;;
;;; Their purpose is relatively simple
;;; and is explained in the accompanying comments.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Establish how many processors we have to work with ;
;;; Default is 4                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *number-of-cores* 4)

(defun set-number-of-cores (n)
  (setf *number-of-cores* n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic variables to determine whether Testy attempts to ;
;;; load-tests or run-tests using multiple threads           ;
;;; by default                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *multi-thread?* T)

(defun set-multi-thread (predicate)
  (setf *multi-thread?* predicate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *testy-active-name* identifies the system that
;; testy is operating on.
;;
;; *testy-active-path* contains the path to the appropriate test directory,
;; used to store tests when serialised to disk.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *testy-active-name* nil)
(defvar *testy-active-path* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *print-verbosity* exists to determine how much detail is displayed
;; in the printed forms of tests if you print a test object.
;;
;; Valid values are high or low, set by their respective (high-verbosity)
;; and (low-verbosity) functions defined with the test object definition later.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *print-verbosity* 'high)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *test-names* and *test-tags* are hash tables used to store quickly
;; accessible references to their respective members
;; (test-names identify tests themselves, and test-tags allow quick
;;  lookup of groups of tests via tags)
;;
;; When tests are created/destroyed properly through exported functions,
;; each of these variables should be automatically updated.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-names* (make-hash-table :test 'equalp)) 
(defvar *test-tags* (make-hash-table :test 'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The last two variables *test-empty-function* and *expectation-table* are
;; the most complex instances of the dynamic variables, but
;; thankfully, they're still pretty simple.
;;
;; *test-empty-function* houses an "empty function" used internally throughout
;; testy for quite mundane purposes: to simplify code, skip compilation of
;; functions that don't really do anything while maintaining funcall'ability,
;; etc.
;;
;; *expectation-table* contains functions used for the
;; types of expectations found within the tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
