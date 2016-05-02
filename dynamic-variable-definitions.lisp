;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dynamic-variable-definitions.lisp
;;;
;;; This file establishes the general environment variables for testy operation.
;;; No dynamic variables are explicitly exported from the testy package,
;;; nor were they designed for direct user interaction.
;;;
;;; Their purpose is relatively straight forward
;;; and is explained in accompanying comments.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *testy-active-name* defines the name that identifies the system that
;; testy is currently operating on.  It's main purpose is to define
;; default test names if the user does not supply them when
;; creating tests.
;;
;; *testy-active-path* contains the path the appropriate test directory,
;; used to store tests when they are serialised to disk.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *testy-active-name* nil)
(defvar *testy-active-path* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *print-verbosity* exists solely to determine how much detail is displayed
;; in the printed forms of tests if you actually print the objects themselves.
;;
;; Valid values are high or low, set by their respective (high-verbosity)
;; and (low-verbosity) functions defined later.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *print-verbosity* 'high)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The two variables *test-names*, *test-tags* are
;; hash tables used to store quickly accessible references to their respective
;; members (with test-names being the one used to access tests themselves).
;;
;; When objects are created/destroyed properly through exported functions,
;; each of these variables should have values automatically inserted/deleted.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-names* (make-hash-table :test 'equalp)) 
(defvar *test-tags* (make-hash-table :test 'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The last two variables *test-empty-function* and *expectation-table* are
;; the most complicated instances of the dynamic variables in testy, but
;; thankfully, they aren't very complicated at all.
;;
;; *test-empty-function* houses an "empty function" used internally throughout
;; testy for quite mundane purposes, to simplify code, concepts, memory footprint
;; and who knows maybe even run-times.
;;
;; *expectation-table* contains functions used for the various
;; types of expectations found within the tests themselves.
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
