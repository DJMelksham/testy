;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The test object is the fundamental data structure underlying Testy.
;;; Its definition looks relatively large and monolithic, and that's largely
;;; because it is.
;;;
;;; I think the overall program is better for it.
;;; There is nothing overly complex about what a test object is,
;;; it is merely that its definition contains a large amount of data
;;; and structure that must be defined up front.
;;;
;;; From a design perspective, should a test object be broken down
;;; and either inherit from, or be composed of, simpler objects?
;;; I do not believe so.  To borrow from an anonymous principle, in some
;;; problems when you aim to abstract away complexity from one particular aspect,
;;; you push that complexity to other parts of the problem or implementation.
;;;
;;; In this case, if I made the test object simple by composing it,
;;; the complexity would be pushed into the number of objects
;;; and the relationships the compositional parts that make up a test.
;;; Here, they are all defined in one place, and the test object
;;; may then be manipulated as a single "unit" throughout the rest of
;;; Testy's world, which is what, I think, should be the starting point
;;; of a testing framework.
;;;
;;; I admit the aesthetics of OO code do not sit well with me.
;;; Some objects, I suppose, are big and clunky, relatively speaking,
;;; and sufficiently defining their structure ends up looking as
;;; unappealing as a big table definition in SQL.
;;;
;;; But the end result, I think, works smashingly well. By establishing
;;; this singular structure, it will bring us massive benefits in
;;; hitting the prioritised goals of the Testy system: that is to say,
;;; speed, scale, and industrialisation.
;;;
;;; And so, in Testy, we have one atomic object representing a test,
;;; and its structural definition is found below.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defclass test ()
  ((name
    :initarg :name
    :initform (error "A test must have a unique identifiable name.")
    :type 'string
    :accessor name
    :documentation "Textual name of a test.  Must be unique.")
   (file-on-disk
    :initarg :file-on-disk
    :initform nil
    :type 'string
    :accessor file-on-disk
    :documentation "The name of the test as would be to disk in a project's test folder")
   (description
    :initarg :description
    :initform "No description text has been entered for this test"
    :type 'string
    :accessor description
    :documentation "Long form textual description of the test.")
   (expectation
    :initarg :expectation
    :initform (error "A test must include an expectation string to compare the result of running the test function")
    :type 'string
    :accessor expectation
    :documentation "A string representing the expectation that compares the value returned from the test with what was expected")
   (expectation-function
    :initarg :expectation-function
    :initform *test-empty-function*
    :type 'function
    :accessor expectation-function
    :documentation "A two argument function that will be applied to determine whether test expectation has been met")
   (tags
    :initarg :tags
    :initform nil
    :type 'list
    :accessor tags
    :documentation "A list of tags applicable to the test")
   (source
    :initarg :source 
    :initform (error "A test must provide source code that defines the test")
    :type 'list
    :accessor source
    :documentation "Source code that defines the function of the test")
   (compiled-form
    :initarg :compiled-form
    :initform (error "A test must provide a compiled-form of the function applied when the test is called")
    :type 'function
    :accessor compiled-form
    :documentation "The compiled-form of the function applied when the test is called")
   (re-evaluate
    :initarg :re-evaluate
    :accessor re-evaluate
    :initform NIL
    :documentation "A T/NIL flag that determines whether a test should re-evaluate its source code before running.  Helpful/necessary for testing code including user-defined macros.  However, comes with a performance hit.")
   (expected-value
    :initarg :expected-value
    :initform (error "A test object must include an expected value for the result of the test")
    :accessor expected-value
    :documentation "The value required by the test in order to result in a pass.")
   (run-value
    :initarg :run-value
    :initform nil
    :accessor run-value
    :documentation "The last value obtained (if any) when the test function was last called")
   (run-time
    :initarg :run-time
    :initform 0.0
    :accessor run-time
    :type 'float
    :documentation "The time taken for the test to complete, excluding test overhead")
   (result
    :initarg :result
    :initform nil
    :accessor result
    :documentation "A T or NIL flag to determine whether the test passed or failed")
   (before-function-source
    :initarg :before-function-source
    :initform nil
    :type 'list
    :accessor before-function-source
    :documentation "Source for a zero argument function funcall'd before the test")
   (before-function-compiled-form
    :initarg :before-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor before-function-compiled-form
    :documentation "A compiled zero argument function funcall'd before a test")
   (before-function-run-status
    :initarg :before-function-run-status
    :initform nil
    :accessor before-function-run-status
    :documentation "Determines whether before-function raised a condition")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :type 'list
    :accessor after-function-source
    :documentation "Source for a zero argument function funcall'd after the test")
   (after-function-compiled-form
    :initarg :after-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor after-function-compiled-form
    :documentation "A compiled zero argument function funcall'd after the test")
   (after-function-run-status
    :initarg :after-function-run-status
    :initform nil
    :accessor after-function-run-status
    :documentation "Determines whether after-function application raised a condition")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How a test object is printed...                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ; 
;;; This can almost certainly be implemented better, but is of          ;
;;; relatively little consequence                                       ;
;;;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((object test) stream)
  (print-unreadable-object (object stream)
    (with-accessors ((name name)
		     (file-on-disk file-on-disk)
		     (description description)
		     (expectation expectation)
		     (tags tags)
		     (source source)
		     (expected-value expected-value)
		     (run-value run-value)
		     (run-time run-time)
		     (re-evaluate re-evaluate)
		     (result result)
		     (before-function-source before-function-source)
		     (before-function-run-status before-function-run-status)
		     (after-function-source after-function-source)
		     (after-function-run-status after-function-run-status)) object
      
      (cond ((eq *print-verbosity* 'high)
	     (progn
	       (format stream "-------------------->~& NAME: ~a" name)
	       (format stream "~& DESCRIPTION: ~a" description)
	       (format stream "~& FILE-ON-DISK: ~a" file-on-disk)
	       (format stream "~& TAGS: ~a" tags)
	       (format stream "~& RE-EVALUATE EACH RUN: ~a" re-evaluate)
	       (format stream "~& SOURCE: ")
	       (format stream "~{~s~^~&~}" (cddr source))
	       (format stream "~& EXPECTATION: ~a" expectation)
	       (format stream "~& EXPECTED VALUE: ~a" expected-value)
	       (format stream "~& RUN VALUE: ~a" run-value)
	       (format stream "~& TEST PASSED: ~a" result)
	       (format stream "~& RUN-TIME IN SECONDS: ~3$" run-time)
	       (format stream "~& FUNCTION THAT RUNS BEFORE THE TEST: ~a" before-function-source)
	       (format stream "~& RUN STATUS OF BEFORE-TEST FUNCTION: ~a" before-function-run-status)
	       (format stream "~& FUNCTION THAT RUNS AFTER THE TEST: ~a" after-function-source)
	       (format stream "~& RUN STATUS OF AFTER-TEST FUNCTION: ~a" after-function-run-status)))
	    ((eq *print-verbosity* 'low)
	     (format stream "~a: ~a" name (if result "PASS" "FAIL")))))))
