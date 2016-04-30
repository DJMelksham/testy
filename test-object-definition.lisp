(in-package :testy)

;; This function is currently living here because it is needed later
;; and I'm thinking about where to put it, and how to structure the 
;; project.

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
    :documentation "The name of the test as written out to disk in the project's test folder")
   (description
    :initarg :description
    :initform "No description text has been entered for this test"
    :type 'string
    :accessor description
    :documentation "A long form textual description of the test.")
   (expectation
    :initarg :expectation
    :initform (error "A test must include an expectation string to compare the result of running the test function")
    :type 'string
    :accessor expectation
    :documentation "A string representing the expectation of what happens/what function to use when the test actually runs")
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
    :documentation "A T/NIL flag that determines whether a test should always re-evaluate its source code before running.  Helpful/necessary for testing code including user-defined macros.  However, comes with a performance hit.")
   (expected-value
    :initarg :expected-value
    :initform (error "A test object must include an expected value for the result of the test")
    :accessor expected-value
    :documentation "The value required by the test in order to result in a pass.")
   (run-value
    :initarg :run-value
    :initform nil
    :accessor run-value
    :documentation "The last value obtained (if any) when the test function was last applied successfully")
   (run-time
    :initarg :run-time
    :initform 0.0
    :accessor run-time
    :documentation "The time taken for the test to complete, including before/after functions.")
   (result
    :initarg :result
    :initform nil
    :accessor result
    :documentation "A T or NIL flag to determine whether the test passed or failed, respectively")
   (before-function-source
    :initarg :before-function-source
    :initform nil
    :type 'list
    :accessor before-function-source
    :documentation "Source for a zero argument function that will be funcall'd before the test.")
   (before-function-compiled-form
    :initarg :before-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor before-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd  before the test.")
   (before-function-run-status
    :initarg :before-function-run-status
    :initform nil
    :accessor before-function-run-status
    :documentation "Determines whether before-function application raised a condition")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :type 'list
    :accessor after-function-source
    :documentation "Source for a zero argument function that will be funcall'd after the test.")
   (after-function-compiled-form
    :initarg :after-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor after-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd after the test")
   (after-function-run-status
    :initarg :after-function-run-status
    :initform nil
    :accessor after-function-run-status
    :documentation "Determines whether after-function application raised a condition")
   (type-of-test
    :initarg :type-of-test
    :initform nil
    :type 'list
    :accessor type-of-test
    :documentation "A flag that may be used in the future")))
    
(defgeneric serialise (pathname object)
  (:documentation "Serialise the given object to disk"))

(defmethod serialise (pathname (object test))
  (let ((local-pathname 
	 (uiop:merge-pathnames* (uiop:ensure-directory-pathname pathname) (file-on-disk object))))
    
    (with-open-file (stream local-pathname
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (print (list (cons 'NAME (name object))
		 (cons 'FILE-ON-DISK (concatenate 'string 
						  (pathname-name local-pathname)
						  "."
						  (pathname-type local-pathname)))
		 (cons 'DESCRIPTION (description object))
		 (cons 'EXPECTATION (expectation object))
		 (cons 'TAGS (tags object))
		 (cons 'SOURCE (source object))
		 (cons 'RE-EVALUATE (re-evaluate object))
		 (cons 'EXPECTED-VALUE (expected-value object))
		 (cons 'RUN-VALUE (proper-output (run-value object)))
		 (cons 'RUN-TIME (run-time object))
		 (cons 'RESULT (result object))
		 (cons 'BEFORE-FUNCTION-SOURCE (before-function-source object))
		 (cons 'BEFORE-FUNCTION-RUN-STATUS (proper-output (before-function-run-status object)))
		 (cons 'AFTER-FUNCTION-SOURCE (after-function-source object))
		 (cons 'AFTER-FUNCTION-RUN-STATUS (proper-output (after-function-run-status object)))
		 (cons 'TYPE-OF-TEST (type-of-test object))) stream))
    
    local-pathname))

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
		       (status status)
		       (result result)
		       (before-function-source before-function-source)
		       (before-function-run-status before-function-run-status)
		       (after-function-source after-function-source)
		       (after-function-run-status after-function-run-status)) object

	(cond ((eq *print-verbosity* 'high)
	       (progn
		 (format stream "-------------------->~& NAME: ~a~& DESCRIPTION: ~a~& FILE-ON-DISK: ~a~& TAGS: ~a~& RE-EVALUATE EACH RUN: ~a"		name description file-on-disk tags re-evaluate)
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
	    
