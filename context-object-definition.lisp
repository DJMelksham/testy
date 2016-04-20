(in-package :testy)

(defclass context ()
  ((name
    :initarg :name
    :initform (error "A context must be created with a name")
    :type 'string
    :accessor name
    :documentation "The name of the context")
   (description
    :initarg :description
    :initform "No description available"
    :type 'string
    :accessor description
    :documentation "A long form textual description of the context.")
   (file-on-disk
    :initarg :file-on-disk
    :initform nil
    :type 'string
    :accessor file-on-disk
    :documentation "The name of the context as written out to disk in the project's context folder")
   (re-evaluate
    :initarg :re-evaluate
    :accessor re-evaluate
    :initform NIL
    :documentation "A T/NIL flag that determines whether a context should always re-evaluate its source code before running.  Helpful/necessary for testing code including user-defined macros.  However, comes with a performance hit.")
   (before-function-source
    :initarg :before-function-source
    :initform nil
    :type 'list
    :accessor before-function-source
    :documentation "Source for a zero argument function that will be funcall'd before any tests run with this context.")
   (before-function-compiled-form
    :initarg :before-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor before-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd  before any tests run within this context.")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :type 'list
    :accessor after-function-source
    :documentation "Source for a zero argument function that will be funcall'd after any tests run with this context.")
   (after-function-compiled-form
    :initarg :after-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor after-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd after any tests run within this context")))

(defmethod print-object ((object context) stream)
    (print-unreadable-object (object stream :type t)
      (with-accessors ((name name)
		       (description description)
		       (file-on-disk file-on-disk)
		       (re-evaluate re-evaluate)
		       (before-function-source before-function-source)
		       (after-function-source after-function-source)) object
	(format stream "~& NAME: ~a~& DESCRIPTION: ~a~& FILE-ON-DISK: ~a~& RE-EVALUATE EACH RUN: ~a~&"		
		name description file-on-disk re-evaluate)
	(if before-function-source (format stream "~& SOURCE OF FUNCTION TO SETUP CONTEXT: ~a"  before-function-source))
	(if after-function-source (format stream "~& SOURCE OF FUNCTION TO TEAR DOWN CONTEXT: ~a" after-function-source)))))

(defun make-context (&key
		       name
		       description
		       file-on-disk
		       re-evaluate
		       before-function-source
		       after-function-source)

  (let ((real-name nil)
	(real-desc nil)
	(real-fod nil)
	(real-before-function-source nil)
	(real-compiled-before-function-form nil)
	(real-after-function-source nil)
	(real-compiled-after-function-form nil)
	(final-context nil))

    ;;producing the test-context name
    (if (not (stringp name))
	(progn
	  (format t "A context must be created with a name, which must be a string.")
	   (return-from make-context nil)))
    
    (setf real-name (string-upcase name))

    (if (gethash real-name *test-contexts*)
		(progn
		  (format t "The name ~a is already registered to a currently loaded context.~&" 
			  real-name)
		  (return-from make-context nil)))

    ;;producing description
    (if (or (not description)
	    (not (stringp description)))
	(setf real-desc "No valid description available.")
	(setf real-desc description))
    
    ;;producing a potential file-name for the test on disk 
    (cond ((not file-on-disk) (setf real-fod (concatenate 'string real-name ".context")))
	  ((and (stringp file-on-disk) (ends-with-p file-on-disk ".context"))
	   (setf real-fod file-on-disk))
	  (t (setf real-fod (concatenate 'string file-on-disk ".context"))))
    
    ;;producing before-function-source
    (cond ((null before-function-source)
	   (setf real-before-function-source nil))
	  ((and (listp before-function-source) (not (equal (car before-function-source) 'lambda)))
	   (setf real-before-function-source (list 'lambda nil before-function-source)))
	  ((and (listp before-function-source) (equal (car before-function-source) 'lambda))
	   (setf real-before-function-source before-function-source))
	  (t 
	   (setf real-before-function-source (list 'lambda nil before-function-source))))
    ;;producing before-function-compiled
    (if (null real-before-function-source)
	(setf real-compiled-before-function-form *test-empty-function*)
	(setf real-compiled-before-function-form (eval real-before-function-source)))
    
    ;;producing after-function-source
    (cond ((null after-function-source)
	   (setf real-after-function-source nil))
	  ((and (listp after-function-source) (not (equal (car after-function-source) 'lambda)))
	   (setf real-after-function-source (list 'lambda nil after-function-source)))
	  ((and (listp after-function-source) (equal (car after-function-source) 'lambda))
	   (setf real-after-function-source after-function-source))
	  (t 
	   (setf real-after-function-source (list 'lambda nil after-function-source))))
    ;;producing after-function-compiled
    (if (null real-after-function-source)
	(setf real-compiled-after-function-form *test-empty-function*)
	(setf real-compiled-after-function-form (eval real-after-function-source)))

	(setf final-context (make-instance 'context
				       :name real-name
				       :description real-desc
				       :re-evaluate re-evaluate
				       :before-function-source real-before-function-source
				       :before-function-compiled-form real-compiled-before-function-form
				       :after-function-source real-after-function-source
				       :after-function-compiled-form real-compiled-after-function-form))
	
	(setf (gethash (name final-context) *test-contexts*) final-context)
	
	final-context))

(defmethod serialise (pathname (object context))
  (let ((local-pathname (if (cl-fad:directory-pathname-p pathname)
			    (cl-fad:merge-pathnames-as-file pathname (concatenate 'string (name object) ".context"))
			    pathname)))
    
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
		   (cons 'RE-EVALUATE (re-evaluate object))
		   (cons 'BEFORE-FUNCTION-SOURCE (before-function-source object))
		   (cons 'AFTER-FUNCTION-SOURCE (after-function-source object))) stream))
    
    local-pathname))

(defun load-context (pathname)
  (with-open-file (stream pathname
			  :direction :input
			  :if-does-not-exist :error)
    (let ((a-list (read stream)))
      (make-context :name (cdr (assoc 'NAME a-list))
		    :file-on-disk (cdr (assoc 'FILE-ON-DISK a-list))
		    :description (cdr (assoc 'DESCRIPTION a-list))
		    :re-evaluate (cdr (assoc 'RE-EVALUATE a-list))
		    :before-function-source (cdr (assoc 'BEFORE-FUNCTION-SOURCE a-list))
		    :after-function-source (cdr (assoc 'AFTER-FUNCTION-source a-list))))))

