;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment functions/macros manage basic interaction with Testy's own 
;;; internal environment.  This includes:
;;;
;;; - Establish active project names and paths to serialised test directories  
;;; - Registration and removal of test objects in the global hash
;;; - Registration and removal of test objects in the tag-hash concordant
;;;   with the tags each test are members of
;;; - Registration of contexts objects in the context hash
;;; - Load serialised tests
;;; - Delete serialised tests in directories
;;; - Save/serialise tests 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun set-testy-active-project (&optional path name)
  (cond ((and (null path) (null name))
	 (setf *testy-active-name* (package-name *package*)))
	((null path)
	 (setf *testy-active-name* name))
	((null name)
	 (setf *testy-active-name* (first (last (pathname-directory (uiop:ensure-directory-pathname path))))
	       *testy-active-path* (uiop:ensure-directory-pathname path)))
	(t (setf *testy-active-name* name
		 *testy-active-path* (uiop:ensure-directory-pathname path)))))

(defun register-test (test)
  (if (not (typep test 'test))
      (return-from register-test nil))

  (if (gethash (name test) *test-names*)
	(error (concatenate 'string "A test named " (string-upcase (name test)) " is already registered. Change the name of the test before registering again, or deregister the other test first.")))

  (setf (gethash (name test) *test-names*) test)
  (loop for tag in (tags test)
     do (hash-ext-array-insert tag test *test-tags*))

  test)

(defun deregister-test (identifier)
  (let ((test (get-test identifier)))
    (if (null test)
	(return-from deregister-test nil))
    
    (with-accessors ((name name)
		     (tags tags)
		     (file-on-disk file-on-disk)) test
      (remhash name *test-names*)
      (loop
	 for tag in tags
	 do (hash-ext-array-remove tag test *test-tags*))
      T)))

(defun-with-synonyms (low-verbosity verbosity-low) ()
  (setf *print-verbosity* 'low))

(defun-with-synonyms (high-verbosity verbosity-high) ()
  (setf *print-verbosity* 'high))

(defun verbosity (value)
  "Manually set the verbosity level of printed test objects"
  (if (and (not (eq value 'high))
	   (not (eq value 'low)))
      (return-from verbosity nil)
      (setf *print-verbosity* value)))
