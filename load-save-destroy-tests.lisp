;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDEX
;;;
;;; These functions are responsible for loading, saving, and deleting tests.
;;; 
;;; 1. Serialising or saving a test writes a representation of the test out to
;;; a directory, as a serialised associated list in a format
;;; equivalent enough to the internal structure of a test object as to reproduce
;;; that test object upon demand.
;;;
;;; 2. Loading a test reads serialised test-objects from disk back into the
;;; lisp image (registering the test in Testy's dynamic variables as it does so).
;;;
;;; 3. Destroying a test combines the actions of de-registering a test from the
;;; the Testy dynamic variables, as well as deleting the serialised 
;;; representation from its location on disk (assuming it is easily found
;;; and we are working in the confines of Testy's assumed workflow).
;;;
;;; 4. Two last functions do exactly what their name says: delete-all-tests-in-dir,
;;; and delete-test-from-disk.  The first deletes all files with a ".test"
;;; extension from a directory.  The second deletes individual .test
;;; serialised objects from directories. 
;;;
;;; Many of these functions have default arguments assuming Testy has an active
;;; project established, which then allows directories and file names to be
;;; established for tests automatically.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. a) Serialise a test object out to disk ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric serialise (pathname object)
  (:documentation "Serialise the given object to disk"))

(defmethod serialise (pathname (object test))
  "Write a test object to a .test representation on disk"
  (let ((local-pathname 
	 (uiop:merge-pathnames* (uiop:ensure-directory-pathname pathname) (file-on-disk object))))
    (with-open-file (stream local-pathname
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (print
       (list (cons 'NAME (name object))
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
	     (cons 'AFTER-FUNCTION-RUN-STATUS (proper-output (after-function-run-status object)))) stream))
    
    local-pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. b) Serialise all tests in an array out to a directory ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serialise-tests (&optional
			  (directory-path *testy-active-path*)
			  (test-sequence (all-tests)))
  "Write all tests in a test-sequence out to .test files in a directory"
  (loop
     for i = 0 then (incf i)
     for test across test-sequence
     do (serialise directory-path test)
     finally (return i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. a) Load a serialised test object into the lisp image ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-test (pathname)
  "Load an individual test from a .test file into the lisp image"
  (with-open-file (stream pathname
			  :direction :input
			  :if-does-not-exist :error)
    (let* ((*package* (find-package *testy-active-name*))
	   (a-list (read stream)))
      (make-test :name (string=lookup 'NAME a-list)
		 :file-on-disk (string=lookup 'FILE-ON-DISK a-list)
		 :description (string=lookup 'DESCRIPTION a-list)
		 :expectation (string=lookup 'EXPECTATION a-list)
		 :tags (string=lookup 'TAGS a-list)
		 :re-evaluate (string=lookup 'RE-EVALUATE a-list)
		 :source (string=lookup 'SOURCE a-list)
		 :expected-value (string=lookup 'EXPECTED-VALUE a-list)
		 :run-value (string=lookup 'RUN-VALUE a-list)
		 :run-time (string=lookup 'RUN-TIME a-list)
		 :result (string=lookup 'RESULT a-list)
		 :before-function-source (string=lookup 'BEFORE-FUNCTION-SOURCE a-list)
		 :before-function-run-status (string=lookup 'BEFORE-FUNCTION-RUN-STATUS a-list)
		 :after-function-source (string=lookup 'AFTER-FUNCTION-source a-list)
		 :after-function-run-status (string=lookup 'AFTER-FUNCTION-RUN-STATUS a-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. b) Perform the load operation on a series of tests in a directory ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-tests (&optional (directory-path *testy-active-path*))
  "Load all .test objects in a directory into the current lisp image"
  (loop
     for i = 0 then (incf i)
     for test-path in (uiop:directory-files (uiop:ensure-directory-pathname directory-path) "*.test")
     do (load-test test-path)
     finally (return i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. a) Destroy (de-register and delete from disk) a test ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destroy-test (identifier &optional (path *testy-active-path*))
  "Delete and de-register a test"
  (let ((test (get-test identifier))
	(local-path (uiop:ensure-directory-pathname path)))

    (if (or (null test)
	    (null path))
	(return-from destroy-test nil))

    (deregister-test test)

    (delete-file (uiop:merge-pathnames* local-path (file-on-disk test)))

    T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. b) Destroy a series of tests contained in an array of tests ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destroy-tests (test-sequence &optional (path *testy-active-path*))
  "Delete and de-register a series of tests"
  (loop for i = 0 then (incf i)
     for test across test-sequence	 
	   do (destroy-test test path)
	finally (return i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. a) Delete all files in a directory with a .test extension ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-all-tests-in-dir (&optional (directory-path *testy-active-path*))
  "Delete all files in a directory with a .test extension"
  (loop
     for i = 0 then (incf i)
     for test-path in (uiop:directory-files (uiop:ensure-directory-pathname directory-path) "*.test")
     do (delete-file test-path)
       finally (return i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. b) Delete a test from disk ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-test-from-disk (test-identifier &optional (directory-path *testy-active-path*))
  "Delete a specific .test file from a directory on disk"
  (let ((test (get-test test-identifier)))
    (delete-file (uiop:merge-pathnames* (uiop:ensure-directory-pathname directory-path) (file-on-disk test)))))
