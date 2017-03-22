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
;;; Or at least, that's what the totality of the concept describes.  Really,
;;; there are 3 discrete steps in this process:
;;;  a) reading the serialised source into the image as an association list.
;;;  b) conversion of an association list into a corresponding
;;;     test object.
;;;  c) registration of the test test object
;;;
;;; Functions exist that perform, and combine/wrap a number of these functions. 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Load a serialised test object ; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a) Load a serialised test object into an assoc-list    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-test-into-assoc (pathname)
  "Load a test serialised object into an in-lisp-image assoc list"
  (with-open-file (stream pathname
			  :direction :input
			  :if-does-not-exist :error)
    (let ((*package* (find-package *testy-active-name*)))
	  (read stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; b) Convert an association list into a test object ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-assoc-to-test (a-list)
  "Make a test object from an assoc list"
  (create-test-object :name (string=lookup 'NAME a-list)
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
			:after-function-run-status (string=lookup 'AFTER-FUNCTION-RUN-STATUS a-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c) Combine a conversion of a serialised object into an association list ;
;;;    and a creation of a test object, but do not register the object with ;
;;;    Testy's dynamic variables                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun basic-load-test (pathname)
  "Load a test without registering it into the Testy system"
  (let ((a-list (load-test-into-assoc pathname)))
    (convert-assoc-to-test a-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; d) All of the fun of basic-load-test, but with test registration as well ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-test (pathname)
  "Load an individual test from a .test file into the lisp image"
  (register-test (basic-load-test pathname)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; e) Perform the load-test operation on a series of tests in a directory ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-tests (&optional (directory-path *testy-active-path*))
  "Load all .test objects in a directory into the current lisp image"

  ;;; Should we multi-thread this sucka?
  (if (or (not *multi-thread?*)
	  (= *number-of-cores* 1))
      ;;; No, we should't multi-thread this sucka...
    
	(return-from load-tests
	(loop
	   for i = 0 then (incf i)
	   for test-path in (uiop:directory-files (uiop:ensure-directory-pathname directory-path) "*.test")
	   do (load-test test-path)
	   finally (return i))))

  ;;; Yeah, let's multi-thread this sucka, what could possibly go wrong!?
  ;;; If you have more than 1 core, we do some heavy lifting that should be refactored...
  ;;; Attempting to implement a lockless algorithm
  ;;; that will work pretty well most of the time,
  ;;; and maintain good speed up
  ;;; and good likely minimum run-time.
  ;;; Probably (definitely) doing it suboptimally at this stage...
  
  (let* ((files-for-threads (make-array *number-of-cores*))
	 (tests-for-threads (make-array *number-of-cores*))
	 (threads nil))
    
    ;; Build test-path arrays for all the threads
    (loop
       for i from 0 to (- *number-of-cores* 1)
       do (setf (svref files-for-threads i) (make-array 0 :adjustable t :fill-pointer 0)))
    
    ;; Populate test-path arrays for all the threads
    (loop
       for test-path in (uiop:directory-files (uiop:ensure-directory-pathname directory-path) "*.test")
       for i = 1 then (incf i)
       for j = (- *number-of-cores* i)
       do (vector-push-extend test-path (svref files-for-threads j)) 
       do (if (= i *number-of-cores*)
	      (setf i 0)))
    
    ;; Build the test arrays for all the threads 
    (loop
       for i from 0 to (- *number-of-cores* 1)	    
       do (setf (svref tests-for-threads i) (make-array (length (svref files-for-threads i)))))
    
    ;; Make threads and assign tests to the test-for-threads array
    (loop
       for files across files-for-threads
       for tests across tests-for-threads
       do (push (sb-thread:make-thread
		 (lambda (x y)
		   (loop
		      for test-path across x
		      for j = 0 then (incf j)
		      do (setf (svref y j) (basic-load-test test-path))))
		 :arguments (list files tests))
		threads))
    
    ;; Ensure all threads have finished their work
    
    (loop for thread in threads
       do (sb-thread:join-thread thread))
    
    ;; Put the tests made by the threads, put into
    ;; one array and register them
    
    (loop for test-arrays across tests-for-threads
       do (loop for test across test-arrays
	     do (register-test test)))
    
    ;; Return the number of registered tests
    (hash-table-count *test-names*)))
  
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
