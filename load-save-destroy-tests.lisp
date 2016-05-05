;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

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
		 (cons 'AFTER-FUNCTION-RUN-STATUS (proper-output (after-function-run-status object)))) stream))
    
    local-pathname))

(defun serialise-tests (&optional (directory-path *testy-active-path*)  (test-sequence (all-tests)))
  (loop
     for i = 0 then (incf i)
     for test across test-sequence
     do (serialise directory-path test)
       finally (return i)))

(defun load-test (pathname)
  (with-open-file (stream pathname
			  :direction :input
			  :if-does-not-exist :error)
    (let* ((*package* (find-package *testy-active-name*))
	   (a-list (read stream)))
      (make-test :name (string=lookup 'NAME a-list)
		 :file-on-disk (string=lookup 'FILE-ON-DISK a-list)
		 :description (string=lookup 'DESCRIPTION a-list)
		 :expectation (string=lookup 'EXPECTATION a-list)
		 :tags (string=lookup  'TAGS a-list)
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

(defun load-tests (&optional (directory-path *testy-active-path*))
  (loop
     for i = 0 then (incf i)
     for test-path in (uiop:directory-files (uiop:ensure-directory-pathname directory-path) "*.test")
     do (load-test test-path)
     finally (return i)))

(defun destroy-test (identifier &optional (path *testy-active-path*))
  (let ((test (get-test identifier))
	(local-path (uiop:ensure-directory-pathname path)))

    (if (or (null test)
	    (null path))
	(return-from destroy-test nil))

    (deregister-test test)

    (delete-file (uiop:merge-pathnames* local-path (file-on-disk test)))

    T))

(defun destroy-tests (test-sequence &optional (path *testy-active-path*))
  (loop for i = 0 then (incf i)
     for test across test-sequence	 
	   do (destroy-test test path)
	finally (return i)))

(defun delete-all-tests-in-dir (&optional (directory-path *testy-active-path*))
  (loop
     for i = 0 then (incf i)
     for test-path in (uiop:directory-files (uiop:ensure-directory-pathname directory-path) "*.test")
     do (delete-file test-path)
       finally (return i)))

(defun delete-test-from-disk (test-identifier &optional (directory-path *testy-active-path*))
  (let ((test (get-test test-identifier)))
    (delete-file (uiop:merge-pathnames* (uiop:ensure-directory-pathname directory-path) (file-on-disk test)))))
