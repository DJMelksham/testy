(in-package :testy)

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

(defun deregister-context (context-name)
		    (remhash context-name *test-contexts*))

(defun deregister-tests (&optional (test-sequence (all-tests)))
  (loop
     for test across test-sequence
       for i = 1 then (incf i)
     do (deregister-test test)
       finally (return i)))

(defun run-tests (&optional (test-sequence (all-tests)) (re-evaluate 'auto) (stop-on-fail nil))
  (let ((result t)
	(indv-test-status t))
    (if stop-on-fail
	(loop for tests across test-sequence
	   while (setf indv-test-status (funcall #'run-test tests re-evaluate))
	   finally (setf result indv-test-status))
	(loop for tests across test-sequence
	   do (if (not (setf indv-test-status (funcall #'run-test tests re-evaluate)))
		  (setf result indv-test-status))))
	result))

(defun run-tags (tags)
  (run-test (fetch-tests-from-tags tags)))

;(defun high-verbosity ()
;  (setf *print-verbosity* 'high))

;(defun verbosity-high ()
;  (setf *print-verbosity* 'high))

;(defun low-verbosity ()
;  (setf *print-verbosity* 'low))

;(defun verbosity-low ()
;  (setf *print-verbosity* 'low))

(defun detail-tests (test-sequence)
  (let ((*print-verbosity* 'high))
    (loop for tests across test-sequence
	 do (print tests))))

(defun print-results (&optional (test-sequence (all-tests)) (stream t))
  
    (loop 
       for test across test-sequence
       for result = (result test) then (result test)
       for name = (name test) then (name test)
       for position = 1 then (incf position)

       do (if (equal result t)
	      (format stream ".")
	      (progn (setf position 0)
		     (format stream "~&~a~&" name)))
       do (if (> position 50)
	      (progn
		(setf position 0)
		(format stream "~&")))))

(defun serialise-tests (&optional (directory-path *testy-active-path*)  (test-sequence (all-tests)))
  (loop
     for i = 0 then (incf i)
     for test across test-sequence
     do (serialise directory-path test)
       finally (return i))) 

(defmacro with-context (context-identifier &rest forms)
  (with-gensyms (result)
    (once-only (context-identifier)
      `(let ((,result nil))
	 
	 (if (re-evaluate (get-context ,context-identifier))
	     (funcall (eval (before-function-source (get-context ,context-identifier))))
	     (funcall (before-function-compiled-form (get-context ,context-identifier))))
	 
	 (setf ,result (progn
			,@forms))
	 
	 (if (re-evaluate (get-context ,context-identifier))
	     (funcall (eval (after-function-source (get-context ,context-identifier))))
	     (funcall (after-function-compiled-form (get-context ,context-identifier))))
	 
	 ,result))))
