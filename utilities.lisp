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

(defun all-tests ()
  (let ((result-array (make-array (hash-table-count *test-names*))))
    (loop for tests being the hash-values in *test-names*
       for i = 0 then (incf i)
	 do (setf (svref result-array i) tests))
    result-array))			     

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun get-context (context-identifier)
   (cond ((symbolp context-identifier)
	  (gethash (string-upcase context-identifier) *test-contexts*))
	 ((stringp context-identifier) 
	  (gethash (string-upcase context-identifier) *test-contexts*))
	 (t nil)))

(defun fetch-context (context-identifier)
  (get-context context-identifier))

(defun deregister-context (context-name)
		    (remhash context-name *test-contexts*))

(defun tag-cond (tag-identifier)
  (remove-if-not 
   (lambda (x) (gethash x *test-tags*))
   (cond ((symbolp tag-identifier)
	  (list (string-upcase tag-identifier)))
	 ((and (not (listp tag-identifier))
	       (not (stringp tag-identifier))
	       (notevery #'stringp tag-identifier)) 
	  nil)
	 ((stringp tag-identifier) 
	  (list (string-upcase tag-identifier)))
	 ((typep tag-identifier 'sequence) 
	  (remove-duplicates (map 'list #'string-upcase tag-identifier) :test #'equalp))
	 (t nil))))

(defun get-tag (tag-identifier)
  (car (tag-cond tag-identifier)))

(defun fetch-tag (tag-identifier)
  (car (tag-cond tag-identifier)))

(defun fetch-tests (test-identifier)
  (let* ((result nil))
    
    (if (null test-identifier)
	(return-from fetch-tests nil))

    (if (or (not (typep test-identifier 'sequence))
	    (typep test-identifier 'string))
	(setf result (make-array 1 :initial-element (test-cond test-identifier)))
	(setf result (map 'vector #'test-cond test-identifier)))
    (remove-duplicates result :test #'eq)))

(defun get-tests (test-identifier)
  (fetch-tests test-identifier))
    
(defun fetch-tests-from-tags (tag-identifiers)
  (let ((result (loop for tags in (tag-cond tag-identifiers)
		   unless (null (gethash tags *test-tags*))
		   collect (gethash tags *test-tags*))))

    (remove-duplicates (apply #'concatenate 'vector result) :test #'eq)))

(defun get-tests-from-tags (tag-identifiers)
  (fetch-tests-from-tags tag-identifiers))

(defun deregister-tests (&optional (test-sequence (all-tests)))
  (loop
     for test across test-sequence
       for i = 1 then (incf i)
     do (deregister-test test)
       finally (return i)))

(defun combine-test-sequences (&rest test-sequences)
 (remove nil 
	 (remove-duplicates 
	  (apply #'concatenate 'vector 
		 (map 'list #'fetch-tests test-sequences)) 
	  :test #'eq)))

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

(defun tests-if (predicate-func test-sequence)
  (remove-if-not predicate-func (fetch-tests test-sequence)))

(defun tests-if-not (predicate-func test-sequence)
  (remove-if predicate-func (fetch-tests test-sequence)))

(defun map-tests (func test-sequence &key (result-type 'vector))
  (map result-type func (fetch-tests test-sequence)))

(defun failed-tests (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) nil)) test-sequence))

(defun passed-tests (&optional (test-sequence (all-tests)))
  (tests-if (lambda (x) (equal (result x) t)) test-sequence))

(defun failing-tests (test-sequence)
  (failed-tests test-sequence))
(defun passing-tests (test-sequence)
  (passed-tests test-sequence))

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
