(in-package :testy)

(defun hash-ext-array-insert (key value hash)
  (if (nth-value 1 (gethash key hash))
      (vector-push-extend value (gethash key hash))
      (setf (gethash key hash) (make-array 1  
					   :initial-element value
					   :adjustable t
					   :fill-pointer 1)))
      (fill-pointer (gethash key hash)))

(defun hash-ext-array-remove (key value hash)
  (if (nth-value 1 (gethash key hash))
      (progn 
	(setf (gethash key hash) (delete value (gethash key hash)))
	(if (= 0 (fill-pointer (gethash key hash)))
	    (remhash key hash)
	    T))))

(defun set-testy-active-project (&optional path name)

  (cond ((and (null path) (null name))
	 (setf *testy-active-name* (package-name *package*)))
	((null path)
	 (setf *testy-active-name* name))
	((null name)
	 (setf *testy-active-name* (first (last (pathname-directory path)))))
	(t (setf *testy-active-name* name
		 *testy-active-path* path))))

(defun register-test (test)

  (if (not (typep test 'test))
      (return-from register-test nil))

  (if (gethash (string-upcase (name test)) *test-names*)
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
      (loop for tag in tags
	 do (hash-ext-array-remove tag test *test-tags*))
  
      T)))

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
  (let ((result t))
	(loop for test across test-sequence
	   do (if (not (funcall #'destroy-test test path))
		  (setf result nil)))
	result))

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

(defun deregister-tests (test-sequence)
  (map 'vector #'identity (loop for test across (fetch-tests test-sequence)
			     do (if (typep test 'test) 
				    (deregister-test (id test)))
			     collect test)))

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

(defun failed-tests (test-sequence)
  (tests-if (lambda (x) (equal (result x) nil)) test-sequence))

(defun passed-tests (test-sequence)
  (tests-if (lambda (x) (equal (result x) t)) test-sequence))

(defun failing-tests (test-sequence)
  (failed-tests test-sequence))
(defun passing-tests (test-sequence)
  (passed-tests test-sequence))

(defun high-verbosity ()
  (setf *print-verbosity* 'high))

(defun verbosity-high ()
  (setf *print-verbosity* 'high))

(defun low-verbosity ()
  (setf *print-verbosity* 'low))

(defun verbosity-low ()
  (setf *print-verbosity* 'low))

(defun detail-tests (test-sequence)
  (let ((*print-verbosity* 'high))
    (fetch-tests test-sequence)))

(defun print-results (&optional test-sequence (stream t))
  
  (let ((tests (fetch-tests (if test-sequence
				test-sequence
				(all-tests)))))
    (loop 
       for test across tests
       for result = (result test) then (result test)
       for id = (id test) then (id test)
       for position = 1 then (incf position)

       do (if (equal result t)
	      (format stream ".")
	      (progn (setf position 0)
		     (format stream "~&~a~&" (id test))))
       do (if (> position 50)
	      (progn
		(setf position 0)
		(format stream "~&"))))
    
    tests))

(defun serialise-tests (directory-path &optional (test-sequence (all-tests)))
  (map 'vector #'identity (loop for test across (fetch-tests test-sequence)
       collect (serialise directory-path test))))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

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

(defmacro defun-with-synonyms (names &rest body)
  (append (list 'progn) 
	  (loop for name in names
	     for head-form = (list 'defun name) then (list 'defun name)
	       for tail-form = nil then nil
	       do (loop for things in body
		       do (push things tail-form)
		     finally (setf tail-form (reverse tail-form)))
	       collect (append head-form tail-form))))

(defmacro defmacro-with-synonyms (names &rest body)
  (append (list 'progn) 
	  (loop for name in names
	     for head-form = (list 'defmacro name) then (list 'defmacro name)
	       for tail-form = nil then nil
	       do (loop for things in body
		       do (push things tail-form)
		     finally (setf tail-form (reverse tail-form)))
	       collect (append head-form tail-form))))
