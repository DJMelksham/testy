(defun make-test (&key 
		    id
		    name
		    file-on-disk
		    description
		    expectation
		    tags
		    re-evaluate
		    source
		    expected-value
		    (run-value nil)
		    (run-time 0.0)
		    (result nil)
		    before-function-source
		    after-function-source
		    (type-of-test (if (null *test-style-warnings*)
				      'nw
				      nil)))
  
  
  (macrolet ((nw-eval? (&rest body)
	       `(if (not (eq real-type-of-test 'nw))
		    (eval ,@body)
		    (handler-bind
			((style-warning 
			  #'(lambda (w) 
			      (when (undefined-warning-p w)
				(invoke-restart 'muffle-warning)))))
		      (eval ,@body)))))
    
    (let ((real-id (if (null id) (new-test-id) id))
	  (real-name nil)
	  (real-fod nil)
	  (real-desc nil)
	  (real-exp nil)
	  (real-tags nil)
	  (real-re-evaluate nil)
	  (real-source nil)
	  (real-compiled-form nil)
	  (real-expected-value nil)
	  (real-before-function-source nil)
	  (real-compiled-before-function-form nil)
	  (real-after-function-source nil)
	  (real-compiled-after-function-form nil)
	  (real-type-of-test nil))
      
      ;;producing test name
      
      (if (not name) 
	  (setf real-name (concatenate 'string (package-name *package*) ":TEST-" (write-to-string real-id)))
	  (setf real-name (string-upcase name)))
      
      (if (gethash real-name *test-names*)
	  (progn
	    (format t "The name ~a is already registered to Test ID ~a~&" 
		    real-name
		    (id (gethash real-name *test-names*)))
	    (return-from make-test nil)))
      
      ;;producing a potential file-name for the test on disk 
      
      (cond ((not file-on-disk) (setf real-fod (concatenate 'string real-name ".test")))
	    ((and (stringp file-on-disk) (ends-with-p file-on-disk ".test"))
	     (setf real-fod file-on-disk))
	    (t (setf real-fod (concatenate 'string file-on-disk ".test"))))
      
      ;;set type-of-test
      (if (or (null type-of-test)
	      (eq type-of-test 'nw))
	  (setf real-type-of-test type-of-test)
	  (setf real-type-of-test nil))

      ;;producing test file description
      
      (if (or (not description)
	      (not (stringp description)))
	  (setf real-desc "No valid description has been supplied for this test.")
	  (setf real-desc description))
      
      ;;producing test expectation
      ;;hardcoded test expectations for now...
      (cond ((null expectation) 
	     (setf real-exp "EQUALP"))
	    ((member (string-upcase expectation)
		     (list "EQ" "=" "EQL" "EQUAL" "EQUALP" "NULL" "NOTNULL" "T" "CONDITION" "ERROR") :test #'equal)
	     (setf real-exp (string-upcase expectation)))
	    (t (progn
		 (format t "Expectation ~a is not a valid kind of expectation.~&" expectation)
		 (format t "Expectation must be one of ~S, ~S, ~S, ~S, ~S, ~S, ~S, ~S, ~S or ~S.~&"
			 "EQ"
			 "="
			 "EQL"
			 "EQUAL"
			 "EQUALP"
			 "NULL"
			 "NOTNULL"
			 "T"
			 "CONDITION"
			 "ERROR")
		 (return-from make-test nil))))
      
      ;;producing test tags
      
      (cond ((and (not (listp tags))
		  (not (stringp tags))
		  (notevery #'stringp tags)) 
	     (setf real-tags nil))
	    ((stringp tags) 
	     (setf real-tags (list (string-upcase tags))))
	    ((typep tags 'sequence)
	     (setf real-tags 
		   (remove-duplicates (map 'list #'string-upcase tags) :test #'equalp)))
	    (t (setf real-tags nil)))
      
      ;;producing re-evaluate
      (cond ((eq re-evaluate t) (setf real-re-evaluate t))
	    ((equal re-evaluate nil) (setf real-re-evaluate nil))
	    (t (error "Re-evaluate can be set to either NIL or T")))
      
      ;;producing test source
      (cond ((null source)
	     (progn
	       (format t "You must provide a valid lisp expression that can be used for the test.")
	       (return-from make-test nil)))
	    ((and (listp source) (not (equal (car source) 'lambda)))
	     (setf real-source (list 'lambda nil source)))
	    ((and (listp source) (equal (car source) 'lambda))
	     (setf real-source source))
	    (t 
	     (setf real-source (list 'lambda nil source))))
      
      ;;producing test compiled form
      ;;assumes SBCL-esque default behaviour where everything is compiled unless otherwise
      ;;stated.  May need to be changed if this lisp code is ever made purely portable.
      (setf real-compiled-form (nw-eval? real-source))
      
      ;;producing test expected value
      (if (null expected-value)
	  (setf real-expected-value T)
	  (setf real-expected-value expected-value))
      
      ;;producing test before-function-source
      (cond ((null before-function-source)
	     (setf real-before-function-source '(lambda () nil)))
	    ((and (listp before-function-source) (not (equal (car before-function-source) 'lambda)))
	     (setf real-before-function-source (list 'lambda nil before-function-source)))
	    ((and (listp before-function-source) (equal (car before-function-source) 'lambda))
	     (setf real-before-function-source before-function-source))
	    (t 
	     (setf real-before-function-source (list 'lambda nil before-function-source))))
      ;;producing test before-function-compiled
      (if (or (null real-before-function-source)
	      (equal real-before-function-source '(lambda () nil)))
	  (setf real-compiled-before-function-form *test-empty-function*)
	  (setf real-compiled-before-function-form (nw-eval? real-before-function-source)))
      
      ;;producing test after-function-source
      (cond ((null after-function-source)
	     (setf real-after-function-source '(lambda () nil)))
	    ((and (listp after-function-source) (not (equal (car after-function-source) 'lambda)))
	     (setf real-after-function-source (list 'lambda nil after-function-source)))
	    ((and (listp after-function-source) (equal (car after-function-source) 'lambda))
	     (setf real-after-function-source after-function-source))
	    (t 
	     (setf real-after-function-source (list 'lambda nil after-function-source))))
      ;;producing test after-function-compiled
      (if (or (null real-after-function-source)
	      (equal real-after-function-source '(lambda () nil)))
	  (setf real-compiled-after-function-form *test-empty-function*)
	  (setf real-compiled-after-function-form (nw-eval? real-after-function-source)))
      
      (register-test  (make-instance 'test
		   :id real-id
		   :name real-name
		   :file-on-disk real-fod
		   :description real-desc
		   :expectation real-exp
		   :re-evaluate real-re-evaluate
		   :tags real-tags
		   :source real-source
		   :compiled-form real-compiled-form
		   :expected-value real-expected-value
		   :run-value run-value
		   :run-time run-time
		   :result result
		   :before-function-source real-before-function-source
		   :before-function-compiled-form real-compiled-before-function-form
		   :after-function-source real-after-function-source
		   :after-function-compiled-form real-compiled-after-function-form
		   :type-of-test real-type-of-test)))))


(defun load-test (pathname)
  (with-open-file (stream pathname
			  :direction :input
			  :if-does-not-exist :error)
    (let ((a-list (read stream)))
      (make-test :name (cdr (assoc 'NAME a-list))
		 :file-on-disk (cdr (assoc 'FILE-ON-DISK a-list))
		 :description (cdr (assoc 'DESCRIPTION a-list))
		 :expectation (cdr (assoc 'EXPECTATION a-list))
		 :tags (cdr (assoc 'TAGS a-list))
		 :re-evaluate (cdr (assoc 'RE-EVALUATE a-list))
		 :source (cdr (assoc 'SOURCE a-list))
		 :expected-value (cdr (assoc 'EXPECTED-VALUE a-list))
		 :run-value (cdr (assoc 'RUN-VALUE a-list))
		 :run-time (cdr (assoc 'RUN-TIME a-list))
		 :result (cdr (assoc 'RESULT a-list))
		 :before-function-source (cdr (assoc 'BEFORE-FUNCTION-SOURCE a-list))
		 :after-function-source (cdr (assoc 'AFTER-FUNCTION-source a-list))
		 :type-of-test (cdr (assoc 'TYPE-OF-TEST a-list))))))

(defun load-tests (directory-path)
  (map 'vector #'identity 
       (loop 
	  for test-path in (remove-if #'cl-fad:directory-pathname-p (cl-fad:list-directory directory-path))
	  collect (load-test test-path))))
