;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The make-test function is your fundamental test creating function.
;;; If you wish to define test objects programatically or in source code,
;;; the make-test function is likely your place to go.
;;;
;;; My preference, however is to create tests interactively at the REPL
;;; during development, not as separate .lisp files or at run-time.
;;; While observances I have made of the first phenomenon lead me to believe
;;; it is slow and flow-breaking, and observances I have made of the second
;;; lead me to believe many automated tests result in many superfluous
;;; tests of questionable structure and value, I accept there are valid
;;; applications of both techniques.
;;;
;;; Regardless, it is possible, maybe even preferable, to use
;;; Testy without ever explicitly calling the make-test function.
;;; 
;;; But the macros responsible for test authorship, defined in later code,
;;; are merely convenient wrappers around make-test, and carry the restriction
;;; of establishing tests based on input at compile time, and so cannot cover
;;; every conceivable use case.
;;;
;;; Arguments do exist for taking out some of the monolithic logic contained
;;; within make-tests and putting them into their own functions in the future...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun make-test (&key 
		    name
		    file-on-disk
		    description
		    expectation
		    tags
		    re-evaluate
		    source
		    expected-value
		    run-value
		    (run-time 0.0)
		    result
		    before-function-source
		    before-function-run-status
		    after-function-source
		    after-function-run-status)
    
    (let ((real-name nil)
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
	  (real-compiled-after-function-form nil))
      
      ;;producing test name
      
      (if (not name) 
	  (setf real-name
		(let ((resulting-real-name (concatenate 'string (string *testy-active-name*) ":TEST-0")))
		  (loop for i = 0 then (incf i)
		     while (gethash resulting-real-name *test-names*)
		     do (setf resulting-real-name (concatenate 'string (string *testy-active-name*) ":TEST-" (write-to-string i))))
		  resulting-real-name))
	  (setf real-name (string-upcase name)))
      
      (if (gethash real-name *test-names*)
	  (progn
	    (format t "The name ~a is already registered.~&" real-name)
	    (return-from make-test nil)))
      
      ;;producing a potential file-name for the test on disk 
      
      (cond ((not file-on-disk) (setf real-fod (concatenate 'string real-name ".test")))
	    ((and (stringp file-on-disk) (ends-with-p file-on-disk ".test"))
	     (setf real-fod file-on-disk))
	    (t (setf real-fod (concatenate 'string file-on-disk ".test"))))

      ;;producing test file description
      
      (if (or (not description)
	      (not (stringp description)))
	  (setf real-desc "No valid description has been supplied for this test.")
	  (setf real-desc description))
      
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
      (setf real-compiled-form (eval real-source))
      
      ;;producing test expected value
      (if (null expected-value)
	  (setf real-expected-value nil)
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
	  (setf real-compiled-before-function-form (eval real-before-function-source)))
      
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
	  (setf real-compiled-after-function-form (eval real-after-function-source)))
      
      (register-test  (make-instance 'test
		   :name real-name
		   :file-on-disk real-fod
		   :description real-desc
		   :expectation real-exp
		   :expectation-function (gethash real-exp *expectation-table*)
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
		   :before-function-run-status before-function-run-status
		   :after-function-compiled-form real-compiled-after-function-form
		   :after-function-run-status after-function-run-status))))
