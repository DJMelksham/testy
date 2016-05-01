;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; First level helper functions/macros provide initial low level programs
;;; that help us write and implement the rest of Testy.  They either help provide 
;;; much needed functionality, or provide a level of summarisation of common
;;; operations to enable later higher level programs to be expressed much more
;;; succinctly or efficiently.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index
;;;
;;; 1. With-Gensym macro
;;;    A copy of the classic macro-assisting macro used to make macros more
;;;    hygenic.
;;;
;;; 2. Once-Only macro
;;;    A copy of the classic macro-assisting macro used to ensure forms
;;;    passed to macros are evaluated only once, and thus conform to the
;;;    principal of least surprise.
;;;
;;; 3. Simplify insertion and removal of items into hashed extendedable arrays 
;;;    Used to store references to tests in arrays hashed by tag names.
;;;    a) Hashed-extendable-array insert value 
;;;    b) Hashed-extendable-array remove value
;;;
;;; 4. Directory-path-tail
;;;    Return the textual name of the deepest directory in a path
;;;
;;; 5. Defun-with-synonyms
;;;    A convenience macro for synonymous functions.  Assign a function 
;;;    definition to multiple symbols at the same time with
;;;    one (defun) form.  Not robust as of yet, but not designed
;;;    for anything more than our particular use, for which it suffices.
;;;
;;; 6. Defmacro-with-synonyms
;;;    A convenience macro for synonymous macros.  Assign a macro 
;;;    definition to multiple symbols  at the same time with
;;;    one (defmacro) form.  Not robust as of yet, but not designed
;;;    for anything more than our particular use, for which it suffices.
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defun hash-ext-array-insert (key value hash)
  "Insert a value into an extendable array stored in a hash"
  (if (nth-value 1 (gethash key hash))
      (vector-push-extend value (gethash key hash))
      (setf (gethash key hash) (make-array 1
					   :initial-element value
					   :adjustable t
					   :fill-pointer 1)))
  (fill-pointer (gethash key hash)))

(defun hash-ext-array-remove (key value hash)
  "Remove a value from an extendable array stored in a hash"
  (if (nth-value 1 (gethash key hash))
      (progn 
	(setf (gethash key hash) (delete value (gethash key hash)))
	(if (= 0 (fill-pointer (gethash key hash)))
	    (remhash key hash)
	    T))))

(defun directory-tail (path)
  "Return the name of the deepest directory in a path"
  (first (last (pathname-directory (uiop:ensure-directory-pathname path)))))

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

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun proper-output (thing)
  (if (typep thing 'condition)
      (with-output-to-string (out) (format out "~a" thing))
      thing))

(defun string=lookup (symbol assoc)
  (cdr (assoc symbol assoc :test #'string=)))

(defun undefined-warning-p (w)
  (let ((control (simple-condition-format-control w)))
         (string= control "undefined ~(~A~): ~S")))

(defmacro nw-eval? (conditional-clause &rest body)
  `(if ,conditional-clause 
		    (eval ,@body)
		    (locally
			(declare  #+sbcl(sb-ext:muffle-conditions sb-int:type-warning))
		      (handler-bind
			  ((style-warning #'(lambda (w) 
					      (when (undefined-warning-p w)
						(invoke-restart 'muffle-warning))))
			   (sb-int:type-warning #'muffle-warning))
		      (eval ,@body)))))
