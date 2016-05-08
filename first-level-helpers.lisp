;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; First level helper functions/macros provide initial low level programs
;;; that help us write and implement the rest of Testy.  They either provide 
;;; much needed functionality, or a level of summarisation of common
;;; operations to enable later programs to be expressed more
;;; succinctly or efficiently.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index
;;;
;;; 1. With-Gensym macro & Once-Only macro
;;;    A copy of the classic macro-assisting-macros.
;;;    Used to make macros more hygenic and generally conform to the
;;;    principal of least surprise.
;;;
;;; 2. Simplify insertion and removal of items into hashed extendedable arrays 
;;;    Used in Testy to store references to tests in arrays hashed by tag names.
;;;    a) Hashed-extendable-array insert value 
;;;    b) Hashed-extendable-array remove value
;;;
;;; 3. Directory-path-tail
;;;    Return the textual name of the deepest directory in a path.
;;;
;;; 4. Defun-with-synonyms & defmacro-with-synonyms
;;;    A macro for synonymous functions and macro definitions.
;;;    Allows one to define a function/macro to multiple symbols at the same time
;;;    Not robust as of yet, but not designed for anything more than our
;;;    particular use, for which it suffices.
;;;
;;; 5. Ends-with-p
;;;    This simple function determines whether a string ends with
;;;    another string.
;;;
;;; 6. Proper-output
;;;    This...unfortunate...function is used in Testy to
;;;    figure out how to print/serialise conditions in tests.
;;;    To be honest, its a bit hacky ... happy for suggestions
;;;    on how to change it, but its only used in a handful of places, and
;;;    its not really visible to the user/system any way.
;;;
;;; 7. String=lookup
;;;    Used to simplify the source required for reading of Testy serialised
;;;    objects and the making of their requisite test objects.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. With-Gensym macro & Once-Only macro ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. hash-ext-array-insert & hash-ext-array-remove ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-ext-array-insert (key value hash)
  "Insert a value into an extendable array stored in a hash.  Create if it doesn't exist."
  (if (nth-value 1 (gethash key hash))
      (vector-push-extend value (gethash key hash))
      (setf (gethash key hash) (make-array 1
					   :initial-element value
					   :adjustable t
					   :fill-pointer 1)))
  (fill-pointer (gethash key hash)))

(defun hash-ext-array-remove (key value hash)
  "Remove a value from an extendable array stored in a hash.  Remove key if array is now empty."
  (if (nth-value 1 (gethash key hash))
      (progn 
	(setf (gethash key hash) (delete value (gethash key hash)))
	(if (= 0 (fill-pointer (gethash key hash)))
	    (remhash key hash)
	    T))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. directory-tail ;
;;;;;;;;;;;;;;;;;;;;;;;

(defun directory-tail (path)
  "Return the name of the deepest directory in a path"
  (first (last (pathname-directory (uiop:ensure-directory-pathname path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. defun-with-synonyms and defmacro-with-synonyms ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defun-with-synonyms (names &rest body)
 "A macro to allow multiple defun's to be defined for multiple symbols
  at the same time"
  (append (list 'progn) 
	  (loop for name in names
	     for head-form = (list 'defun name) then (list 'defun name)
	       for tail-form = nil then nil
	       do (loop for things in body
		       do (push things tail-form)
		     finally (setf tail-form (reverse tail-form)))
	       collect (append head-form tail-form))))

(defmacro defmacro-with-synonyms (names &rest body)
 "A macro to allow multiple defmacros to be defined and assigned to multiple
  symbols at the same time"
  (append (list 'progn) 
	  (loop for name in names
	     for head-form = (list 'defmacro name) then (list 'defmacro name)
	       for tail-form = nil then nil
	       do (loop for things in body
		       do (push things tail-form)
		     finally (setf tail-form (reverse tail-form)))
	       collect (append head-form tail-form))))

;;;;;;;;;;;;;;;;;;;;
;;; 5. ends-with-p ;
;;;;;;;;;;;;;;;;;;;;

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

;;;;;;;;;;;;;;;;;;;;;;
;;; 6. proper-output ;
;;;;;;;;;;;;;;;;;;;;;;

(defun proper-output (thing)
  "If a thing is a condition, print its representation as a string"
  (if (typep thing 'condition)
      (with-output-to-string (out) (format out "~a" thing))
      thing))

;;;;;;;;;;;;;;;;;;;;;;
;;; 7. string=lookup ;
;;;;;;;;;;;;;;;;;;;;;;

(defun string=lookup (symbol assoc)
  "Lookup a value in an assoc list using the string= function as the test" 
  (cdr (assoc symbol assoc :test #'string=)))
