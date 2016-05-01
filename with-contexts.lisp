;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)		     

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
