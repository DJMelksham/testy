;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defun set-testy-active-project (&optional path name)
  (cond ((and (null path) (null name))
	 (setf *testy-active-name* (package-name *package*)))
	((null path)
	 (setf *testy-active-name* name))
	((null name)
	 (setf *testy-active-name* (directory-tail path)
	       *testy-active-path* (uiop:ensure-directory-pathname path)))
	(t (setf *testy-active-name* name
		 *testy-active-path* (uiop:ensure-directory-pathname path)))))
