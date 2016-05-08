;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; See package.lisp for details on what functionality and functions
;;; are exported from which files.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem #:testy
  :name "testy"
  :version "0.0.0"
  :maintainer "Damien John Melksham"
  :author "Damien John Melksham"
  :licence "All rights reserved"
  :description "A Testing Framework for Common Lisp"
  :long-description "A testing framework following my own peculiar and 
                     experimental philosophy for how testing should be 
                     prioritised and designed rather than the more common 
                     OO or Xunit inspired testers"
  :serial t
  :components ((:file "package")
	       (:file "dynamic-variable-definitions")
	       (:file "first-level-helpers")
	       (:file "set-testy-active-project")
	       (:file "set-print-verbosity")
               (:file "test-object-definition")
	       (:file "retrieve-tests")
	       (:file "test-registration")
	       (:file "make-test-function")
	       (:file "test-maker-macros")
	       (:file "load-save-destroy-tests")
	       (:file "run-test-function")
	       (:file "run-tests-and-tags")
	       (:file "statistics")
	       (:file "reporting")
	       (:file "convenient-test-accessors"))

  :depends-on (#:uiop))
