;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
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
               (:file "test-object-definition")
               (:file "test-runner")
               (:file "utilities")
	       (:file "test-makers")
	       (:file "statistics-generation")
	       (:file "context-object-definition")
	       (:file "test-macros"))

  :depends-on (#:uiop))
