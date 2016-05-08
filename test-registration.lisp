;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testy has a couple of dynamic variables defined which work as a register
;;; of tests loaded in the system for quick, easy, and arbitrary retrieval.
;;; Functions responsible for making and deleting/removing tests ensure tests
;;; are registered upon creation/loading and deregistered upon
;;; deletion/active-project changes by calling the functions defined
;;; in this file.
;;; 
;;; Unless you have forked Testy and are implementing your own testing system
;;; and paradigm on top of Testy's internal workings,
;;; or fixing things up when they have gone wrong, there should be little
;;; reason to call these functions directly.
;;;
;;; If you did remove tests from Testy's dynamic variables
;;; by de-registering them, they effectively become invisible to many
;;; Testy higher-level functions coming up later in the source code, so don't
;;; do it unless you know what you're doing.
;;;
;;; On the other hand, if you wanted to extend what happens upon registration/
;;; removal of tests, this is a great place to do it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;
;;; 1. Register a test object in the Testy dynamic variable hash-tables ;
;;;    so tests are quickly retrievable by reference to name            ;
;;;    or tag. Return T if we think the operation looks legitimate,     ;
;;;    and NIL if the object you've tried to register isn't a test      ;
;;;    or if a test is already registered by that name.                 ;
;;;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-test (test)
  "Register a test object in Testy's dynamic variables"
  (if (or (not (typep test 'test))
	  (gethash (name test) *test-names*))
	  (return-from register-test nil))

  (setf (gethash (name test) *test-names*) test)
  (loop for tag in (tags test)
     do (hash-ext-array-insert tag test *test-tags*))

  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;
;;; 2. Deregister test objects from Testy's dynamic variables.          ;
;;;    If the object was not a test, or it was not already found in     ;
;;;    Testy's variables, then this function returns nil.               ;
;;;    If it is found, it is removed, and then T is returned.           ;
;;;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun deregister-test (identifier)
  "Remove a test object from Testy's dynamic variables"
  (let ((test (get-test identifier)))
    (if (or (null test)
	    (and (typep test 'sequence)
		 (= (length test) 0)))
	(return-from deregister-test nil))
    
    (remhash (name test) *test-names*)
    
    (loop
       for tag in (tags test)
       do (hash-ext-array-remove tag test *test-tags*))
    T))


(defun deregister-tests (&optional (test-sequence (all-tests)))
  "Remove multiple test objects from Testy's dynamic variables"
  (loop
     for test across test-sequence
     for i = 1 then (incf i)
     do (deregister-test test)
     finally (return i)))
