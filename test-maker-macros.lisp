;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :testy)

(defmacro test (&key 
		  name
		  description
		  (source +)
		  (expected-value *)
		  (expectation "EQUALP")
		  before-function-source
		  after-function-source
		  type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :source ',source
	      :expectation ,expectation
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-EQ (&key 
		     name
		     description
		     (source +)
		     (expected-value *)
		     before-function-source
		     after-function-source
		     type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQ"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test= (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source
		   type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "="
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-EQL (&key 
		      name
		      description
		      (source +)
		      (expected-value *)
		      before-function-source
		      after-function-source
		      type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-EQUAL (&key 
			name
			description
			(source +)
			(expected-value *)
			before-function-source
			after-function-source
			type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUAL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-EQUALP (&key 
			 name
			 description
			 (source +)
			 (expected-value *)
			 before-function-source
			 after-function-source
			 type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUALP"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-NULL (&key 
		       name
		       description
		       (source +)
		       (expected-value nil)
		       before-function-source
		       after-function-source
		       type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "NULL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-NOT-NULL (&key 
			   name
			   description
			   (source +)
			   (expected-value *)
			   before-function-source
			   after-function-source
			   type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "NOT-NULL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-condition (&key 
			    name
			    description
			    source 
			    (expected-value 'condition)
			    before-function-source
			    after-function-source
			    type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "CONDITION"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))

(defmacro test-error (&key 
			name
			description
			source 
			(expected-value 'error)
			before-function-source
			after-function-source
			type-of-test)
  `(make-test :name ,name
	      :description ,description
	      :expectation "CONDITION"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source
	      :type-of-test ,type-of-test))
