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
		  tags
		  before-function-source
		  after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :source ',source
	      :expectation ,expectation
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-EQ (&key 
		     name
		     description
		     (source +)
		     (expected-value *)
		     tags
		     before-function-source
		     after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQ"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test= (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		   tags
		   before-function-source
		   after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "="
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-EQL (&key 
		      name
		      description
		      (source +)
		      (expected-value *)
		      tags
		      before-function-source
		      after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQL"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-EQUAL (&key 
			name
			description
			(source +)
			(expected-value *)
			tags
			before-function-source
			after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUAL"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-EQUALP (&key 
			 name
			 description
			 (source +)
			 (expected-value *)
			 tags
			 before-function-source
			 after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUALP"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-NULL (&key 
		       name
		       description
		       (source +)
		       (expected-value nil)
		       tags
		       before-function-source
		       after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "NULL"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-NOT-NULL (&key 
			   name
			   description
			   (source +)
			   (expected-value *)
			   tags
			   before-function-source
			   after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "NOT-NULL"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-condition (&key 
			    name
			    description
			    source 
			    (expected-value 'condition)
			    tags
			    before-function-source
			    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "CONDITION"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro test-error (&key 
			name
			description
			source 
			(expected-value 'error)
			tags
			before-function-source
			after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "CONDITION"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))
