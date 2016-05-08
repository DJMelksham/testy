;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The test-making macros wrap thinly around the make-test function and are
;;; designed for a quick interactive technique of test authorship at the REPL.
;;;
;;; The general idea behind the test macros is a workflow that is somewhat
;;; different to the traditional pattern of TDD design, which usually deems you
;;; should define your tests up front, have them fail, and then write code until
;;; your tests pass.
;;;
;;; But I argue the test-macro philosophy is more applicable to how I
;;; would develop programs in LISP, given that I have a fully operational REPL
;;; available to me, the glorious compiler of SBCL/slime/emacs, designs of my
;;; domain often cannot be established up front, and I now have a full
;;; test producing and automation framework.
;;;
;;; Honestly, I'm not sold that TDD's test-fail-code-pass cycle leads to
;;; inherently better design at all (you can, after all, write tests that
;;; fail and then pass for really badly designed code).  Indeed
;;; I think that design and testing are two conceptually unrelated ideas,
;;; or more realistically, that real-life programmers will highly correlate
;;; doing both activities well or poorly, regardless of which they
;;; attempt to do first.  A bad or clueless designer in their domain,
;;; writing tests first, will produce a badly designed, and "thoroughly tested",
;;; pile of crap if they manage to produce anything at all.
;;;
;;; Philosophical arguments and nuances aside, dare I say it, I think the LISP
;;; way of experimenting with your code and design at the REPL is better, not
;;; least of all because I have not encountered a single interesting or complex
;;; problem that allows you to formulate your design up front.
;;; If your problem is so simple that you may design its solution from the get
;;; go, realistically you have no problem at all.
;;;
;;; But, I think there is a perfectly valid point in that not enough people
;;; test their code formally, or automate their tests sufficiently if they
;;; test manually at the REPL.  
;;;
;;; The test macros thus assume the following general workflow:
;;;
;;; - Assume the user has forms and code that they are passing to and
;;;   and testing in the REPL, being aware that they know what proper
;;;   behaviour would look like when they do so.
;;;
;;; - Once proper behaviour has been achieved via testing in the REPL,
;;;   what you are likely to have is a LISP form, evaluated at the REPL,
;;;   with a subsequent return value that represents a working example of
;;;   correct output for that piece of code. 
;;;
;;; - This is where morale usually breaks down: testing at this point in
;;;   time is "too much work".  And maybe it is if you haven't settled on
;;;   a final design or definition of what it means to have working code yet.
;;;   But if you do have working code and you are quite happy with its form,
;;;   its passing case is sitting there waiting to be captured, automated,
;;;   and documented.
;;;
;;; The testing macros thus take as their default "source" and "expected-value"
;;; arguments the last evaluated form, and the last returned value.
;;;
;;; Even name and description, if you are so precious/irresponsible as to
;;; think yourself above having to worry about such things while programming,
;;; will automatically be filled out if you now call the test-making macros
;;; (albeit, not very well, but, maybe, better auto-filled fields and tests
;;; than no tests at all).
;;;
;;; The equality predicate used in the test (which tests the returned value of
;;; future runs of the test against the returned value recorded at this point)
;;; is contained in the name of the test-making-macro itself, with the
;;; exception of (test) which defaults to the rather generic but safe EQUALP
;;; predicate if you don't want to even think about that.
;;;
;;; Thus, if you have working code that you have developed, and you evaluate
;;; it at the REPL, you can create working tests that capture
;;; that working state, in a test, with one tiny function call at the REPL
;;; immediately following the evaluation of your working code.
;;;
;;; You can also use the test-making macros to define regular tests at
;;; the REPL without the need to worry about quoting
;;; your source code or supplying arguments to the "expectation"
;;; parameter of make-test.
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
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
  "A test-making macro that helps make quick tests"
  `(make-test :name ,name
	      :description ,description
	      :expectation "CONDITION"
	      :source ',source
	      :expected-value ',expected-value
	      :tags ,tags
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))
