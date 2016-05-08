;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The test-making macros wrap thinly around the make-test function and are
;;; designed for quick interactive test authorship via the REPL.
;;;
;;; The general idea behind the test-maker macros is a workflow somewhat
;;; different to the traditional pattern of TDD design, which suggests you
;;; should define your tests up front, have them fail, write the simplest code
;;; possible until your tests pass, and then "refactor".
;;;
;;; I argue the test-macro philosophy is more applicable to how I
;;; would develop programs in LISP given a fully operational REPL,
;;; paradigm-agnostic code options, the compiler of SBCL,
;;; the slime/emacs IDE, and given that proper designs in domains I work
;;; with usually cannot be established up front.
;;;
;;; And I now have a test producing and automation framework.
;;;
;;; Honestly, I'm not sold that TDD's test-fail-code-pass-refactor cycle leads
;;; to inherently better design at all (you can, after all, write horrible tests
;;; that fail and then pass for really badly designed code).  Indeed
;;; I think that design and testing in practice are often two conceptually
;;; unrelated ideas. More realistically, real-life programmers highly correlate
;;; doing both activities well or poorly, regardless of which they
;;; attempt to do first. I posit a bad or clueless programmer will likely
;;; be a poor designer in the target domain.  Following TDD, they will likely
;;; write poor tests first, and then produce a badly designed and
;;; "thoroughly tested" pile of crap, if they manage to produce anything at all
;;; in a particular complex programming domain.
;;;
;;; Philosophical arguments and nuances aside, dare I say it, I think the LISP
;;; way of experimenting with your code and design at the REPL is better,
;;; all other things being equal, not least of all because I have not encountered
;;; a single interesting or complex problem that allows you to formulate your
;;; design up front.  If your problem is so simple that you may design its solution
;;; from the get go, realistically, you barely have a problem at all.
;;;
;;; But, I think there is a perfectly valid point in that not enough people
;;; test their code formally, or automate their tests sufficiently if they
;;; test manually at the REPL.  
;;;
;;; The test macros thus assume the following general workflow:
;;;
;;; - Assume the user has forms and code they are passing to and
;;;   testing in the REPL, being aware what correct
;;;   behaviour would look like when they do so.
;;;
;;; - Once correct behaviour has been achieved via testing in the REPL,
;;;   what you are likely to have is a LISP form, evaluated at the REPL,
;;;   with a subsequent return value that represents a working example of
;;;   correct output for that piece of code. 
;;;
;;; - Testing at this point in time is often deemed "too much work".
;;;   And maybe it is if you haven't settled on a final design or definition
;;;   of what it means to have working code.
;;;   But if you do have working code and you are quite happy with its form,
;;;   correct case are likely sitting there in the REPL history  waiting to
;;;   be captured, automated, and documented.
;;;
;;; The testing macros thus take as their default "source" and "expected-value"
;;; arguments the last evaluated form, and the last returned value. Allowing
;;; you go verify code compiles, evaluates, and works in the REPL, and then be
;;; consumed straight into test creation.
;;;
;;; Even name and description, if you are so precious/irresponsible as to
;;; think yourself above having to worry about such things while programming,
;;; will automatically be filled out if you now call the test-making macros
;;; (albeit, not very well, but, maybe, better auto-filled fields and tests
;;; than no tests at all).
;;;
;;; The equality predicate used in the test (which tests the returned value of
;;; future runs against the returned value recorded at this point)
;;; is contained in the name of the test-making-macro itself, with the
;;; exception of (test), which defaults to the rather generic but safe EQUALP
;;; predicate if you don't want to have to think about such things.
;;;
;;; Thus, if you have working code that you have developed, and you evaluate
;;; it at the REPL, you can create working tests that capture
;;; that working state, in a test, with one function call at the REPL
;;; immediately following the evaluation of your working code.
;;;
;;; You can also use the test-making macros to define regular tests at
;;; the REPL without the need to worry about quoting
;;; source code or supplying arguments to the "expectation"
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
