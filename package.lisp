;;;; package.lisp

(defpackage :testy
  (:use #:cl)
  (:export
   :load-tests
   :load-test
   :deregister-test
   :deregister-tests
   :qtest
   :run-test
   :make-test
   :run-tests
   :all-tests
   :serialise-test
   :serialise-tests
   :stat-number-tests))
