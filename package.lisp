;;;; package.lisp

(defpackage :testy
  (:use #:cl)
  (:export
   :qtest
   :run-test
   :make-test
   :run-tests
   :all-tests
   :serialise-test
   :serialise-tests))
