(in-package :cl-user)
(defpackage testy
  (:use :cl))
(in-package :testy)

(ql:quickload 'cl-fad)

(defun relative-load (filename)
  (load (merge-pathnames filename *load-truename*)))

(relative-load "variable-definitions.lisp")
(relative-load "test-object-definition.lisp")
(relative-load "test-runner.lisp")
(relative-load "utilities.lisp")
(relative-load "test-makers.lisp")
(relative-load "statistics-generation.lisp")
(relative-load "context-object-definition.lisp")
(relative-load "test-macros.lisp")
