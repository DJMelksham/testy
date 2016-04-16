(in-package :cl-user)
(defpackage cl-project-manager
  (:use :cl))
(in-package :cl-project-manager)

(ql:quickload 'cl-fad)

(defun relative-load (filename)
  (load (merge-pathnames filename *load-truename*)))

(relative-load "git-management.lisp")
(relative-load "utility.lisp")
(relative-load "variable-definitions.lisp")
(relative-load "document-functions.lisp")
(relative-load "file-management.lisp")
(relative-load "config-low-level-functions.lisp")
(relative-load "module-management.lisp")
(relative-load "test-variable-definitions.lisp")
(relative-load "test-object-definition.lisp")
(relative-load "test-runner.lisp")
(relative-load "test-utilities.lisp")
(relative-load "test-makers.lisp")
(relative-load "test-stats.lisp")
(relative-load "test-context-definition.lisp")
(relative-load "test-testmacros.lisp")

;(relative-load "project-manager.lisp")
;(relative-load "test-management.lisp")
