;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Testy - A Testing Framework and a Triple Entendre in One!
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-tags (test)
  "Get a list of applicable tags from a test"

)

(defun add-tags (test-identifiers tags)
  "Add tag/tags to tests"
  
)


(defun remove-tags (test-identifiers tags)
  "Remove tag/tags from tests"

)


(defun get-source (test)
"Get the source-code of a test"

)

(defun set-source (test source)
"Set the source code of a test"

)

(defun get-after-source (test)
  "Get the after-function source code of a test"
  
  )

(defun set-after-source (test source)
  "Set the after-function source code of a test"
  
  )

(defun set-after-sources (test-identifiers source)
  "Set the after-function source code of multiple tests"
  
  )

(defun get-before-source (test-name)
  "Get the before-function source code from a test"
  
  )

(defun set-before-source (test-name source)
  "Set the before-function source code of a test"
  
  )

(defun set-before-sources (test-identifiers source)
  "Set the before-function source code of multiple tests"
  
  )

(defun get-names (test-identifiers)

  )

(defun set-name (test new-name)
  "Set/change the name of a test"
  
  )

(defun get-description (test)
  "Get the description of a test"
  
  )

(defun set-description (test description)
  "Set/change the description of a test"
  
  )

(defun get-expectation (test)
  "Get the expectation type of a test"
  
  )

(defun set-expectation (test)
  "Set/change the expectation type of a test"
  
  )

(defun get-run-value (test)
  "Get the latest run-value of a test"
  
  )

(defun get-test-status (test)
  "Get the current status of a test: whether it is T (PASS) or NIL (FAILED)."
  
  )

