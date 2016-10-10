;;;; base.lisp
;; File containing common settings for all unit tests
;; 
(in-package :cl-user)
(defpackage git-api.test.base
  (:use :cl
        :prove)
  (:export testfile))
   
(in-package :git-api.test.base)

;; turn off ansi colors in report output
(setf prove.color:*enable-colors* nil)
;; change type of the reporter to Test Anything Protocol
(setf prove:*default-reporter* :tap)

(defvar *test-data-path* (fad:merge-pathnames-as-directory (asdf:system-relative-pathname :git-api-test #P"t/") #P"data/"))

(defun testfile (filename)
  (merge-pathnames *test-data-path* filename))
