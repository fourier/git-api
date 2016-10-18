#|
  This file is a part of git-api project.
  Copyright (c) 2016 Alexey Veretennikov (alexey.veretennikov@gmail.com)

  Test package.
  Usage:
  (ql:quickload :git-api-test)
  (asdf/operate:test-system :git-api)

  In order to perform code coverage with these tests (currenty supported
  only on LispWorks 7), run the following:
  (asdf/operate:operate 'git-api-test-asd::coverage-op :git-api-test)
|#



(in-package :cl-user)
(defpackage git-api-test-asd
  (:use :cl :asdf)
  (:export coverage-op))

(in-package :git-api-test-asd)


(defclass coverage-op (selfward-operation)
  ((selfward-operation :initform 'load-op :allocation :class))
  (:documentation "Test coverage operation"))


(defsystem git-api-test
  :author "Alexey Veretennikov"
  :license "BSD"
  :depends-on (:git-api
               :cl-fad
               :flexi-streams
               :prove)
  :components ((:module "t"
                :components
                ((:file "base")
                 (:test-file "pack-test")
                 (:test-file "utils-test"))))
  :description "Test system for git-api"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c))
  :perform (coverage-op (op c)
                        (run-tests-with-coverage)
                        (asdf:clear-system c)))


#+lispworks7
(defun generate-coverage-output-path ()
  (multiple-value-bind (second minute hour date month year);; day)
      (get-decoded-time)
    (let ((results-directory-name
           (pathname
            (format nil "git-api-coverage_~4,'0d-~2,'0d-~2,'0d_~2,'0d_~2,'0d_~2,'0d/index.html"
                    year month date hour minute second))))
      
      (merge-pathnames results-directory-name (hcl:get-temp-directory)))))

  
#+lispworks7
(defun run-lw-test-coverage ()
  (hcl:clear-code-coverage)
  (hcl:with-code-coverage-generation ()
    (asdf/operate:load-system :git-api :force t))
  (asdf/operate:test-system :git-api-test)
  (let ((output-file (generate-coverage-output-path)))
    (hcl:code-coverage-data-generate-coloring-html output-file)
    (format *standard-output* "Generated coverage report to ~a" output-file)
    #+macosx
    (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "openURL:"
                 (objc:invoke "NSURL" "URLWithString:"
                              (concatenate 'string "file://" (namestring output-file))))))


(defun run-tests-with-coverage ()
  #+lispworks7
  (run-lw-test-coverage)
  #-lispworks7
  (error "Code coverage generation currently supported only on LispWorks 7 and above"))
