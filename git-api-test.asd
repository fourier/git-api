#|
  This file is a part of git-api project.
  Copyright (c) 2016 Alexey Veretennikov (alexey.veretennikov@gmail.com)

  Test package.
  Usage:
  (ql:quickload :git-api-test)
  (asdf/operate:test-system :git-api)

  In order to perform code coverage with these tests (currenty supported
  only on LispWorks 7), run the following:
  (asdf/operate:operate 'git-api-test-asd:coverage-op :git-api-test)
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
                 (:file "coverage")
                 (:test-file "pack-test")
                 (:test-file "utils-test"))))
  :description "Test system for git-api"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c))
  :perform (coverage-op (op c)
                        (funcall (intern #.(string :run-tests-with-coverage) :git-api.test.coverage))
                        (asdf:clear-system c)))


