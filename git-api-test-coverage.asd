#|
  This file is a part of git-api project.
  Copyright (c) 2016 Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

;; Executes git-api tests with the test coverage on LispWorks 7.x
;; Usage:
;; (ql:quickload :git-api-test-coverage
;; (asdf/operate:test-system :git-api-test-coverage)

(in-package :cl-user)
(defpackage git-api-test-coverage-asd
  (:use :cl :asdf))
(in-package :git-api-test-coverage-asd)

(defsystem git-api-test-coverage
  :author "Alexey Veretennikov"
  :license "BSD"
  :depends-on (:git-api
               :git-api-test)
  :components ((:module "t"
                :components
                ((:file "coverage"))))
  :description "Run tests with test coverage Test system for git-api"

  :perform (test-op :after (op c)
                    ;;(funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (funcall (intern #.(string :run-tests-with-coverage) :git-api.test.coverage))
                    (asdf:clear-system c)))
