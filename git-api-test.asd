#|
  This file is a part of git-api project.
  Copyright (c) 2016 Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

(in-package :cl-user)
(defpackage git-api-test-asd
  (:use :cl :asdf))
(in-package :git-api-test-asd)

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
                    (asdf:clear-system c)))
