(in-package :cl-user)
(defpackage utils-test
  (:use :cl
        :git-api.utils
        :prove))
(in-package :utils-test)

;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.

(plan nil)

;; blah blah blah.

(is 1 1)

(finalize)
