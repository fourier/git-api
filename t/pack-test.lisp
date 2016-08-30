(in-package :cl-user)
(defpackage pack-test
  (:use :cl
        :git-api.pack
        :prove))
(in-package :pack-test)

;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.

(plan nil)

;; blah blah blah.

(is 1 1)

(finalize)
