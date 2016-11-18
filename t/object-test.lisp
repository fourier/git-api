;;;; object-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.object-test
  (:use :cl
        :alexandria
        :git-api.test.base
        :git-api.utils
        :git-api.object
        :prove))

(in-package git-api.test.object-test)

;; import unexterned(private for package) functions
(from nibbles import write-ub64/be write-ub32/be)
(from git-api.utils import sha1-to-hex)
      


(finalize)
