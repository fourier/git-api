;;;; pack-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.pack-test
  (:use :cl
        :git-api.pack
        :prove))
(in-package :pack-test)



(plan nil)

;; blah blah blah.

(is 1 1)

(finalize)
