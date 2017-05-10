;;;; filemasks-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.filemasks-test
  (:use :cl
        :git-api.utils
        :git-api.test.base
        :git-api.plumbing.helpers.details.filemasks
        :prove))
(in-package :git-api.test.filemasks-test)

(plan nil)

(subtest "Testing wildcard-to-regex"
  (is (wildcard-to-regex "Photo*.jpg")
      "^Photo.*\\.jpg$"
      "Testing input: Photo*.jpg")
  (is (wildcard-to-regex "Photo*.jpg" :case-sensitive-p nil) 
      "(?i)^Photo.*\\.jpg$"
      "Testing input: Photo*.jpg case-insensitive"))

(finalize)
