;;;; repo-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.repo-test
  (:use :cl
        :alexandria
        :git-api.test.base
        :git-api.utils
        :git-api.repo
        :prove))

(in-package git-api.test.repo-test)

;; import unexterned(private for package) functions
(from nibbles import write-ub64/be write-ub32/be)
(from git-api.utils import sha1-to-hex)


(plan nil)

(subtest "Smoke test of the example repository"
  (let* ((repo (make-git-repo (namestring (testfile "example-repo")))))
    (is-type repo 'git-api.repo::git-repo "Check if we able to create repository object")
    (let ((commits-tree (get-commit-tree repo (get-head-commit repo))))
      (is-type commits-tree 'hash-table "Check if we able to create a hash table of commits")
      (is (hash-table-count commits-tree) 3 "Check the number of commits")
      (is-type (gethash "cb96e53d08dbfc0d358c5f312029aecaf584a390" commits-tree) 'git-api.object:commit
               "Check commit 1")
      (is-type (gethash "a2194f882da560df01357af06a4a7cc91614ee94" commits-tree) 'git-api.object:commit
               "Check commit 2")
      (is-type (gethash "0ec3337eede3a64aaed50f737adf163e9f8d92dc" commits-tree) 'git-api.object:commit
               "Check commit 3"))))


(finalize)
