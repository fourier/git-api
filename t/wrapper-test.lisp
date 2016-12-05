;;;; wrapper-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.wrapper-test
  (:use :cl
        :alexandria
        :git-api.utils
        :git-api.test.base
        :git-api.zlib.wrapper
        :prove))

(in-package git-api.test.wrapper-test)

;; import unexterned(private for package) functions
(from git-api.zlib.wrapper import uncompress-git-file uncompress-git-file-cffi)

(defparameter +big-data-uncompressed+
  #(98 108 111 98 32 55 57 0 109 121 32 118 101 114 121 32 108 101 110 103 116 104 121 32 115 116 114 105 110 103 46 32 65 112 112 97 114 101 110 116 108 121 32 116 111 32 101 120 99 101 101 100 32 116 104 101 32 51 50 32 98 121 116 101 115 32 115 105 122 101 32 111 102 32 116 111 116 97 108 32 97 114 99 104 105 118 101))

(defparameter +small-data-uncompressed+
  #(98 108 111 98 32 49 54 0 119 104 97 116 32 105 115 32 117 112 44 32 100 111 99 63))
  
(plan nil)

(subtest "Test of the uncompress-git-file-zlib"
  (let ((git-api.zlib.cffi:*zlib-loaded* nil))
    (is (uncompress-git-file (testfile "example-objects/small-git-object.dat"))
        +small-data-uncompressed+ "Test uncompress-git-file with CL zlib on small file" :test #'equalp)
    (is (uncompress-git-file (testfile "example-objects/big-git-object.dat"))
        +big-data-uncompressed+ "Test uncompress-git-file with CL zlib on big file" :test #'equalp)))

(subtest "Test of the uncompress-git-file-cffi"
  (when git-api.zlib.cffi:*zlib-loaded* t
    (is (uncompress-git-file (testfile "example-objects/small-git-object.dat"))
        +small-data-uncompressed+ "Test uncompress-git-file with cffi-zlib on small file" :test #'equalp)
    (is (uncompress-git-file (testfile "example-objects/big-git-object.dat"))
        +big-data-uncompressed+ "Test uncompress-git-file with cffi-zlib on big file" :test #'equalp)))


(finalize)
