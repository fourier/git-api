(in-package :cl-user)
(defpackage utils-test
  (:use :cl
        :git-api.utils
        :prove))
(in-package :utils-test)


(defvar *test-data-path* (fad:merge-pathnames-as-directory (asdf:system-relative-pathname :git-api-test #P"t/") #P"data/"))

(defparameter *read-one-line-contents* #(65 32 116 101 115 116 32 111 102 32 111 110 101 32 108 105 110 101 32 114 101 97 100 105 110 103 32 102 114 111 109 32 116 104 101 32 102 105 108 101 46 10 84 104 101 32 114 101 115 116 32 115 104 111 117 108 100 32 110 111 116 32 98 101 32 114 101 97 100 46 10)
  "Contents of the readoneline.txt")

(defun testfile (filename)
  (merge-pathnames *test-data-path* filename))

;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.

(plan nil)

(is (read-one-line (testfile "readoneline.txt"))
     "A test of one line reading from the file."
     "Test of read-one-line")

(is (file-size (testfile "readoneline.txt")) (length *read-one-line-contents*)
    "Test of file-size")

(is (read-binary-file (testfile "readoneline.txt"))
    *read-one-line-contents*
    :test #'equalp
    "Test of read-binary-file")

(subtest "Testing read-header"
  (is (read-header (testfile "readoneline.txt") 5)
      (subseq *read-one-line-contents* 0 5)
      :test #'equalp
      "Test of read-header: read first 5 bytes")
  (is (read-header (testfile "readoneline.txt") (+ 10 (length *read-one-line-contents*)))
      *read-one-line-contents*
      :test #'equalp
      "Test of read-header: read first more than size of file"))
    

(finalize)
