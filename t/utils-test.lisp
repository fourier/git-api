;;;; utils-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.utils-test
  (:use :cl
        :git-api.test.base
        :git-api.utils
        :prove))
(in-package :git-api.test.utils-test)


(defvar *test-data-path* (fad:merge-pathnames-as-directory (asdf:system-relative-pathname :git-api-test #P"t/") #P"data/"))

(defparameter *read-one-line-contents* #(65 32 116 101 115 116 32 111 102 32 111 110 101 32 108 105 110 101 32 114 101 97 100 105 110 103 32 102 114 111 109 32 116 104 101 32 102 105 108 101 46 10 84 104 101 32 114 101 115 116 32 115 104 111 117 108 100 32 110 111 116 32 98 101 32 114 101 97 100 46 10)
  "Contents of the readoneline.txt")

(defparameter *sha-sequence-1* '(00 00 65 32 116 101 115 116 32 111 102 32 111 110 101 32 108 105 110 101 32 114))
(defparameter *sha-sequence-1-hex* "412074657374206f66206f6e65206c696e652072")

(defparameter *sha-sequence-2* '(00 00 #xf0 #x0d #x0e #xec #x43 #xec #x0e #x28 #xd1 #x33 #xba #xe8 #xa2 #x97 #xf8 #x1e #x23 #x78 #x22 #x09))
(defparameter *sha-sequence-2-hex* "f00d0eec43ec0e28d133bae8a297f81e23782209")


(defun testfile (filename)
  (merge-pathnames *test-data-path* filename))


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


(subtest "Testing sha1-to-hex"
  (is (flexi-streams:with-input-from-sequence (seq (subseq *sha-sequence-1* 2))
        (sha1-to-hex seq))
      *sha-sequence-1-hex*
      "Stream to hex test 1")
  (is (flexi-streams:with-input-from-sequence (seq (subseq *sha-sequence-2* 2))
        (sha1-to-hex seq))
      *sha-sequence-2-hex*
      "Stream to hex test 2")
  (is (sha1-to-hex *sha-sequence-1* 2)
      *sha-sequence-1-hex*
      "List to hex test 1")
  (is (sha1-to-hex *sha-sequence-2* 2)
      *sha-sequence-2-hex*
      "List to hex test 2")
  (is (sha1-to-hex
       (make-array 22 :element-type '(unsigned-byte 8) :initial-contents *sha-sequence-1*) 2)
       *sha-sequence-1-hex*
       "Array to hex test 1")
  (is (sha1-to-hex
       (make-array 22 :element-type '(unsigned-byte 8) :initial-contents *sha-sequence-2*) 2)
       *sha-sequence-2-hex*
       "Array to hex test 2")
  (is (sha1-hex-to-array *sha-sequence-1-hex*)
      (make-array 20 :element-type '(unsigned-byte 8) :initial-contents (subseq *sha-sequence-1* 2))
      :test #'equalp
      "Hex string to array test 1")
  (let* ((result (make-array 20 :element-type '(unsigned-byte 8)))
         (expected-result (make-array 20 :element-type '(unsigned-byte 8) :initial-contents (subseq *sha-sequence-2* 2)))
         (actual-result (sha1-hex-to-array *sha-sequence-2-hex* result)))
    (is-type actual-result '(simple-array (unsigned-byte 8) (20))
             "Hex string to array test 2 - result should be an array of size 20")
    (is actual-result
        expected-result
        :test #'equalp
      "Hex string to array test 3 - result array given, check return value")
    (is result
        expected-result
        :test #'equalp
      "Hex string to array test 4 - check if result array is hte same as expected result")))

      
  
  

(finalize)
