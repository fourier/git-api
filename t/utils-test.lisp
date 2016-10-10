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


(defparameter *binary-file-contents* #(11 254 217 187 174 147 78 237 39 142 185 14 115 180 215 10 231 38 250 27 196 134 119 20 161 85 86 185 24 28 103 110 142 20 59 126 103 36 48 203 95 24 76 22 26 247 254 150 174 87 83 131 0 164 115 135 231 240 39 250 114 24 242 237 209 84 131 200 201 3 126 212)
  "Contents of the binary.dat")

(defparameter *sha-sequence-1* '(00 00 65 32 116 101 115 116 32 111 102 32 111 110 101 32 108 105 110 101 32 114))
(defparameter *sha-sequence-1-hex* "412074657374206f66206f6e65206c696e652072")

(defparameter *sha-sequence-2* '(00 00 #xf0 #x0d #x0e #xec #x43 #xec #x0e #x28 #xd1 #x33 #xba #xe8 #xa2 #x97 #xf8 #x1e #x23 #x78 #x22 #x09))
(defparameter *sha-sequence-2-hex* "f00d0eec43ec0e28d133bae8a297f81e23782209")


(plan nil)

(is (read-one-line (testfile "readoneline.txt"))
     "A test of one line reading from the file."
     "Test of read-one-line")

(is (file-size (testfile "binary.dat")) (length *binary-file-contents*)
    "Test of file-size")

(is (read-binary-file (testfile "binary.dat"))
    *binary-file-contents*
    :test #'equalp
    "Test of read-binary-file")

(subtest "Testing read-header"
  (is (read-header (testfile "binary.dat") 5)
      (subseq *binary-file-contents* 0 5)
      :test #'equalp
      "Test of read-header: read first 5 bytes")
  (is (read-header (testfile "binary.dat") (+ 10 (length *binary-file-contents*)))
      *binary-file-contents*
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
