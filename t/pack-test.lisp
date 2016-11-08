;;;; pack-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.pack-test
  (:use :cl
        :alexandria
        :git-api.test.base
        :git-api.utils
        :git-api.pack
        :prove))

(in-package git-api.test.pack-test)

;; import unexterned(private for package) functions
(from nibbles import write-ub64/be write-ub32/be)
(from git-api.pack import read-network-vli read-delta-vli)
(from git-api.pack import read-pack-entry-header)
(from git-api.pack import parse-index-file)
(from git-api.pack import
      pack-filename-to-index
      index-filename-to-pack
      incorrect-file-name-error
      corrupted-index-file-error)
(from git-api.pack import read-offsets read-fanout-table)
;; deltas
(from git-api.pack import decode-delta-copy-cmd apply-delta)

(defparameter +network-vli-tests+
  '((240 128 112) 
    (306 129 50) 
    (260 129 4) 
    (1006 134 110) 
    (385 130 1) 
    (24091 128 187 27) 
    (736 132 96) 
    (217215 140 159 127) 
    (217106 140 159 18) 
    (581215 162 187 95) 
    (401277 151 189 125) 
    (1244382 202 248 94) 
    (1422664 213 233 72) 
    (1856515 240 167 3) 
    (399141 151 173 37) 
    (1769209 234 252 121) 
    (1900855 243 129 55) 
    (523882 158 251 106) 
    (2711024 128 164 186 112) 
    (2763065 128 167 209 57) 
    (2816187 128 170 240 59) 
    (160111 136 225 111) 
    (2199442 128 133 158 18) 
    (2210863 128 133 247 47) 
    (629324 165 179 76) 
    (250884 142 167 4) 
    (2532326 128 153 198 102))
  "Test data for read-network-vli function")

(defparameter +delta-vli-tests+
  '((356 228 2) 
    (3374 174 26) 
    (1014 246 7) 
    (1553 145 12) 
    (31895 151 249 1) 
    (6217 201 48) 
    (321 193 2) 
    (20157 189 157 1) 
    (8113 177 63) 
    (29519 207 230 1)
    (624 240 4) 
    (755 243 5) 
    (743 231 5) 
    (360 232 2) 
    (333 205 2) 
    (498 242 3))
  "Test data for read-delta-vli function")

(plan nil)

(defmacro stream-readers-test (description function test-data)
  `(subtest ,description
     (loop for test-case in ,test-data
           for value = (car test-case)
           and stream = (flexi-streams:make-in-memory-input-stream (cdr test-case))
           do
           (is value (,function stream) (format nil "reading value ~a" value)))))


(stream-readers-test "Testing read-network-vli" read-network-vli +network-vli-tests+)
(stream-readers-test "Testing read-delta-vli" read-delta-vli +delta-vli-tests+)


(subtest "Testing pack-filename-to-index"
  (let ((pack1 "//some/weird-filename1.pack")
        (pack2 "//some/weird filename2.PACK")
        (pack3 "weird filename3.PackK")
        (pack4 "completely weird filename"))
    (is (pack-filename-to-index pack1) "//some/weird-filename1.idx" :test #'string=
        "normal case")
    (is (pack-filename-to-index pack2) "//some/weird filename2.idx" :test #'string=
        "upper-case extension")
    (is-error (pack-filename-to-index pack3) 'incorrect-file-name-error
        "too long extension")
    (is-error (pack-filename-to-index pack4) 'incorrect-file-name-error
        "completely weird filename")))

(subtest "Testing index-filename-to-pack"
  (let ((idx1 "//some/weird-filename1.idx")
        (idx2 "//some/weird filename2.IDX")
        (idx3 "weird filename3.iDXX")
        (idx4 "completely weird filename"))
    (is (index-filename-to-pack idx1) "//some/weird-filename1.pack" :test #'string=
        "normal case")
    (is (index-filename-to-pack idx2) "//some/weird filename2.pack" :test #'string=
        "upper-case extension")
    (is-error (index-filename-to-pack idx3) 'incorrect-file-name-error
        "too long extension")
    (is-error (index-filename-to-pack idx4) 'incorrect-file-name-error
        "completely weird filename")
    (is idx1 (pack-filename-to-index (index-filename-to-pack idx1)) :test #'string=
        "conversion from index to pack and back")))

(subtest "Testing parse-index-file"
  ;; catch the error condition
  (is-error (parse-index-file (testfile "binary.dat")) 'corrupted-index-file-error
            "Test raised condition on corrupted file")
  ;; parse file
  (multiple-value-bind (offsets index)
      (parse-index-file (testfile "test.idx"))
    ;; verify what return values are not empty
    (isnt offsets nil "check returned offsets not nil")
    (isnt index nil "check returned index not nil")
    ;; read the index from pre-parsed data
    (let ((saved-index
           (with-open-file (s (testfile "test_idx.sexp") :direction :input)
             (read s))))
      (is index saved-index "check index read is the same as expected"
          :test #'equalp))
    (let ((expected-offsets (make-hash-table :test #'eq :size (length index))))
      (loop for (x . y) across index
            do (setf (gethash x expected-offsets) y))
      (is offsets expected-offsets "check offsets are the same as in index array"
          :test #'equalp))))


(subtest "Testing read-fanout-table"
  ;; prepare the test data
  (let* ((fanout-table ; the array with encoded 256 numbers
          (make-array 256 :initial-contents
                      (loop for i from 0 below 256 collect (random (ash 2 30)))))
         (encoded-array
          (flexi-streams:with-output-to-sequence (stream) ; encode to in-memory stream 
            (loop for x across fanout-table
                  do (write-ub32/be x stream)))))
    ;; now reopen the test data as a stream
    (flexi-streams:with-input-from-sequence (stream encoded-array)
      (is (read-fanout-table stream) fanout-table "check random 256 values in fanout table"
          :test #'equalp))))


(defun create-small-random-offsets (size)
  (make-array size :initial-contents
              (loop for i from 0 below size collect (random (ash 2 30)))))

(defun create-big-random-offsets (size)
  (make-array size :initial-contents  
              (loop for i from 0 below size collect (random (ash 2 63)))))

(subtest "Testing read-offsets"
  ;; read-offsets
  (let* ((size-smalls 2)
         (size-bigs 1)
         (table-small (create-small-random-offsets size-smalls))
         (table-big (create-big-random-offsets size-bigs)))
    (declare (ignore table-big))
    ;; encode smalls into the stream
    (let ((table 
           (flexi-streams:with-output-to-sequence (stream)
             (loop for x across table-small do (write-ub32/be x stream)))))
      (flexi-streams:with-input-from-sequence (stream table)
        (is (read-offsets stream size-smalls) table-small
            "check simple table with values < 2^31"
            :test #'equalp)))
    ;; test of small offsets + big offsets
;;    (let ((order (random-shuffle (iota (+ size-smalls size-bigs)))
    (skip 1 "TODO: reimplement large offsets handling and enable this test")
    #|
    (let ((table 
           (flexi-streams:with-output-to-sequence (stream)
             (loop for x across table-small do (write-ub32/be x stream))
             (loop for i below size-bigs do (write (logior (ash 1 31) i)))
             (loop for x across table-big do (write-ub64/be x stream)))))
      (flexi-streams:with-input-from-sequence (stream table)
        (is (read-offsets stream (+ size-smalls size-bigs)) table-small :test #'equalp)))))
    |#
    ))



(subtest "Testing decode-delta-copy-cmd"
  (let* ((size-bytes #(#x00 #xaa #x00 #x00))
         (offset-bytes #(#x00 #x00 #xcc #xdd))
         (size-encoded-bits #b010)
         (offset-encoded-bits #b1100)
         ;; the data itself
         (data (vector (logior #x80 (ash size-encoded-bits 4) offset-encoded-bits)
                       #xcc #xdd ; first parts of offset
                       #xaa)))   ; and then parts of size
    (multiple-value-bind (new-pos offset size)
        (decode-delta-copy-cmd data 0)
      (is new-pos 3 "Check if new position is 3")
      (is offset (nibbles:ub32ref/le offset-bytes 0) "check if decoded offset is correct")
      (is size (nibbles:ub32ref/le size-bytes 0) "check if decoded size is correct"))))


(subtest "Testing apply-delta"
  (fail "Not implemented"))

(finalize)
