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
      corrupted-index-file-error
      corrupted-pack-file-error)
(from git-api.pack import read-offsets read-fanout-table)
;; deltas
(from git-api.pack import decode-delta-copy-cmd apply-delta)
;; pack file itself
(from git-api.pack import create-pack-entries-table-initial)
;; aux function
(from git-api.utils import sha1-to-hex)


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

(defparameter +delta-result-value+
  #(116 114 101 101 32 56 101 101 55 52 53 49 51 55 99 53 49 57 51 56 49 57 52 56 55 48 100 57
        48 50 99 100 55 52 102 57 98 99 48 98 101 54 49 56 102 10 97 117 116 104 111 114 32 65
        108 101 120 101 121 32 86 101 114 101 116 101 110 110 105 107 111 118 32 60 97 108 101
        120 101 121 46 118 101 114 101 116 101 110 110 105 107 111 118 64 103 109 97 105 108 46
        99 111 109 62 32 49 52 55 56 55 50 48 48 51 52 32 43 48 49 48 48 10 99 111 109 109 105
        116 116 101 114 32 65 108 101 120 101 121 32 86 101 114 101 116 101 110 110 105 107 111
        118 32 60 97 108 101 120 101 121 46 118 101 114 101 116 101 110 110 105 107 111 118 64
        103 109 97 105 108 46 99 111 109 62 32 49 52 55 56 55 50 48 48 51 52 32 43 48 49 48 48
        10 10 73 110 105 116 105 97 108 32 105 109 112 111 114 116 10)
  "Test data for apply-delta test function: the expected result (initial commit since delta
applies from the current towards the oldest value")

(defparameter +delta-delta-value+
  #(135 2 217 1 45 116 114 101 101 32 56 101 101 55 52 53 49 51 55 99 53 49 57 51 56 49 57 52 56
        55 48 100 57 48 50 99 100 55 52 102 57 98 99 48 98 101 54 49 56 102 145 93 67 3 48 51 52
        145 163 76 26 48 51 52 32 43 48 49 48 48 10 10 73 110 105 116 105 97 108 32 105 109 112
        111 114 116 10)
  "Test data for apply-delta test function: the delta itself")

(defparameter +delta-base-value+
  #(116 114 101 101 32 57 54 48 53 55 102 48 97 54 55 102 55 97 100 48 100 51 51 52 56 50 48 54
        56 57 52 49 48 102 100 54 53 97 50 53 101 52 55 99 52 10 112 97 114 101 110 116 32 48 101
        99 51 51 51 55 101 101 100 101 51 97 54 52 97 97 101 100 53 48 102 55 51 55 97 100 102 49
        54 51 101 57 102 56 100 57 50 100 99 10 97 117 116 104 111 114 32 65 108 101 120 101 121
        32 86 101 114 101 116 101 110 110 105 107 111 118 32 60 97 108 101 120 101 121 46 118 101
        114 101 116 101 110 110 105 107 111 118 64 103 109 97 105 108 46 99 111 109 62 32 49 52 55
        56 55 50 48 49 50 50 32 43 48 49 48 48 10 99 111 109 109 105 116 116 101 114 32 65 108 101
        120 101 121 32 86 101 114 101 116 101 110 110 105 107 111 118 32 60 97 108 101 120 101 121
        46 118 101 114 101 116 101 110 110 105 107 111 118 64 103 109 97 105 108 46 99 111 109 62
        32 49 52 55 56 55 50 48 49 50 50 32 43 48 49 48 48 10 10 67 104 97 110 103 101 100 32 116
        101 120 116 10)
  "Test data for apply-delta test function: the base value (most recent commit)")

(defparameter +pack-entries-table-test-data-input+
  (cons 3400
        #((12 . #(170 125 88 188 193 190 42 175 147 74 87 169 203 58 186 107 15 103 88 124))
          (211 . #(210 21 120 108 198 104 190 23 48 254 232 11 226 181 22 86 210 131 245 22))
          (451 . #(215 110 78 238 213 150 237 246 100 169 216 63 218 18 212 248 93 78 247 12))
          (622 . #(235 180 136 136 92 169 146 109 105 199 247 232 24 222 110 204 250 25 51 58))
          (828 . #(181 216 220 163 20 71 80 173 2 90 234 46 119 42 155 75 227 83 162 203))
          (1019 . #(58 131 164 187 77 34 121 139 29 109 205 39 71 234 166 37 11 134 207 39))
          (1206 . #(143 185 84 240 78 42 211 17 210 36 163 218 7 197 77 208 117 215 142 233))
          (1395 . #(4 154 127 33 70 80 131 143 14 225 65 220 78 105 169 138 72 227 131 110))
          (1581 . #(140 72 116 24 232 136 241 161 51 61 12 46 227 158 222 147 88 38 28 95))
          (1764 . #(47 230 155 236 217 187 24 60 42 137 182 128 61 142 80 105 207 102 72 113))
          (1980 . #(105 117 108 40 248 131 29 71 127 160 168 252 69 236 212 6 162 123 115 132))
          (2187 . #(70 189 125 78 236 66 90 134 154 201 214 215 216 146 167 54 62 89 29 209))
          (2375 . #(35 69 68 243 218 171 233 181 171 210 112 41 160 86 154 155 128 150 112 81))
          (2554 . #(229 251 67 170 147 134 28 164 56 217 250 109 48 209 234 68 81 137 191 110))
          (2745 . #(233 236 85 96 214 78 104 22 108 170 89 173 203 28 99 207 43 195 18 187))
          (2942 . #(51 160 204 102 7 187 241 68 175 198 69 193 17 225 67 125 172 155 34 248))
          (3323 . #(208 111 197 43 241 252 140 124 39 197 75 198 249 136 34 252 120 250 43 59))))
  "Test data for the create-pack-entries-table-initial function test")

(defparameter +pack-entries-table-test-data-output+
  '((12 . 199)
    (211 . 240)
    (451 . 171)
    (622 . 206)
    (828 . 191)
    (1019 . 187)
    (1206 . 189)
    (1395 . 186)
    (1581 . 183)
    (1764 . 216)
    (1980 . 207)
    (2187 . 188)
    (2375 . 179)
    (2554 . 191)
    (2745 . 197)
    (2942 . 381)
    (3323 . 57))
  "Contents (values) of the hash table generated by create-pack-entries-table-initial function")

(plan nil)



(subtest "Test conditions"
  (let ((general-condition
         (make-condition 'git-api.pack::pack-error :text "general pack error")))
    (ok (starts-with-subseq "Pack file error:" (format nil "~A" general-condition)))))

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
  (let* ((size-bytes (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#x00 #xaa #x00 #x00)))
         (offset-bytes (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '( #x00 #x00 #xcc #xdd)))
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
  (is-error (apply-delta #(1 2 3 4 5) +delta-delta-value+)
            'corrupted-pack-file-error
            "Test what apply-delta raise an error when the base of incorrect size provided")
  (is-error (apply-delta +delta-base-value+ (concatenate 'vector (subseq +delta-delta-value+ 0 4) #(0 0 0 0 0 0)))
            'corrupted-pack-file-error
            "Test what apply-delta raise an error when incorrect delta-command(0) encountered")
  (is 
   (apply-delta +delta-base-value+ +delta-delta-value+)
   +delta-result-value+
   "Test of commit deltas"
   :test #'equalp))


(subtest "Test of create-pack-entries-table-initial"
  (let ((table (create-pack-entries-table-initial
                 (cdr +pack-entries-table-test-data-input+) ;; table
                 (car +pack-entries-table-test-data-input+)))) ;; size
    (is (hash-table-count table) (length (cdr +pack-entries-table-test-data-input+))
        "Check what the hash table contains all the entries")
    (loop for x across (cdr +pack-entries-table-test-data-input+)
          for y in +pack-entries-table-test-data-output+
          for sha1 = (cdr x)
          do
          (is (gethash sha1 table) y (format nil "check ~a has (offset size) of ~a" (sha1-to-hex sha1) y)))))


(subtest "Test of read-pack-entry-header"
  (let ((test1 '(149 236 3)))
    (diag (format nil "Test decoding of the bytes ~a, type 1 and size 7877" test1))
    (flexi-streams:with-input-from-sequence (stream test1)
      (multiple-value-bind (type len base-hash base-offset)
          (read-pack-entry-header stream)
        (declare (ignore base-hash base-offset))
        (is type 1 "Check if type is 1")
        (is len 7877 "Check if length is 7877"))))
  (let ((test2 '(230 13 134 110))) ;; (230 13) 2 bytes of type delta-offset
    ;; 1006 - offset
    (diag (format nil "Test decoding of the bytes ~a, type 6 and offset 1006" test2))
    (flexi-streams:with-input-from-sequence (stream test2)
      (multiple-value-bind (type len base-hash base-offset)
          (read-pack-entry-header stream)
        (declare (ignore base-hash))
        (is type 6 "Check if type is 6")
        (is len 214 "Check if length is 214")
        (is base-offset 1006 "Check if offset is 1006")))))


(finalize)
