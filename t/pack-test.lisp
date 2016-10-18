;;;; pack-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.pack-test
  (:use :cl
        :git-api.test.base
        :git-api.utils
        :git-api.pack
        :prove))

(in-package git-api.test.pack-test)

;; import unexterned(private for package) functions
(from git-api.pack import read-network-vli read-delta-vli)
(from git-api.pack import read-pack-entry-header)
(from git-api.pack import parse-index-file)

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
  (fail "not implemented")
  )

(subtest "Testing index-filename-to-pack"
  (fail "not implemented")
  )

(subtest "Testing parse-index-file"
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


(subtest "Testing read-offsets"
  ;; read-offsets
  (fail "not implemented"))


(finalize)
