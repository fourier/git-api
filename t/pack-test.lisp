;;;; pack-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.pack-test
  (:use :cl
        :git-api.utils
        :git-api.pack
        :prove))

(in-package git-api.test.pack-test)

;; import unexterned(private for package) functions
(from git-api.pack import read-network-vli read-delta-vli)
(from git-api.pack import read-pack-entry-header)

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

(plan nil)

(subtest "Testing read-network-vli"
  (loop for test-case in +network-vli-tests+
        for value = (car test-case)
        and stream = (flexi-streams:make-in-memory-input-stream (cdr test-case))
        do
        (is value (read-network-vli stream))))



(finalize)
