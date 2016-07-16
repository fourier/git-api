;;;; gitplot.asd

(asdf:defsystem #:git-api
  :description "Library for accessing git repository"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "BSD" ;; https://opensource.org/licenses/bsd-license.php
  :depends-on (#:alexandria     ; general utilities - Public domain
               #:cl-fad         ; files manipulation - BSD
               #:cl-annot       ; export annotations - LLGPL
               #:cl-ppcre       ; portable regular expressions - BSD
               #:babel          ; bytes to string - MIT
               #:zlib           ; zlib to deal with git objects - LLGPL
               #:cffi           ; to access dlls (libz) - MIT
               #:static-vectors ; to use common arrays between C and Lisp code - MIT
               #:split-sequence ; general split - public domain
               #:nibbles        ; to parse binary data - BSD
               #:flexi-streams  ; to create in-memory streams - BSD
               #:ironclad)      ; sha1 checksum - X11/MIT-like license
  :serial t
  :components ((:file "utils")
               (:file "pack-get-data")
               (:file "pack")
               (:file "object")
               (:file "repo")))

