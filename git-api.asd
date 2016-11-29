#|
  This file is a part of git-api project.
  Copyright (c) 2016 Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

#|
  Library for accessing git repository

  Author: Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

(in-package :cl-user)
(defpackage git-api-asd
  (:use :cl :asdf))
(in-package :git-api-asd)

(defsystem #:git-api
  :version "0.1"
  :author "Alexey Veretennikov"
  :license "BSD" ;; https://opensource.org/licenses/bsd-license.php
  :depends-on (#:alexandria     ; general utilities - Public domain
               #:cl-fad         ; files manipulation - BSD
               #:cl-ppcre       ; portable regular expressions - BSD
               #:babel          ; bytes to string - MIT
               #:zlib           ; zlib to deal with git objects - LLGPL
               #:cffi           ; to access dlls (libz) - MIT
               #:static-vectors ; to use common arrays between C and Lisp code - MIT
               #:split-sequence ; general split - public domain
               #:nibbles        ; to parse binary data - BSD
               #:flexi-streams  ; to create in-memory streams - BSD
               #:ironclad)      ; sha1 checksum - X11/MIT-like license
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:module "zlib"
                          :depends-on ("utils")
                          :serial t
                          :components
                          ((:file "cffi")
                           (:file "wrapper")))
                 (:file "pack")
                 (:file "object")
                 (:file "repo"))))
  :description "Library for accessing git repository"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op git-api-test))))
