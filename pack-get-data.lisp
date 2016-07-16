;;;; pack-get-data.lisp
;;
;; This package reads the compressed entry from the pack file
;;
(defpackage #:git-api.pack.get-data
  (:use #:cl #:cl-annot.class #:alexandria #:git-api.utils))


(in-package #:git-api.pack.get-data)
(annot:enable-annot-syntax)

;; imports
(from nibbles import read-ub32/be ub32ref/be read-ub64/be)
(from babel import octets-to-string)


(defparameter *temporary-read-buffer* (make-array 8192
                                                  :element-type '(unsigned-byte 8)
                                                  :fill-pointer t))


(defparameter *temporary-output-buffer* (make-array 8192
                                                    :element-type '(unsigned-byte 8)
                                                    :fill-pointer 0))
@export
(defparameter *use-temporary-output-buffer* t)

@export
(defun get-object-data (offset compressed-size uncompressed-size stream)
  "Return the uncompressed data for pack-entry from the opened file stream.
BUFFER is a optional buffer to read compressed data"
  ;; move to position data-offset
  (file-position stream offset)
  (let ((read-buffer
         (if (> compressed-size 8192)
             (make-array compressed-size
                         :element-type '(unsigned-byte 8)
                         :fill-pointer t)
             *temporary-read-buffer*))
        (output-buffer
         (if (and *use-temporary-output-buffer*
                  (<= uncompressed-size 8192))
             (progn
               (setf (fill-pointer *temporary-output-buffer*) 0)
               *temporary-output-buffer*)
             (make-array uncompressed-size 
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0))))
    ;; sanity check
    (assert (>= (array-total-size read-buffer) compressed-size))
    ;; read the data
    (read-sequence read-buffer stream :end compressed-size)
    ;; uncompress chunk
    (zlib:uncompress read-buffer :output-buffer output-buffer :start 0 :end compressed-size)))
