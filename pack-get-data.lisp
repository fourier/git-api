;;;; pack-get-data.lisp
;;
;; This package reads the compressed entry from the pack file
;;
(defpackage #:git-api.pack.get-data
  (:use #:cl #:cl-annot.class #:alexandria #:git-api.utils #:static-vectors))

(in-package #:git-api.pack.get-data)
(annot:enable-annot-syntax)


@export
(defparameter *try-use-temporary-output-buffer* t)


(defparameter *temporary-read-buffer* (make-array 8192
                                                  :element-type '(unsigned-byte 8)
                                                  :fill-pointer t))


(defparameter *temporary-output-buffer* (make-array 8192
                                                    :element-type '(unsigned-byte 8)
                                                    :fill-pointer 8192))


(defparameter *temporary-static-read-buffer* (make-static-vector 8192))


(defparameter *temporary-static-output-buffer* (make-static-vector 8192))

(defvar *uncompressed-size-ptr* (cffi:foreign-alloc :unsigned-long))


@export
(defun get-object-data-old (offset compressed-size uncompressed-size stream)
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
         (if (and *try-use-temporary-output-buffer*
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



@export
(defun get-object-data-general (offset compressed-size uncompressed-size stream)
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
         (if (and *try-use-temporary-output-buffer*
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


@export
(defun get-object-data (offset compressed-size uncompressed-size stream)
  ;; move to position data-offset
  (file-position stream offset)
  (let ((input *temporary-static-read-buffer*)
        (output *temporary-static-output-buffer*)
        (output-buffer *temporary-static-output-buffer*))
    ;; set value of the pointer to size output buffer    
    (setf (cffi:mem-ref *uncompressed-size-ptr* :unsigned-long) uncompressed-size)
    (handler-case
        (progn
          ;; check if we requested to use temporary buffers
          (unless *try-use-temporary-output-buffer*
            (setf input (make-static-vector compressed-size)
                  output (make-static-vector uncompressed-size)
                  output-buffer (make-array uncompressed-size 
                                            :element-type '(unsigned-byte 8))))
          ;; check if size of input buffer suits
          (when (and *try-use-temporary-output-buffer* (> compressed-size 8192))
            (setf input (make-static-vector compressed-size)))
          ;; and check if size of output buffer suits 
          (when (and *try-use-temporary-output-buffer* (> uncompressed-size 8192))
            (setf output (make-static-vector uncompressed-size)
                  output-buffer (make-array uncompressed-size 
                                            :element-type '(unsigned-byte 8))))
          ;; read the data
          (read-sequence input stream :end compressed-size)
          ;; uncompress chunk
          (let* ((foreign-output (static-vector-pointer output))
                 (result
                  (git-api.zlib-wrapper::uncompress
                   foreign-output
                   *uncompressed-size-ptr*
                   (static-vector-pointer input)
                   compressed-size)))
            ;; check for error
            (unless (= result 0)
              (error (format nil "zlib::uncompress returned ~d" result)))
            ;; if necessary convert data from C to LISP format
            (unless (eq output-buffer *temporary-static-output-buffer*)
              (loop for i from 0 below uncompressed-size
                    for val = (the (unsigned-byte 8) (cffi:mem-aref foreign-output :unsigned-char i))
                    do (setf (aref output-buffer i) val)))
            ;; if necessary remove foreign arrays
            (unless (eq input *temporary-static-read-buffer*)
              (free-static-vector input))
            (unless (eq output *temporary-static-output-buffer*)
              (free-static-vector output))
            ;; good, output buffer now contains the data
            output-buffer))
      (error (e)
        (progn
          ;; if necessary remove foreign arrays
          (unless (eq input *temporary-static-read-buffer*)
            (free-static-vector input))
          (unless (eq output *temporary-static-output-buffer*)
            (free-static-vector output))
          (error e))))))
