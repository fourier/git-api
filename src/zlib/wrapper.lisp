;;;; wrapper.lisp
;;
;; This package reads the compressed entry from the pack file
;;
(defpackage #:git-api.zlib.wrapper
  (:use #:cl #:alexandria #:git-api.utils #:static-vectors #:git-api.zlib.cffi)
  (:export *try-use-temporary-output-buffer*
   uncompress-stream
   uncompress-git-file))

(in-package #:git-api.zlib.wrapper)


(defparameter *try-use-temporary-output-buffer* t
  "When set to T the functions will use intermediate buffers to
unpack and return the data to avoid excessive memory allocations.
However, then the applying deltas recursive procedures in place,
the result output buffer should not be preallocated since it
will be merged with itself. So for deltas processing in pack files
this variable should be set to NIL")

(defparameter +buffer-size+ 8192
  "The size of the intermediate buffer")

(defparameter +git-object-header-size+ 32
  "The maximum size of the git object header (string \"type + size\")")

(defparameter *temporary-read-buffer* (make-array +buffer-size+
                                                  :element-type '(unsigned-byte 8)
                                                  :fill-pointer t)
  "Static read buffer used to read small amounts of data from the stream")


(defparameter *temporary-output-buffer* (make-array +buffer-size+
                                                    :element-type '(unsigned-byte 8)
                                                    :fill-pointer +buffer-size+)
  "Static output buffer used containing uncompressed data. This buffer will
be returned if the uncompressed data size is less than +buffer-size+ and
if the variable *try-use-temporary-output-buffer* is T")  


(defparameter *temporary-static-read-buffer* (make-static-vector +buffer-size+)
  "Static read buffer used to read small amounts of data from the stream.
This buffer is used with CFFI version of zlib")
  

(defparameter *temporary-static-output-buffer* (make-static-vector +buffer-size+)
  "Static output buffer used containing uncompressed data. This buffer will
be returned if the uncompressed data size is less than +buffer-size+ and
if the variable *try-use-temporary-output-buffer* is T.
This buffer is used with CFFI version of zlib")  
  
(defparameter *git-object-header-static-buffer* (make-static-vector +git-object-header-size+)
  "Static buffer used to read git object header")


(defvar *uncompressed-size-ptr* (cffi:foreign-alloc :unsigned-long)
  "A pointer to the uncompressed size used by CFFI zlib uncompress function")


(define-condition zlib-error (error)
  ((text :initarg :text))
  (:report (lambda (condition stream)
             (format stream "zlib error: ~a" (slot-value condition 'text)))))


(defmacro unless-result-is (binding &body forms)
  "Defines a binding for numeric value returned by the second argument in BINDING.
The binding is the local variable named RESULT.
Examples:
For single case:
(unless (+ok+ (inflate-init_ strm (zlib-version) +z-stream-size+))
  (raise \"inflate-init error: result is ~d\" result))

For multiple cases: 
(unless-result-is ((0 1 2) (inflate-init_ strm (zlib-version) +z-stream-size+))
  (raise \"inflate-init error: result is ~d\" result))"
  (unless (= (length binding) 2)   ; 2 arguments possible
    (error "2 arguments must be supplied - expected result[s] and a form"))
  (let* ((bindings (if (atom (car binding))
                       (list (car binding))
                       (car binding))))
    `(let ((result ,(cadr binding)))
       (unless (or ,@(mapcar (lambda (x) `(= result ,x)) bindings))
       ,@forms))))


(defmacro with-temp-static-array ((array size temp-array) &body body)
  "Similar to with-static-array, but tries to use temproray buffer temp-array
if the size < +buffer-size+"
  `(let ((,array 
         (if (or (not *try-use-temporary-output-buffer*) (> ,size +buffer-size+))
             (make-static-vector ,size)
             ,temp-array)))
    (unwind-protect
        (locally ,@body)
      (unless (eq ,array ,temp-array)
        (free-static-vector ,array)))))


(defun raise (error-text &rest args)
  "Raises the zlib-error condition with the format text ERROR-TEXT and format ARGS"
  (error 'zlib-error :text (apply #'format error-text args)))


(declaim (inline copy-static-vector))
(defun copy-static-vector (static-vector normal-vector size &key (input-offset 0) (output-offset 0) )
  "Copy the SIZE bytes from static vector STATIC-VECTOR to the lisp vector of type (unsigned-byte 8)
NORMAL-VECTOR starting in source vector from INPUT-OFFSET and into the OUTPUT-OFFSET of the
destination vector"
  (declare (optimize (speed 3) (safety 0) (debug 0) (float 0)))
  (loop for i from input-offset below (+ size input-offset)
        for offset = (+ i output-offset)
        for val = (the (unsigned-byte 8)
                       (cffi:mem-aref (static-vector-pointer static-vector) :unsigned-char i))
        do (setf (aref normal-vector offset) val))
  normal-vector)


(declaim (inline uncompress-stream))
(defun uncompress-stream (offset compressed-size uncompressed-size stream)
  "Return the zlib-uncompressed data for the stream starting with position OFFSET
reading COMPRESSED-SIZE bytes and assuming uncompressed data is of size
UNCOMPRESSED-SIZE bytes. This shoud be known in advance before using the function"
  ;; try to guess which version to use
  (cond
   ;; first try CFFI version as the fastest
   (git-api.zlib.cffi:*zlib-loaded*
    (uncompress-stream-cffi offset compressed-size uncompressed-size stream))
   ;; as a fallback solution try to use patched CL zlib
   ;; patched means it supports manually specified output buffer
   ((or (> zlib::+zlib-major-version+ 0)
        (> zlib::+zlib-minor-version+ 1))
    (uncompress-stream-patched-zlib offset compressed-size uncompressed-size stream))
   ;; ... and finally try to use default (unpatched) CL zlib
   (t
    (uncompress-stream-git-zlib offset compressed-size uncompressed-size stream))))


(defun uncompress-stream-git-zlib (offset compressed-size uncompressed-size stream)
  "Return the uncompressed data for pack-entry from the opened file stream.
This function uses the CL zlib library from https://gitlab.common-lisp.net/
This zlib library version doesn't allow to specify output buffer, hence
the *try-use-temporary-output-buffer* variable will have no effect - new
buffers allocated all the time"
  ;; move to position data-offset
  (file-position stream offset)
  ;; uncompress chunk 
  (zlib:uncompress
   ;; of size compressed-size
   (let ((object (make-array compressed-size
                             :element-type '(unsigned-byte 8)
                             :fill-pointer t)))
     (read-sequence object stream)
     object) :uncompressed-size uncompressed-size))


(defun uncompress-stream-patched-zlib (offset compressed-size uncompressed-size stream)
  "Return the uncompressed data for pack-entry from the opened file stream.
This function uses the CL zlib library from https://github.com/fourier/zlib
This zlib library version allows to specify output buffer, so the implementation
will take the variable *try-use-temporary-output-buffer* into consideration"
  ;; move to position data-offset
  (file-position stream offset)
  (let ((read-buffer
         (if (> compressed-size +buffer-size+)
             (make-array compressed-size
                         :element-type '(unsigned-byte 8)
                         :fill-pointer t)
             *temporary-read-buffer*))
        (output-buffer
         (if (and *try-use-temporary-output-buffer*
                  (<= uncompressed-size +buffer-size+))
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


(defun uncompress-stream-cffi (offset compressed-size uncompressed-size stream)
  "Return the uncompressed data for pack-entry from the opened file stream.
This function uses the C zlib library using CFFI. The implementation
will take the variable *try-use-temporary-output-buffer* into consideration"
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
          (when (and *try-use-temporary-output-buffer* (> compressed-size +buffer-size+))
            (setf input (make-static-vector compressed-size)))
          ;; and check if size of output buffer suits 
          (when (and *try-use-temporary-output-buffer* (> uncompressed-size +buffer-size+))
            (setf output (make-static-vector uncompressed-size)
                  output-buffer (make-array uncompressed-size 
                                            :element-type '(unsigned-byte 8))))
          ;; read the data
          (read-sequence input stream :end compressed-size)
          ;; uncompress chunk
          ;; check for error
          (unless-result-is (0 (git-api.zlib.cffi:uncompress
                                (static-vector-pointer output)
                                *uncompressed-size-ptr*
                                (static-vector-pointer input)
                                compressed-size))
            (raise "zlib::uncompress returned ~d" result))
          ;; if necessary convert data from C to LISP format
          (unless (eq output-buffer *temporary-static-output-buffer*)
            (copy-static-vector output output-buffer uncompressed-size))
          ;; if necessary remove foreign arrays
          (unless (eq input *temporary-static-read-buffer*)
            (free-static-vector input))
          (unless (eq output *temporary-static-output-buffer*)
            (free-static-vector output))
          ;; good, output buffer now contains the data
          output-buffer)
      (error (e)
        (progn
          ;; if necessary remove foreign arrays
          (unless (eq input *temporary-static-read-buffer*)
            (free-static-vector input))
          (unless (eq output *temporary-static-output-buffer*)
            (free-static-vector output))
          (error e))))))


(declaim (inline uncompress-git-file))
(defun uncompress-git-file (filename)
  "Uncompress the file with git object - blob, commit etc.
Will try to uncompress used C zlib if available, if not fallback
to the CL zlib"
  (todo "return (values type content) to avoid double parsing")
  ;; try to guess which version to use
  (if git-api.zlib.cffi:*zlib-loaded*
      ;; first try CFFI version as the fastest
      (uncompress-git-file-cffi filename)
      (uncompress-git-file-zlib filename)))



(defun uncompress-git-file-zlib (filename)
  "Uncompress the file with git object using CL zlib library"
  ;;          (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
  ;;                 (chipz:decompress nil 'chipz:zlib stream)))
  (let ((data (read-binary-file filename)))
    (zlib:uncompress data)))
  

(defun uncompress-git-file-cffi (filename)
  "Uncompress the git object file using C-version of ZLIB"
  ;; Git object format:
  ;; header\0(content)
  ;; where the header is: {type string}#\Space{content size string}
  ;; From this the uncompressed size is (header size) + 1 + (content size)
  ;; Algorithm:
  ;; 1. Read up to 32 (+git-object-header-size+) bytes of output buffer
  ;; 2. Parse the input
  ;; 3. If content size <= 32 - (header size + 1)
  ;;    meaning it fit completely into the 32 bytes, just return the result
  ;; 4. Otherwise if content size < 8192 - (header size + 1)
  ;;    meaning we can use pre-allocated buffer - use it
  ;; 5. Otherwise allocate the buffer of the size (header size + 1 + content size) and use it
  ;; open the stream and read the file contents into the static vector
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((size (file-length stream)))
      (with-temp-static-array (input size *temporary-static-read-buffer*)
        (read-sequence input stream)
        ;; create a stream struct
        (cffi:with-foreign-object (strm '(:struct z-stream))
          ;; clear the stream struct
          (memset strm 0 +z-stream-size+)
          ;; initalize values in struct
          (cffi:with-foreign-slots ((next-in avail-in next-out avail-out total-out)
                                    strm (:struct z-stream))
            (setf next-in (static-vector-pointer input)
                  avail-in (min 64 size) ; read no more than 64 bytes - it is enough to read a header
                  next-out (static-vector-pointer *git-object-header-static-buffer*)
                  ;; header assumed to be is no more than 32 bytes,
                  ;; at least in Git implementation itself it is the assumption
                  avail-out +git-object-header-size+)
            ;; initialize the stream
            (unless-result-is (+z-ok+ (inflate-init_ strm (zlib-version) +z-stream-size+))
              (raise "zlib inflate-init returned ~d on a file ~a" result filename))
            (unwind-protect
                (let ((result (inflate strm +z-finish+)))
                  (unless (or (= result +z-stream-end+)
                              (= result +z-buf-error+)
                              (= result +z-ok+))
                    (raise "zlib inflate returned ~d while unpacking header on a file ~a" result filename))
                  ;; when we have uncompressed everything, meaning
                  ;; header + size <= 32 bytes,
                  ;; just return result as a copy of static buffer
                  (if (= result +z-stream-end+)
                      (copy-static-vector *git-object-header-static-buffer*
                                          (make-array total-out :element-type '(unsigned-byte 8))
                                          total-out)
                      ;; otherwise find the end of the header with type and size
                      (let* ((header-size (position 0 *git-object-header-static-buffer*))
                             (header (split-sequence:split-sequence
                                      #\Space
                                      (babel:octets-to-string *git-object-header-static-buffer*
                                                              :end header-size :encoding :utf-8)))
                             ;; extract the size
                             (content-size (parse-integer (cadr header)))
                             (uncompressed-size (+ 1 header-size content-size)))
                        ;; finally allocate static vector to uncompress the rest of the data
                        ;; the static vector size is at maximum content-size,
                        ;; but could be (typically) less since parts of content was already
                        ;; uncompressed to the *git-object-header-static-buffer*
                        (with-temp-static-array (static-content content-size *temporary-static-output-buffer*)
                          ;; update stream with the rest of the data
                          (setf avail-in (- size avail-in)
                                avail-out content-size
                                next-out (static-vector-pointer static-content))
                          ;; and finally uncompress the rest
                          (unless-result-is ((+z-ok+ +z-stream-end+) (inflate strm +z-finish+))
                            (raise "zlib inflate returned ~d on a file ~a" result filename))
                          ;; create the output vector
                          (let ((content (make-array uncompressed-size
                                                     :element-type '(unsigned-byte 8))))
                            ;; first take the first 32 uncompressed bytes of header + parts of content...
                            (copy-static-vector *git-object-header-static-buffer*
                                                content
                                                +git-object-header-size+)
                            ;; ... and then take the remaining uncompressed content
                            (copy-static-vector static-content
                                                content
                                                (- uncompressed-size +git-object-header-size+)
                                                :output-offset +git-object-header-size+))))))
              (inflate-end strm))))))))


