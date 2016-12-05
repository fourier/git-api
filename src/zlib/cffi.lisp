;;;; cffi.lisp
;;
;; CFFI bindings to zlib
;; also exports auxulary cffi functions like memset
;;
(defpackage #:git-api.zlib.cffi
  (:use #:cl #:cffi)
  (:export
   *zlib-loaded*
   uncompress
   z-stream
   zlib-version
   inflate-init_
   inflate
   inflate-end
   memset
   next-in avail-in next-out avail-out total-in total-out
   +z-stream-size+
   +z-finish+ +z-ok+ +z-stream-end+ +z-buf-error+))

(in-package #:git-api.zlib.cffi)

;;----------------------------------------------------------------------------
;; Zlib wrapper 
;;----------------------------------------------------------------------------
(define-foreign-library zlib
  (t (:default "libz")))


(defvar *zlib-loaded* nil
  "Variable set to T if we were able to load ZLIB C library")

;; try to load foreign library
(ignore-errors 
  (use-foreign-library zlib)
  (setf *zlib-loaded* t))

(defparameter +z-stream-size+
  (if *zlib-loaded* (cffi:foreign-type-size '(:struct z-stream)) 0)
  "The size of z-stream structure. If zlib is not available then 0")

;;----------------------------------------------------------------------------
;; Constants 
;;----------------------------------------------------------------------------

;; Allowed flush values for deflate and inflate
(defconstant +z-no-flush+      0)
(defconstant +z-partial-flush+ 1)
(defconstant +z-sync-flush+    2)
(defconstant +z-full-flush+    3)
(defconstant +z-finish+        4)
(defconstant +z-block+         5)
(defconstant +z-trees+         6)

;; Return codes for the compression/decompression functions.
;; Negative values are errors, positive values are used for special but normal events.
(defconstant +z-ok+            0)
(defconstant +z-stream-end+    1)
(defconstant +z-need-dict+     2)
(defconstant +z-errno+        -1)
(defconstant +z-stream-error+ -2)
(defconstant +z-data-error+   -3)
(defconstant +z-mem-error+    -4)
(defconstant +z-buf-error+    -5)
(defconstant +z-version-error+ -6)

;; Compression levels. 
(defconstant +z-no-compression+         0)
(defconstant +z-best-speed+             1)
(defconstant +z-best-compression+       9)
(defconstant +z-default-compression+  -1)

;; Compression strategy — see deflateInit2 for details.
(defconstant +z-filtered+            1)
(defconstant +z-huffman-only+        2)
(defconstant +z-rle+                 3)
(defconstant +z-fixed+               4)
(defconstant +z-default-strategy+    0)

;; Possible values of the data_type field (though see inflate).
(defconstant +z-binary+   0)
(defconstant +z-text+     1)
(defconstant +z-ascii+    +z-text+)
(defconstant +z-unknown+  2)

;; The deflate compression method (the only one supported in this version).
(defconstant +z-deflated+   8)

;; not used now but keep it for the future if more fine-tuning required
(defcstruct z-stream
  "z_stream_s wrapper"
  ;;    z_const Bytef *next_in;     /* next input byte */
  (next-in :pointer)
  ;;    uInt     avail_in;  /* number of bytes available at next_in */
  (avail-in :unsigned-int)
  ;;    uLong    total_in;  /* total number of input bytes read so far */
  (total-in :unsigned-long)
  ;;    Bytef    *next_out; /* next output byte should be put there */
  (next-out :pointer)
  ;;    uInt     avail_out; /* remaining free space at next_out */
  (avail-out :unsigned-int)
  ;;    uLong    total_out; /* total number of bytes output so far */
  (total-out :unsigned-long)
  ;;    z_const char *msg;  /* last error message, NULL if no error */
  (msg :pointer)
  ;;    struct internal_state FAR *state; /* not visible by applications */
  (state :pointer)
  ;;    alloc_func zalloc;  /* used to allocate the internal state */
  (zalloc :pointer)
  ;;    free_func  zfree;   /* used to free the internal state */
  (zfree :pointer)
  ;;    voidpf     opaque;  /* private data object passed to zalloc and zfree */
  (opaque :pointer)
  ;;    int     data_type;  /* best guess about the data type: binary or text */
  (data-type :int)
  ;;    uLong   adler;      /* adler32 value of the uncompressed data */
  (adler :unsigned-long)
  ;;    uLong   reserved;   /* reserved for future use */
  (reserved :unsigned-long))


(defcfun ("compress" compress) :int
  (dest  (:pointer :unsigned-char))
  (dest-len (:pointer :unsigned-long))
  (source (:pointer :unsigned-char))
  (source-len :unsigned-long))


;;;  uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
;;;  enough memory, Z_BUF_ERROR if there was not enough room in the output
;;;  buffer, or Z_DATA_ERROR if the input data was corrupted or incomplete.
(defcfun ("uncompress" uncompress) :int
  (dest  (:pointer :unsigned-char))
  (dest-len (:pointer :unsigned-long))
  (source (:pointer :unsigned-char))
  (source-len :unsigned-long))


;;;  const char * ZEXPORT zlibVersion
(defcfun ("zlibVersion" zlib-version) (:pointer :char))


;;; inflateInit
;; inflateInit_
;; inflateInit_((strm), ZLIB_VERSION, (int)sizeof(z_stream) = 112)
(defcfun ("inflateInit_" inflate-init_) :int
  (strm (:pointer (:struct z-stream)))
  (version :string)
  (sizeofstream :int))

;;; inflate
;; int ZEXPORT inflate OF((z_streamp strm, int flush))
(defcfun ("inflate" inflate) :int
  (strm (:pointer (:struct z-stream)))
  (flush :int))


;;; inflateEnd
;; int ZEXPORT inflateEnd OF((z_streamp strm));
(defcfun ("inflateEnd" inflate-end) :int
  (strm (:pointer (:struct z-stream))))


;;; auxulary c function - memset
(defcfun ("memset" memset) :int
  (b :pointer)
  (c :int)
  (len :unsigned-int))


(defun test-inflate-init ()
  ;; get the size of the struct (on 64 bits and 32 bits they are different)
  (let ((stream-size (foreign-type-size '(:struct z-stream))))
    ;; create a stream struct
    (with-foreign-object (strm '(:struct z-stream))
      ;; clear the stream struct
;;      (foreign-funcall "memset" :pointer strm :int 0 :int stream-size)
      (memset strm 0 stream-size)
      ;; initialize the stream
      (inflate-init_ strm (zlib-version) stream-size)
      (inflate-end strm))))


(defun uncompress-first-bytes (data uncompressed-size)
  ;; get the size of the struct (on 64 bits and 32 bits they are different)
  (let ((stream-size (foreign-type-size '(:struct z-stream))))
    ;; create a stream struct
    (with-foreign-object (strm '(:struct z-stream))
      ;; clear the stream struct
;      (foreign-funcall "memset" :pointer strm :int 0 :int stream-size)
;      (foreign-funcall "memset" :pointer strm :int 0 :int stream-size)
      ;; initalize values in struct
      (with-foreign-slots ((next-in avail-in next-out avail-out) strm (:struct z-stream))
        (setf next-in data
              avail-in (length data)
              next-out nil; buf
              avail-out uncompressed-size))
      ;; initialize the stream
      (inflate-init_ strm (zlib-version) stream-size)
      (inflate strm +z-finish+)
      (inflate-end strm))))
  
