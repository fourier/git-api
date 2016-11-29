;;;; utils.lisp
(defpackage #:git-api.utils
  (:use #:cl #:alexandria)
  (:export
   from
   read-one-line
   compiler-warning
   fixme
   todo   file-size
   read-binary-file
   read-header
   sha1-to-hex
   sha1-hex-to-array
   make-array-view))

(in-package #:git-api.utils)

;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(defparameter *zero-ascii-begin* (char-code #\0))
(defparameter *char-ascii-begin* (char-code #\a))
(declaim (type fixnum *zero-ascii-begin* *char-ascii-begin*))


;;----------------------------------------------------------------------------
;; Utility macros
;;----------------------------------------------------------------------------
(defmacro from (package import name &rest others)
  "Import symbol(s) NAME ... from the package PACKAGE.
Examples:
(from mediaimport.utils import interleave partition +regex-escape-chars+)
(from mediaimport.ui import save-edit-controls-history)
(from mediaimport.utils import *)
In the last example imports all the exported symbols from the package given."
  (unless (string-equal import 'import)
    (error "Unexpected keyword: expected IMPORT, got ~A" import))
  (let* ((pkg (string-upcase (symbol-name package))) ;; package name as a string
         (symbols ; symbols to be imported
          (if (and (not others) (string-equal name "*"))
              ;; if called like (from something import *)
              (let (symbols)
                (do-external-symbols (s pkg)
                  (push s symbols))
                symbols)
              ;; otherwise just arguments list
              (cons name others))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
       ,@(mapcar (lambda (symb)
                   (let ((import-symbol (find-symbol (string-upcase (symbol-name symb)) pkg)))
                     `(shadowing-import ,(list 'quote import-symbol))))
                 symbols)))))


(defmacro read-one-line (filename)
  "Read exactly one first line from the file"
  (let ((stream-var (gensym)))
    `(with-open-file (,stream-var ,filename :direction :input)
       (read-line ,stream-var))))


(defmacro compiler-warning (datum &rest arguments)
  "Issue the compiler warning"
  (apply 'warn datum arguments))

(defmacro fixme (datum &rest arguments)
  "Issue the compiler warning starting with FIXME: string"
  (apply 'warn (concatenate 'string "FIXME: " datum) arguments))

(defmacro todo (datum &rest arguments)
  "Issue the compiler warning starting with TODO: string"
  (apply 'warn (concatenate 'string "TODO: " datum) arguments))


;;----------------------------------------------------------------------------
;; Utility functions
;;----------------------------------------------------------------------------
(defun file-size (filename)
  "Return the size of the file with the name FILENAME in bytes"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (file-length in)))


(defun read-binary-file (filename)
  "Return an array of file contents"
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (buffer (make-array size
                               :element-type '(unsigned-byte 8)
                               :fill-pointer t)))
      (read-sequence buffer stream)
      buffer)))


(defun read-header (filename size)
  "Read SIZE bytes from the file FILENAME. If the file size is less than SIZE,
read up to the size of file"
  (let ((elt-type '(unsigned-byte 8)))
    (with-open-file (in filename :element-type elt-type)
      (let* ((fsize (file-length in))
             (buffer (make-array (min size fsize) :element-type elt-type)))
        (read-sequence buffer in)
        buffer))))
            

(defun sha1-to-hex (input &optional (offset 0))
  "Reads the SHA1 code from either:
- stream,
- vector of unsigned bytes,
- list of integers (unoptimized version, used for debugging/logging etc)
- array of integers (unoptimized version, used for debugging/logging etc)
returns the downcase string representing SHA1 code in hex format.
NOTE: OFFSET is ignored for streams"
  (typecase input
    ((simple-array (unsigned-byte 8)) (sha1-optimized-array-to-hex input offset))
    (list (sha1-list-to-hex input offset))
    (stream (sha1-stream-to-hex input))
    (array 'integer (sha1-normal-array-to-hex input offset))
    (t nil)))


(defun sha1-list-to-hex (lst offset)
  (string-downcase
   (with-output-to-string (s)  
    (loop for i from offset below (+ offset 20)
          do
          (format s "~2,'0x" (nth i lst)))
    s)))

(defun sha1-normal-array-to-hex (arr offset)
  ;; a SLOW version used only for dumping output to logs etc
  (string-downcase
   (with-output-to-string (s)  
    (loop for i from offset below (+ offset 20)
          do
          (format s "~2,'0x" (aref arr i)))
    s)))



(defmacro digit-to-hex (dig)
  "Convert number (0..15) to corresponding hex character"
  (let ((digit-var (gensym)))
    `(let ((,digit-var ,dig))
       (declare (type fixnum ,digit-var))
       (the character
            (code-char
             (if (< ,digit-var 10)
                 (+ *zero-ascii-begin* ,digit-var)
                 (+ (the fixnum (- ,digit-var 10)) *char-ascii-begin*)))))))


(defun sha1-optimized-array-to-hex (array offset)
  ;;(declare (:explain :variables :calls))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum offset))
  (declare (type (array (unsigned-byte 8)) array))
  (let ((hex (make-array 40 :element-type 'character :adjustable nil))) 
    (dotimes (x 20)
      (declare (type fixnum x))
      (let ((byte (aref array (the fixnum (+ x offset)))))
        (declare (type fixnum byte)) 
        (let* ((upper-byte (ash byte -4))
               (lower-byte (the fixnum (- byte (the fixnum (ash upper-byte 4)))))
               (pos (the fixnum (* 2 x))))
          (declare (type fixnum offset lower-byte upper-byte))
          (setf (schar hex pos) (digit-to-hex upper-byte)
                (schar hex (the fixnum (1+ pos))) (digit-to-hex lower-byte)))))
    hex))


(defun sha1-stream-to-hex (stream)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((hex (make-array 40 :element-type 'character :adjustable nil))) 
    (dotimes (x 20)
      (declare (type fixnum x))
      (let ((byte (read-byte stream)))
        (declare (type fixnum byte)) 
        (let* ((upper-byte (ash byte -4))
               (lower-byte (the fixnum (- byte (the fixnum (ash upper-byte 4)))))
               (pos (the fixnum (* 2 x))))
          (declare (type fixnum pos lower-byte upper-byte))
          (setf (schar hex pos) (digit-to-hex upper-byte)
                (schar hex (the fixnum (1+ pos))) (digit-to-hex lower-byte)))))
    hex))


(defun sha1-hex-to-array (sha1string &optional result)
  "Convert the given sha1 string in hex (with lower case characters)
to the byte array.
If RESULT array is given - write to this array"
  (declare (optimize (speed 3) (safety 0)))
  ;;(declare (:explain :variables :calls))
  (unless result
    (setf result (make-array 20 :element-type '(unsigned-byte 8) :adjustable nil)))
  (macrolet ((hex-to-number (hex)
               (let ((hex-var (gensym)))
                 `(let ((,hex-var (the fixnum (char-code ,hex))))
;;                    (declare (type fixnum ,hex-var))
                    (the fixnum (if (>= ,hex-var *char-ascii-begin*)
                                    (+ 10 (the fixnum (- ,hex-var *char-ascii-begin*)))
                                    (- ,hex-var *zero-ascii-begin*)))))))
    (dotimes (x 20)
      (declare (type fixnum x))
      (let* ((pos (the fixnum (* 2 x)))
             (upper-val (hex-to-number (schar sha1string pos)))
             (lower-val (hex-to-number (schar sha1string (the fixnum (1+ pos))))))
        (declare (fixnum pos upper-val lower-val))
        (setf (aref result x) (the fixnum (+ (the fixnum (ash upper-val 4)) lower-val))))))
  result)

(defun make-array-view (vector start end)
  "Returns array displaced to the vector (starting with start, ending on end)"
  (make-array (- end start 1)
              :displaced-to vector
              :displaced-index-offset start
              :element-type (array-element-type vector)))


