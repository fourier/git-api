 ;;;; pack.lisp
;;
;; This package reads the git pack and index files
;; according to the official documentation:
;; https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt
;;
;; See also this blog entry:
;; https://codewords.recurse.com/issues/three/unpacking-git-packfiles/
;;
;; To understand how the pack file is constructed also helps to
;; read the source: https://github.com/git/git/blob/master/pack-write.c
;;
(defpackage #:git-api.pack
  (:use #:cl #:alexandria #:git-api.utils #:git-api.pack.get-data)
  (:export
   pack-open-stream
   pack-close-stream
   parse-pack-file
   pack-get-object-by-hash))


(in-package #:git-api.pack)

;; imports
(from nibbles import read-ub32/be ub32ref/be read-ub64/be)
(from babel import octets-to-string)

;;----------------------------------------------------------------------------
;; Pack file format
;;----------------------------------------------------------------------------

(defparameter +pack-file-header-word+ "PACK"
  "First bytes of the pack file")

(defparameter +index-file-header-word+ #(255 116 79 99)
  "First bytes of the index file")

(defconstant +pack-version+ 2
  "Version of the pack file format")

(defconstant +sha1-size+ 20
  "Number of bytes of SHA1 checksum")

;;----------------------------------------------------------------------------
;; Constants from https://github.com/git/git/blob/master/cache.h
;;----------------------------------------------------------------------------

(defconstant OBJ-BAD -1)
(defconstant OBJ-NONE 0)
(defconstant OBJ-COMMIT 1)
(defconstant OBJ-TREE 2)
(defconstant OBJ-BLOB 3)
(defconstant OBJ-TAG 4)
;; 5 for future expansion 
(defconstant OBJ-OFS-DELTA 6)
(defconstant OBJ-REF-DELTA 7)
(defconstant OBJ-ANY 8)
(defconstant OBJ-MAX 9)

;;----------------------------------------------------------------------------
;; Globals
;;----------------------------------------------------------------------------
(defparameter *sha1-binary-array* (make-array +sha1-size+
                                              :element-type '(unsigned-byte 8)
                                              :adjustable nil)
  "Temporary 20 bytes array used to convert sha1 hash from string to binary format")

;;----------------------------------------------------------------------------
;; Conditions
;;----------------------------------------------------------------------------
(define-condition pack-error (error)
  ((text :initarg :text))
  (:report (lambda (condition stream)
             (format stream "Pack file error: ~a" (slot-value condition 'text)))))

(define-condition corrupted-pack-file-error (pack-error) nil)

(define-condition corrupted-index-file-error (pack-error) nil)

(define-condition incorrect-file-name-error (pack-error) nil)


(defclass pack-entry  ()
  ((offset :initarg :offset :initform nil :type fixnum
           :accessor pack-entry-offset
           :documentation "offset of the entry in a pack file. Offset followed by the VLI-length")
   (data-offset :initarg :data-offset :initform 0 :type fixnum
                :accessor pack-entry-data-offset
                :documentation "real offset to the place in file there the compressed data starts")
   (compressed-size :initarg :compressed-size :initform 0 :type fixnum
                    :accessor pack-entry-compressed-size
                    :documentation "size in bytes of the compressed data")
   (uncompressed-size :initarg :uncompressed-size :initform 0 :type fixnum
                      :accessor pack-entry-uncompressed-size
                      :documentation "size the data should hold after the unpacking")
   (type :initarg :type :initform nil :type (or fixnum keyword null)
         :reader pack-entry-type
         :documentation "type of the entry. Ether commit(1), tree(2), blob(3) or tag(4"))
  (:documentation "Entry in the pack file. This struct represents all the information
one could get for the pack file entry without actually unpacking it.
The data could be used in the following way:
1. Seek the file to the DATA-OFFSET
2. Read the COMPRESSED-SIZE bytes
3. Uncompress it to the array of size at least UNCOMPRESSED-SIZE bytes
4. Parse the data using parser for the TYPE
For deltas additional steps required."))


(defmethod (setf pack-entry-type) (value (entry pack-entry))
  "Setter for the entry type. VALUE is the integer
from cache.h. The setter will map the VALUE to the appropriate
keyword, unless the entry type is delta, in this case the VALUE
will remain as is"
  (setf (slot-value entry 'type) value)
  (switch (value)
    (OBJ-COMMIT (setf (slot-value entry 'type) :commit))
    (OBJ-TAG (setf (slot-value entry 'type) :tag))
    (OBJ-TREE (setf (slot-value entry 'type) :tree))
    (OBJ-BLOB (setf (slot-value entry 'type) :blob)))
  (slot-value entry 'type))


(defmethod print-object ((entry pack-entry) stream)
  "Print to STREAM the entry contents in the format:
TYPE SIZE SIZE-IN-PACKFILE OFFSET-IN-PACKFILE"
  (format stream "~a ~a ~a ~a"
          (switch ((pack-entry-type entry))
            (:commit "commit")
            (:tree "tree")
            (:blob "blob")
            (:tag "tag"))
          (pack-entry-uncompressed-size entry)
          (pack-entry-compressed-size entry)
          (pack-entry-offset entry)))


(defclass pack-entry-delta (pack-entry)
  ((base-hash :initarg :base-hash :initform nil
              :accessor pack-entry-base-hash
              :documentation "the SHA1 code of the base object"))
  (:documentation "Pack entry of type delta"))


(defmethod print-object :after ((entry pack-entry-delta) stream)
  "Append to the information printed to the stream with base-SHA-1"
  (when (pack-entry-base-hash entry)
    (format stream " ~a"
            (sha1-to-hex (pack-entry-base-hash entry)))))


(defclass pack-file ()
  ((pack-filename :initarg :pack-filename :initform nil :reader pack-filename
                  :type string
                  :documentation "Full path to the PACK file")
   (pack-stream :initarg :pack-stream :initform nil :reader pack-stream
                :type (or stream null)
                :documentation "Stream for pack file (if already opened)")
   (index-table :initform nil :reader index-table
                :documentation "The table from parsed index file:
The hash table with the mapping between sha1 binary code and entry")
   (offsets-table :initform nil :reader offsets-table
                  :documentation
                  "A hash table with the offset as a key and the hash as a value"))
  (:documentation "A class representing the pack file contents"))


(defmethod initialize-instance :after ((self pack-file) &key &allow-other-keys)
  "Constructor for the pack-file class"
  (with-slots (pack-filename) self
    (parse-pack-file-impl self pack-filename)))


(defmethod pack-open-stream ((self pack-file))
  "Opens the file stream for the pack-file SELF.
It is convenient to open stream once for all search operations in the packfile.
Don't forget to close it with corresponding call pack-close-stream"
  (with-slots (pack-filename pack-stream) self
    (when pack-stream
      (close pack-stream)
      (setf pack-stream nil))
    (setf pack-stream (open pack-filename :direction :input :element-type '(unsigned-byte 8)))))


(defmethod pack-close-stream ((self pack-file))
  "Closes the file stream for the pack-file SELF.
The stream is previously opened with pack-close-stream"
  (with-slots (pack-stream) self
    (when pack-stream
      (close pack-stream)
      (setf pack-stream nil))))


(defun pack-filename-to-index (filename)
  "Convert pack file name to index file name (by replacing extension)"
  (multiple-value-bind (pos end)
      (ppcre:scan "(?i).pack$" filename)
    (when (or (null pos)
              (/= (- end pos) 5))
      (error 'incorrect-file-name-error :text
             (format nil "Incorrect pack file name ~a" filename)))
    (concatenate 'string (subseq filename 0 pos) ".idx")))


(defun index-filename-to-pack (filename)
  "Convert index file name to pack file name (by replacing extension)"
  (multiple-value-bind (pos end)
      (ppcre:scan "(?i).idx$" filename)
    (when (or (null pos)
              (/= (- end pos) 4))
      (error 'incorrect-file-name-error :text
             (format nil "Incorrect index file name ~a" filename)))
    (concatenate 'string (subseq filename 0 pos) ".pack")))


(defmethod parse-pack-file-impl ((self pack-file) filename)
  "Parse the pack file(and index file) and return the hash table
containing all the entries in this file.
The key in the hash table is a hex-string SHA1 checksum of the object;
the value is a instance of PACK-ENTRY structure."
  (with-slots (index-table) self
    ;; first parse index file
    (multiple-value-bind (offsets-table index)
        (parse-index-file (pack-filename-to-index filename))
      (setf (slot-value self 'offsets-table) offsets-table)
      ;; then open the pack file itself.
      ;; at this point we know offsets of entries in pack file from the index file
      (with-open-file (stream filename
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let ((header (make-array 4
                                  :element-type '(unsigned-byte 8)
                                  :fill-pointer t)))
          (read-sequence header stream :end 4)
          ;; check header word
          (when (and (string= (octets-to-string header) +pack-file-header-word+)
                     ;; and version = 2
                     (= +pack-version+ (read-ub32/be stream)))
            (let ((objects-count (read-ub32/be stream)))
              ;; sanity check
              (unless (= objects-count (length index))
                (error 'corrupted-pack-file-error :text
                       (format nil "Corrupted pack file ~a. Number of objects ~d != index length ~d" filename objects-count (length index))))
              ;; finally create the hash table with the mapping between
              ;; sha1 binary code and entry 
              (setf index-table
                    (create-pack-entries-table-initial index stream)))))))))


(defun create-pack-entries-table-initial (index stream)
  "Create the hash table with the key as sha1 and the value is a cons:
(offset . compressed-size),  offset in the pack file and compressed
size(including header).
INDEX is a sorted list of pairs (sha1 . offset)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type integer offset compressed-size i file-length))
  (let ((table (make-hash-table :test #'equalp :size (length index)))
        (file-length (file-length stream)))
    ;; fill the table.
    (loop for i from 0 below (length index)
          for offset = (car (aref index i))
          ;; calculate the compressed size (size in pack file).
          ;; This size includes the header size as well
          ;; The size is the  difference between data offset of the current
          ;; and next entry...
          for compressed-size =
          (- (if (< i (1- (length index)))
                 (car (aref index (1+ i)))
                 ;; or end of file(without SHA-1 trailer of 20 bytes)
                 (- file-length 20)) 
             offset)
          do
          (setf (gethash (cdr (aref index i)) table)
                (cons offset compressed-size)))
    table))
                                  

(defun read-pack-entry-header (stream)
  "Reads the current stream for git pack entry header.
Returns the following VALUES list (use multiple-value-bind etc.):
(TYPE LEN BASE-HASH BASE-OFFSET)
where:
TYPE is type field of values OBJ-.. (see constants above)
LEN is the length of the uncompressed data
BASE-HASH if the type is OBJ-REF-DELTA the hash value of the base
object for the current object (which is delta). Base hash is a hex string.
BASE-OFFSET if the type is OBJ-OFS-DELTA the relative offset in the file
to the base object.

Pack entry header format (from the official documentation
on https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt)
-------------------------------------------------------
1-byte size extension bit (MSB)
       type (next 3 bit)
       size0 (lower 4-bit)
n-byte sizeN (as long as MSB is set, each 7-bit)
        size0..sizeN form 4+7+7+..+7 bit integer, size0
	is the least significant part, and sizeN is the
	most significant part.
-------------------------------------------------------
Example: encoded entry of type 1 and of size 7877.
Binary representation: 1 1110 1100 0101
Let's group it  by 7 7 4 bits
1111011000101
aabbbbbbbcccc

First group(cccc) is 0101 = 5. Let's add type and msb.
Since the type is 1 then the first byte is 149:
149 = 1001 0101
      '^^^ ~~~~                                                            
      | |   |
 MSB--+ |   |
 type---+   |
 value------+

second byte is 110 1100. Add msb = 1110 1100 = 236
Third 11 (the last byte, msb not needed) = 0000 0011 = 3
so it gives us 3 bytes : 149 236 3.

Decoding.
Take first byte: 149
149 apply masks (remove msb and type: (x & 112)>>4) -> 5
Second byte: 236
Remove msb -> 108, shift << 4 = 1728.
1728 + 5(extracted from 1st byte) = 1733
Third byte, no msb -> 3, shift 11 (4 + 7) = 6144
And finally the length is 6144 + 1733 = 7833"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum head shift type len))
  (let* ((base-hash (make-array +sha1-size+
                                :element-type '(unsigned-byte 8)
                                :fill-pointer 0 :adjustable nil))
         (base-offset nil)
         (head (read-byte stream))
         (shift 4) ; first part of size is shifted by 4 bits
         ;; type is encoded first (after MSB)
         (type (ash (logand head 112) (- shift)))
         (len (logand 15 head)))
    ;; calculate variable-length integer (uncompressed size)
    (loop while (>= head 128)
          do
          (setf head (the fixnum (read-byte stream)))
          (incf len (the fixnum (ash (the fixnum (logand 127 head)) shift)))
          (incf shift 7))
    ;; check if type is OBJ-REF-DELTA or OBJ-OFS-DELTA
    (switch (type)
      (OBJ-REF-DELTA
       ;; just read the base hash
       (read-sequence base-hash stream))
      (OBJ-OFS-DELTA
       ;; read the variable-length integer
       (setf base-offset (the fixnum (read-network-vli stream)))))
    (values type len base-hash base-offset)))


(defun read-network-vli (stream)
  "Read the variable-length integer from the stream.
From the documentation:
-----------------------
offset encoding:
	  n bytes with MSB set in all but the last one.
	  The offset is then the number constructed by
	  concatenating the lower 7 bit of each byte, and
	  for n >= 2 adding 2^7 + 2^14 + ... + 2^(7*(n-1))
	  to the result.
-----------------------"
  (declare (optimize (float 0)))
  ;; read the first byte
  (let* ((head (read-byte stream))
         (value (logand 127 head)))
    (loop while (>= head 128)
          do
          (incf value)
          (setf head (read-byte stream))
          (setf value (+ (ash value 7) (logand head 127))))
    value))


(defun read-delta-vli (stream)
  "Read the encoded variable-length integer used
to specify size of delta.

Details: this size is encoded in
little-endian format, therefore the most significant byte comes last"
  (declare (optimize (float 0)))
  ;; read the first byte
  (let* ((head (read-byte stream))
         ;; first value is this byte without MSB
         (value (logand 127 head))
         ;; shitf accumulator
         (shift 7))
    (loop while (>= head 128)
          do
          ;; next byte
          (setf head (read-byte stream))
          ;; shift read byte (without MSB) to accumulated shift
          ;; so every next byte is more significant than previous
          ;; and accumulate value
          (incf value (ash (logand head 127) shift))
          ;; increase a shift for the next significant byte
          (incf shift 7))
    value))
  
                              
;;----------------------------------------------------------------------------
;; Index file format
;;----------------------------------------------------------------------------
(defun parse-index-file (filename)
  "Parse the pack index file.
Returns VALUES (offsets-table index)
where:
- offsets-table is the hash-table, with:
  key is an offset in the pack file
  value is the array 20 bytes hash code of the object
- index is a sorted array of pairs: (offset hash) (as the value in the offsets-table)
Index table is used in pack files where deltas sometimes specify not the real offset or
hash code, but rather relative offset in the pack file, so we would have to determine
what is the hash code of the parent delta object"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let (offsets-table)
    (with-open-file (stream filename :direction :input
                            :element-type '(unsigned-byte 8))
      (let ((header (make-array 4
                                :element-type '(unsigned-byte 8)
                                :fill-pointer t)))
        ;; 1. read the header 4 bytes
        (read-sequence header stream :end 4)
        ;; check header word
        (unless (and (equalp header +index-file-header-word+)
                     ;; read the version 4 bytes and check version = 2
                     (= +pack-version+ (read-ub32/be stream)))
          (error 'corrupted-index-file-error :text
                 (format nil "Corrupted index file ~a. Header is incorrect" filename)))
        ;; 2. read the fanout table
        (let* ((fanout (read-fanout-table stream))
               (size (aref fanout 255))
               (objects-pos (file-position stream))
               ;; 3. skip the sorted list of object names for now
               ;; 4. skip CRCs of compressed objects
               ;; so the position is moved to 4(size of CRC) + 20(size of SHA1)
               ;; multiplied by the total size
               (pos (file-position stream
                                   (+ objects-pos (* 24 size))))
               ;; 5. and finally read offsets in the PACK file
               (offsets (read-offsets stream size))
               ;; index will contain conses of pack offsets/hashes
               ;; sorted by offset. It is needed to determine the
               ;; end of the compressed entry in the pack file
               (index (make-array size :adjustable nil :element-type '(cons ))))
          (declare (ignore pos))
          ;; now move to the position where the objects started
          ;; we will read them one by one to fill the index table
          ;; and offsets table in one loop
          (file-position stream objects-pos)
          ;; the offsets table is a hash table with the offset as a key
          ;; and the hash as a value
          (setf offsets-table (make-hash-table :test #'eq :size size))
          (loop for i fixnum from 0 below size
                for offset fixnum = (aref offsets i)
                for hash =
                (make-array +sha1-size+
                            :element-type '(unsigned-byte 8)
                            :adjustable nil)
                do 
                (read-sequence hash stream)                  
                (setf (aref index i) (cons offset hash)
                      (gethash offset offsets-table) hash))
          ;; finally return the offsets-table and sorted array of conses (offset . hash)
          (values offsets-table
                  (sort index (lambda (x y) (declare (integer x y)) (< x y)) :key (lambda (x) (the integer (car x))))))))))

          
(defun read-fanout-table (stream)
  "Reads the fanout table for index file from stream.
According to the documentation (https://www.kernel.org/pub/software/scm/git/docs/technical/pack-format.txt):
----------------------------------------------------------
256 4-byte network byte order
integers.  N-th entry of this table records the number of
objects in the corresponding pack, the first byte of whose
object name is less than or equal to N.  
----------------------------------------------------------
One can deduct number of entries in pack file - it should be in the last
element of the table (since any byte sequence starts with the number
less or equal to 256, as the byte <= 256"
  (let ((fanout (make-array 256 :element-type 'integer :fill-pointer t)))
    (loop for i fixnum from 0 to 255
          do
          (setf (aref fanout i) (read-ub32/be stream)))
    fanout))
          
    
(defun read-offsets (stream size)
  "Read offset table[s] and return the array of offsets in PACK file.
STREAM is the stream to read from with the current position is
the beginning of the offsets table.
SIZE is the size of offsets table."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; From the documentation:
  ;; 'A table of 4-byte offset values (in network byte order).
  ;;     These are usually 31-bit pack file offsets, but large
  ;;     offsets are encoded as an index into the next table with
  ;;     the msbit set.'
  ;; The next table for big files:
  ;; 'A table of 8-byte offset entries (empty for pack files less
  ;;     than 2 GiB).  Pack files are organized with heavily used
  ;;     objects toward the front, so most object references should
  ;;     not need to refer to this table.'
  (let* ((offsets (make-array size :element-type 'fixnum :adjustable nil))
         (big-offsets nil)
         (bytes-size (* size 4))
         (table (make-array bytes-size :element-type '(unsigned-byte 8)  :adjustable nil)))
    ;; we will read all the table into bytes array
    (read-sequence table stream)
    (loop for i fixnum from 0 below bytes-size by 4 do
          ;; processing separately depending if the MSB is set on the first
          ;; byte of encoded length
          (if (>= (the fixnum (aref table i)) 128)
              ;; large files: if set we clear the msb 
              (progn
                (setf (aref table i) (logand 127 (aref table i)))
                ;; and push to the list of "big offsets" as a pair:
                ;; index in original table and index in big offsets table
                (push (cons (ash i -2) (ub32ref/be table i)) big-offsets))
              ;; otherwise just NTOHL the value into the offsets array
              (setf (aref offsets (ash i -2)) (ub32ref/be table i))))
    ;; WARNING!
    ;; the code below for large (> 2gb) files in pack files
    ;; does not work since the size of big files couldn't fit into
    ;; the fixnum and therefore since all the code written with assumption
    ;; of fixnum sizes it is fail on type checks.
    ;; Need to introduce separate (not optimized) way to handle big files.
    ;; after processing of small offsets let's process big offsets
    (when big-offsets
      ;; read all values from the stream
      (let ((big-offsets-table
             (make-array (length big-offsets) :element-type 'integer)))
        (dotimes (i (length big-offsets))
          (setf (aref big-offsets-table i) (read-ub64/be stream)))
        ;; process the list of needed big offsets and update original
        ;; offsets array
        (dolist (x big-offsets)
          (setf (aref offsets (car x))
                (aref big-offsets-table (cdr x))))))
    offsets))


(defun parse-pack-file (filename)
  "Returns an instance of the PACK-FILE class with parsed pack file
information - index and offsets table, used for quick access to the
data inside the pack file"
  (make-instance 'pack-file :pack-filename filename))


(defmethod create-new-entry ((self pack-file) hash entry stream)
  "Create the pack file ENTRY by parsing the information of the
entry in the STREAM. The created entry will be added with the key HASH
to the INDEX-TABLE slot of the pack-file SELF object."
  (with-slots (index-table offsets-table) self
    (let ((current-entry (make-instance 'pack-entry
                                        :offset (car entry)
                                        :compressed-size (cdr entry))))
      ;; parse new entry
      ;; move position to the current entry offset
      (file-position stream (pack-entry-offset current-entry))
      ;; read the header - type (car header)
      ;; and uncompressed size (cdr header)
      (multiple-value-bind (type uncompr-len base-hash base-offset)
          (read-pack-entry-header stream)
        (setf (pack-entry-type current-entry) type
              (pack-entry-uncompressed-size current-entry) uncompr-len
              ;; the actual data starts here (we have read just up to
              ;; the data
              (pack-entry-data-offset current-entry) (file-position stream)
              ;; update calculate the compressed size (size in pack file)
              (pack-entry-compressed-size current-entry)
              (- (cdr entry) (- (pack-entry-data-offset current-entry) (car entry))))
        ;; handle entries with deltas
        (when (or (= type OBJ-REF-DELTA)
                  (= type OBJ-OFS-DELTA))
          ;; convert to the ref-delta class
          (change-class current-entry 'pack-entry-delta)
          ;; set the parent hash
          (setf (pack-entry-base-hash current-entry)
                (if (> (length base-hash) 0) base-hash ; for the REF delta types
                    ;; otherwise get from parent offset
                    (let ((base-abs-offset 
                           (- (pack-entry-offset current-entry)
                              base-offset)))
                      (gethash base-abs-offset offsets-table))))))
      (setf (gethash hash index-table) current-entry)
      current-entry)))


(defmethod pack-get-object-by-hash ((self pack-file) hash)
  "Find the object in the packfile. Return the uncompressed object
from the pack file as a vector of bytes.
HASH is as SHA1 code as as string (40 hex characters)"
  (sha1-hex-to-array hash *sha1-binary-array*)
  (pack-get-object-by-array-hash self *sha1-binary-array*))


(defmethod pack-get-object-by-array-hash ((self pack-file) hash)
  "Find the object in the packfile.
HASH is as SHA1 code as a byte array (20 values)
Returns (values):
- uncompressed object from the pack file as a vector of bytes.
- size of this vector (number of meaningful bytes)
- type of object, symbol"
  (flet ((stream-get-object-by-array-hash (stream entry)
           (when (typep entry 'cons) ; not an entry yet, create one
             ;; create new entry will add it automatically to the index-table
             (setf entry (create-new-entry self hash entry stream)))
           ;; get-object chunk returns values chunk,size
           (multiple-value-bind (chunk size)
               (get-object-chunk entry self stream)
             (values chunk
                     size
                     (pack-entry-type entry)))))
    (with-slots (pack-filename index-table pack-stream) self
      ;; find the object
      (when-let (entry (gethash hash index-table))
        ;; if stream is opened already
        (if pack-stream
            (stream-get-object-by-array-hash pack-stream entry)
            ;; otherwise open pack file
            (with-open-file (stream
                             pack-filename
                             :direction :input
                             :element-type '(unsigned-byte 8))
              (stream-get-object-by-array-hash stream entry)))))))


(defmethod get-object-chunk ((entry pack-entry) (pack pack-file) stream)
  "Return the uncompressed data for pack-entry from the opened file stream
and size of this data as values(data, size)"
  (declare (ignore pack))
  (values 
   (get-object-data (pack-entry-data-offset entry)
                    (pack-entry-compressed-size entry)
                    (pack-entry-uncompressed-size entry)
                    stream)
   (pack-entry-uncompressed-size entry)))



(defmethod get-object-chunk ((entry pack-entry-delta) (pack pack-file) stream)
  "Return the uncompressed data for pack-entry from the opened file stream
and size of this data as values(data, size).
Here size != uncompressed size of data since total size of deltified
object is known only when we parse delta object."
  ;; move to position data-offset
  (file-position stream (pack-entry-data-offset entry))
  ;; current object is delta. Let's get its parent's chunk
  ;; (recursively if necessary)
  ;; bind the special variable to nil to prevent using static arrays
  ;; while recursively building the delta
  ;; otherwise we can end up in a situation when the parent and
  ;; our arrays is the same static array
  (let ((*try-use-temporary-output-buffer* nil))
    (multiple-value-bind (parent size type)
        (pack-get-object-by-array-hash pack (pack-entry-base-hash entry))
      (declare (ignore size))
      ;; set the type from parent
      (setf (pack-entry-type entry) type)
      ;; merge
      (let ((chunk 
             (apply-delta parent (get-object-data (pack-entry-data-offset entry)
                                                  (pack-entry-compressed-size entry)
                                                  (pack-entry-uncompressed-size entry)
                                                  stream))))
        ;; finally return chunk and its length
        (values chunk (length chunk))))))
    

(defun apply-delta (base delta)
  "Applies the DELTA to the BASE object. DELTA is the byte-array of
uncompressed delta data for the pack file entry stored in delta format"
  (let* ((stream (flexi-streams:make-in-memory-input-stream delta))
         (source-length (read-delta-vli stream))
         (target-length (read-delta-vli stream))
         (pos (file-position stream))
         (end (length delta))
         (result (make-array target-length
                             :element-type '(unsigned-byte 8))))
    ;; sanity check
    (unless (= (length base) source-length)
      (error 'corrupted-pack-file-error
             :text
             (format nil
                     "Delta error: base length ~a does not match decoded length ~a"
                     (length base)
                     source-length)))
    ;; implementation of the patching
    ;; switch the diff type
    (let ((dest-pos 0)) ;; position in result buffer
      (loop while (< pos end)
            do
            (cond ((> (logand (aref delta pos) #x80) 0); MSB is set, operation is copy
                   (multiple-value-bind (new-pos offset len)
                       (decode-delta-copy-cmd delta  pos)
                     ;; do the magic
                     (replace result base :start1 dest-pos :end1 (+ dest-pos len)
                              :start2 offset :end2 (+ offset len))
                     (setf pos (1+ new-pos))
                     (incf dest-pos len)))
                  ((> (aref delta pos) 0) ;; MSB is not set, operation is insert
                   ;; looks fine in debugger
                   (let ((current-byte (aref delta pos)))
                     (incf pos)
                     (replace result delta :start1 dest-pos :end1 (+ dest-pos current-byte)
                              :start2 pos :end2 (+ pos current-byte))
                     (incf pos current-byte)
                     (incf dest-pos current-byte)))
                  (t (error 'corrupted-pack-file-error :text "Unexpected delta command 0")))))
    result))



(defun decode-delta-copy-cmd (delta pos)
  "Decodes the delta copy command inside pack DELTA array.
The POS is the current position on the DELTA array.
Returns values: (new position, offset, size) to copy"
  ;; tested against patch-delta.c from git
  ;; The format of the copy command is the following:
  ;; 2 things we have to decode: offset in the original data
  ;; and the amount of bytes to copy
  ;; The MSB of copy command is set to 1.
  ;; Therefore remaining 7 bits should contain necessary information
  ;; in compressed format:
  ;; - middle 3 bits are the compressed size
  ;; - last 4 bits are the compressed offset
  ;; Compression implemented in the following way:
  ;; amount of bytes to follow is the number of bits set to 1s,
  ;; the bits set to 0 indicate the skipped byte with zeros only.
  ;; for example 1001 mean byte1,00000000,00000000,byte2
  ;; This integer is in the little-endian format.
  ;; The bytes follows: first are the offset bytes
  ;; next are the size bytes
  (let* ((current-byte (aref delta pos))
         (offset (logand 15 current-byte))
         (len (ash (logand 112 current-byte) -4))
         (real-offset 0)
         (real-len 0))
    (macrolet ((uncompress-byte (compr-var out-var)
                 `(when (> (logand (ash 1 i) ,compr-var) 0)
                      (setf current-byte (aref delta (incf pos))
                            ,out-var (logior ,out-var (ash current-byte (* i 8)))))))
      (loop for i from 0 to 3 do (uncompress-byte offset real-offset))
      (loop for i from 0 to 2 do (uncompress-byte len real-len)))
    (values pos real-offset real-len)))


