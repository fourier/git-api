;;;; object.lisp
(defpackage #:git-api.object
  (:use #:cl #:alexandria #:git-api.utils)
  (:export
   ;; object base
   object-hash 
   ;; commit object
   commit
   commit-tree
   commit-author
   commit-committer
   commit-comment
   commit-parents
   ;; blob
   blob
   blob-content
   ;; tree entry readers
   tree-entry-name tree-entry-mode tree-entry-hash
   ;; tree object
   tree
   tree-entries
   ;; tag object
   tag
   tag-object
   tag-type
   tag-tagger
   tag-comment
   ;; exported functions
   parse-git-file
   parse-git-object
   ))


(in-package #:git-api.object)


;;----------------------------------------------------------------------------
;; Git Object class
;;----------------------------------------------------------------------------
(defclass git-object ()
  ((hash :initarg :hash :reader object-hash :type '(vector unsigned-byte 8) :initform nil
         :documentation "Every git object has a SHA1-hash"))
  (:documentation "Base class for Git objects"))


;;----------------------------------------------------------------------------
;; Git Commit class
;;----------------------------------------------------------------------------
(defclass commit (git-object)
  ((tree :initarg :tree :reader commit-tree :initform "" :type simple-string
         :documentation "SHA1-hash of the associated tree object")
   (author :initarg :author :reader commit-author :initform "" :type simple-string
           :documentation "String representing commit author field")
   (committer :initarg :committer :reader commit-committer :initform "" :type simple-string
              :documentation "String representing committer field")
   (comment :initarg :comment :reader commit-comment :initform "" :type string
            :documentation "Commit comment. Empty string if not comment")
   (parents :initarg :parents :reader commit-parents :initform nil :type list
            :documentation "A list of sha1 hashes of parents commits, even if only one parent"))
  (:documentation "Git Commit object"))

(defmethod print-object ((self commit) stream)
  "Print to STREAM the commit contents"
  (with-slots (hash tree author committer comment parents) self
    (format stream "commit: ~a~%" hash)
    (format stream "tree ~a~%" tree)
    (format stream "author ~a~%" author)
    (format stream "committer ~a~%" committer)
    (format stream "parents ~{~a~^, ~}~%" parents)
    (format stream "comment~%~a" comment))) 

;;----------------------------------------------------------------------------
;; Git Blob class
;;----------------------------------------------------------------------------
(defclass blob (git-object)
  ((content :initarg :content :reader blob-content :initform nil :type '(vector unsigned-byte 8)
            :documentation "The blob object contents as a vector of unsigned bytes"))
  (:documentation "Git Blob object"))

(defmethod print-object ((self blob) stream)
  "Print to STREAM the size of the blob object"
  (with-slots (content) self
    (format stream "blob of size ~d bytes~%" (length content))))



;;----------------------------------------------------------------------------
;; Git Tree class
;;----------------------------------------------------------------------------
(defstruct tree-entry
  "A struct representing particular tree entry: NAME, MODE, SHA1-hash"
  name mode hash)

(defmethod print-object ((self tree-entry) stream)
  "Print to STREAM contents of the tree entry in format: MODE NAME HASH"
  (format stream "~a ~a ~a"
          (tree-entry-mode self)
          (tree-entry-name self)
          (tree-entry-hash self)))


(defclass tree (git-object)
  ((entries :initarg :entries :reader tree-entries :initform nil :type (or list nil)
            :documentation "A list of tree entries (instances of TREE-ENTRY struct)"))
  (:documentation "Git Tree object"))


(defmethod print-object ((self tree) stream)
  "Print to STREAM the every tree entry in the tree object"
  (with-slots (entries) self
    (mapc (lambda (e)
            (format stream "~a~%" e))
          entries)))


;;----------------------------------------------------------------------------
;; Git Tag class
;;----------------------------------------------------------------------------
;; TODO: review and comment it
(defclass tag (git-object)
  ((object :initarg :tree :reader tag-object :initform "")
   (type :initarg :author :reader tag-type :initform "")
   (tagger :initarg :committer :reader tag-tagger :initform "")
   (comment :initarg :comment :reader tag-comment :initform "")
   (tag :initarg :parents :reader tag :initform nil))
  (:documentation "Git Tag object"))

(defmethod print-object ((self tag) stream)
  (with-slots (object type tagger tag comment) self
    (format stream "object ~a~%" object)
    (format stream "type ~a~%" type)
    (format stream "tagger ~a~%" tagger)
    (format stream "tag ~a~%" tag)
    (format stream "comment~%~a" comment)))


;;----------------------------------------------------------------------------
;; Exported functions
;;----------------------------------------------------------------------------
(defun parse-git-file (filename)
  (declare (optimize speed))
  (let* ((data
          ;; TODO: use zlib-wrapper
;;          (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
;;                 (chipz:decompress nil 'chipz:zlib stream)))
          (zlib:uncompress (read-binary-file filename)))
         (content-start (position 0 data))
         (header 
          (babel:octets-to-string data :start 0 :end content-start
                                  :encoding :utf-8))
         (header-split (split-sequence:split-sequence #\Space header))
         ;; guess the hash. hash is the last 41 (40 hash + 1 directory separator)
         ;; characters of the filename, if the filename is in git repository
         ;; and not renamed git object
         (name (if (pathnamep filename) (namestring filename) filename))
         (hash-filename (subseq name (- (length name) 41))))
    (parse-git-object (intern (string-upcase (car header-split)) "KEYWORD")
                      data
                      (remove #\/ hash-filename) ;; remove dir separator
                      :start (1+ content-start)
                      :size (parse-integer (cadr header-split)))))

(defgeneric parse-git-object (type data hash &key start size))

(defmethod parse-git-object ((obj (eql :blob)) data hash &key start size)
  (let ((blob (make-instance 'blob :hash hash :content (subseq data start (+ start size)))))
    blob))


;;----------------------------------------------------------------------------
;; Implementation
;;----------------------------------------------------------------------------
(defun parse-text-git-data (data start size)
  "Parses the data for text git objects (commit,tag)
and returns a PAIR:
  (car PAIR) = list of lines before the comment
  (cdr PAIR) = comment"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type simple-string text header comment))
  (declare (type fixnum start size last-char-pos newline-position))
  (let* ((text (babel:octets-to-string data
                                       :start start
                                       :end (+ start size)
                                       :errorp nil
                                       :encoding :utf-8))
         (text-length (the fixnum (length text)))
         ;; 3 cases:
         ;; 1. 2 consecutive newlines in the middle of the text
         ;; 2. consecutive newlines at the end of the text - newline-position = (length - 2)
         ;; 3. no consecutive newlines  - newline-position = length
         (newline-position (the fixnum (find-consecutive-newlines text)))
         (header (the string (subseq text 0 newline-position)))
         (comment
          (if (>= newline-position (- text-length 2))
              ""
              (subseq text (+ 2 newline-position)))))
    (declare (type string text header comment)
             (type fixnum text-length newline-position))
    (cons (split-sequence:split-sequence #\newline header)
          comment)))


(defun find-consecutive-newlines (str &key (first 0) (last (length str)))
  "Find 2 consecutive newlines in the string STR.
Returns the index of the first element found or size of STR if
nothing found"
  (declare (type string str)
           (type fixnum first last))
  (declare (optimize (speed 3) (safety 0)))
  (if (/= first last)
      (loop with next fixnum = (1+ first)
            while (/= next last)
            when (char= (the character (char str first))
                        (the character (char str next)) #\newline)
            return first
            do
            (incf first)
            (incf next)
            finally (return last))
      last))


(defmethod parse-git-object ((obj (eql :commit)) data hash &key start size)
  (let* ((parsed-data (parse-text-git-data data start size))
         (commit (make-instance 'commit :hash hash :comment (cdr parsed-data))))
    (with-slots (tree author committer parents) commit
      (dolist (line (car parsed-data))
        (let* ((space-pos (position #\Space line))
               (key (subseq line 0 space-pos))
               (value (subseq line (1+ space-pos))))
          (switch (key :test #'string=)
            ("tree" (setf tree value))
            ("author" (setf author value))
            ("committer" (setf committer value))
            ("parent" (push value parents))))))
     commit))
    


(defmethod parse-git-object ((obj (eql :tag)) data hash &key start size)
  (let* ((parsed-data (parse-text-git-data data start size))
         (self (make-instance 'tag :hash hash :comment (cdr parsed-data))))
    (dolist (line (car parsed-data))
      (let* ((space-pos (position #\Space line))
             (key (subseq line 0 space-pos))
             (value (subseq line (1+ space-pos))))
        (setf (slot-value self (intern (string-upcase key) :git-api.object)) value)))
    self))


(defun parse-tree-entry (data start)
  "Returns values: entry and position after the entry"
  (let* ((separator (position 0 data :start start)) ; 0-separator separating header and hash-code
         (header (split-sequence:split-sequence #\Space ; split header into mode and filename
                                                (babel:octets-to-string data :start start :end separator)))
         ;; finally extract 20 bytes of hash
         (hash (subseq data (1+ separator) (+ separator 21))))
    ;; return the cons entry + next position to parse
    (cons (make-tree-entry :mode (car header) :name (cadr header)
                           :hash
                           ;; need to downcase to be compatible with the representation
                           ;; in the file system
                           (sha1-to-hex hash))
          (+ 21 separator))))

(defmethod parse-git-object ((obj (eql :tree)) data hash &key start size)
  ;; format:
  ;; [mode] [file/folder name]\0[SHA-1 of referencing blob or tree]
  ;; mode is a string, file/folder name is a string,
  ;; SHA-1 code is 20 bytes
  (let ((self (make-instance 'tree :hash hash))
        (next-start start))
    ;; parse in the loop until the end reached
    (loop while (< next-start (+ start size))
          do
          (let ((parsed (parse-tree-entry data next-start)))
            ;; push the value
            (push (car parsed) (slot-value self 'entries))
            ;; ... and increase the position
            (setf next-start (cdr parsed))))
    ;; finally reverse the parsed list
    (setf (slot-value self 'entries) (nreverse (slot-value self 'entries)))
    self))

