;;;; repo.lisp
(defpackage #:git-api.repo
  (:use #:cl #:cl-annot.class #:alexandria
   #:git-api.utils #:git-api.pack #:git-api.object))

(in-package #:git-api.repo)
(annot:enable-annot-syntax)

;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

(defparameter +git-objects-dir-regexp+ (ppcre:create-scanner "(?i)/([0-9|a-f]){2}/$")
  "Regular expression scanner used to determine which of directories
in .git/objects are containing objects (not a packfiles or info)")

;;----------------------------------------------------------------------------
;; Conditions
;;----------------------------------------------------------------------------
(define-condition not-existing-repository-error (error)
  ((text :initarg :text :reader text)))

(define-condition corrupted-repository-error (error)
  ((text :initarg :text :reader text)))


;;----------------------------------------------------------------------------
;; Repository class
;;----------------------------------------------------------------------------
@export-class
(defclass git-repo ()
  ((path :initarg :path :reader repo-path
         :documentation "Path to the repository")
   (object-files :reader object-files :initform (make-hash-table :test #'equal)
                 :documentation "A hash table with the SHA1 hex as a key and filename as a value for all unpacked objects in .git/objects directory")
   (pack-files :reader pack-files :initform nil
               :documentation "List of pack-file objects")
   (packed-refs :reader packed-refs :initform (make-hash-table :test #'equal)
                :documentation "Map between ref string and SHA1 text code in packed-refs file")
   (annotated-tags :reader annotated-tags :initform (make-hash-table :test #'equal)
                   :documentation "Map between ref string of annotated tag and the SHA1 text code of the commit this annotated tag points to")
   (commits :reader commits :initform (make-hash-table :test #'equal)
            :documentation "A cache of commit objects to avoid double-reading"))
  (:documentation "Class representing git repository"))

(defmacro repo-file (str)
  `(concatenate 'string (slot-value self 'path) ".git/" ,str))

@export
(defun make-git-repo (path)
  (make-instance 'git-repo :path path))


(defmethod initialize-instance :after ((self git-repo) &key &allow-other-keys)
  "Constructor for the git-repo class"
  (with-slots (path pack-files packed-refs annotated-tags object-files) self
    ;; append trailing "/"
    (unless (ends-with "/" path)
      (setf path (concatenate 'string path "/")))
    ;; sanity checks
    (unless (fad:file-exists-p path)
      (error 'not-existing-repository-error
             :text (format nil "Path ~a doesn't exist" path)))
    (unless (fad:file-exists-p (concatenate 'string path ".git"))
      (error 'not-existing-repository-error
             :text (format nil "No .git found in ~a" path)))
    (unless (every #'fad:file-exists-p
                   (mapcar (curry #'concatenate 'string path)
                           '(".git/HEAD" ".git/objects" ".git/config" ".git/refs")))
      (error 'corrupted-repository-error
             :text (format nil "Repository in ~a has a corrupted structure" path)))
    ;; collect all pack files
    (let ((files (directory (repo-file "objects/pack/*.pack"))))
      (mapcar (lambda (pack)
                (push (parse-pack-file (namestring pack)) pack-files))
              files))
    ;; open file streams in pack files
    (dolist (pack pack-files)
      (pack-open-stream pack))
    ;; read all refs from the packed-refs
    (let ((packed-refs-filename (repo-file "packed-refs")))
      (when (fad:file-exists-p packed-refs-filename)
        (with-open-file (stream packed-refs-filename
                                :direction :input)
          (let (prev-ref) ; previous ref
            (loop for line = (read-line stream nil)
                  while line
                  ;; skip comments
                  unless (starts-with #\# (string-trim '(#\Space #\Tab) line))            
                  do
                  (let ((ref (split-sequence:split-sequence #\space line)))
                    ;; check if the current line is the peeled ref (points to
                    ;; the line above which is an annotated tag)
                    (if (starts-with #\^ line)
                        (setf (gethash prev-ref annotated-tags)
                              (subseq (car ref) 1))
                        (setf (gethash (cadr ref) packed-refs) (car ref)
                              prev-ref (cadr ref)))))))))
    ;; find all not-packed object files in repo
    (update-repo-objects self)))


(defmethod update-repo-objects ((self git-repo))
  (with-slots (object-files path) self
    (let ((object-dirs 
           (remove-if-not
            (curry #'ppcre:scan +git-objects-dir-regexp+)
            (mapcar #'namestring
                    (fad:list-directory (repo-file "objects/"))))))
      (loop for dir in object-dirs
            do
            (loop for fil in (mapcar #'namestring (fad:list-directory dir))
                  do
                  (let ((pos (position #\/ fil :from-end t)))
                    (setf (gethash
                           (concatenate 'string (subseq fil (- pos 2) pos)
                                        (subseq fil (1+ pos)))
                           object-files)
                          (pathname fil))))))))
                                  
    

(defmethod git-repo-close ((self git-repo))
  ;; TODO: add this to finalizer
  (with-slots (pack-files) self
    ;; open file streams in pack files
    (dolist (pack pack-files)
      (pack-close-stream pack))))
    


@export
(defmethod get-object-by-hash ((self git-repo) hash &key result-block)
  "Returns the object by the given hash string"
  ;; first try if the file exists
  (with-slots (path pack-files object-files) self
    ;; no file exist
    (let* ((result nil)
           (found-pack
            ;; iterate oven all pack files trying to find the one having hash
            (loop for pack in pack-files
                  when
                  (= (length  
                      (setf result (multiple-value-list
                                    (pack-get-object-by-hash pack hash))))
                     3)
                  return pack)))
      ;; ok pack and corresponding index entry found 
      (if (and result found-pack)
          ;; get the data from pack file
          ;; pack-get-object-by-hash returns values: (data, size, type)
          (let ((data (car result))
                (size (cadr result))
                (type (caddr result)))
            (when data
              ;; and finally parse the data
              (parse-git-object type
                                data
                                hash
                                :start 0
                                :size size)))
          ;; otherwise read git file
          (parse-git-file (gethash hash object-files))))))


@export
(defmethod get-head-hash ((self git-repo))
  (let* ((head-file (repo-file "HEAD"))
         (head-contents (read-one-line head-file)))
    ;; check if the HEAD points to the detached commit
    (if (not (starts-with-subseq "ref: " head-contents))
        head-contents
        ;; otherwise search if this file exists
        (let* ((head-ref (cadr (split-sequence:split-sequence #\space head-contents))))
          (get-hash-by-ref self head-ref)))))


(defmethod get-hash-by-ref ((self git-repo) ref)
  "Returns the hash string by given ref string.
Examples of ref strings:
ref/heads/master
refs/tags/v1.0"
  (with-slots (packed-refs) self
    (let ((ref-file (repo-file ref)))
      ;; check if the ref is a normal file
      (if (fad:file-exists-p ref-file)
          (read-one-line ref-file)
          ;; otherwise find in packed refs
          (gethash ref packed-refs)))))
    
@export
(defmethod get-head-commit ((self git-repo))
  (get-commit self (get-head-hash self)))


@export
(defmethod get-commit-parents ((self git-repo) (object git-api.object:commit))
  (mapcar (curry #'get-commit self) (commit-parents object)))


@export
(defmethod get-commit ((self git-repo) hash)
  (with-slots (commits) self
    (if-let (commit (gethash hash commits))
        commit
      (setf (gethash hash commits) (get-object-by-hash self hash)))))


(defmethod get-commit-tree ((self git-repo) (object git-api.object:commit))
  (let ((tree (make-hash-table :test #'equal))
        (children (list object)))
    (loop while children
          do
          (let* ((current (pop children))
                 (kids (get-commit-parents self current)))
            (dolist (x (remove-if (lambda(x) (gethash (object-hash x) tree)) kids))
              (setf (gethash (object-hash x) tree) x) (push x children))))
    tree))
