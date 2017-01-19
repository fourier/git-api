;;;; repo.lisp
;;
;; Usage (test) example:
;; (setf *repo* (git-api.repo:make-git-repo "~/Sources/lisp/git-api"))
;; (git-api.repo::get-commit-tree *repo* (git-api.repo:get-head-commit *repo*))
;;

(defpackage #:git-api.repo
  (:use #:cl #:alexandria
   #:git-api.utils #:git-api.pack #:git-api.object)
  (:export
   make-git-repo
   get-head-commit
   get-commit-tree
   get-object-by-hash
   get-head-hash
   rev-parse
   get-commit-parents
   get-commit
   ))

(in-package #:git-api.repo)

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
(defclass git-repo ()
  ((path :initarg :path :reader git-repo-path
         :documentation "Path to the repository")
   (git-prefix :initform ".git/" 
               :documentation "Prefix for the repository files - either .git or \"\" if bare repo")
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


(defun make-git-repo (path)
  (make-instance 'git-repo :path path))


(defmethod repo-path ((self git-repo) str)
  (concatenate 'string (slot-value self 'path) (slot-value self 'git-prefix) str))


(defmethod initialize-instance :after ((self git-repo) &key &allow-other-keys)
  "Constructor for the git-repo class"
  (with-slots (path pack-files packed-refs annotated-tags object-files git-prefix) self
    ;; append trailing "/"
    (unless (ends-with "/" path)
      (setf path (concatenate 'string path "/")))
    ;; sanity checks
    (unless (fad:file-exists-p path)
      (error 'not-existing-repository-error
             :text (format nil "Path ~a doesn't exist" path)))
    ;; check if path .git exist, otherwise assume bare repository
    (unless (fad:file-exists-p (concatenate 'string path ".git"))
      (setf git-prefix ""))
    (unless (every #'fad:file-exists-p
                   (mapcar (curry #'repo-path self)
                           '("objects" "refs/heads" "refs/tags")))
      (error 'corrupted-repository-error
             :text (format nil "Repository in ~a has a corrupted structure" path)))
    ;; collect all pack files
    (let ((files (directory (repo-path self "objects/pack/*.pack"))))
      (mapcar (lambda (pack)
                (push (parse-pack-file (namestring pack)) pack-files))
              files))
    ;; open file streams in pack files
    (dolist (pack pack-files)
      (pack-open-stream pack))
    ;; read all refs from the packed-refs
    (let ((packed-refs-filename (repo-path self "packed-refs")))
      (when (fad:file-exists-p packed-refs-filename)
        (with-open-file (stream packed-refs-filename
                                  :external-format
                                  #+(and :ccl :windows) (ccl::make-external-format :line-termination :CRLF)
                                  #-(and :ccl :windows) :default
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
                    (fad:list-directory (repo-path self "objects/"))))))
      (loop for dir in object-dirs
            do
            (loop for fil in (mapcar #'namestring (fad:list-directory dir))
                  for pos = (position #\/ fil :from-end t)
                  do
                  (setf (gethash
                         (concatenate 'string (subseq fil (- pos 2) pos)
                                      (subseq fil (1+ pos)))
                         object-files)
                        (pathname fil)))))))
                                  
    

(defmethod git-repo-close ((self git-repo))
  ;; TODO: add this to finalizer
  (with-slots (pack-files) self
    ;; open file streams in pack files
    (dolist (pack pack-files)
      (pack-close-stream pack))))
    


(defmethod get-object-by-hash ((self git-repo) hash)
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


(defmethod get-head-hash ((self git-repo))
  (rev-parse self "HEAD"))


(defmethod rev-parse ((self git-repo) ref)
  "Returns the hash string by given ref string.
Examples of ref strings:
@
HEAD
refs/heads/master
refs/tags/v1.0"
  (flet ((ref-or-sha1 (str)
           (if (not (starts-with-subseq "ref: " str))
               str
               (second (split-sequence:split-sequence #\space str)))))
  (cond ((sha1-string-p ref) ref)
        ((or (string= ref "HEAD")
             (string= ref "@"))
         (let ((ref-file (repo-path self "HEAD")))
           (when (fad:file-exists-p ref-file)
             (rev-parse self (ref-or-sha1 (read-one-line ref-file))))))
        (t 
         (with-slots (packed-refs) self
           (let ((ref-file (repo-path self ref)))
             ;; check if the ref is a normal file
             (if (fad:file-exists-p ref-file)
                 (rev-parse self (ref-or-sha1 (read-one-line ref-file)))
                 ;; otherwise find in packed refs
                 (gethash ref packed-refs))))))))
  

(defmethod get-head-commit ((self git-repo))
  (get-commit self (get-head-hash self)))


(defmethod get-commit-parents ((self git-repo) (object git-api.object:commit))
  (mapcar (curry #'get-commit self) (commit-parents object)))


(defmethod get-commit ((self git-repo) hash)
  (with-slots (commits) self
    (if-let (commit (gethash hash commits))
        commit
      (setf (gethash hash commits) (get-object-by-hash self hash)))))


(defmethod get-commit-tree ((self git-repo) (object git-api.object:commit))
  (let ((tree (make-hash-table :test #'equal))
        (children (list object)))
    (setf (gethash (object-hash object) tree) object)
    (loop while children
          do
          (let* ((current (pop children))
                 (kids (get-commit-parents self current)))
            (dolist (x (remove-if (lambda(x) (gethash (object-hash x) tree)) kids))
              (setf (gethash (object-hash x) tree) x) (push x children))))
    tree))
