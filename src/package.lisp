#|
  This file is a part of git-api project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2019
|#

(in-package :cl-user)
(defpackage git-api
  (:use :git-api.repo :git-api.object)
  (:export
   ;; repo
   make-git-repo
   git-repo-close
   rev-parse           
   get-commit
   get-object-by-hash
   cat-file
   ;; object base
   git-object
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
   ;; tree-entry struct
   tree-entry
   tree-entry-mode
   tree-entry-name
   tree-entry-hash
   ;; tag object
   tag
   tag-object
   tag-type
   tag-tagger
   tag-comment))


