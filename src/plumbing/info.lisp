;;;; object.lisp
(defpackage #:git-api.plumbing.info
  (:use #:cl #:alexandria #:git-api.utils #:git-api.repo)
  (:export))
  
(in-package #:git-api.plumbing.info)

(from git-api.repo import git-repo)

(defmethod git-cat-file ((self git-repo))
  "Provide content or type and size information for repository objects.")

(defmethod git-diff-files ((self git-repo))
  "Compares files in the working tree and the index.")

(defmethod git-diff-index ((self git-repo))
  "Compare a tree to the working tree or index.")

(defmethod git-diff-tree ((self git-repo))
    "Compares the content and mode of blobs found via two tree objects.")

(defmethod git-for-each-ref ((self git-repo))
  "Output information on each ref.")

(defmethod git-ls-files ((self git-repo))
  "Show information about files in the index and the working tree.")

(defmethod git-ls-remote ((self git-repo))
  "List references in a remote repository.")

(defmethod git-ls-tree ((self git-repo))
  "List the contents of a tree object.")

(defmethod git-merge-base ((self git-repo))
  "Find as good common ancestors as possible for a merge.")

(defmethod git-name-rev ((self git-repo))
  "Find symbolic names for given revs.")

(defmethod git-pack-redundant ((self git-repo))
  "Find redundant pack files.")

(defmethod git-rev-list ((self git-repo))
  "Lists commit objects in reverse chronological order.")

(defmethod git-show-index ((self git-repo))
  "Show packed archive index.")

(defmethod git-show-ref ((self git-repo))
  "List references in a local repository.")

(defmethod git-unpack-file ((self git-repo))
  "Creates a temporary file with a blob’s contents.")

(defmethod git-var ((self git-repo))
  "Show a Git logical variable.")

(defmethod git-verify-pack ((self git-repo))
  "Validate packed Git archive files.")



