;;;; object.lisp
(defpackage #:git-api.plumbing.manip
  (:use #:cl #:alexandria #:git-api.utils)
  (:export))
  
(in-package #:git-api.plumbing.manip)


#|

git-apply ((self git-repo))

    Apply a patch to files and/or to the index.
git-checkout-index ((self git-repo))

    Copy files from the index to the working tree.
git-commit-tree ((self git-repo))

    Create a new commit object.
git-hash-object ((self git-repo))

    Compute object ID and optionally creates a blob from a file.
git-index-pack ((self git-repo))

    Build pack index file for an existing packed archive.
git-merge-file ((self git-repo))

    Run a three-way file merge.
git-merge-index ((self git-repo))

    Run a merge for files needing merging.
git-mktag ((self git-repo))

    Creates a tag object.
git-mktree ((self git-repo))

    Build a tree-object from ls-tree formatted text.
git-pack-objects ((self git-repo))

    Create a packed archive of objects.
git-prune-packed ((self git-repo))

    Remove extra objects that are already in pack files.
git-read-tree ((self git-repo))

    Reads tree information into the index.
git-symbolic-ref ((self git-repo))

    Read, modify and delete symbolic refs.
git-unpack-objects ((self git-repo))

    Unpack objects from a packed archive.
git-update-index ((self git-repo))

    Register file contents in the working tree to the index.
git-update-ref ((self git-repo))

    Update the object name stored in a ref safely.
git-write-tree ((self git-repo))

    Create a tree object from the current index.
 |#

