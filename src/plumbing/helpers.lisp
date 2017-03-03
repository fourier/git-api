;;;; object.lisp
(defpackage #:git-api.plumbing.helpers
  (:use #:cl #:alexandria #:git-api.utils #:git-api.repo)
  (:export))
  
(in-package #:git-api.plumbing.helpers)


;; imports
(from git-api.repo import git-repo)
;;(from babel import octets-to-string)

;;----------------------------------------------------------------------------
;; helpers used by other functions
;; Read https://www.kernel.org/pub/software/scm/git/docs/git.html
;; for more information
;;----------------------------------------------------------------------------


(defmethod git-check-attr ((self git-repo))
  "Get gitattributes information.")

(defmethod git-check-ignore ((self git-repo))
  "Debug gitignore / exclude files.")

(defmethod git-check-mailmap ((self git-repo))
  "Show canonical names and email addresses of contacts.")
    
(defmethod git-check-ref-format ((self git-repo))
  "Ensures that a reference name is well formed.")


(defmethod git-column ((self git-repo))
  "Display data in columns.")

(defmethod git-credential ((self git-repo))
  "Retrieve and store user credentials.")


(defmethod git-credential-cache ((self git-repo))
  "Helper to temporarily store passwords in memory.")
    
(defmethod git-credential-store ((self git-repo))
  "Helper to store credentials on disk.")

(defmethod git-fmt-merge-msg ((self git-repo))
  "Produce a merge commit message.")

(defmethod git-interpret-trailers ((self git-repo))
  "help add structured information into commit messages.")

(defmethod git-mailinfo ((self git-repo))
  "Extracts patch and authorship from a single e-mail message.")

(defmethod git-mailsplit ((self git-repo))
  "Simple UNIX mbox splitter program.")


(defmethod git-merge-one-file ((self git-repo))
  "The standard helper program to use with git-merge-index.")

(defmethod git-patch-id ((self git-repo))
  "Compute unique ID for a patch.")

(defmethod git-sh-i18n ((self git-repo))  
  "Git's i18n setup code for shell scripts.")
  
(defmethod git-sh-setup ((self git-repo))
  "Common Git shell script setup code.")

(defmethod git-stripspace ((self git-repo))
  "Remove unnecessary whitespace.")
