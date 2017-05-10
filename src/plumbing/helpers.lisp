;;;; object.lisp
(defpackage #:git-api.plumbing.helpers
  (:use #:cl #:alexandria #:git-api.utils #:git-api.repo)
  (:export
   git-check-attr
   git-check-ignore
   git-check-mailmap
   git-check-ref-format
   git-column
   git-credential
   git-credential-cache
   git-credential-store
   git-fmt-merge-msg
   git-interpret-trailers
   git-mailinfo
   git-mailsplit
   git-merge-one-file
   git-patch-id
   git-sh-i18n
   git-sh-setup
   git-stripspace
   ))
  
(in-package #:git-api.plumbing.helpers)


;; imports
(from git-api.repo import git-repo)
;;(from babel import octets-to-string)

;;----------------------------------------------------------------------------
;; helpers used by other functions
;; Read https://www.kernel.org/pub/software/scm/git/docs/git.html
;; for more information
;;----------------------------------------------------------------------------


(defun parse-attribute-line (line)
  "For a given string line LINE produces a pair:
(PATTERN . LIST-OF-ATTRIBUTES)
where the PATTERN is the file pattern,
LIST-OF-ATTRIBUTES is a LIST-OF-ATTRIBUTES is a list of pairs:
(ATTRIBUTE-NAME . VALUE)
Here VALUE could be either :set, :unset or text value of the attribute.
If the attribute value is set as 'unspecified' then this attribute will
be omitted from the list"
nil)  

(defmethod git-check-attr ((self git-repo) pathname1 &rest pathnames)
  "Get gitattributes information for given paths.

Returs a list of pairs:(PATHNAME . LIST-OF-ATTRIBUTES)
Where the LIST-OF-ATTRIBUTES is a list of pairs:
(ATTRIBUTE-NAME . VALUE)
Here VALUE could be either :set, :unset or text value of the attribute.
If the attribute value is set as 'unspecified' then this attribute will
be omitted from the list"
  ;; read https://www.kernel.org/pub/software/scm/git/docs/git-check-attr.html
  ;; for details


)


(defun pattern-resolve (pathname pattern)
  "Resolves a PATHNAME against the PATTERN
Returns PATHNAME if PATHNAME matches the PATTERN and NIL otherwise."
  (todo "Implement this function: pattern-resolve")
  ;; Pattern format is taken from gitignore manual:
  ;; https://www.kernel.org/pub/software/scm/git/docs/gitignore.html
  ;;
  ;; * A blank line matches no files, so it can serve as a separator for readability.
  ;; * A line starting with # serves as a comment. Put a backslash ("\") in front
  ;;   of the first hash for patterns that begin with a hash.
  ;; * Trailing spaces are ignored unless they are quoted with backslash ("\").
  ;; * An optional prefix "!" which negates the pattern; any matching file
  ;;   excluded by a previous pattern will become included again. It is not
  ;;   possible to re-include a file if a parent directory of that file is
  ;;   excluded. Git doesn't list excluded directories for performance reasons,
  ;;   so any patterns on contained files have no effect, no matter where they
  ;;   are defined. Put a backslash ("\") in front of the first "!" for patterns
  ;;   that begin with a literal "!", for example, "\!important!.txt".
  ;; * If the pattern ends with a slash, it is removed for the purpose of the
  ;;   following description, but it would only find a match with a directory.
  ;;   In other words, foo/ will match a directory foo and paths underneath it,
  ;;   but will not match a regular file or a symbolic link foo (this is
  ;;   consistent with the way how pathspec works in general in Git).
  ;; * If the pattern does not contain a slash /, Git treats it as a shell
  ;;   glob pattern and checks for a match against the pathname relative
  ;;   to the location of the .gitignore file (relative to the toplevel of
  ;;   the work tree if not from a .gitignore file).
  ;; * Otherwise, Git treats the pattern as a shell glob suitable for
  ;;   consumption by fnmatch(3) with the FNM_PATHNAME flag: wildcards in the
  ;;   pattern will not match a / in the pathname. For example,
  ;;   "Documentation/*.html" matches "Documentation/git.html" but not
  ;;   "Documentation/ppc/ppc.html" or "tools/perf/Documentation/perf.html".
  ;; * A leading slash matches the beginning of the pathname. For example,
  ;;   "/*.c" matches "cat-file.c" but not "mozilla-sha1/sha1.c".
  ;;
  ;; Two consecutive asterisks ("**") in patterns matched against full
  ;; pathname may have special meaning:
  ;; * A leading "**" followed by a slash means match in all directories.
  ;;   For example, "**/foo" matches file or directory "foo" anywhere,
  ;;   the same as pattern "foo". "**/foo/bar" matches file or directory
  ;;   "bar" anywhere that is directly under directory "foo".
  ;; * A trailing "/**" matches everything inside. For example, "abc/**"
  ;;   matches all files inside directory "abc", relative to the location
  ;;   of the .gitignore file, with infinite depth.
  ;; * A slash followed by two consecutive asterisks then a slash matches
  ;;   zero or more directories. For example, "a/**/b" matches "a/b",
  ;;   "a/x/b", "a/x/y/b" and so on.
  ;; * Other consecutive asterisks are considered invalid.
  pathname)

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
