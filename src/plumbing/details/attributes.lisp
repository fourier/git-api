;;;; attributes.lisp
(defpackage #:git-api.plumbing.helpers.details.attributes
  (:use #:cl #:alexandria #:git-api.utils)
  (:export
   parse-attribute-line
   parse-attribute-macro
   ))
  
(in-package #:git-api.plumbing.helpers.details.attributes)


;; imports
;;(from git-api.repo import git-repo)
;;(from babel import octets-to-string)

;;----------------------------------------------------------------------------
;; Helper functions for git attributes parsing/processing
;; Read https://www.kernel.org/pub/software/scm/git/docs/gitattributes.html
;; for more some information
;;----------------------------------------------------------------------------

(defun parse-collected-attributes (attrs)
  "Parse collected attributes.
Example: giving the attrs as an array ('-diff' '-merge' '-text')
returns alist (('diff' . :unset) ('merge' . :unset) ('text' . unset))"
  (flet ((parse-attr (attr)
           (cond ((eql #\- (char attr 0))
                  (cons (subseq attr 1) :unset))
                 ((eql #\! (char attr 0))
                  (cons (subseq attr 1) :unspecified))
                 ((find #\= attr)
                  (let ((kv (split-sequence:split-sequence #\= attr)))
                    (cons (first kv) (second kv))))
                 (t (cons attr :set)))))
    (mapcar (lambda (attr)
                          (parse-attr attr))
                   attrs)))


(defun parse-attribute-line (line)
  "For a given string line LINE produces a pair:
(PATTERN LIST-OF-ATTRIBUTES)
where the PATTERN is the file pattern,
LIST-OF-ATTRIBUTES is a LIST-OF-ATTRIBUTES is a list of pairs:
(ATTRIBUTE-NAME . VALUE)
Here VALUE could be either :set, :unset, :unspecified or text value of the attribute.
If the attribute value is set as 'unspecified' then this attribute will
be used to override attributes on other levers as unspecified.
Example:
(parse-attribute-line \"abc     -foo -bar\") will return
(\"abc\" (\"foo\" . :unset) (\"bar\" . :unset))"
  (let ((parsed 
         (remove-if (lambda (x) (= (length x) 0)) (ppcre:split "\\s" line))))
    (cons (car parsed) 
          (parse-collected-attributes (cdr parsed)))))
          

(defun parse-attribute-macro (line)
  "Parses line containing Git attribute macros. Returns NIL if the line is not
a Git attribute macro.
Example:
(parse-attribute-macro \"[attr]binary -diff -merge -text\") will return a list:
(\"binary\" (\"diff\" . :unset) (\"merge\" . :unset) (\"text\" . :unset))"
  (let ((parsed 
         (remove-if (lambda (x) (= (length x) 0)) (ppcre:split "\\s" line))))
    (when (starts-with-subseq "[attr]" (car parsed))
      (cons (subseq (car parsed) 6)
            (parse-collected-attributes (cdr parsed))))))

