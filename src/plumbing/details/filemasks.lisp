;;;; filemasks.lisp
(defpackage #:git-api.plumbing.helpers.details.filemasks
  (:use #:cl #:alexandria #:git-api.utils)
  (:export
   wildcard-to-regex))
  
(in-package #:git-api.plumbing.helpers.details.filemasks)


;; NOTE:wildcard to regexp functionality taken from my project
;; mediaimport. Probably move it to separate utility library?

(defconstant +regex-escape-chars+
  '(#\\
    #\*
    #\+
    #\?
    #\|
    #\{
    #\}
    #\[
    #\]
    #\(
    #\)
    #\^
    #\$
    #\.
    #\#
    #\Space)
"List of special characters to be escaped in file mask")


(defun wildcard-to-regex (wildcard &key (case-sensitive-p t))
  "Convert file wildcards to regular expressions. By default the regular
expression is case sensitive. This is regulated by keyword argument
CASE-SENSITIVE-P
Example:
=> (wildcard-to-regex \"Photo*.jpg\") 
\"^Photo.*\\\\.jpg$\"
=> (wildcard-to-regex \"Photo*.jpg\" :case-sensitive-p t) 
\"(?i)^Photo.*\\\\.jpg$\""

  ;; special case: *.* means * in reality
  (if (string= wildcard "*.*")
      ".*"
      ;; otherwise do processing
      (let ((regex
             (make-array (+ 8 (length wildcard))
                         :element-type
                         #+lispworks7 'lw:bmp-char
                         #-lispworks7 'character
                         :fill-pointer 0
                         :adjustable t)))
        (unless case-sensitive-p
          (vector-push-extend #\( regex)
          (vector-push-extend #\? regex)
          (vector-push-extend #\i regex)
          (vector-push-extend #\) regex))
        (vector-push-extend #\^ regex)
        (loop for char across wildcard do
              (cond ((eq char #\*)
                     (progn
                       (vector-push-extend #\. regex)
                       (vector-push-extend #\* regex)))
                    ((eq char #\?)
                     (vector-push-extend #\. regex))
                    ((find char +regex-escape-chars+)
                     (progn
                       (vector-push-extend #\\ regex)
                       (vector-push-extend char regex)))
                    (t (vector-push-extend char regex))))
        (vector-push-extend #\$ regex)
        regex)))
