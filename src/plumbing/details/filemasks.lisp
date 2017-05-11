;;;; filemasks.lisp
(defpackage #:git-api.plumbing.helpers.details.filemasks
  (:use #:cl #:alexandria #:git-api.utils)
  (:export
   wildcard-to-regex
   translate))
  
(in-package #:git-api.plumbing.helpers.details.filemasks)



(defun wildcard-to-regex (pattern &key (case-sensitive-p t) (beginning-of-string t) (end-of-string t))
  "Convert file wildcards to regular expressions. By default the regular
expression is case sensitive. This is regulated by keyword argument
CASE-SENSITIVE-P.
Parameters BEGINNING-OF-STRING and END-OF-STRING identify whether the beginning of the string (^)
or end of string ($) marker should be present.
Supported patters:

 * - everything
 ? - any character
 [range] - any character in range
 [!range] - any character not in range

Example:
=> (wildcard-to-regex \"Photo*.jpg\") 
\"^Photo.*\\\\.jpg$\"
=> (wildcard-to-regex \"Photo*.jpg\" :case-sensitive-p nil) 
\"(?i)^Photo.*\\\\.jpg$\""  
  (let ((regex
         (loop for i below (length pattern)
               for c = (char pattern i)
               if (char= c #\*) ; process * mask
               collect ".*" into result
               else if (char= c #\?) ; process ?
               collect "." into result
               else if (char= c #\[) ; range found
               collect ;;(extract-range i)
               (if-let (close-pos (position #\] pattern :start i)) ;; find closing ]
                   ;; found, replace \ with \\
                   (let ((res (ppcre:regex-replace-all "\\" (subseq pattern (1+ i ) close-pos) "\\\\" )))
                     (setf i close-pos) ; increase current position to the end of range
                     (format nil "[~a]"
                             (cond ((char= (char res 0) #\!)
                                    (concatenate 'string "^" (subseq res 1)))
                                   ((char= (char res 0) #\^)
                                    (concatenate 'string "\\" res))
                                   (t res))))
                 ;; no closing range character found, assuming special
                 "\\[")
               into result
               else ; finally just append rest (quoting specials of course)
               collect (ppcre:quote-meta-chars (string c)) into result
               end
               finally
               (return (apply #'concatenate 'string result)))))
    (concatenate 'string
                 (unless case-sensitive-p "(?i)")
                 (when beginning-of-string "^")
                 regex
                 (when end-of-string "$"))))



