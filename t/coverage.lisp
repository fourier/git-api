;;;; base.lisp
;; File containing common settings for all unit tests
;; 
(in-package :cl-user)
(defpackage git-api.test.coverage
  (:use :cl)
  (:export run-tests-with-coverage))
   
(in-package :git-api.test.coverage)

#+lispworks7
(defun generate-coverage-output-path ()
  (multiple-value-bind (second minute hour date month year);; day)
      (get-decoded-time)
    (let ((results-directory-name
           (pathname
            (format nil "git-api-coverage_~4,'0d-~2,'0d-~2,'0d_~2,'0d_~2,'0d_~2,'0d/index.html"
                    year month date hour minute second))))
      
      (merge-pathnames results-directory-name (hcl:get-temp-directory)))))

  
#+lispworks7
(defun run-lw-test-coverage ()
  (hcl:clear-code-coverage)
  (hcl:with-code-coverage-generation ()
    (asdf/operate:load-system :git-api :force t))
  (asdf/operate:test-system :git-api-test)
  (let ((output-file (generate-coverage-output-path)))
    (hcl:code-coverage-data-generate-coloring-html output-file)
    (format *standard-output* "Generated coverage report to ~a" output-file)
    #+macosx
    (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "openURL:"
                 (objc:invoke "NSURL" "URLWithString:"
                              (concatenate 'string "file://" (namestring output-file))))))


(defun run-tests-with-coverage ()
  #+lispworks7
  (run-lw-test-coverage)
  #-lispworks7
  (error "Code coverage generation currently supported only on LispWorks 7 and above"))