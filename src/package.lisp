#|
  This file is a part of git-api project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2019
|#

(in-package :cl-user)
(defpackage git-api
  (:use :git-api.repo :git-api.object)
  (:export make-git-repo
           git-repo-close
           rev-parse           
           get-commit
           get-object-by-hash
           cat-file))


