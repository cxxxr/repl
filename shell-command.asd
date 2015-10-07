#-asdf
(require :asdf)

(defpackage :shell-command-asd
  (:use :cl :asdf))

(in-package :shell-command-asd)

(defsystem shell-command
  :serial t
  :components ((:file "shell-command"))
  :depends-on (:cffi))
