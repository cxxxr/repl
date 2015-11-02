(load "shell-command.asd")

#-asdf
(require :asdf)

(defpackage :repl-asd
  (:use :cl :asdf))

(in-package :repl-asd)

(defsystem repl
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "complete")
               (:file "editor")
               (:file "sexp")
               (:file "command")
               (:file "repl"))
  :depends-on (:cl-readline
               :shell-command
               :cl-fad
               :split-sequence))
