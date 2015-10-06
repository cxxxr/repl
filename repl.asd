(defpackage :repl
  (:use :cl)
  (:export :repl))

(require :asdf)

(defpackage :repl-asd
  (:use :cl :asdf))

(in-package :repl-asd)

(defsystem repl
  :serial t
  :components ((:file "repl"))
  :depends-on (:cl-readline
               :trivial-shell))
