(defpackage :shell-command
  (:use :cl :cffi)
  (:export :shell-command))

(in-package :shell-command)

(define-foreign-library libshell-command
  (:unix "shell-command.so"))

(defun load-directory ()
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :directory (pathname-directory here))))

(let ((*foreign-library-directories* (list (load-directory))))
  (load-foreign-library 'libshell-command))

(defcfun "shell_command" :int
         (cmd :string))
