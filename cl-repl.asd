(defpackage :cl-repl
  (:nicknames :repl)
  (:use :cl)
  (:export :repl))

(defsystem cl-repl
           :serial t
           :components ((:file "cl-repl"))
           :depends-on (:cl-fad :sb-posix))
