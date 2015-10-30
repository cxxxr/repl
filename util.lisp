(in-package :repl)

(defun pdebug (x)
  (with-open-file (out "DEBUG"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (print x out)))
