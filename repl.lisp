(in-package :repl)

(defun add-history (str)
  (cffi:foreign-funcall "add_history"
                        :string str
                        :void))

(defvar *eof-value* (gensym "EOF"))

(defun read-args-from-string (str)
  (with-input-from-string (in str)
    (loop :for x := (read in nil *eof-value*)
      :until (eq x *eof-value*)
      :collect x)))

(defun readline-read (prompt)
  (let ((string-expr (readline prompt)))
    (if (null string-expr)
        *eof-value*
        (multiple-value-bind (x i)
            (read-from-string string-expr nil)
          (add-history string-expr)
          (if (and (symbolp x) (not (null x)))
              (cond ((fboundp x)
                     `(,x ,@(read-args-from-string
                             (subseq string-expr i))))
                    ((boundp x)
                     x)
                    ((let ((str (string-left-trim '(#\space #\tab)
                                                  string-expr)))
                       (if (and (< 0 (length str))
                                (char= #\! (aref str 0)))
                           `(shell-command ,(subseq str 1))
                           x))))
              x)))))

(defun load-rc (pathname)
  (let ((pathname (probe-file pathname)))
    (when pathname
      (format t "~&loading ~a~%" pathname)
      (load pathname :verbose nil)
      (format t "~&loaded ~a~%" pathname)
      t)))

(let ((loaded nil))
  (defun init (&optional force)
    (when (or (not loaded) force)
      (setq loaded t)
      (or (load-rc (merge-pathnames ".replrc.lisp" (user-homedir-pathname)))
          (load-rc "replrc.lisp")))))

(let (* ** *** - + ++ +++ / // /// values)
  (defun eval-print (-)
    (setq values
          (multiple-value-list (eval -)))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (mapc #'pprint values)
    (terpri))
  (defun repl ()
    (init)
    (loop
      (setq - (readline-read
               (format nil "~&~%[~a]~%"
                       (package-name *package*))))
      (when (eq - *eof-value*)
        (return))
      (restart-case (eval-print -)
        (restart-toplevel () :report "Restart toplevel.")))))
