(in-package :repl)

(defun cd (dirname)
  (uiop:chdir dirname))

(defun pwd ()
  (uiop:getcwd))

(defun dir ()
  (dolist (path (cl-fad:list-directory (uiop:getcwd)))
    (format t "~&~a~%" (enough-namestring path (uiop:getcwd)))))

(defun ld (pathspec &rest args)
  (apply #'load
         (merge-pathnames (pathname pathspec)
                          (uiop:getcwd))
         args))

(defun add-history (str)
  (cffi:foreign-funcall "add_history"
                        :string str
                        :void))

(defun finish-sexp-p (string)
  (handler-case (progn
                  (read-from-string string nil)
                  t)
    (error ()
           (return-from finish-sexp-p nil))))

(cffi:defcfun ("rl_newline") :int
              (count :int)
              (key :int))

(defun newline (arg key)
  (declare (ignore arg key))
  (if (finish-sexp-p rl:*line-buffer*)
      (rl-newline 1 0)
      (rl:insert-text (string #\newline))))

(rl:bind-keyseq (string #\newline) #'newline)
(rl:bind-keyseq (string #\return) #'newline)

(defvar *eof-value* (gensym "EOF"))

(defun read-args-from-string (str)
  (with-input-from-string (in str)
    (loop :for x := (read in nil *eof-value*)
      :until (eq x *eof-value*)
      :collect x)))

(defun readline-read (prompt)
  (let ((string-expr (rl:readline :prompt prompt)))
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
               (format nil "~&~a> "
                       (package-name *package*))))
      (when (eq - *eof-value*)
        (return))
      (restart-case (eval-print -)
        (restart-toplevel () :report "Restart toplevel.")))))
