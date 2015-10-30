;; -*- mode: lisp; package: repl -*-

(defpackage :repl
  (:use :cl :shell-command :util)
  (:export
   :repl
   :cd
   :pwd
   :dir
   :ld))

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

(defun common-prefix (items)
  (subseq (car items)
          0
          (apply #'min
                 (mapcar #'(lambda (item)
                             (or (mismatch (car items) item)
                                 (length item)))
                         (cdr items)))))

(defun package-prefix (str)
  (cond ((let ((pos (search "::" str)))
           (when pos
             (list (subseq str (+ pos 2)) (subseq str 0 pos) nil))))
        ((let ((pos (position #\: str)))
           (when pos
             (list (subseq str (+ pos 1))
                   (if (zerop pos)
                       "KEYWORD"
                       (subseq str 0 pos))
                   t))))
        (t
         (list str nil nil))))

(defun symbol-complete (text start end)
  (declare (ignore start end))
  (let ((text (string-upcase text))
        (els))
    (flet ((body (sym text prefix)
                 (let ((name (string sym)))
                   (when (eql 0 (search text name))
                     (push (format nil "~(~a~a~)" prefix name)
                           els)))))
      (destructuring-bind (symbol-name package external-p)
          (package-prefix text)
        (cond ((and package external-p)
               (do-external-symbols (sym package)
                 (body sym symbol-name
                       (if (equal (package-name :keyword)
                                  (package-name package))
                           ":"
                           (format nil "~a:" package)))))
              (package
               (do-symbols (sym package)
                 (body sym symbol-name (format nil "~a::" package))))
              (t
               (do-symbols (sym *package*)
                 (body sym symbol-name ""))
               (dolist (package (list-all-packages))
                 (body (format nil "~a:" (package-name package))
                       symbol-name "")
                 (dolist (package-name (package-nicknames package))
                   (body (format nil "~a:" package-name)
                         symbol-name "")))))))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

(defun filter-filenames (path text)
  (let* ((path (probe-file path))
         (pathname-length (length (namestring path))))
    (loop
      :for pathname :in (cl-fad:list-directory path)
      :for name := (enough-namestring pathname path)
      :when (eql 0 (search text name))
      :collect name)))

(defun file-complete (text default-directories-fn)
  (if (find #\/ text)
      (let* ((slash-pos (1+ (position #\/ text :from-end t)))
             (dir (subseq text 0 slash-pos))
             (name (subseq text slash-pos)))
        (mapcar #'(lambda (name)
                    (format nil "~a~a" dir name))
                (filter-filenames dir name)))
      (mapcan #'(lambda (path)
                  (filter-filenames path text))
              (funcall default-directories-fn))))

(defun do-filename-complete-p (text start end)
  (declare (ignore end))
  (or (and (< 0 start)
           (eql #\" (aref rl:*line-buffer*
                          (1- start))))
      (let ((str (string-trim '(#\space #\tab) rl:*line-buffer*)))
        (and (< 0 (length str))
             (eql #\! (aref str 0))))))

(defun shell-command-complete (text)
  (let ((els
         (file-complete text
                        #'(lambda ()
                            (split-sequence:split-sequence
                             #\: (uiop:getenv "PATH"))))))
    (cond ((null els)
           nil)
          ((cdr els)
           (cons (format nil "!~a" (common-prefix els)) els))
          (t
           (list (format nil "!~a" (car els)))))))

(defun shell-complete (text start end)
  (when (zerop start)
    (setq text
          (subseq (string-trim '(#\space #\tab)
                               text)
                  1)))
  (shell-command-complete text))

(defun repl-complete (text start end)
  (let ((linebuf (string-trim '(#\space #\tab) rl:*line-buffer*)))
    (cond ((and (zerop start)
                (< 0 (length linebuf))
                (eql #\! (aref linebuf 0)))
           (shell-complete text start end))
          ((do-filename-complete-p text start end)
           (let ((els
                  (file-complete text
                                 #'(lambda ()
                                     (list (uiop:getcwd))))))
             (if (cdr els)
                 (cons (common-prefix els) els)
                 els)))
          (t
           (symbol-complete text start end)))))

(rl:register-function :complete #'repl-complete)

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
