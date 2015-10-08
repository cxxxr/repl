;; -*- mode: lisp; package: repl -*-

(defpackage :repl
  (:use :cl :shell-command)
  (:export
   :repl
   :cd
   :pwd
   :dir))

(in-package :repl)

(defun cd (dirname)
  (uiop:chdir dirname))

(defun pwd ()
  (uiop:getcwd))

(defun dir ()
  (dolist (path (cl-fad:list-directory "."))
    (format t "~&~a~%" path)))

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
               (dolist (pkg (list-all-packages))
                 (body (format nil "~a:" (package-name pkg))
                       symbol-name ""))))))
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

(defun add-history-expr (x)
  (add-history (string-downcase (prin1-to-string x))))

(defun read-args-from-string (str)
  (with-input-from-string (in str)
    (loop :for x := (read in nil '#1=#:eof)
      :until (eq x '#1#)
      :collect x)))

(defun readline-read (prompt)
  (let ((line (rl:readline :prompt prompt)))
    (loop
      :with x :and pos
      :for error-p := nil :do
      (handler-case (setf (values x pos)
                          (read-from-string line nil))
        (error () (setq error-p t)))
      (cond (error-p
             (setq line
                   (concatenate 'string line " "
                                (rl:readline :already-prompted t))))
            ((and (symbolp x) (not (null x)))
             (add-history line)
             (cond ((fboundp x)
                    (let ((expr
                           `(,x ,@(read-args-from-string
                                   (subseq line pos)))))
                      (return expr)))
                   ((boundp x)
                    (return x))
                   (t
                    (let ((str (string-left-trim '(#\space #\tab) line)))
                      (when (and (< 0 (length str))
                                 (eql #\! (aref str 0)))
                        (return `(shell-command ,(subseq str 1)))))
                    (return x))))
            (t
             (add-history-expr x)
             (return x))))))

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
      (restart-case (eval-print
                     (setq - (readline-read
                              (format nil "~&~a> "
                                      (package-name *package*)))))
        (restart-toplevel () :report "Restart toplevel.")))))
