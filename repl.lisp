(in-package :repl)

(defparameter *eof-value* (gensym "EOF"))
(defparameter *backtrace* nil)

(defparameter *commands*
  '((:ld load command-args)
    (:e ed command-args)
    (:ed ed command-args)
    (:pwd shell-pwd)
    (:dir shell-dir)
    (:cd shell-cd shell-args)
    ))

(defun shell-pwd ()
  (princ (sb-posix:getcwd))
  (values))

(defun shell-dir ()
  (mapc #'(lambda (path)
            (fresh-line)
            (princ path))
        (cl-fad:list-directory (sb-posix:getcwd)))
  (values))

(defun shell-cd (&optional dir)
  (sb-posix:chdir
   (typecase dir
     (string (cl-fad:pathname-as-directory 
              (if (string= dir "~") "~/" dir)))
     (pathname dir)
     (otherwise (pathname (user-homedir-pathname)))))
  (values))

(defun shell-args ()
  (unread-char #\newline)
  (let ((line (read-line)))
    (with-input-from-string (in line)
      (when (peek-char t in nil)
        (list (string-trim '(#\space #\tab) (read-line in)))))))

(defun read-args ()
  (unread-char #\newline)
  (let ((line (read-line))
        (args))
    (loop
      (multiple-value-bind (x start)
          (read-from-string line nil *eof-value*)
        (cond ((eq x *eof-value*)
               (return))
              (t
               (setf line (subseq line start))
               (push x args)))))
    (nreverse args)))

(defun symbol-to-file-name (x)
  (if (not (symbolp x))
      x
      (let ((name (string-downcase (symbol-name x))))
        (if (not (find #\. name))
            (concatenate 'string name ".lisp")
            name))))

(defun command-args ()
  (mapcar #'symbol-to-file-name (read-args)))

(defun exec-command (x)
  (let ((elt (assoc x *commands*)))
    (when elt
      (values (apply (second elt)
                     (if (third elt)
                         (funcall (third elt))
                         (read-args)))
              t))))

(defun one-of (choices)
  (do ((c choices (cdr c))
       (n 1 (1+ n)))
      ((null c))
    (format t "~&[~d] ~a~%" n (car c)))
  (let ((n (length choices))
        (i))
    (do () ((typep i `(integer 1 ,n)))
      (let ((x (readline-read ">>")))
        (setf i x)
        (when (and (not (integerp x))
                   (not (eq x *eof-value*)))
          (handler-case (eval-print x)
            (error (condition)
                   (princ condition *error-output*)
                   (terpri *error-output*)))))
      (fresh-line))
    (nth (1- i) choices)))

(defun debugger (condition me-or-my-condition)
  (format t "~&~a" condition)
  (let ((choice (one-of (compute-restarts))))
    (assert choice)
    (let ((*debugger-hook* me-or-my-condition))
      (invoke-restart-interactively choice))))

(defun error-handle (condition)
  (declare (ignore condition))
  (setf *backtrace* (sb-debug:backtrace-as-list)))

(defun readline-read (prompt)
  (let ((line (rl:readline :prompt prompt :add-history t)))
    (loop
      (let ((x (handler-case (read-from-string line nil)
                 (error (cdt)
                        (declare (ignore cdt)) nil))))
        (if x
            (return x)
            (setf line
                  (concatenate 'string line " "
                   (rl:readline :add-history t :already-prompted t))))))))

(let (* ** *** - + ++ +++ / // /// vals)
  (defun eval-print (-)
    (multiple-value-bind (x exec-p)
        (exec-command -)
      (setf vals
            (if exec-p
                (list x)
                (multiple-value-list (eval -)))))
    (setf +++ ++ /// //   *** (first ///)
          ++  +  //  /    **  (first //)
          +   -  /   vals *   (first /))
    (mapc #'print vals)
    (terpri))
  (defun repl ()
    (let ((*debugger-hook* #'debugger))
      (loop
        (setf - (readline-read ">"))
        (when (eq - *eof-value*)
          (return))
        (restart-case
            (handler-bind ((error #'error-handle))
              (eval-print -))
          (restart-toplevel ()
                            :report "Restart toplevel."))))))
