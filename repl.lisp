(in-package :repl)

(defvar *eof-value* (gensym "EOF"))

(defun add-history (str)
  (cffi:foreign-funcall "add_history"
                        :string str
                        :void))

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
              (cond ((keywordp x)
                     (values x
                             (read-args-from-string
                              (subseq string-expr i))))
                    ((fboundp x)
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

(defvar *backtrace-string* nil)

(defun one-of (choices)
  (loop
    :for n :from 0 :by 1
    :for c :in choices
    :do (format t "~&[~d] ~a~%" n c))
  (let ((*commands* *commands*))
    (add-command '(:bt :backtrace)
                 #'(lambda ()
                     (format t "~&~a~%" *backtrace-string*))
                 "Print backtrace.")
    (add-command '(:c :cont :continue)
                 #'(lambda (arg)
                     (when (typep arg `(integer 0 ,(1- (length choices))))
                       (return-from one-of (nth arg choices))))
                 "Continue execution.")
    (loop
      (multiple-value-bind (x args)
          (readline-read (format nil "~%[DEBUGGER]~%"))
        (cond ((typep x `(integer 0 ,(1- (length choices))))
               (return (nth x choices)))
              ((eq x *eof-value*)
               (return (find-restart 'restart-toplevel)))
              (t
               (let ((result (find-command x)))
                 (handler-case
                     (if result
                         (apply result args)
                         (eval x))
                   (error (condition)
                          (format t "~&~a~%" condition))))))))))

(defun debugger (condition me-or-my-encapsulation)
  (setq *backtrace-string*
        (with-output-to-string (out)
          (uiop/image:print-backtrace
           :stream out
           :condition condition)))
  (format t "~&~a~%" condition)
  (let* ((restart (one-of (compute-restarts)))
         (*debugger-hook* me-or-my-encapsulation))
    (invoke-restart-interactively restart)))

(let (* ** *** - + ++ +++ / // /// values)
  (defun eval-print (-)
    (setq values
          (multiple-value-list
           (let ((*debugger-hook* #'debugger))
             (eval -))))
    (setq +++ ++ /// //     *** (car ///)
          ++  +  //  /      **  (car //)
          +   -  /   values *   (car /))
    (mapc #'pprint values)
    (terpri))
  (defun repl ()
    (init)
    (loop :with args := nil :do
      (multiple-value-setq (- args)
                           (readline-read
                            (format nil "~&~%[~a]~%"
                                    (package-name *package*))))
      (when (eq - *eof-value*)
        (return))
      (let ((result (find-command -)))
        (if result
            (apply result args)
            (restart-case (eval-print -)
              (restart-toplevel () :report "Restart toplevel.")))))))
