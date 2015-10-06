;; -*- mode: lisp; package: repl -*-

(in-package :repl)

(defvar *command-table* (make-hash-table))

(defun command-p (x)
  (gethash x *command-table*))

(defmacro define-command (name parms &body body)
  `(setf (gethash ',name *command-table*)
         #'(lambda ,parms ,@body)))

(defun call-command (cmd args)
  (apply (gethash cmd *command-table*) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             (list (subseq str (+ pos 1)) (subseq str 0 pos) t))))
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
                 (body sym symbol-name (format nil "~a:" package))))
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

(rl:register-function :complete #'symbol-complete)

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
      :for error-p := nil
      :for count :from 0 :do
      (handler-case (setf (values x pos)
                          (read-from-string line nil))
        (error () (setq error-p t)))
      (cond (error-p
             (setq line
                   (concatenate 'string line " "
                                (rl:readline :already-prompted t))))
            ((and (zerop count) (symbolp x) (not (null x)))
             (add-history line)
             (cond ((command-p x)
                    (let ((args
                           (read-args-from-string
                            (subseq line pos))))
                      (return (call-command x args))))
                   ((fboundp x)
                    (let ((expr
                           `(,x ,@(read-args-from-string
                                   (subseq line pos)))))
                      (return expr)))
                   (t
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
      (setq - (readline-read "> "))
      (restart-case (eval-print -)
        (restart-toplevel () :report "Restart toplevel.")))))
