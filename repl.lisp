;; -*- mode: lisp; package: repl -*-

(in-package :repl)

(defun command (cmd arg-string)
  (multiple-value-bind (stdout-string stderr-string $?)
      (trivial-shell:shell-command
       (format nil "~(~a~) ~a" cmd arg-string))
    (princ stdout-string)
    (princ stderr-string)
    $?))

(defun readline-read (prompt)
  (let ((line (rl:readline :prompt prompt :add-history t)))
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
                                (rl:readline :already-prompted t
                                             :add-history t))))
            ((and (zerop count) (symbolp x))
             (return `(command ',x ,(subseq line pos))))
            (t
             (return x))))))

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
    (loop
      (setq - (readline-read "> "))
      (restart-case (eval-print -)
        (restart-toplevel () :report "Restart toplevel.")))))
