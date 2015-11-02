(in-package :repl)

(defvar *commands* nil)

(defun add-command (names function docstring)
  (push (list names function docstring)
        *commands*))

(defmacro define-command ((name &rest rest-names) parameters &body body)

  `(add-command ',(cons name rest-names)
                #'(lambda ,parameters ,@body)
                ,(if (and body (stringp (car body)))
                     (car body)
                     "")))

(defun find-command (x)
  (loop :for (names function docstring) :in *commands* :do
    (when (member x names)
      (return-from find-command function))))

(define-command (:h :help) ()
  (loop :for (names function docstring) :in *commands* :do
    (format t "~&~{~s~^ ~}~20,5T ~a~%" names docstring)))

(define-command (:ap :apropos) (symbol &optional package external-only)
  (apropos symbol package external-only))

(define-command (:in :in-package) (name)
  (let ((package (find-package name)))
    (if package
        (setq *package* package)
        (format t "~&package not found: ~a~%" name))))

(define-command (:use :use-package) (name)
  (let ((package (find-package name)))
    (if package
        (use-package package)
        (format t "~&package not found: ~a~%" name))))

(define-command (:de :describe) (symbol)
  (describe symbol))

(define-command (:tr :trace) (&rest specs)
  (eval `(trace ,@specs)))

(define-command (:untr :untrace) (&rest specs)
  (eval `(untrace ,@specs)))
