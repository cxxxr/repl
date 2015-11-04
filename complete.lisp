(in-package :repl)

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
        (when (and package (not (find-package package)))
          (return-from symbol-complete nil))
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
  (loop :with path = (probe-file path)
    :for pathname :in (cl-fad:list-directory path)
    :for name := (enough-namestring pathname path)
    :when (eql 0 (search text name))
    :collect name))

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
  (declare (ignore text end))
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
  (declare (ignore end))
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
