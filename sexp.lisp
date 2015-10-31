(in-package :repl)

(defvar *whitespaces* (list #\space #\tab #\newline))
(defvar *expr-prefix-chars* (list #\' #\, #\` #\@ #\#))

(defvar *syntax-types*
  '(:space
    :symbol
    :open-paren
    :closed-paren
    :string-quote
    :escape
    :expr-prefix))

(defun bool (x)
  (if x t nil))

(defun space-char-p (c)
  (bool (member c *whitespaces*)))

(defun symbol-char-p (c)
  (bool (or (char<= #\a c #\z)
            (char<= #\A c #\Z)
            (char<= #\0 c #\9)
            (not (member c '(#\( #\) #\" #\space #\tab #\newline))))))

(defun expr-prefix-char-p (c)
  (bool (member c *expr-prefix-chars*)))

(defun forward-syntax-type (line-buffer point)
  (unless (<= 0 point (1- (length line-buffer)))
    (return-from forward-syntax-type :space))
  (let ((c (aref line-buffer point)))
    (cond ((and (< 0 point)
                (char= #\\
                       (aref line-buffer (1- point))))
           :symbol)
          ((space-char-p c)
           :space)
          ((symbol-char-p c)
           :symbol)
          ((char= c #\()
           :open-paren)
          ((char= c #\))
           :closed-paren)
          ((char= c #\")
           :string-quote)
          ((char= c #\\)
           :symbol)
          ((expr-prefix-char-p c)
           :expr-prefix)
          (t
           :symbol))))

(defun skip-forward (line-buffer point skip-p)
  (cond ((<= (length line-buffer) point)
         (length line-buffer))
        ((funcall skip-p (aref line-buffer point))
         (skip-forward line-buffer (1+ point) skip-p))
        (t
         point)))

(defun skip-space-forward (line-buffer point)
  (skip-forward line-buffer point #'space-char-p))

(defun skip-expr-prefix-forward (line-buffer point)
  (skip-forward line-buffer point #'expr-prefix-char-p))

(defun skip-list-forward (line-buffer point &optional (depth 0))
  (loop
    (incf point)
    (when (<= (length line-buffer) point)
      (throw 'scan-error nil))
    (case (forward-syntax-type line-buffer point)
      ((:open-paren)
       (incf depth))
      ((:closed-paren)
       (when (minusp (decf depth))
         (return (1+ point))))
      ((:string-quote)
       (setq point (skip-string-forward line-buffer point))))))

(defun skip-string-forward (line-buffer point)
  (loop
    (incf point)
    (when (<= (length line-buffer) point)
      (throw 'scan-error nil))
    (case (forward-syntax-type line-buffer point)
      ((:string-quote)
       (return (1+ point))))))

(defun skip-atom-forward (line-buffer point)
  (loop
    (incf point)
    (when (<= (length line-buffer) point)
      (return (length line-buffer)))
    (case (forward-syntax-type line-buffer point)
      ((:symbol))
      (otherwise
       (return point)))))

(defun skip-sexp-forward (line-buffer point)
  (setq point (skip-expr-prefix-forward line-buffer point))
  (case (forward-syntax-type line-buffer point)
    ((:open-paren)
     (skip-list-forward line-buffer point))
    ((:closed-paren)
     point)
    ((:string-quote)
     (skip-string-forward line-buffer point))
    (otherwise
     (skip-atom-forward line-buffer point))))

(defun forward-sexp-point (line-buffer point)
  (setq point (skip-space-forward line-buffer point))
  (or (catch 'scan-error
        (skip-sexp-forward line-buffer point))
      point))

(defun forward-sexp (arg key)
  (setq rl:*point*
        (forward-sexp-point rl:*line-buffer*
                            rl:*point*)))

(rl:bind-keyseq "\\e\\C-f" #'forward-sexp)

(defun backward-syntax-type (line-buffer point)
  (forward-syntax-type line-buffer (1- point)))

(defun skip-backward (line-buffer point skip-p)
  (cond ((<= point 0)
         0)
        ((funcall skip-p (aref line-buffer (1- point)))
         (skip-backward line-buffer (1- point) skip-p))
        (t
         point)))

(defun skip-space-backward (line-buffer point)
  (skip-backward line-buffer point #'space-char-p))

(defun skip-expr-prefix-backward (line-buffer point)
  (skip-backward line-buffer point #'expr-prefix-char-p))

(defun skip-list-backward (line-buffer point &optional (depth 0))
  (loop
    (decf point)
    (when (<= point 0)
      (throw 'scan-error nil))
    (case (backward-syntax-type line-buffer point)
      ((:closed-paren)
       (incf depth))
      ((:open-paren)
       (when (minusp (decf depth))
         (return (1- point))))
      ((:string-quote)
       (setq point (skip-string-backward line-buffer point))))))

(defun skip-string-backward (line-buffer point)
  (loop
    (decf point)
    (when (<= point 0)
      (throw 'scan-error nil))
    (case (backward-syntax-type line-buffer point)
      ((:string-quote)
       (return (1- point))))))

(defun skip-atom-backward (line-buffer point)
  (loop
    (decf point)
    (when (<= point 0)
      (return 0))
    (case (backward-syntax-type line-buffer point)
      ((:symbol))
      (otherwise
       (return point)))))

(defun skip-sexp-backward (line-buffer point)
  (setq point (skip-expr-prefix-backward line-buffer point))
  (case (backward-syntax-type line-buffer point)
    ((:closed-paren)
     (skip-list-backward line-buffer point))
    ((:open-paren)
     point)
    ((:string-quote)
     (skip-string-backward line-buffer point))
    (otherwise
     (skip-atom-backward line-buffer point))))

(defun backward-sexp-point (line-buffer point)
  (setq point (skip-space-backward line-buffer point))
  (or (catch 'scan-error
        (skip-sexp-backward line-buffer point))
      point))

(defun backward-sexp (arg key)
  (setq rl:*point*
        (backward-sexp-point rl:*line-buffer*
                             rl:*point*)))

(rl:bind-keyseq "\\e\\C-b" #'backward-sexp)

(defun down-list-internal (line-buffer point)
  (loop
    (when (<= (length line-buffer) point)
      (return nil))
    (when (eq :open-paren (forward-syntax-type line-buffer point))
      (return (1+ point)))
    (incf point)))

(defun down-list (arg key)
  (let ((point (down-list-internal rl:*line-buffer* rl:*point*)))
    (when point
      (setq rl:*point* point))))

(rl:bind-keyseq "\\e\\C-d" #'down-list)

(defun up-list-internal (line-buffer point)
  (loop
    (when (<= point 0)
      (return nil))
    (when (eq :open-paren (backward-syntax-type line-buffer point))
      (return (1- point)))
    (decf point)))

(defun up-list (arg key)
  (let ((point (up-list-internal rl:*line-buffer* rl:*point*)))
    (when point
      (setq rl:*point* point))))

(rl:bind-keyseq "\\e\\C-u" #'up-list)
