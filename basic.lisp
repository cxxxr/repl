(in-package :repl)

(cffi:defcfun ("rl_newline") :int
              (count :int)
              (key :int))

(cffi:defcfun ("rl_forward_char") :int
              (count :int)
              (key :int))

(cffi:defcfun ("rl_backward_char") :int
              (count :int)
              (key :int))

(cffi:defcfun ("rl_beg_of_line") :int
              (count :int)
              (key :int))

(cffi:defcfun ("rl_end_of_line") :int
              (count :int)
              (key :int))

(cffi:defcfun ("rl_get_previous_history") :int
              (count :int)
              (key :int))

(cffi:defcfun ("rl_get_next_history") :int
              (count :int)
              (key :int))

(defvar *editor-column* 0)

(defun accept-line ()
  (setq *editor-column* 0)
  (rl-newline 1 0))

(defun finish-sexp-p (string)
  (handler-case (progn
                  (read-from-string string nil)
                  t)
    (error ()
           (return-from finish-sexp-p nil))))

(defun newline (arg key)
  (declare (ignore arg key))
  (if (finish-sexp-p rl:*line-buffer*)
      (accept-line)
      (rl:insert-text (string #\newline))))

(rl:bind-keyseq (string #\newline) #'newline)
(rl:bind-keyseq (string #\return) #'newline)

(defun readline (prompt)
  (rl:readline :prompt (format nil "~a~%" prompt)))

(defun current-linum ()
  (1+ (count #\newline rl:*line-buffer* :end rl:*point*)))

(defun nlines ()
  (1+ (count #\newline rl:*line-buffer*)))

(defun current-column ()
  (- rl:*point* (bol-point rl:*line-buffer* rl:*point*)))

(defun bol-point (&optional (line-buffer rl:*line-buffer*)
                            (point rl:*point*))
  (or (loop :for i :downfrom point :to 1 :do
        (when (char= #\newline (aref line-buffer (1- i)))
          (return i)))
      0))

(defun eol-point (&optional (line-buffer rl:*line-buffer*)
                            (point rl:*point*))
  (or (position #\newline line-buffer :start point)
      (length line-buffer)))

(defun prev-line (arg key)
  (declare (ignore arg key))
  (when (< 1 (current-linum))
    (let* ((bol-point (bol-point rl:*line-buffer* rl:*point*))
           (column (current-column))
           (pl-bol-point (bol-point rl:*line-buffer* (1- bol-point)))
           (pl-width (- bol-point pl-bol-point 1)))
      (setq *editor-column* (max *editor-column* column))
      (setq rl:*point* (+ pl-bol-point (min pl-width *editor-column*))))))

(rl:bind-keyseq "\\C-p" #'prev-line)

(defun next-line (arg key)
  (declare (ignore arg key))
  (when (< (current-linum) (nlines))
    (let* ((bol-point (bol-point rl:*line-buffer* rl:*point*))
           (column (current-column))
           (nl-bol-point (1+ (eol-point rl:*line-buffer* rl:*point*)))
           (nl-width (- (eol-point rl:*line-buffer* nl-bol-point)
                        nl-bol-point)))
      (setq *editor-column* (max *editor-column* column))
      (setq rl:*point* (+ nl-bol-point (min nl-width *editor-column*))))))

(rl:bind-keyseq "\\C-n" #'next-line)

(defun forward-char (arg key)
  (declare (ignore arg key))
  (when (< rl:*point* (length rl:*line-buffer*))
    (incf rl:*point*)
    (setq *editor-column* (current-column))))

(rl:bind-keyseq "\\C-f" #'forward-char)

(defun backward-char (arg key)
  (declare (ignore arg key))
  (when (< 0 rl:*point*)
    (decf rl:*point*)
    (setq *editor-column* (current-column))))

(rl:bind-keyseq "\\C-b" #'backward-char)

(defun beginning-of-line (arg key)
  (declare (ignore arg key))
  (setq rl:*point* (bol-point))
  (setq *editor-column* (current-column)))

(rl:bind-keyseq "\\C-a" #'beginning-of-line)

(defun end-of-line (arg key)
  (declare (ignore arg key))
  (setq rl:*point* (eol-point))
  (setq *editor-column* (current-column)))

(rl:bind-keyseq "\\C-e" #'end-of-line)

(defun beginning-of-buffer (arg key)
  (declare (ignore arg key))
  (rl-beg-of-line 1 0))

(rl:bind-keyseq "\\e<" #'beginning-of-buffer)

(defun end-of-buffer (arg key)
  (declare (ignore arg key))
  (rl-end-of-line 1 0))

(rl:bind-keyseq "\\e>" #'end-of-buffer)

(defun previous-history (arg key)
  (declare (ignore arg key))
  (rl-get-previous-history 1 0))

(rl:bind-keyseq "\\ep" #'previous-history)

(defun next-history (arg key)
  (declare (ignore arg key))
  (rl-get-next-history 1 0))

(rl:bind-keyseq "\\en" #'next-history)
