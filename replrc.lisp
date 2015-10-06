(require :lem)

(push #'(lambda (x)
          (if x
              (lem:lem x)
              (lem:lem))
          t)
      sb-ext:*ed-functions*)

(rl:bind-keyseq "\\ee"
                #'(lambda (arg key)
                    (declare (ignore arg key))
                    (ed)))

(rl:bind-keyseq "\\C-x\\C-y"
                #'(lambda (arg key)
                    (declare (ignore key))
                    (rl:insert-text
                     (lem:kill-ring-nth-string arg))))
