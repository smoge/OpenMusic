(in-package :om)

(defvar *scheduler-files* nil)

(setq *scheduler-files*
      '("s-scheduler"
        "schedulable-action"
        "schedulable-object"
        "fifo"))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *scheduler-files*))