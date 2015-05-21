(in-package :om)

(defvar *omimpro-files* nil)

(setq *omimpro-files*
      '("impro;improtek;sources;LoadImprotek.lisp"
        "database;scenarios;scenarios.lisp"
        "database;oracles;oracles.lisp"
        ;"handler;impro-handler"
        "handler;smc-handler"
        "handler;smc-query"))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *omimpro-files*))
