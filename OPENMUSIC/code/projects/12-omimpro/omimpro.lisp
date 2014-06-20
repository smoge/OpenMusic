(in-package :om)

(defvar *omimpro-files* nil)

(setq *omimpro-files*
      '("ImproteK\&co;improtek;sources;LoadImprotek.lisp"
        "database;scenarios;scenarios.lisp"
        "database;oracles;oracles.lisp"
        "handler;impro-handler"))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *omimpro-files*))
