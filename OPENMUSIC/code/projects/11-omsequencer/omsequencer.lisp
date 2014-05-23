(in-package :om)

(defvar  *omsequencer-files* nil)

(setf *omsequencer-files* 
      '("scheduler;om-scheduler"
        "scheduler;om-sequencer-scheduler"
        "worker;om-thread-pool"
        "worker;om-event-engine"
        "maquettelink;sch-maquette_link-patch"))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *omsequencer-files*))

(sch::init-sequencer-scheduler)
(sch::init-thread-pool 8)


#|
(setq *task-tree* (list
                   (make-t-task :name "END"
                                :routine #'(lambda (a) (print "----Tree computation done----") (sleep 3)
                                             ;(print (list "resultat" a))
                                             )
                                :data (list nil nil) 
                                :callback #'(lambda (self) ;(print "END ALL") 
                                              ))
                   (list
                    (make-t-task :name "1"
                                 :routine #'(lambda (a) (print "----Tree computation begins----")
                                              a)
                                 :data '(nil nil) 
                                 :callback #'(lambda (self) ;(print "END 1")
                                               ))
                    (make-t-task :name "5"
                                 :routine #'(lambda (a) ;(print "START 5")
                                              (sleep 2.5)
                                              a)
                                 :data '(5) 
                                 :callback #'(lambda (self) ;(print "END 5")
                                               ))
                    (make-t-task :name "6"
                                 :routine #'(lambda (a) ;(print "START 6")
                                              (sleep 2)
                                              a)
                                 :data '(6) 
                                 :callback #'(lambda (self) ;(print "END 6")
                                               )))
                   (list (make-t-task :name "2"
                                      :routine #'(lambda (a) ;(print "START 2") 
                                                   (sleep 3) a)
                                      :data '(nil nil) 
                                      :callback #'(lambda (self) ;(print "END 2")
                                                    ))
                         (make-t-task :name "3"
                                      :routine #'(lambda (a) ;(print "START 3") 
                                                   (sleep 1) a)
                                      :data '(3) 
                                      :callback #'(lambda (self) ;(print "END 3")
                                                    ))
                         (make-t-task :name "4"
                                      :routine #'(lambda (a) ;(print "START 4")
                                                   (sleep 6) a)
                                      :data '(4) 
                                      :callback #'(lambda (self) ;(print "END 4")
                                                    )))))

(setf *treetest* (build-t-tree *task-tree*))
|#
;(setq *om-sequencer-queue* nil)