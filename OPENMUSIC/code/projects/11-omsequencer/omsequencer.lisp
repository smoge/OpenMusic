(in-package :om)

(defvar *omsequencer-files* nil)

(setf *omsequencer-files* 
      '("scheduler;scheduler"
        "scheduler;scheduler-api"
        "scheduler;sequencer-scheduler"
        "scheduler;sequencer-scheduler-api"
        "worker;thread-pool"
        "worker;thread-pool-api"
        "worker;event-engine"
        "worker;event-engine-api"
        ;"maquettelink;sch-maquette_link-patch"
        ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *omsequencer-files*))

(om-init-sequencer-scheduler)
(om-init-thread-pool 8)
(om-init-event-engine)