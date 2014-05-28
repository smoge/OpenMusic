(in-package :om)

(defvar *omsequencer-files* nil)

(setf *omsequencer-files* 
      '("scheduler;scheduler"
        "scheduler;scheduler-api"
        "scheduler;sequencer-scheduler"
        "scheduler;sequencer-scheduler-api"
        "worker;thread-pool"
        "worker;event-engine"
        ;"maquettelink;sch-maquette_link-patch"
        ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *omsequencer-files*))

(sch::init-sequencer-scheduler)
(sch::init-thread-pool 8)
(evt::init-event-engine)