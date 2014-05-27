(in-package :om)

(defvar *omsequencer-files* nil)

(setf *omsequencer-files* 
      '("scheduler;om-scheduler"
        "scheduler;om-sequencer-scheduler"
        "worker;om-thread-pool"
        "worker;om-event-engine"
        ;"maquettelink;sch-maquette_link-patch"
        ))

(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *omsequencer-files*))

(sch::init-sequencer-scheduler)
(sch::init-thread-pool 8)
(evt::init-om-event-engine)