#|
Sequencer Scheduler API (cf. sequencer-scheduler.lisp)

Author : D.Bouche
Ircam (C) 2014
|#


(in-package :om)

;;;=============================================================================================Sequencer Scheduler Structure
(defun om-init-sequencer-scheduler ()
  (sch:init-sequencer-scheduler))
(defun om-abort-sequencer-scheduler ()
  (sch:abort-sequencer-scheduler))

(defun om-get-sequencer-scheduler ()
  sch:*sequencer-scheduler*)

(defun om-get-sequencer-scheduler-type ()
  (if (eq sch:*sequencer-scheduler-type* sch::sch_synchronous)
      (values sch::sch_synchronous :synchronous)
    (values sch::sch_asynchronous :asynchronous)))
(defun om-set-sequencer-scheduler-type (type)
  (if (eq type :synchronous) 
      (setq sch:*sequencer-scheduler-type* sch::sch_synchronous) 
    (setq sch:*sequencer-scheduler-type* sch::sch_asynchronous)))

(defun om-get-sequencer-scheduler-queue ()
  sch:*sequencer-queue*)
(defun om-set-sequencer-scheduler-queue (queue)
  (setq sch:*sequencer-queue* queue))

(defmethod om-get-sequencer-scheduler-queue-position ((self sch::sequencer-scheduler))
  (sch::sequencer-scheduler-queue-position self))

;;;=============================================================================================Sequencer Task Structure
(defun om-build-sequencer-task (&key name id event object data (readyp t) timestamp)
  (sch:build-sch-task :name (or name "sch-task") 
                      :id (or id (sch:generate-task-id)) 
                      :event (or event #'(lambda (self))) 
                      :object object 
                      :data data 
                      :readyp readyp 
                      :timestamp (or timestamp 0)))

(defmethod om-release-sequencer-task ((self sch::sch-task))
  (sch:release-sch-task self))

(defmethod om-get-sequencer-task-name ((self sch::sch-task))
  (sch::sch-task-name self))
(defmethod om-set-sequencer-task-name ((self sch::sch-task) name)
  (setf (sch::sch-task-name self) name))

(defmethod om-get-sequencer-task-id ((self sch::sch-task))
  (sch::sch-task-id self))
(defmethod om-set-sequencer-task-id ((self sch::sch-task) id)
  (setf (sch::sch-task-id self) id))

(defmethod om-get-sequencer-task-fun ((self sch::sch-task))
  (sch::sch-task-event self))
(defmethod om-set-sequencer-task-fun ((self sch::sch-task) fun)
  (setf (sch::sch-task-event self) fun))

(defmethod om-get-sequencer-task-obj ((self sch::sch-task))
  (sch::sch-task-object self))
(defmethod om-set-sequencer-task-obj ((self sch::sch-task) object)
  (setf (sch::sch-task-object self) object))

(defmethod om-get-sequencer-task-data ((self sch::sch-task))
  (sch::sch-task-data self))
(defmethod om-set-sequencer-task-data ((self sch::sch-task) data)
  (setf (sch::sch-task-data self) data))

(defmethod om-get-sequencer-task-timestamp ((self sch::sch-task))
  (sch::sch-task-timestamp self))
(defmethod om-set-sequencer-task-timestamp ((self sch::sch-task) timestamp)
  (setf (sch::sch-task-timestamp self) timestamp))

(defmethod om-sequencer-task-readyp ((self sch::sch-task))
  (sch::sch-task-readyp self))
(defmethod om-sequencer-task-ready ((self sch::sch-task))
  (setf (sch::sch-task-readyp self) t))
(defmethod om-sequencer-task-not-ready ((self sch::sch-task))
  (setf (sch::sch-task-readyp self) nil))

;;;=============================================================================================Sequencer Tasks Scheduling
(defmethod om-schedule-sequencer-task ((self sch::sch-task))
  (sch:schedule-sch-task self))
(defmethod om-reschedule-sequencer-task ((self sch::sch-task) new-time)
  (sch:reschedule-sch-task self new-time))
