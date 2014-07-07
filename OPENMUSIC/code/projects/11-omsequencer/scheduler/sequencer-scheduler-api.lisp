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
      (values :synchronous sch::sch_synchronous)
    (values :asynchronous sch::sch_asynchronous)))
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

;;;=============================================================================================Sequencer Scheduler Transport
(defmethod om-start-sequencer-scheduler ((self sch::sequencer-scheduler) &optional time)
  (sch:start-scheduler self time))

(defmethod om-pause-sequencer-scheduler ((self sch::sequencer-scheduler) &optional time)
  (sch:pause-scheduler self time))

(defmethod om-continue-sequencer-scheduler ((self sch::sequencer-scheduler) &optional time)
  (sch:continue-scheduler self time))

(defmethod om-stop-sequencer-scheduler ((self sch::sequencer-scheduler))
  (sch:stop-scheduler self))

(defmethod om-jump-sequencer-scheduler ((self sch::sequencer-scheduler) time)
  (sch::jump-scheduler self time))

;;;=============================================================================================Sequencer Object Structure
(defun om-build-seq-object (&key name id tasklist (duration 0) (timestamp 0))
  (sch:build-seq-object :name (or name "seq-object") 
                        :id (or id (sch:generate-task-id)) 
                        :tasklist tasklist 
                        :duration (or duration 0) 
                        :timestamp (or timestamp 0)))

(defmethod om-release-seq-object ((self sch::seq-object))
  (sch:release-seq-object self))

(defmethod om-get-seq-object-name ((self sch::seq-object))
  (sch::seq-object-name self))
(defmethod om-set-seq-object-name ((self sch::seq-object) name)
  (setf (sch::seq-object-name self) name))

(defmethod om-get-seq-object-id ((self sch::seq-object))
  (sch::seq-object-id self))
(defmethod om-set-seq-object-id ((self sch::seq-object) id)
  (setf (sch::seq-object-id self) id))

(defmethod om-get-seq-object-tasklist ((self sch::seq-object))
  (sch::seq-object-tasklist self))
(defmethod om-set-seq-object-tasklist ((self sch::seq-object) tasklist)
  (setf (sch::seq-object-tasklist self) tasklist))

(defmethod om-get-seq-object-duration ((self sch::seq-object))
  (sch::seq-object-duration self))
(defmethod om-set-seq-object-duration ((self sch::seq-object) duration)
  (setf (sch::seq-object-duration self) duration))

(defmethod om-get-seq-object-timestamp ((self sch::seq-object))
  (sch::seq-object-timestamp self))
(defmethod om-set-seq-object-timestamp ((self sch::seq-object) timestamp)
  (setf (sch::seq-object-timestamp self) timestamp))

;;;=============================================================================================Sequencer Objects Scheduling
(defmethod om-schedule-seq-object ((self sch::seq-object))
  (sch:schedule-seq-object self))
(defmethod om-reschedule-seq-object ((self sch::seq-object) new-time)
  (sch:reschedule-seq-object self new-time))
(defmethod om-unschedule-seq-object ((self sch::seq-object))
  (sch::unschedule-seq-object self))

;;;=============================================================================================Sequencer Task Structure
(defun om-build-obj-task (&key name id event data (readyp t) timestamp)
  (sch:build-obj-task :name (or name "obj-task") 
                      :id (or id (sch:generate-task-id)) 
                      :event (or event #'(lambda (data))) 
                      :data data 
                      :readyp readyp 
                      :timestamp (or timestamp 0)))

(defmethod om-release-obj-task ((self sch::obj-task))
  (sch:release-obj-task self))

(defmethod om-get-obj-task-name ((self sch::obj-task))
  (sch::obj-task-name self))
(defmethod om-set-obj-task-name ((self sch::obj-task) name)
  (setf (sch::obj-task-name self) name))

(defmethod om-get-obj-task-id ((self sch::obj-task))
  (sch::obj-task-id self))
(defmethod om-set-obj-task-id ((self sch::obj-task) id)
  (setf (sch::obj-task-id self) id))

(defmethod om-get-obj-task-fun ((self sch::obj-task))
  (sch::obj-task-event self))
(defmethod om-set-obj-task-fun ((self sch::obj-task) fun)
  (setf (sch::obj-task-event self) fun))

(defmethod om-get-obj-task-data ((self sch::obj-task))
  (sch::obj-task-data self))
(defmethod om-set-obj-task-data ((self sch::obj-task) data)
  (setf (sch::obj-task-data self) data))

(defmethod om-get-obj-task-timestamp ((self sch::obj-task))
  (sch::obj-task-timestamp self))
(defmethod om-set-obj-task-timestamp ((self sch::obj-task) timestamp)
  (setf (sch::obj-task-timestamp self) timestamp))

(defmethod om-obj-task-readyp ((self sch::obj-task))
  (sch::obj-task-readyp self))
(defmethod om-obj-task-ready ((self sch::obj-task))
  (setf (sch::obj-task-readyp self) t))
(defmethod om-obj-task-not-ready ((self sch::obj-task))
  (setf (sch::obj-task-readyp self) nil))

;;;=============================================================================================Sequencer Tasks Scheduling
(defmethod om-schedule-obj-task ((self sch::obj-task))
  (sch:schedule-obj-task self))
(defmethod om-reschedule-obj-task ((self sch::obj-task) new-time)
  (sch:reschedule-obj-task self new-time))
(defmethod om-unschedule-obj-task ((self sch::obj-task))
  (sch::unschedule-obj-task self))
