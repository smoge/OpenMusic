#|
Scheduler API (cf. scheduler.lisp)

Author : D.Bouche
Ircam (C) 2014
|#


(in-package :om)

;;;=============================================================================================Scheduler Structure
(defun om-make-scheduler (&key (name "scheduler") (offset 0) (tick 0.01) queue)
  (sch:make-scheduler :name name :offset offset :tick (coerce (max tick 0.001) 'single-float) :queue queue))

(defmethod om-get-scheduler-name ((self sch::scheduler))
  (sch::scheduler-name self))
(defmethod om-set-scheduler-name ((self sch::scheduler) name)
  (setf (sch::scheduler-name self) name))

(defmethod om-get-scheduler-offset ((self sch::scheduler))
  (sch::scheduler-offset self))
(defmethod om-set-scheduler-offset ((self sch::scheduler) offset)
  (setf (sch::scheduler-offset self) offset))

(defmethod om-get-scheduler-tick ((self sch::scheduler))
  (sch::scheduler-tick self))
(defmethod om-set-scheduler-tick ((self sch::scheduler) tick)
  (sch:change-scheduler-tick self tick))
(defmethod om-autoset-scheduler-tick ((self sch::scheduler))
  (sch:scheduler-auto-tick-setting self))

(defmethod om-get-scheduler-queue ((self sch::scheduler))
  (sch::scheduler-queue self))
(defmethod om-set-scheduler-queue ((self sch::scheduler) queue)
  (setf (sch::scheduler-queue self) queue))

(defmethod om-get-scheduler-lock ((self sch::scheduler))
  (sch::scheduler-queue-lock self))

(defmethod om-get-scheduler-state ((self sch::scheduler))
  (sch::scheduler-state self))
;;;=============================================================================================Transport functions for a single scheduler
(defmethod om-start-scheduler ((self sch::scheduler))
  (sch:start-scheduler self))

(defmethod om-pause-scheduler ((self sch::scheduler))
  (sch:pause-scheduler self))

(defmethod om-continue-scheduler ((self sch::scheduler))
  (sch:continue-scheduler self))

(defmethod om-stop-scheduler ((self sch::scheduler))
  (sch:stop-scheduler self))

;;;=============================================================================================Transport functions for multiple scheduler sync
(defmethod om-start-multiple-scheduler ((self list))
  (sch:start-multiple-scheduler self))

(defmethod om-pause-multiple-scheduler ((self list))
  (sch:pause-multiple-scheduler self))

(defmethod om-continue-multiple-scheduler ((self list))
  (sch:continue-multiple-scheduler self))

(defmethod om-stop-multiple-scheduler ((self list))
  (sch:stop-multiple-scheduler self))

;;;=============================================================================================Scheduler Tools
(defmethod om-scheduler-set-tick ((self sch::scheduler) tick)
  (sch:change-scheduler-tick self tick))

(defmethod om-scheduler-autoset-tick ((self sch::scheduler))
  (sch:scheduler-auto-tick-setting self))

(defmethod om-get-clock-time ((self sch::scheduler))
  (sch:get-clock-time self))

(defmethod om-with-scheduler ((queue list))
  "Starts a scheduler that will handle the 'queue' list execution. 
Queue is of type : ((timestamp1 Function1 &optional Datalist1) (timestamp2 Function2 &optional Datalist2)...)"
  (om-start-scheduler (om-autoset-scheduler-tick (om-make-scheduler :queue queue))))