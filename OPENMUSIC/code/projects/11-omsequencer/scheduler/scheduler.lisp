#|
Basic scheduler.

A basic scheduler is defined by the "scheduler" structure :
   - "name" : a name,
   - "start-time" : the time at which the scheduler started (absolute : from the get-internal-real-time function),
   - "pause-time" : the time at which the scheduler paused (relative : from get-internal-real-time - start-time),
   - "offset" : the offset of the "process" in ms (tipically 0),
   - "tick" : the tick of the "process",
   - "process" : the process which runs popping events,
   - "state" : state of the scheduler, ie. :stop, :pause, :play,
   - "queue" : the event-queue, which must look like ((timestamp1 function1 &rest args) (timestamp2 function2 &rest args)...)
   - "queue-lock" : a lock which is grabbed and then released by any process acting on the scheduler queue.

This kind of scheduler should be used to efficiently run tasks which won't be modified after the execution started. The task events are popped from it's queue.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage "Scheduler"
  (:nicknames "SCH")
   (:use "COMMON-LISP" "CL-USER" "OM" "OM-API" "LISPWORKS" "HCL" "OM-LISP"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sch)

(push :sch *features*)

(export 
 '(;;;Structure
   make-scheduler

   ;;;Transport
   start-scheduler
   start-multiple-scheduler
   pause-scheduler
   pause-multiple-scheduler
   continue-scheduler
   continue-multiple-scheduler
   stop-scheduler
   stop-multiple-scheduler

   ;;;Tools
   scheduler-auto-tick-setting
   get-clock-time
   change-scheduler-tick) :sch)

(defstruct (scheduler)
  (name "scheduler" :type string)
  (start-time 0 :type integer)
  (ref-time 0 :type integer)
  (pause-time 0 :type integer)
  (offset 0 :type integer)
  (tick 0.01 :type single-float)
  (process nil :type (or mp::process null))
  (state :stop :type symbol)
  (queue nil :type list)
  (queue-lock (mp::make-lock :name "scheduler-lock") :type mp::lock))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Transport functions
;;;Start one scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod start-scheduler ((self scheduler) &optional time)
  (when (eq (scheduler-state self) :stop)
    (hcl:avoid-gc)
    (setf (scheduler-state self) :play
          (scheduler-start-time self) (round (or time (get-internal-real-time)))
          (scheduler-ref-time self) (scheduler-start-time self)
          (scheduler-process self) (mp:process-run-function (format nil "~A process" (scheduler-name self)) nil 'check-scheduler-event self))))

;;;Pause one scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod pause-scheduler ((self scheduler) &optional time)
  (when (eq (scheduler-state self) :play)
    (setf (scheduler-pause-time self) (round (if time
                                                    (+ (scheduler-offset self) (- time (scheduler-start-time self)))
                                                  (get-clock-time self)))
          (scheduler-state self) :pause)
    (hcl:normal-gc)))

;;;Continue one scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod continue-scheduler ((self scheduler) &optional time)
  (when (eq (scheduler-state self) :pause)  
    (hcl:avoid-gc)
    (setf (scheduler-state self) :play
          (scheduler-ref-time self) (round (if time
                                                  (+ (scheduler-offset self) (- time (scheduler-pause-time self)))
                                                (+ (scheduler-offset self) (- (get-internal-real-time) (scheduler-pause-time self)))))
          (scheduler-start-time self) (- (get-internal-real-time) (+ (scheduler-pause-time self) (scheduler-ref-time self))))
    (mp:process-unstop (scheduler-process self))))

;;;Stop one scheduler.
(defmethod stop-scheduler ((self scheduler))
  (setf (scheduler-state self) :stop)
  (when (scheduler-process self)
    (mp:process-kill (scheduler-process self)))
  (hcl:normal-gc))

;;;Start multiple schedulers simultaneously.
(defmethod start-multiple-scheduler ((self list))
  (let ((time (get-internal-real-time)))
    (loop for scheduler in self do
          (start-scheduler scheduler time))))

;;;Pause multiple schedulers simultaneously.
(defmethod pause-multiple-scheduler ((self list))
  (let ((time (get-internal-real-time)))
    (loop for scheduler in self do
          (pause-scheduler scheduler time))))

;;;Continue multiple schedulers simultaneously.
(defmethod continue-multiple-scheduler ((self list))
  (let ((time (get-internal-real-time)))
    (loop for scheduler in self do
          (pause-scheduler scheduler time))))

;;;Stop multiple schedulers simultaneously.
(defmethod stop-multiple-scheduler ((self list))
  (loop for scheduler in self do
        (stop-scheduler scheduler)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tools
;;;Compute the GCD of a list of values.
(defmethod gcdlist ((self list))
  (let ((res 0))
    (loop for val in self do
          (setf res (gcd val res))) 
    res))

;;;Auto-set a scheduler tick according to it's queue (taking the GCD of the timestamps).
(defmethod scheduler-auto-tick-setting ((self scheduler))
  (if (scheduler-queue self)
      (setf (scheduler-tick self)
            (/ (let (timelist)
                 (setq timelist (loop for evt in (scheduler-queue self) collect (car evt)))
                 (gcdlist timelist)) 1000.0))
    (print "The scheduler-auto-tick-setting can't work with an empty queue.")))

;;;Get the current time of a scheduler
(defmethod get-clock-time ((self scheduler))
  (cond ((eq (scheduler-state self) :pause)
         (scheduler-pause-time self))
        ((eq (scheduler-state self) :stop) 0)
        (t
         (+ (scheduler-offset self) (- (get-internal-real-time) (scheduler-ref-time self))))))

;;;Get elapsed time since the scheduler started (can be different form the clock time because of jumps)
(defmethod get-elapsed-time ((self scheduler))
  (- (get-internal-real-time) (scheduler-start-time self)))

;;;Change a scheduler tick.
(defmethod change-scheduler-tick ((self scheduler) tick-s)
  (setf (scheduler-tick self) (coerce (max tick-s 0.001) 'single-float))
  (mp:process-stop (scheduler-process self))
  (mp:process-unstop (scheduler-process self)))

;;;Check if the next event of a scheduler queue needs to be played. If queue is empty, stop the scheduler.
;;;If the scheduler state was set to pause, pause the process.
(defmethod check-scheduler-event ((self scheduler))
  (loop
   (loop while (and (scheduler-queue self) 
                    (<= (caar (scheduler-queue self))
                        (get-clock-time self)))
         do
         (execute-scheduler-event self))
   (when (not (scheduler-queue self)) (stop-scheduler self))
   (when (eq (scheduler-state self) :pause) (mp:process-stop (scheduler-process self)))
   (mp:process-wait-with-timeout (format nil "Sleeping for ~As" (scheduler-tick self)) (scheduler-tick self))))

;;;Execute an event from a scheduler queue
(defmethod execute-scheduler-event ((self scheduler))
  (mp:with-lock ((scheduler-queue-lock self))
    (let* ((task (pop (scheduler-queue self)))
           (fct (nth 1 task))
           (data (nth 2 task)))
      (if fct (if data (apply fct data) (funcall fct))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;