#|
Basic scheduler.

A basic scheduler is defined by the "om-scheduler" structure :
   - "name" : a name,
   - "start-time" : the time at which the scheduler started (absolute : from the get-internal-real-time function),
   - "pause-time" : the time at which the scheduler paused (relative : from get-internal-real-time - start-time),
   - "offset" : the offset of the "process" in ms (tipically 0),
   - "tick" : the tick of the "process",
   - "process" : the process which runs popping events,
   - "state" : state of the scheduler, ie. :stop, :pause, :play,
   - "queue" : the event-queue,
   - "queue-lock" : a lock which is grabbed and then released by any process acting on the scheduler queue.

This kind of scheduler should be used to efficiently run tasks which won't be modified after the execution started. The task events are popped from it's queue.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage "Scheduler"
  (:nicknames "SCH")
   (:use "COMMON-LISP" "CL-USER" "OpenMusic" "OM-API" "LISPWORKS" "HCL" "OM-LISP"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sch)

(push :sch *features*)

(export 
 '(;;;Structure
   make-om-scheduler

   ;;;Transport
   om-start-scheduler
   om-start-multiple-scheduler
   om-pause-scheduler
   om-pause-multiple-scheduler
   om-continue-scheduler
   om-continue-multiple-scheduler
   om-stop-scheduler
   om-stop-multiple-scheduler

   ;;;Tools
   scheduler-auto-tick-setting
   get-clock-time
   change-scheduler-tick) :sch)

(defstruct (om-scheduler)
  (name "om-scheduler" :type string)
  (start-time 0 :type integer)
  (ref-time 0 :type integer)
  (pause-time 0 :type integer)
  (offset 0 :type integer)
  (tick 0.01 :type single-float)
  (process nil :type (or mp::process null))
  (state :stop :type symbol)
  (queue nil :type list)
  (queue-lock (mp::make-lock :name "om-scheduler-lock") :type mp::lock))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Transport functions
;;;Start one scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod om-start-scheduler ((self om-scheduler) &optional time)
  (hcl:avoid-gc)
  (setf (om-scheduler-state self) :play
        (om-scheduler-start-time self) (round (or time (get-internal-real-time)))
        (om-scheduler-ref-time self) (om-scheduler-start-time self)
        (om-scheduler-process self) (mp:process-run-function (format nil "~A process" (om-scheduler-name self)) nil 'check-scheduler-event self)))

;;;Pause one scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod om-pause-scheduler ((self om-scheduler) &optional time)
  (setf (om-scheduler-pause-time self) (round (if time
                                                  (+ (om-scheduler-offset self) (- time (om-scheduler-start-time self)))
                                                (get-clock-time self)))
        (om-scheduler-state self) :pause)
  (hcl:normal-gc))

;;;Continue one scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod om-continue-scheduler ((self om-scheduler) &optional time)
  (hcl:avoid-gc)
  (setf (om-scheduler-state self) :play
        (om-scheduler-ref-time self) (round (if time
                                                  (+ (om-scheduler-offset self) (- time (om-scheduler-pause-time self)))
                                                (+ (om-scheduler-offset self) (- (get-internal-real-time) (om-scheduler-pause-time self)))))
        (om-scheduler-start-time self) (- (get-internal-real-time) (+ (om-scheduler-pause-time self) (om-scheduler-ref-time self))))
  (mp:process-unstop (om-scheduler-process self)))

;;;Stop one scheduler.
(defmethod om-stop-scheduler ((self om-scheduler))
  (setf (om-scheduler-state self) :stop)
  (when (om-scheduler-process self)
    (mp:process-kill (om-scheduler-process self)))
  (hcl:normal-gc))

;;;Start multiple schedulers simultaneously.
(defmethod om-start-multiple-scheduler ((self list))
  (let ((time (get-internal-real-time)))
    (loop for scheduler in self do
          (om-start-scheduler scheduler time))))

;;;Pause multiple schedulers simultaneously.
(defmethod om-pause-multiple-scheduler ((self list))
  (let ((time (get-internal-real-time)))
    (loop for scheduler in self do
          (om-pause-scheduler scheduler time))))

;;;Continue multiple schedulers simultaneously.
(defmethod om-continue-multiple-scheduler ((self list))
  (let ((time (get-internal-real-time)))
    (loop for scheduler in self do
          (om-pause-scheduler scheduler time))))

;;;Stop multiple schedulers simultaneously.
(defmethod om-stop-multiple-scheduler ((self list))
  (loop for scheduler in self do
        (om-stop-scheduler scheduler)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tools
;;;Compute the GCD of a list of values.
(defmethod gcdlist ((self list))
  (let ((res 0))
    (loop for val in self do
          (setf res (gcd val res))) 
    res))

;;;Auto-set a scheduler tick according to it's queue (taking the GCD of the timestamps).
(defmethod scheduler-auto-tick-setting ((self om-scheduler))
  (if (om-scheduler-queue self)
      (setf (om-scheduler-tick self)
            (/ (let (timelist)
                 (setq timelist (loop for evt in (om-scheduler-queue self) collect (car evt)))
                 (gcdlist timelist)) 1000.0))
    (print "The scheduler-auto-tick-setting can't work with an empty queue.")))

;;;Get the current time of a scheduler
(defmethod get-clock-time ((self om-scheduler))
  (if (eq (om-scheduler-state self) :pause)
      (om-scheduler-pause-time self)
    (+ (om-scheduler-offset self) (- (get-internal-real-time) (om-scheduler-ref-time self)))))

;;;Get elapsed time since the scheduler started (can be different form the clock time becaus eof jumps)
(defmethod get-elapsed-time ((self om-scheduler))
  (- (get-internal-real-time) (om-scheduler-start-time self)))

;;;Change a scheduler tick.
(defmethod change-scheduler-tick ((self om-scheduler) tick-s)
  (setf (om-scheduler-tick self) (coerce (max tick-s 0.001) 'single-float))
  (mp:process-stop (om-scheduler-process self))
  (mp:process-unstop (om-scheduler-process self)))

;;;Check if the next event of a scheduler queue needs to be played. If queue is empty, stop the scheduler.
;;;If the scheduler state was set to pause, pause the process.
(defmethod check-scheduler-event ((self om-scheduler))
  (loop
   (loop while (and (om-scheduler-queue self) 
                    (<= (caar (om-scheduler-queue self))
                        (get-clock-time self)))
         do
         (execute-scheduler-event self))
   (when (not (om-scheduler-queue self)) (om-stop-scheduler self))
   (when (eq (om-scheduler-state self) :pause) (mp:process-stop (om-scheduler-process self)))
   (mp:process-wait-with-timeout (format nil "Sleeping for ~As" (om-scheduler-tick self)) (om-scheduler-tick self))))

;;;Execute an event from a scheduler queue
(defmethod execute-scheduler-event ((self om-scheduler))
  (mp:with-lock ((om-scheduler-queue-lock self))
    (funcall (or
              (cadr (pop (om-scheduler-queue self)))
              #'(lambda ())))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;