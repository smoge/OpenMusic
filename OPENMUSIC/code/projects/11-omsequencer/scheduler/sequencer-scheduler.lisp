#|
Sequencer scheduler.

This scheduler structure is a child of the "scheduler" structure, with 1 more slots : 
   - "queue-position" : an integer which represents the current position in the *sequencer-queue*.
The sequencer-scheduler process can be asynchronous or synchronous, when the scheduler process is always synchronous.
To change the process behaviour, set the global variable *sequencer-scheduler-type* to the constant SCH_SYNCHRONOUS or SCH_ASYNCHRONOUS (default is SCH_SYNCHRONOUS).
The change will take effect at the next start.

*sequencer-queue* is a list of this type : ((Timestamp1 Task1 ID1) (Timestamp2 Task2 Id2) ...).
This list can be modified and it's access is locked while it's being read, so operations like inserting, deleting or moving an element have to wait.

Tasks are defined by the "obj-task" structure :
   - "name" : a task name,
   - "id" : an unique ID, generally used to reschedule tasks,
   - "event" : a lambda function representing the triggering of the task, typically this function needs to achieve it's goal instantly (not much computations).
               this function will be called with the task itself as an argument. So it needs 1 and only 1 argument.
   - "data" : a list of additional data if needed,
   - "readyp" : if the task needs computations to be triggered, the boolean readyp must be filled to know if the computation is achieved,
   - "timestamp" : time at which the task needs to be triggered.

Author : D.Bouche
|#

(in-package :sch)

(export 
 '(;;;Structure
   init-sequencer-scheduler
   abort-sequencer-scheduler

   ;;;Task tools
   build-obj-task
   release-obj-task
   schedule-obj-task
   reschedule-obj-task
   generate-task-id

   ;;;Object tools
   build-seq-object
   release-seq-object
   schedule-seq-object
   reschedule-seq-object

   ;;;Variables
   *sequencer-scheduler*
   *sequencer-scheduler-type*
   *sequencer-queue*) :sch)

(defvar *sequencer-scheduler* nil)
(defvar *sequencer-queue* nil)
(defvar *sequencer-alarm* nil)
(defvar *obj-task-pool* nil)
(defvar *seq-object-pool* nil)

(defconstant SCH_ASYNCHRONOUS 0)
(defconstant SCH_SYNCHRONOUS 1)

(defvar *sequencer-scheduler-type* SCH_SYNCHRONOUS)

(defstruct (sequencer-scheduler (:include scheduler))
  (queue-position 0 :type integer))

(defstruct (seq-object)
  (name "seq-object" :type string)
  (id nil :type string)
  (tasklist nil :type list)
  (duration 0 :type integer)
  (timestamp 0 :type integer))

(defstruct (obj-task)
  (name "obj-task" :type string)
  (id nil :type string)
  (event #'(lambda (data)) :type function)
  (data nil)
  (readyp nil :type boolean)
  (timestamp 0 :type integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Init and Abort
(defun init-sequencer-scheduler ()
  (when (not *obj-task-pool*)
    (setq *obj-task-pool* (loop for i from 1 to 150 collect (make-obj-task))))
  (when (not *seq-object-pool*)
    (setq *seq-object-pool* (loop for i from 1 to 50 collect (make-seq-object))))
  (when (not *sequencer-scheduler*)
    (setq *sequencer-scheduler* (make-sequencer-scheduler
                                    :name "sequencer-scheduler"
                                    :tick 0.001))))

(defun abort-sequencer-scheduler ()
  (when (typep (sequencer-scheduler-process *sequencer-scheduler*) 'mp::process)
    (mp:process-kill (sequencer-scheduler-process *sequencer-scheduler*)))
  (setf (sequencer-scheduler-process *sequencer-scheduler*) nil)
  (setq *sequencer-scheduler* nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Transport functions
;;;Start a sequencer scheduler with the type set by *sequencer-scheduler-type* (synchronous or asynchronous).
;;;The optional time slot is used to synchronise multiple schedulers.
(defmethod start-scheduler ((self sequencer-scheduler) &optional time)
  (when (or (not (sequencer-scheduler-process self)) (eq (mp:process-state (sequencer-scheduler-process self)) :killed))
    (hcl:avoid-gc)
    (if (setf (sequencer-scheduler-state self) :play
              (sequencer-scheduler-start-time self) (or time (get-internal-real-time))
              (sequencer-scheduler-ref-time self) (sequencer-scheduler-start-time self)
              (sequencer-scheduler-process self) (if (= *sequencer-scheduler-type* SCH_SYNCHRONOUS)
                                                        (mp:process-run-function 
                                                         (format nil "~A synchronous process" (sequencer-scheduler-name self)) 
                                                         nil 'check-sequencer-event-sync)
                                                      (mp:process-run-function 
                                                       (format nil "~A asynchronous process" (sequencer-scheduler-name self)) 
                                                       nil 'check-sequencer-event-async)))
        "Sequencer play...")))

;;;Pause a sequencer scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod pause-scheduler ((self sequencer-scheduler) &optional time)
  (if (call-next-method) "Sequencer pause..."))

;;;Continue a sequencer scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod continue-scheduler ((self sequencer-scheduler) &optional time)
  (if (call-next-method) "Sequencer continue..."))

;;;Stop a sequencer scheduler and set it's queue position to 0.
(defmethod stop-scheduler ((self sequencer-scheduler))
  (setf (sequencer-scheduler-queue-position *sequencer-scheduler*) 0)
  (if (call-next-method) "Sequencer stop..."))

;;;Jump to a specific timed location
(defmethod jump-scheduler ((self sequencer-scheduler) time)
  (setf (sequencer-scheduler-ref-time self) (- (get-internal-real-time) time))
  (restore-main-scheduler-cursor))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Scheduler Tools
;;;Get the current time of a scheduler.
(defmethod get-clock-time ((self sequencer-scheduler) &optional internal-time)
  (call-next-method))

;;;Get the current queue position of a sequencer scheduler.
(defmethod get-sequencer-queue-position ((self sequencer-scheduler))
  (or (position (get-clock-time self) *sequencer-queue* :test '<= :key 'car) (length *sequencer-queue*)))

;;;Get the time before the next event (only for asynchronous scheduler).
(defun get-sequencer-sleep-time ()
  (let ((t1 (- (or (car (nth (sequencer-scheduler-queue-position *sequencer-scheduler*) *sequencer-queue*)) 0)
               (get-clock-time *sequencer-scheduler*)))
        (t2 -1))
    (/ (if (> t1 0) t1
         (progn (loop while (< t2 0) do
                      (setq t2 (-  (or (car (nth (incf (sequencer-scheduler-queue-position *sequencer-scheduler*)) *sequencer-queue*))
                                       (+ (get-clock-time *sequencer-scheduler*) 10))
                                   (get-clock-time *sequencer-scheduler*))))
           t2))
       1000)))

(defmethod change-scheduler-tick ((self sequencer-scheduler) tick-s)
  (setf (scheduler-tick self) (coerce (max tick-s 0.001) 'single-float))
  (when (= *sequencer-scheduler-type* SCH_SYNCHRONOUS)
    (mp:process-stop (scheduler-process self))
    (mp:process-unstop (scheduler-process self))))

;;;Restore the queue position after modifying the queue. 
;;;If asynchronous, wake up the sleeping process so it sleeps again with an updated alarm clock.
(defun restore-main-scheduler-cursor ()
  (setf (sequencer-scheduler-queue-position *sequencer-scheduler*) (get-sequencer-queue-position *sequencer-scheduler*))
  (when (and (= *sequencer-scheduler-type* SCH_ASYNCHRONOUS) (sequencer-scheduler-process *sequencer-scheduler*))
    (setq *sequencer-alarm* t)
    (mp:process-poke (sequencer-scheduler-process *sequencer-scheduler*))))

;;;Check if the next event of a synchronous scheduler needs to be played. If queue is empty, stop the scheduler.
;;;If the scheduler state was set to pause, pause the process.
(defun check-sequencer-event-sync ()
  (loop
   (if (and *sequencer-queue*
                    (<= (or (car (nth (sequencer-scheduler-queue-position *sequencer-scheduler*) *sequencer-queue*)) most-positive-fixnum) 
                        (get-clock-time *sequencer-scheduler*)))
       (execute-task-event))
 ;  (loop while (and *sequencer-queue*
 ;                   (<= (or (car (nth (sequencer-scheduler-queue-position *sequencer-scheduler*) *sequencer-queue*)) most-positive-fixnum) 
 ;                       (get-clock-time *sequencer-scheduler*)))
 ;        do
 ;        (execute-task-event))
   ;(when (>= (sequencer-scheduler-queue-position *sequencer-scheduler*) (length *sequencer-queue*))
   ;  (stop-scheduler *sequencer-scheduler*))
   (when (eq (sequencer-scheduler-state *sequencer-scheduler*) :pause) 
     (mp:process-stop (sequencer-scheduler-process *sequencer-scheduler*)))
   (mp:process-wait-with-timeout (format nil "Sleeping for ~As" (sequencer-scheduler-tick *sequencer-scheduler*))     
                                 (sequencer-scheduler-tick *sequencer-scheduler*))))

;;;Sleep until the next event of an asynchronous scheduler arrives, then wake up, execute it and sleep again.
(defun check-sequencer-event-async ()
  (loop
   (when (eq (sequencer-scheduler-state *sequencer-scheduler*) :pause) 
     (mp:process-stop (sequencer-scheduler-process *sequencer-scheduler*)))
   (mp:process-wait-with-timeout "Waiting for next event" 
                                 (get-sequencer-sleep-time) #'(lambda () *sequencer-alarm*))
   (if *sequencer-alarm*
       (setq *sequencer-alarm* nil)
     (execute-task-event))))

;;;Execute an event from the queue.
(defun execute-task-event ()
  (mp:with-lock ((sequencer-scheduler-queue-lock *sequencer-scheduler*))
    (let ((task (cadr (nth (sequencer-scheduler-queue-position *sequencer-scheduler*) *sequencer-queue*))))
      (sequencer-scheduler-queue-position *sequencer-scheduler*)
      (when (and task (obj-task-readyp task))
        (if (obj-task-data task) (apply (obj-task-event task) (obj-task-data task)) (funcall (obj-task-event task) task)))))
  ;(when (>= (incf (sequencer-scheduler-queue-position *sequencer-scheduler*)) (length *sequencer-queue*))
  ;  (stop-scheduler *sequencer-scheduler*))
  ;(incf (sequencer-scheduler-queue-position *sequencer-scheduler*))
  (restore-main-scheduler-cursor) ;;;pas top
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Task Tools
;;;Insert an element in a list, at a specific position.
(defun insert-sequencer-element (index newelt) 
    (if (= 0 index)
        (push newelt *sequencer-queue*)
      (push newelt (nthcdr index *sequencer-queue*))))

;;;Remove a nth element of a list.
(defun remove-nth (list n)
  (remove-if (constantly t) list :start n :end (1+ n)))

;;;Get a new obj-task : it looks if there is some free available structure, or it builds a new one.
(defun build-obj-task (&key (name "obj-task") id (event #'(lambda (data))) data (readyp t) (timestamp 0))
  (let ((task (or (pop *obj-task-pool*) (make-obj-task))))
    (setf (obj-task-name task) name
          (obj-task-id task) (or id (generate-task-id))
          (obj-task-event task) event
          (obj-task-data task) (if (listp data) data (list data))
          (obj-task-readyp task) readyp
          (obj-task-timestamp task) timestamp)
    task))

;;;Cleans a task structure
(defmethod clean-obj-task ((self obj-task))
  (setf (obj-task-name self) "obj-task"
        (obj-task-id self) nil
        (obj-task-event self) #'(lambda (data))
        (obj-task-data self) nil
        (obj-task-readyp self) t
        (obj-task-timestamp self) 0)
  self)

;;;Release a task (ie. clean it and push it back in the pool)
(defmethod release-obj-task ((self obj-task))
  (push (clean-obj-task self) *obj-task-pool*))

;;;Schedule a task. According to it's timestamp, it pushes it int the right place of the queue.
(defmethod schedule-obj-task ((self obj-task))
  (when *sequencer-scheduler*
    (mp:with-lock ((sequencer-scheduler-queue-lock *sequencer-scheduler*))
      (insert-sequencer-element
       (let ((p (position (obj-task-timestamp self) *sequencer-queue* :test '>= :key 'car :from-end t)))
         (if p (1+ p) 0))
       (list (obj-task-timestamp self) self (obj-task-id self))))
    (restore-main-scheduler-cursor)))

;;;Unschedule a task
(defmethod unschedule-obj-task ((self obj-task))
  (let ((pos (position (obj-task-id self) *sequencer-queue* :key 'caddr :test 'equalp)))
    (when pos 
      (progn
        (mp:with-lock ((sequencer-scheduler-queue-lock *sequencer-scheduler*))
          (setq *sequencer-queue* (remove-nth *sequencer-queue* pos)))
        (restore-main-scheduler-cursor)))))

;;;Reschedule a task. It removes it from the queue and reschedule it with an updated timestamp.
(defmethod reschedule-obj-task ((self obj-task) new-time)
  (setf (obj-task-timestamp self) new-time)
  (let ((pos (position (obj-task-id self) *sequencer-queue* :key 'caddr :test 'equalp)))
    (when pos 
      (progn
        (mp:with-lock ((sequencer-scheduler-queue-lock *sequencer-scheduler*))
          (setq *sequencer-queue* (remove-nth *sequencer-queue* pos)))
        (schedule-obj-task self)))))


;;;Generate a unique ID.
(defun generate-task-id ()
  (format nil "~4X~4X-~4X-~4X-~4X-~4X~4X" 
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Object Tools
;;;Get a new seq-object : it looks if there is some free available structure, or it builds a new one.
(defun build-seq-object (&key (name "seq-object") id tasklist (duration 0) (timestamp 0))
  (let ((obj (or (pop *seq-object-pool*) (make-seq-object))))
    (setf (seq-object-name obj) name
          (seq-object-id obj) (or id (generate-task-id))
          (seq-object-tasklist obj) tasklist
          (seq-object-duration obj) duration
          (seq-object-timestamp obj) timestamp)
    obj))

;;;Cleans an object structure
(defmethod clean-seq-object ((self seq-object))
  (setf (seq-object-name self) "seq-object"
        (seq-object-id self) nil
        (seq-object-tasklist self) nil
        (seq-object-duration self) 0
        (seq-object-timestamp self) 0)
  self)

;;;Release an object (ie. clean it and push it back in the pool)
(defmethod release-seq-object ((self seq-object))
  (push (clean-seq-object self) *seq-object-pool*))

;;;Schedule an object. It schedules all it's tasks.
(defmethod schedule-seq-object ((self seq-object))
  (loop for task in (seq-object-tasklist self) do
        (schedule-obj-task task)))

;;;Reschedule an object. It reschedules all it's tasks.
(defmethod reschedule-seq-object ((self seq-object) new-time)
  (let ((delay (- new-time (obj-task-timestamp self))))
    (setf (obj-task-timestamp self) new-time)
    (loop for task in (seq-object-tasklist self) do
          (reschedule-obj-task task (+ (obj-task-timestamp task) delay)))))

(defmethod unschedule-seq-object ((self seq-object))
  (loop for task in (seq-object-tasklist self) do
        (unschedule-obj-task task)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Scheduler Tools

;;;QUEUE ORDER TESTS FUNCTIONS
(defun ordered (l)
  (let (temp1)
    (setq temp1 l)
    (equal l (sort (copy-list temp1) #'< :key 'car))))

(defun testorder (n)
  (dotimes (i n)
    (let ((ts (* 1290 i)))
      (schedule-obj-task (make-obj-task :id (generate-task-id) 
                                             :name (format nil "~A" i)
                                             :event #'(lambda () (print (list (get-clock-time *sequencer-scheduler*) ts)))
                                             :readyp t
                                             :timestamp ts))))
  (ordered *sequencer-queue*))


;Nombre de processeurs virtuels
;(parse-integer (string (find-if #'digit-char-p (with-output-to-string (om-lisp:*om-stream*) (om-api:om-cmd-line "sysctl -n hw.ncpu" :redirect-output t)) :from-end t)))