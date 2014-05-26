#|
Sequencer scheduler.

This scheduler structure is a child of the "om-scheduler" structure, with 1 more slots : 
   - "queue-position" : an integer which represents the current position in the *om-sequencer-queue*.
The om-sequencer-scheduler process can be asynchronous or synchronous, when the om-scheduler process is always synchronous.
To change the process behaviour, set the global variable *om-sequencer-scheduler-type* to the constant SCH_SYNCHRONOUS or SCH_ASYNCHRONOUS (default is SCH_SYNCHRONOUS).
The change will take effect at the next start.

*om-sequencer-queue* is a list of this type : ((Timestamp1 Task1 ID1) (Timestamp2 Task2 Id2) ...).
This list can be modified and it's access is locked while it's being read, so operations like inserting, deleting or moving an element have to wait.

Tasks are defined by the "om-task" structure :
   - "name" : a task name,
   - "id" : an unique ID, generally used to reschedule tasks,
   - "event" : a lambda function representing the triggering of the task, typically this function needs to achieve it's goal instantly (no computations),
   - "readyp" : if the task needs computations to be triggered, the boolean readyp must be filled to know if the computation is achieved,
   - "timestamp" : time at which the task needs to be triggered.
|#

(in-package :sch)

(export 
 '(;;;Structure
   init-sequencer-scheduler
   abort-sequencer-scheduler

   ;;;Tools
   get-om-sequencer-scheduler

   ;;;Task tools
   make-om-task
   schedule-sequencer-task
   reschedule-sequencer-task
   generate-task-id

   ;;;Variables
   *om-sequencer-scheduler*
   *om-sequencer-scheduler-type*
   *om-sequencer-queue*) :sch)

(defvar *om-sequencer-scheduler* nil)
(defvar *om-sequencer-queue* nil)
(defvar *om-sequencer-alarm* nil)

(defconstant SCH_ASYNCHRONOUS 0)
(defconstant SCH_SYNCHRONOUS 1)

(defvar *om-sequencer-scheduler-type* SCH_SYNCHRONOUS)

(defstruct (om-sequencer-scheduler (:include om-scheduler))
  (queue-position 0 :type integer))

(defstruct (om-task)
  (name "om-task" :type string)
  (id nil :type string)
  (object nil)
  (event #'(lambda (self)) :type function)
  ;(callback #'(lambda (self)) :type function)
  (readyp nil :type boolean)
  (timestamp 0 :type integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Init and Abort
(defun init-sequencer-scheduler ()
  (when (not *om-sequencer-scheduler*)
    (setq *om-sequencer-scheduler*
          (make-om-sequencer-scheduler
           :name "om-sequencer-scheduler"
           :tick 0.001))))

(defun abort-sequencer-scheduler ()
  (when (typep (om-sequencer-scheduler-process *om-sequencer-scheduler*) 'mp::process)
    (mp:process-kill (om-sequencer-scheduler-process *om-sequencer-scheduler*)))
  (setf (om-sequencer-scheduler-process *om-sequencer-scheduler*) nil)
  (setq *om-sequencer-scheduler* nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Transport functions
;;;Start a sequencer scheduler with the type set by *om-sequencer-scheduler-type* (synchronous or asynchronous).
;;;The optional time slot is used to synchronise multiple schedulers.
(defmethod om-start-scheduler ((self om-sequencer-scheduler) &optional time)
  (when (or (not (om-sequencer-scheduler-process self)) (eq (mp:process-state (om-sequencer-scheduler-process self)) :killed))
    (hcl:avoid-gc)
    (if (setf (om-sequencer-scheduler-state self) :play
              (om-sequencer-scheduler-start-time self) (or time (get-internal-real-time))
              (om-sequencer-scheduler-ref-time self) (om-sequencer-scheduler-start-time self)
              (om-sequencer-scheduler-process self) (if (= *om-sequencer-scheduler-type* SCH_SYNCHRONOUS)
                                                        (mp:process-run-function 
                                                         (format nil "~A synchronous process" (om-sequencer-scheduler-name self)) 
                                                         nil 'check-sequencer-event-sync)
                                                      (mp:process-run-function 
                                                       (format nil "~A asynchronous process" (om-sequencer-scheduler-name self)) 
                                                       nil 'check-sequencer-event-async)))
        "Sequencer play...")))

;;;Pause a sequencer scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod om-pause-scheduler ((self om-sequencer-scheduler) &optional time)
  (if (call-next-method) "Sequencer pause..."))

;;;Continue a sequencer scheduler. The optional time slot is used to synchronise multiple schedulers.
(defmethod om-continue-scheduler ((self om-sequencer-scheduler) &optional time)
  (if (call-next-method) "Sequencer continue..."))

;;;Stop a sequencer scheduler and set it's queue position to 0.
(defmethod om-stop-scheduler ((self om-sequencer-scheduler))
  (setf (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) 0)
  (if (call-next-method) "Sequencer stop..."))

;;;Jump to a specific timed location
(defmethod om-jump-scheduler ((self om-sequencer-scheduler) time)
  (setf (om-sequencer-scheduler-ref-time self) (- (get-internal-real-time) time))
  (restore-main-scheduler-cursor))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Scheduler Tools
;;;Get the *om-sequencer-scheduler* instance
(defun get-om-sequencer-scheduler ()
  *om-sequencer-scheduler*)

;;;Get the current time of a scheduler.
(defmethod get-clock-time ((self om-sequencer-scheduler))
  (call-next-method))

;;;Get the current queue position of a sequencer scheduler.
(defmethod get-sequencer-queue-position ((self om-sequencer-scheduler))
  (or (position (get-clock-time self) *om-sequencer-queue* :test '<= :key 'car) (length *om-sequencer-queue*)))

;;;Get the time before the next event (only for asynchronous scheduler).
(defun get-sequencer-sleep-time ()
  (let ((t1 (- (or (car (nth (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) *om-sequencer-queue*)) 0)
               (get-clock-time *om-sequencer-scheduler*)))
        (t2 -1))
    (/ (if (> t1 0) t1
         (progn (loop while (< t2 0) do
                      (setq t2 (-  (or (car (nth (incf (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*)) *om-sequencer-queue*))
                                       (+ (get-clock-time *om-sequencer-scheduler*) 10))
                                   (get-clock-time *om-sequencer-scheduler*))))
           t2))
       1000)))

(defmethod change-scheduler-tick ((self om-sequencer-scheduler) tick-s)
  (setf (om-scheduler-tick self) (coerce (max tick-s 0.001) 'single-float))
  (when (= *om-sequencer-scheduler-type* SCH_SYNCHRONOUS)
    (mp:process-stop (om-scheduler-process self))
    (mp:process-unstop (om-scheduler-process self))))

;;;Restore the queue position after modifying the queue. 
;;;If asynchronous, wake up the sleeping process so it sleeps again with an updated alarm clock.
(defun restore-main-scheduler-cursor ()
  (setf (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) (get-sequencer-queue-position *om-sequencer-scheduler*))
  (when (and (= *om-sequencer-scheduler-type* SCH_ASYNCHRONOUS) (om-sequencer-scheduler-process *om-sequencer-scheduler*))
    (setq *om-sequencer-alarm* t)
    (mp:process-poke (om-sequencer-scheduler-process *om-sequencer-scheduler*))))

;;;Check if the next event of a synchronous scheduler needs to be played. If queue is empty, stop the scheduler.
;;;If the scheduler state was set to pause, pause the process.
(defun check-sequencer-event-sync ()
  (loop
   (if (and *om-sequencer-queue*
                    (<= (or (car (nth (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) *om-sequencer-queue*)) most-positive-fixnum) 
                        (get-clock-time *om-sequencer-scheduler*)))
       (execute-task-event))
 ;  (loop while (and *om-sequencer-queue*
 ;                   (<= (or (car (nth (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) *om-sequencer-queue*)) most-positive-fixnum) 
 ;                       (get-clock-time *om-sequencer-scheduler*)))
 ;        do
 ;        (execute-task-event))
   ;(when (>= (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) (length *om-sequencer-queue*))
   ;  (om-stop-scheduler *om-sequencer-scheduler*))
   (when (eq (om-sequencer-scheduler-state *om-sequencer-scheduler*) :pause) 
     (mp:process-stop (om-sequencer-scheduler-process *om-sequencer-scheduler*)))
   (mp:process-wait-with-timeout (format nil "Sleeping for ~As" (om-sequencer-scheduler-tick *om-sequencer-scheduler*))     
                                 (om-sequencer-scheduler-tick *om-sequencer-scheduler*))))

;;;Sleep until the next event of an asynchronous scheduler arrives, then wake up, execute it and sleep again.
(defun check-sequencer-event-async ()
  (loop
   (when (eq (om-sequencer-scheduler-state *om-sequencer-scheduler*) :pause) 
     (mp:process-stop (om-sequencer-scheduler-process *om-sequencer-scheduler*)))
   (mp:process-wait-with-timeout "Waiting for next event" 
                                 (get-sequencer-sleep-time) #'(lambda () *om-sequencer-alarm*))
   (if *om-sequencer-alarm*
       (setq *om-sequencer-alarm* nil)
     (execute-task-event))))

;;;Execute an event from the queue.
(defun execute-task-event ()
  (mp:with-lock ((om-sequencer-scheduler-queue-lock *om-sequencer-scheduler*))
    (let ((task (cadr (nth (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*) *om-sequencer-queue*))))
      (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*)
      (when (and task (om-task-readyp task)) 
        (funcall (om-task-event task) task))))
  ;(when (>= (incf (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*)) (length *om-sequencer-queue*))
  ;  (om-stop-scheduler *om-sequencer-scheduler*))
  ;(incf (om-sequencer-scheduler-queue-position *om-sequencer-scheduler*))
  (restore-main-scheduler-cursor) ;;;pas top
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Task Tools
;;;Insert an element in a list, at a specific position.
(defun insert-sequencer-element (index newelt) 
    (if (= 0 index)
        (push newelt *om-sequencer-queue*)
      (push newelt (nthcdr index *om-sequencer-queue*))))

;;;Remove a nth element of a list.
(defun remove-nth (list n)
  (remove-if (constantly t) list :start n :end (1+ n)))

;;;Schedule a task. According to it's timestamp, it pushes it int the right place of the queue.
(defmethod schedule-sequencer-task ((self om-task))
  (when *om-sequencer-scheduler*
    (mp:with-lock ((om-sequencer-scheduler-queue-lock *om-sequencer-scheduler*))
      (insert-sequencer-element
       (let ((p (position (om-task-timestamp self) *om-sequencer-queue* :test '>= :key 'car :from-end t)))
         (if p (1+ p) 0))
       (list (om-task-timestamp self) self (om-task-id self))))
    (restore-main-scheduler-cursor)))

;;;Reschedule a task. It removes it from the queue and reschedule it with an updated timestamp.
(defmethod reschedule-sequencer-task ((self om-task) new-time)
  (setf (om-task-timestamp self) new-time)
  (let ((pos (position (om-task-id self) *om-sequencer-queue* :key 'caddr :test 'equalp)))
    (when pos 
      (progn
        (mp:with-lock ((om-sequencer-scheduler-queue-lock *om-sequencer-scheduler*))
          (setq *om-sequencer-queue* (remove-nth *om-sequencer-queue* pos)))
        (schedule-sequencer-task self)))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Scheduler Tools

;;;QUEUE ORDER TESTS FUNCTIONS
(defun ordered (l)
  (let (temp1)
    (setq temp1 l)
    (equal l (sort (copy-list temp1) #'< :key 'car))))

(defun testorder (n)
  (dotimes (i n)
    (let ((ts (* 1290 i)))
      (schedule-sequencer-task (make-om-task :id (generate-task-id) 
                                             :name (format nil "~A" i)
                                             :event #'(lambda () (print (list (get-clock-time *om-sequencer-scheduler*) ts)))
                                             :readyp t
                                             :timestamp ts))))
  (ordered *om-sequencer-queue*))


;Nombre de processeurs virtuels
;(parse-integer (string (find-if #'digit-char-p (with-output-to-string (om-lisp:*om-stream*) (om-api:om-cmd-line "sysctl -n hw.ncpu" :redirect-output t)) :from-end t)))