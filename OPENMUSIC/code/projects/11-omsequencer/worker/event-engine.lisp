;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage "Event-Engine"
  (:nicknames "EVT")
   (:use common-lisp cl-user))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :evt)

(push :evt *features*)

(export
 '(;;;Structure
   init-event-engine
   abort-event-engine

   ;;;Tools
   change-event-engine-tick

   ;;;Task tools
   send-task-event-engine-top
   send-task-event-engine-bottom

   ;;;Variables
   *event-engine*
   EVT_HIGH_PRIORITY
   EVT_MEDIUM_PRIORITY
   EVT_LOW_PRIORITY) :evt)


(defconstant EVT_HIGH_PRIORITY 1)
(defconstant EVT_MEDIUM_PRIORITY 2)
(defconstant EVT_LOW_PRIORITY 3)

(defvar *event-engine* nil)

(defstruct (event-engine)
  (hp-queue nil :type list)
  (mp-queue nil :type list)
  (lp-queue nil :type list)
  (tick 10.0 :type single-float)
  (process nil :type (or mp::process null))
  (state :inactive :type symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Init and Abort
(defun init-event-engine ()
  (if (or (not *event-engine*)
          (not (event-engine-process *event-engine*))
          (equal (mp:process-state (event-engine-process *event-engine*)) :killed))
      (progn
        (setq *event-engine* (make-event-engine))
        (setf (event-engine-process *event-engine*)
              (mp:process-run-function "Event Engine" nil 'check-event *event-engine*)
              (event-engine-state *event-engine*) :active))
    (print "The Event Engine is already instanced")))

(defun abort-event-engine ()
  (when *event-engine*
    (mp:process-kill (event-engine-process *event-engine*))
    (setf (event-engine-state *event-engine*) :dead)
    (setq *event-engine* nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Engine methods
(defmethod check-event ((self event-engine))
  (loop
   (execute-event self (get-next-queue self))
   (mp:process-wait-with-timeout (format nil "Sleeping for ~As" (event-engine-tick self))
                                 (event-engine-tick self) 'event-engine-alarm)))


(defmethod get-next-queue ((self event-engine))
  (if (event-engine-hp-queue self) EVT_HIGH_PRIORITY
    (if (event-engine-mp-queue self) EVT_MEDIUM_PRIORITY
      (if (event-engine-lp-queue self) EVT_LOW_PRIORITY 0))))

(defmethod execute-event ((self event-engine) n)
  (cond ((= n EVT_HIGH_PRIORITY)
         (let* ((task (pop (event-engine-hp-queue self)))
                (fct (car task))
                (data (caadr task)))
           (if data (apply fct data) (funcall fct))))
        ((= n EVT_MEDIUM_PRIORITY)
         (let* ((task (pop (event-engine-mp-queue self)))
                (fct (car task))
                (data (caadr task)))
           (if data (apply fct data) (funcall fct))))
        ((= n EVT_LOW_PRIORITY)
         (let* ((task (pop (event-engine-lp-queue self)))
                (fct (car task))
                (data (caadr task)))
           (if data (apply fct data) (funcall fct))))
        (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Engine Tools
(defun event-engine-alarm ()
  (if (or (event-engine-hp-queue *event-engine*)
          (event-engine-mp-queue *event-engine*)
          (event-engine-lp-queue *event-engine*)) t))

(defmethod change-event-engine-tick (tick-s) 
  (setf (event-engine-tick *event-engine*) (coerce (max tick-s 0.001) 'single-float))
  (mp:process-stop (event-engine-process *event-engine*))
  (mp:process-unstop (event-engine-process *event-engine*)))

(defmethod send-task-event-engine-top ((self function) priority &rest args)
  (cond ((= priority EVT_HIGH_PRIORITY)
             (push (list self args) (event-engine-hp-queue *event-engine*)))
            ((= priority EVT_MEDIUM_PRIORITY)
             (push (list self args) (event-engine-mp-queue *event-engine*)))
            ((= priority EVT_LOW_PRIORITY)
             (push (list self args) (event-engine-lp-queue *event-engine*)))
            (t nil))
  (when (event-engine-process *event-engine*)
    (mp:process-poke (event-engine-process *event-engine*))))

(defmethod send-task-event-engine-bottom ((self function) priority &rest args)
  (cond ((= priority EVT_HIGH_PRIORITY)
             (nconc (event-engine-hp-queue *event-engine*) (list (list self args))))
            ((= priority EVT_MEDIUM_PRIORITY)
             (nconc (event-engine-mp-queue *event-engine*) (list (list self args))))
            ((= priority EVT_LOW_PRIORITY)
             (nconc (event-engine-lp-queue *event-engine*) (list (list self args))))
            (t nil))
  (when (event-engine-process *event-engine*)
    (mp:process-poke (event-engine-process *event-engine*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;