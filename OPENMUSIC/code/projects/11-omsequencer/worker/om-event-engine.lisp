;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage "Event-Engine"
  (:nicknames "EVT")
   (:use common-lisp cl-user))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :evt)

(push :evt *features*)

(export
 '(;;;Structure
   init-om-event-engine
   abort-om-event-engine

   ;;;Tools
   change-om-event-engine-tick

   ;;;Task tools
   send-task-event-engine-top
   send-task-event-engine-bottom

   ;;;Variables
   *om-event-engine*
   EVT_HIGH_PRIORITY
   EVT_MEDIUM_PRIORITY
   EVT_LOW_PRIORITY) :evt)


(defconstant EVT_HIGH_PRIORITY 1)
(defconstant EVT_MEDIUM_PRIORITY 2)
(defconstant EVT_LOW_PRIORITY 3)

(defvar *om-event-engine* nil)

(defstruct (om-event-engine)
  (hp-queue nil :type list)
  (mp-queue nil :type list)
  (lp-queue nil :type list)
  (tick 10.0 :type single-float)
  (process nil :type (or mp::process null))
  (state :inactive :type symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Init and Abort
(defun init-om-event-engine ()
  (if (or (not *om-event-engine*)
          (not (om-event-engine-process *om-event-engine*))
          (equal (mp:process-state (om-event-engine-process *om-event-engine*)) :killed))
      (progn
        (setq *om-event-engine* (make-om-event-engine))
        (setf (om-event-engine-process *om-event-engine*)
              (mp:process-run-function "OM Event Engine" nil 'check-event *om-event-engine*)
              (om-event-engine-state *om-event-engine*) :active))
    (print "The OM Event Engine is already instanced")))

(defun abort-om-event-engine ()
  (when *om-event-engine*
    (mp:process-kill (om-event-engine-process *om-event-engine*))
    (setf (om-event-engine-state *om-event-engine*) :dead)
    (setq *om-event-engine* nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Engine methods
(defmethod check-event ((self om-event-engine))
  (loop
   (execute-event self (get-next-queue self))
   (mp:process-wait-with-timeout (format nil "Sleeping for ~As" (om-event-engine-tick self))
                                 (om-event-engine-tick self) 'om-event-engine-alarm)))


(defmethod get-next-queue ((self om-event-engine))
  (if (om-event-engine-hp-queue self) EVT_HIGH_PRIORITY
    (if (om-event-engine-mp-queue self) EVT_MEDIUM_PRIORITY
      (if (om-event-engine-lp-queue self) EVT_LOW_PRIORITY 0))))

(defmethod execute-event ((self om-event-engine) n)
  (cond ((= n EVT_HIGH_PRIORITY)
         (let* ((task (pop (om-event-engine-hp-queue self)))
                (fct (car task))
                (data (cadr task)))
           (if data (apply fct data) (funcall fct))))
        ((= n EVT_MEDIUM_PRIORITY)
         (let* ((task (pop (om-event-engine-mp-queue self)))
                (fct (car task))
                (data (cadr task)))
           (if data (apply fct data) (funcall fct))))
        ((= n EVT_LOW_PRIORITY)
         (let* ((task (pop (om-event-engine-lp-queue self)))
                (fct (car task))
                (data (cadr task)))
           (if data (apply fct data) (funcall fct))))
        (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Engine Tools
(defun om-event-engine-alarm ()
  (if (or (om-event-engine-hp-queue *om-event-engine*)
          (om-event-engine-mp-queue *om-event-engine*)
          (om-event-engine-lp-queue *om-event-engine*)) t))

(defmethod change-om-event-engine-tick (tick-s) 
  (setf (om-event-engine-tick *om-event-engine*) (coerce (max tick-s 0.001) 'single-float))
  (mp:process-stop (om-event-engine-process *om-event-engine*))
  (mp:process-unstop (om-event-engine-process *om-event-engine*)))

(defmethod send-task-event-engine-top ((self function) priority &rest args)
  (cond ((= priority EVT_HIGH_PRIORITY)
         (push (list self args) (om-event-engine-hp-queue *om-event-engine*)))
        ((= priority EVT_MEDIUM_PRIORITY)
         (push (list self args) (om-event-engine-mp-queue *om-event-engine*)))
        ((= priority EVT_LOW_PRIORITY)
         (push (list self args) (om-event-engine-lp-queue *om-event-engine*)))
        (t nil))
  (when (om-event-engine-process *om-event-engine*)
    (mp:process-poke (om-event-engine-process *om-event-engine*))))

(defmethod send-task-event-engine-bottom ((self function) priority &rest args)
  (cond ((= priority EVT_HIGH_PRIORITY)
         (nconc (om-event-engine-hp-queue *om-event-engine*) (list (list self args))))
        ((= priority EVT_MEDIUM_PRIORITY)
         (nconc (om-event-engine-mp-queue *om-event-engine*) (list (list self args))))
        ((= priority EVT_LOW_PRIORITY)
         (nconc (om-event-engine-lp-queue *om-event-engine*) (list (list self args))))
        (t nil))
  (when (om-event-engine-process *om-event-engine*)
    (mp:process-poke (om-event-engine-process *om-event-engine*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;