#|
Event Engine API (cf. event-engine.lisp)

Author : D.Bouche
Ircam (C) 2014
|#


(in-package :om)

;;;=============================================================================================Event Engine Structure
(defun om-init-event-engine ()
  (evt:init-event-engine))
(defun om-abort-event-engine ()
  (evt:abort-event-engine))

(defun om-get-event-engine ()
  evt:*event-engine*)

(defmethod om-get-event-engine-hp-queue ((self evt::event-engine))
  (evt::event-engine-hp-queue self))
(defmethod om-set-event-engine-hp-queue ((self evt::event-engine) queue)
  (setf (evt::event-engine-hp-queue self) queue))

(defmethod om-get-event-engine-mp-queue ((self evt::event-engine))
  (evt::event-engine-mp-queue self))
(defmethod om-set-event-engine-mp-queue ((self evt::event-engine) queue)
  (setf (evt::event-engine-mp-queue self) queue))

(defmethod om-get-event-engine-lp-queue ((self evt::event-engine))
  (evt::event-engine-lp-queue self))
(defmethod om-set-event-engine-lp-queue ((self evt::event-engine) queue)
  (setf (evt::event-engine-lp-queue self) queue))

(defmethod om-get-event-engine-tick ((self evt::event-engine))
  (evt::event-engine-tick self))
(defmethod om-set-event-engine-tick ((self evt::event-engine) tick)
  (evt:change-event-engine-tick tick))

(defmethod om-get-event-engine-state ((self evt::event-engine))
  (evt::event-engine-state self))

;;;=============================================================================================Event Engine Entry Points
(defmethod om-send-task-to-event-engine-top ((self function) priority &rest args)
  (evt:send-task-event-engine-top self priority args))
(defmethod om-send-task-to-event-engine-bottom ((self function) priority &rest args)
  (evt:send-task-event-engine-bottom self priority args))
