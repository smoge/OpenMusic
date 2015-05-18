;;===========================================================================
;Copyright (C) 2015 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Author: Dimitri Bouche
;;===========================================================================

;;===========================================================================
;DocFile
;  Dynamic Scheduler
;;===========================================================================
(declaim (optimize (speed 3) (safety 0) (debug 1)))

(in-package :om)

(defvar *om-scheduler* nil)

;;===========================================================================
;;;Dynamic-Scheduler
;;===========================================================================
;;;Structure

(defclass dynamic-scheduler ()
  ((state :initform :stop :accessor state :initarg :state :type symbol)
   ;;;Process
   (scheduler-process :initform nil :accessor scheduler-process :type (or mp::process null))
   (scheduler-tick :initform 0.001 :accessor tick :type single-float)
   ;;;Graphics
   (callback-process :initform nil :accessor callback-process :type (or mp::process null))
   (callback-tick :initform 0.05 :accessor callback-tick :initarg :callback-tick :type single-float)
   (run-callback :initform nil :accessor run-callback :initarg :run-callback)
   (stop-callback :initform nil :accessor stop-callback :initarg :stop-callback)
   ;;;Playing register
   (object-list :initform (list) :type (or null list))
   (object-list-lock :initform (mp:make-lock) :accessor object-list-lock :type mp:lock)
   (object-identifiers :initform '(0) :accessor object-identifiers :type list)
   (sorted-p :initform t :accessor sorted-p :type boolean)
   (sort-time :initform 50 :accessor sort-time :type integer)
   ;;;Short-term planning length
   (time-window :initform 5 :accessor time-window :initarg :time-window :type integer))
  (:documentation "
DYNAMIC-SCHEDULER: A scheduler to play objects of type (or subtypes of) SCHEDULABLE-OBJECT.
There is only 1 scheduler of this type at a time in OpenMusic.

A DYNAMIC-SCHEDULER can be instantiated using init-om-scheduler(&key run-callback stop-callback (callback-tick 0.05) (time-window 5)) where:
- run-callback is a callback function that will be often called to update the graphics the caller of an object (cf schedulable-object.lisp) during its rendering,
- stop-callback is a callback function that will be called when an object is stopped to update the graphics of its caller,
- callback-tick is the desired graphical refresh rate in s, it is 0.05s by default.
- time-window is the desired temporal window of the scheduler for short-term planning, in ms. It is 5ms by default.

It can be aborted using abort-om-scheduler.
It is possible to know if it ius currently idle or not using scheduler-idle-p((self DYNAMIC-SCHEDULER))."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Control function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-om-scheduler (&key run-callback stop-callback (callback-tick 0.05) (time-window 5))
  (or *om-scheduler*
      (progn
        (setq *om-scheduler* (make-instance 'dynamic-scheduler 
                                            :state :start
                                            :run-callback run-callback
                                            :stop-callback stop-callback
                                            :callback-tick callback-tick
                                            :time-window time-window))
        (setf (scheduler-process *om-scheduler*) (mp:process-run-function "OM-Scheduler player process" nil 
                                                                'next-dyn-action-sync *om-scheduler*))
        (setf (callback-process *om-scheduler*) (mp:process-run-function "OM-Scheduler callback process" nil 
                                                                         'scheduler-callback-loop *om-scheduler*))
        *om-scheduler*)))

(defun abort-om-scheduler ()
  (mp:process-kill (scheduler-process *om-scheduler*))
  (mp:process-kill (callback-process *om-scheduler*))
  (setq *om-scheduler* nil))

(defun pause-om-scheduler ()
  (if (mp:process-alive-p (scheduler-process *om-scheduler*))
      (mp:process-stop (scheduler-process *om-scheduler*))))

(defun resume-om-scheduler ()
  (if (mp:process-stopped-p (scheduler-process *om-scheduler*))
      (mp:process-unstop (scheduler-process *om-scheduler*))))
 
(defun get-om-scheduler ()
  *om-scheduler*)

(defmethod change-scheduler-time-window ((self dynamic-scheduler) (new-window integer))
  (setf (time-window self) new-window))

(defmethod object-list ((self dynamic-scheduler))
  (mp:with-lock ((object-list-lock self))
    (slot-value self 'object-list)))
     
(defmethod (setf object-list) (new-list (self dynamic-scheduler))
  (mp:with-lock ((object-list-lock self))
    (setf (slot-value self 'object-list) new-list)))

(defmethod push-in-object-list ((self dynamic-scheduler) object+caller)
  (mp:with-lock ((object-list-lock self))
    (push object+caller (object-list self))))

(defmethod remove-from-object-list ((self dynamic-scheduler) object)
  (mp:with-lock ((object-list-lock self))
    (setf (object-list self) (delete object (object-list self) :key 'car))))

(defmethod n-playing ((self dynamic-scheduler))
  (length (object-list self)))

(defmethod set-time-window ((self dynamic-scheduler) value)
  (setf (time-window self) (max 1 (round value))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Control methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod abort-dynamic-scheduler ((self dynamic-scheduler))
  (abort-om-scheduler))

(defmethod scheduler-idle-p ((self dynamic-scheduler))
  (not (find :play (mapcar #'state (mapcar #'car (object-list self))))))

(defmethod ds-time ((self dynamic-scheduler))
  (om-get-internal-time))

(defmethod wake-up? ((self dynamic-scheduler)) 
  (plusp (length (object-list self))))

(defmethod get-object-type ((self dynamic-scheduler))
  'schedulable-object)

(defmethod find-available-id ((self dynamic-scheduler))
  (loop for i from 0 by 1 do
        (if (not (find i (object-identifiers self)))
            (return i))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (progn (abort-om-scheduler) (init-om-scheduler))
;(scheduler-idle-p *general-player*)
;(set-time-window *general-player* 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Process methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod next-dyn-action-sync ((self dynamic-scheduler))
  (let ()
    (declare (type list current))
    (loop
     (when (scheduler-idle-p self) 
       (setf (sorted-p self) t)
       (normal-gc)
       (loop while *action-garbage* do
             (act-free (pop *action-garbage*)))
       (gc-all)
       (mp:process-stop (scheduler-process self) "Nothing is playing"))
     (avoid-gc)
     ;(if (sorted-p self)
     ;    (object-step-process (caar (object-list self)) self)
     ;  (progn
     ;    (loop for obj in (mapcar #'car (object-list self))
     ;          do
     ;          (object-step-process obj self))
     ;    (if (> (get-scheduler-idle-time self) (sort-time self))
     ;        (sort-object-list self))))
     (loop for obj in (mapcar #'car (object-list self))
           do
           (object-step-process obj self))
     (mp:process-wait-with-timeout "Waiting" 0.001 'wake-up? self))))


(defmethod (setf sorted-p) (value (self dynamic-scheduler))
  (setf (slot-value self 'sorted-p) value))

(defmethod scheduler-callback-loop ((self dynamic-scheduler))
   (loop
    (if (scheduler-idle-p self)
        (mp:process-stop (callback-process self) "Nothing is playing"))
    (let ()
      (loop for object+caller in (object-list self)
            do
            (when (and (run-callback self) 
                       (cadr object+caller)
                       (eq (player-get-object-state self (car object+caller)) :play))
              (om-ignore&print-error
               (funcall (run-callback self) (cadr object+caller) (player-get-object-time self (car object+caller)))))))
    (mp:process-wait-with-timeout (format nil "zzZZ (~As)" (callback-tick self)) (callback-tick self))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Link to the previous architecture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod make-player ((id (eql :dynamic-scheduler)) &key run-callback stop-callback (callback-tick 0.05) (time-window 5))
  (init-om-scheduler :run-callback run-callback
                     :stop-callback stop-callback
                     :callback-tick callback-tick
                     :time-window time-window))

(defmethod destroy-player ((self dynamic-scheduler))
  (abort-dynamic-scheduler self))

(defmethod player-get-time ((self dynamic-scheduler)) 
  (ds-time self))

(defmethod player-stop ((self dynamic-scheduler) &optional play-list)
  (declare (ignore play-list))
  (loop for object in (mapcar #'car (object-list self)) do
        (player-stop-object self object)))

(defmethod player-set-time-interval ((self dynamic-scheduler) from to)
  (declare (ignore self from to))
  nil)

(defmethod player-get-state ((self dynamic-scheduler)) 
  (state self))

(defmethod player-idle-p ((self dynamic-scheduler))
  (scheduler-idle-p self))

(defmethod player-reset ((self dynamic-scheduler))
  (declare (ignore self))
  t)

;(defmethod player-start ((self dynamic-scheduler) &key (start-t 0) (end-t nil))
;  (declare (ignore self start-t end-t))
;  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generate-id ()
  (format nil "~4X~4X-~4X-~4X-~4X-~4X~4X" 
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(declaim (inline sort-object-list))
(defun sort-object-list (self)
  (if (> (length (object-list self)) 1)
    (sort (object-list self) '< :key 'extract-object-next-delay))
  (setf (sorted-p self) t))

(declaim (inline get-scheduler-idle-time))
(defun get-scheduler-idle-time (self)
  (if (object-list self)
      (reduce 'min (mapcar #'car (object-list self)) :key 'get-object-next-delay)
    0))







