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
;  Actions
;;===========================================================================
(declaim (optimize (speed 3) (safety 0) (debug 1)))

(in-package :om)

(defvar *action-engine* nil)
(defvar *action-garbage* '())
  
;;===========================================================================
;;;Action
;;===========================================================================
;;;Structure
(defstruct (action
            (:print-object
             (lambda (a stream)
               (print-unreadable-object (a stream :type t :identity t)
                 (princ `(,(if (act-readyp a) :ready :not-ready) :at ,(act-timestamp a) :. :data :=,(act-data a)) stream))))
            (:conc-name act-))
  (timestamp 0 :type integer)
  (fun #'(lambda ()) :type function)
  (data nil)
  (readyp t :type boolean)
  (identifier nil :type (or null simple-base-string))
  (:documentation "
ACTION: a structure to wrap timed function calls from a SCHEDULABLE OBJECT, or to fill its content.
Actions are mostly automatically used by the schedulable-objects.

Although, it can be instantiated and unsed in a schedulable-object's content providing:
- A timestamp in ms,
- A function to call,
- The data needed by the function, as a list of arguments (otherwise NIL),
- The boolean readyp to say if this action is already ready to be rendered (i.e. if objects/arguments in data are bounded). If set to nil, beware to make it become t when the data is ready."))

;;;Allocate and Free action methods using cache pool
(defun new-action-pool (size)
  (let ((list (make-list size)))
    (loop
     for n on list do
     (setf (car n) (make-action)))
    list))

(let* ((cache-lock (mp:make-lock))
       (cache-size 1024)
       (cache-list '()))
  (declare (type fixnum cache-size))

  (mp:with-lock (cache-lock)
    (setf cache-list (new-action-pool cache-size)))

  (defun act-alloc (&key (timestamp 0) (fun #'(lambda ())) (identifier nil) data (readyp t))
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (new-action-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((act (pop cache-list)))
	(setf (act-timestamp act) timestamp
              (act-fun act) fun
              (act-identifier act) identifier
              (act-data act) (if (listp data) data (list data))
              (act-readyp act) readyp)
	act)))

  (defmethod act-free ((self action))
    (mp:with-lock (cache-lock)
      (setf (act-timestamp self) 0
            (act-fun self) #'(lambda ())
            (act-identifier self) nil
            (act-data self) nil
            (act-readyp self) t)
      (push self cache-list)
      nil)))

;;;Methods
(defmethod play-item ((self function) (sched dynamic-scheduler) &key caller parent)
  (declare (ignore sched caller parent))
  (om-ignore&print-error (funcall self)))

(defmethod play-item ((self action) (sched dynamic-scheduler) &key caller parent)
  (declare (ignore caller parent))
  (fifo-push self (queue *action-engine*))
  (mp:process-poke (process *action-engine*)))
 
(defmethod item-content ((self action))
  nil)

(defmethod item-timestamp ((self action))
  (act-timestamp self))

(defmethod set-item-timestamp ((self action) ts)
  (declare (type integer ts))
  (setf (act-timestamp self) ts))

(defmethod item-readyp ((self action))
  (readyp self))

;;===========================================================================
;;;Action engine
;;===========================================================================
(defclass action-engine ()
  ((process :initform nil :accessor process :initarg :process :type mp:process)
   (queue :initform (fifo-make) :accessor queue :initarg :queue :type list)))

(defun init-action-engine ()
  (setq *action-engine* (make-instance 'action-engine))
  (setf (process *action-engine*)
        (mp:process-run-function "Action Engine" nil 'next-action (queue *action-engine*))))

(defun abort-action-engine ()
  (mp:process-kill (process *action-engine*))
  (setq *action-engine* nil))

(defmethod next-action ((self list))
  (loop
   (om-ignore&print-error (render-action (fifo-pop self)))
   (mp:process-wait-with-timeout "Sleeping" most-positive-fixnum 'action-alarm *action-engine*)))

(defmethod action-alarm ((self action-engine))
  (not (fifo-empty-p (queue self))))

(defmethod render-action ((self t))
  nil)

(defmethod render-action ((self action))
  (if (act-data self)
      (apply (act-fun self) (act-data self))
    (funcall (act-fun self)))
  (push self *action-garbage*))
