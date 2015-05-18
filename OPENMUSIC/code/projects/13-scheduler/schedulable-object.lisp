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
;  Schedulable Object
;;===========================================================================
(declaim (optimize (speed 3) (safety 0) (debug 1)))

(in-package :om)

;;===========================================================================
;;;Score-Object
;;===========================================================================
;;;Structure
(defclass schedulable-object ()
  (;;;Object bag
   (state :initform :stop :accessor state :type symbol)
   (identifier :initform (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t) :accessor identifier)
   (lock :initform (mp:make-lock) :accessor lock :type mp:lock)
   (content :initform '() :accessor content :initarg :content :type list)
   (fun :initform nil :accessor fun :initarg :fun :type function)
   (data :initform nil :accessor data)
   ;;;Object externally assigned slots
   (timestamp :initform 0 :accessor timestamp :initarg :timestamp :type integer)
   (objinterval :initform nil :accessor objinterval :type (or null list))
   ;;;Scheduler related information
   (duration :initform nil :accessor duration :type (or null integer))
   ;(start-time :initform 0 :accessor start-time :type integer) ;;;
   (ref-time :initform 0 :accessor ref-time :type integer)
   (pause-time :initform 0 :accessor pause-time :type integer)
   (current-logical-time :initform 0 :accessor current-logical-time :type integer)
   (search-count :initform 0 :accessor search-count :type integer)
   (children :initform (list) :accessor children)
   (next-plan :initform (list) :accessor next-plan)
   ;(plan-lock :initform (mp:make-lock) :accessor plan-lock :type mp:lock)
   )
  (:documentation "
SCHEDULABLE OBJECT: an object to be played through a DYNAMIC-SCHEDULER.
=======================================================================
A schedulable object can be used using two different rules:

-----
--1--
-----
Making the class schedulable-object a super-class of any that needs to be playable.
If so, some methods of the schedulable-object class has to be redefined for these

- The method get-next-data((self OBJECT-CLASS) time-interval time-bounds).
This method will ask the object what has to be played in [(car time-interval);(cadr time-interval)[.
The output has to be: 
.((Timestamp1 Function1 Data1) ... (TimestampN FunctionN DataN)) where TimestampN is the date in ms when FunctionN has to be called with DataN as a list of arguments (NIL if no arguments needed),
.(NIL NIL NIL) if the object needs to be stopped (if never returned the object never stops),
.NIL if nothing has to be played (no item to play found in time-interval).

- Every access to the data used by get-next-data has to be wrapped in the macro read-scheduling-data((self OBJECT-CLASS) &rest body).
Every edition of the data used by get-next-data has to be wrapped in the macro edit-scheduling-data((self OBJECT-CLASS) &rest body).
It is suggested to implement 1 unique method to access the data and 1 unique to edit making use of these macros.

- If the rendering of the object in OpenMusic actually triggers data rendering on a separate thread, it might be necessary to redefine the player control methods to control the separate thread as well. These methods labels are returned from a call to 'player-get-control-methods. In the redefinitions, it is necessary to perform call-next-methods.
-----

-----
--2--
-----
Instantiating a schedulable object, using the slot 'content to store both other schedulable-objects or actions (cf. schedulable-actions.lisp), ordered by increasing timestamps.
-----

Then, the desired object can be:
- Played using the method player-schedule-object(scheduler object caller &key interval) where the caller is the box that will need graphical updates during rendering, and the interval is a list of type (tmin tmax) or null.
- Paused using player-pause-object,
- Continued using player-continue-object,
- Stopped using player-stop-object.
The state of an object can be retrieved using the 'state method call. It can be :play, :pause or :stop."))

;;;Redéfinir pour chaque objet
;;;OBJET STATIQUE, JUSTE METTRE TOUT L'INTERVAL :-)
(defmethod get-next-data ((object schedulable-object) time-interval time-bounds)
  (let* ((itemlist (content object))
         (tmin (car time-interval))
         (tmax (cadr time-interval))
         (p1 (if (< tmin tmax)
                 (position tmin itemlist :test '<= :key 'item-timestamp))))
    (if p1
        (loop for item in (subseq itemlist
                                  p1
                                  (position tmax itemlist :test '<= :key 'item-timestamp))
              collect
              (list (item-timestamp item)
                    #'(lambda (it)
                        (play-item it *om-scheduler* :parent object))
                    (list item)))
      (list (list nil nil nil)))))


(defun essai ()
  (let ((object (make-instance 'schedulable-object)))
    (nconc-content object (list (act-alloc :timestamp 250 :fun 'print :data "fire")))
    (play-item object *om-scheduler*)
    (nconc-content object (loop for i from 1 to 10 collect
                                (act-alloc :timestamp (+ 250 (* i 250)) :fun 'print :data "fire 2")))))

(defmethod nconc-content ((self schedulable-object) list)
  (edit-scheduling-data (self)
                        (setf (content self) (append (content self) list))))

;(essai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Object methods & macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro edit-scheduling-data ((object &optional time) &rest body)
  `(let (res objtime)
     (mp:with-lock ((lock ,object))
       (setq res (progn ,@body)
             objtime (get-obj-time ,object)))
     (if (and (eq (state ,object) :play) (and ,time (<= ,time (get-object-next-plan-time ,object))))
         (refresh-next-data ,object *om-scheduler* (- objtime (mod objtime (time-window *om-scheduler*))))
     res)))

(defmacro read-scheduling-data (object &rest body)
  `(mp:with-lock ((lock ,object))
       ,@body))

(defmethod play-item ((self schedulable-object) (sched dynamic-scheduler) &key caller parent)
  (player-schedule-object sched self caller :parent parent))

(defmethod refresh-next-data ((self schedulable-object) (sched dynamic-scheduler) &optional time)
  (clear-next-data self sched)
  (if time (setf (current-logical-time self) time))
  (fill-next-schedulable-object-plan self sched))

(defmethod clear-next-data ((self schedulable-object) (sched dynamic-scheduler))
  (setf (next-plan self) :empty))

(defmethod fill-next-schedulable-object-plan ((self schedulable-object) (sched dynamic-scheduler))
  (let* ((current-time (current-logical-time self))
         (time-bounds (objinterval self))
         (win (time-window sched))
         (tmin (+ (or (car time-bounds) 0) current-time))
         (tmax (min (+ (or (car time-bounds) 0) (+ current-time win)) (or (cadr time-bounds) most-positive-fixnum)))
         next)
    (if (>= tmin tmax)
        (setf (next-plan self)
              (list (act-alloc :timestamp tmax
                               :fun #'(lambda (obj) (player-unschedule-object sched obj))
                               :data (list self)
                               :identifier (identifier self))))
      (progn
        (setq next (get-next-data self `(,tmin ,tmax) time-bounds))
        (incf tmin win)
        (loop while (and (not next) (<= (search-count self) 1000))
              do
              (setq next (get-next-data self (list tmin (+ tmin win)) time-bounds))
              (incf tmin win)
              (incf (search-count self)))
        (setf (current-logical-time self) tmin)
        (if (not next)
            (setf (next-plan self)
                  (list (act-alloc :timestamp (+ (current-logical-time self) (or (car (objinterval self)) 0))
                                   :fun #'(lambda (obj) 
                                            (setf (search-count obj) 0)
                                            (fill-next-schedulable-object-plan obj sched))
                                   :data (list self)
                                   :identifier (identifier self))))
          (setf (next-plan self)
                (if (not (cadar next))
                    (list (act-alloc :timestamp (+ (current-logical-time self) (or (car (objinterval self)) 0))
                                     :fun #'(lambda (obj) (player-unschedule-object sched obj))
                                     :data (list self)
                                     :identifier (identifier self)))
                  (mapcar 'list->action next (make-list (length next) :initial-element (identifier self))))
                (search-count self) 0))))))

(declaim (inline object-step-process))
(defun object-step-process (self sched)
  (when (and (next-plan self) (not (eq (next-plan self) :empty)))
    (om-ignore&print-error
     (when (<= (item-timestamp (car (next-plan self))) (player-get-object-time sched self))
       (play-item (pop (next-plan self)) sched)))
    (when (not (next-plan self))
      (setf (next-plan self) :empty)
      (refresh-next-data self sched))
    (setf (sorted-p sched) nil)))

(defmethod set-schedulable-object-id ((self schedulable-object) (number integer))
  (loop for char across (number-to-string number) do
        (vector-push-extend char (identifier self)))
  (vector-push-extend #\. (identifier self)))

;;;Recursive version: elegant but not suitable for unlimited objects
;(defmethod fill-next-schedulable-object-plan ((self schedulable-object) (sched dynamic-scheduler))
;  (let* ((current-time (current-logical-time self))
;         (time-bounds (interval self))
;         (win (time-window sched))
;         (tmin (+ (or (car time-bounds) 0) current-time))
;         (tmax (min (+ (or (car time-bounds) 0) (+ current-time win)) (or (cadr time-bounds) most-positive-fixnum)))
;         next)
;    (if (>= tmin tmax)
;        (setf (next-plan self)
;              (list (act-alloc :timestamp tmax
;                               :fun #'(lambda (obj) (player-unschedule-object sched obj))
;                               :data (list self)
;                               :identifier (identifier self))))
;      (progn
;        (setq next (get-next-data self `(,tmin ,tmax) time-bounds))
;        (incf (current-logical-time self) win)
;        (if (not next)
;            (fill-next-schedulable-object-plan self sched)
;           (setf (next-plan self)
;                 (if (not (cadar next))
;                     (list (act-alloc :timestamp (+ (current-logical-time self) (or (car (interval self)) 0))
;                                      :fun #'(lambda (obj) (player-unschedule-object sched obj))
;                                      :data (list self)
;                                      :identifier (identifier self)))
;                   (mapcar 'list->action next))))))))

(defmethod list->action ((self list) (id string))
  (act-alloc :timestamp (round (or (car self) 0))
             :fun (cadr self)
             :data (caddr self)
             :identifier id))

(defmethod item-timestamp ((self schedulable-object))
  (timestamp self))

(defmethod set-item-timestamp ((self schedulable-object) ts)
  (declare (type integer ts))
  (setf (timestamp self) ts))

(defmethod get-object-next-delay ((self schedulable-object))
  (let ((delay (ignore-errors (- (item-timestamp (car (next-plan self))) (player-get-object-time *om-scheduler* self)))))
    (or delay 0)))

(defmethod get-object-next-plan-time ((self schedulable-object))
  (let ((delay (ignore-errors (item-timestamp (car (next-plan self))))))
    (or delay 0)))

(defun extract-object-next-delay (object+caller)
  (let ((delay (om-ignore&print-error (- (item-timestamp (car (next-plan (car object+caller)))) (player-get-object-time *om-scheduler* (car object+caller))))))
    (or delay 0)))

(defmethod item-content ((self schedulable-object))
  (read-scheduling-data self
                        (content self)))

(defmethod item-readyp ((self schedulable-object))
  (let ((ready t))
    (declare (type boolean ready))
    (loop for item in (content self) do
          (setq ready (and (item-readyp item) ready)))
    ready))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Player methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod play-schedulable-object ((self schedulable-object) (sched dynamic-scheduler) &key internal-time)
  (when (eq (state self) :stop)
    (setf 
     (state self) :play
     (current-logical-time self) 0
     ;(start-time self) 
     (ref-time self) (or internal-time (get-internal-real-time)))
    (fill-next-schedulable-object-plan self sched)
    (if (mp:process-stopped-p (scheduler-process sched))
      (mp:process-unstop (scheduler-process sched)))
    (if (mp:process-stopped-p (callback-process sched))
        (mp:process-unstop (callback-process sched)))))

(defmethod pause-schedulable-object ((self schedulable-object) (sched dynamic-scheduler) &key internal-time)
  (when (eq (state self) :play)
    (setf (pause-time self) (get-obj-time self :internal-time internal-time)
          (current-logical-time self) (pause-time self)
          (state self) :pause)
    (clear-next-data self sched)))

(defmethod continue-schedulable-object ((self schedulable-object) (sched dynamic-scheduler) &key internal-time)
  (when (eq (state self) :pause)
    (setf (state self) :play
          (ref-time self) (round (if internal-time
                                     (- internal-time (pause-time self))
                                   (- (get-internal-real-time) (pause-time self))))
          ;(start-time self) (- (get-internal-real-time) (+ (pause-time self) (ref-time self)))
          )
    (refresh-next-data self sched)
    (if (mp:process-stopped-p (scheduler-process sched))
        (mp:process-unstop (scheduler-process sched)))
    (if (mp:process-stopped-p (callback-process sched))
        (mp:process-unstop (callback-process sched)))))

(defmethod stop-schedulable-object ((self schedulable-object) (sched dynamic-scheduler))
  (setf (state self) :stop)
  (funcall (stop-callback sched) (cadr (find self (object-list sched) :key 'car))))


(defmethod get-obj-time ((self schedulable-object) &key internal-time)
  (cond ((eq (state self) :pause)
         (pause-time self))
        ((eq (state self) :stop) 0)
        (t
         (- (or internal-time (get-internal-real-time)) (ref-time self)))))

(defmethod register-schedulable-object ((object schedulable-object) (sched dynamic-scheduler) caller)
  (setf (next-plan object) :empty)
  (push-in-object-list sched (list object caller))
  (setf (sorted-p sched) nil))

(defmethod unregister-schedulable-object ((object schedulable-object) (sched dynamic-scheduler))
  (remove-from-object-list sched object))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Links to previous architecture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod player-schedule-object ((self dynamic-scheduler) (object schedulable-object) caller &key parent (at 0) interval params)
  (declare (ignore at params))
  (setf (objinterval object) interval) 
  (if parent
      (progn
        (push object (children parent))
        (set-schedulable-object-id object (length (children parent))))
    (set-schedulable-object-id object (find-available-id self)))
  (register-schedulable-object object self caller)
  (play-schedulable-object object self))

(defmethod player-unschedule-object ((self dynamic-scheduler) (object schedulable-object))
  (stop-schedulable-object object self)
  (loop for obj in (children object) do
        (player-unschedule-object self obj))
  (setf (children object) nil)
  (unregister-schedulable-object object self))

(defmethod player-stop-object ((self dynamic-scheduler) (object schedulable-object) &key interval)
  (loop for obj in (children object) do
        (player-unschedule-object self obj))
  (player-unschedule-object self object))

(defmethod player-pause-object ((self dynamic-scheduler) (object schedulable-object) &key interval)
  (loop for obj in (children object) do
        (player-pause-object self obj))
  (pause-schedulable-object object self))

(defmethod player-continue-object ((self dynamic-scheduler) (object schedulable-object) &key interval)
  (loop for obj in (children object) do
        (player-continue-object self obj))
  (continue-schedulable-object object self))

(defmethod player-get-object-state ((self dynamic-scheduler) (object schedulable-object))
  (state object))

(defmethod player-get-object-time ((self dynamic-scheduler) (object schedulable-object))
  (if (objinterval object) (+ (car (objinterval object)) (get-obj-time object)) (get-obj-time object)))

(defmethod player-get-control-methods ((self dynamic-scheduler))
  (values 'player-schedule-object 'player-pause-object 'player-continue-object 'player-stop-object))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;