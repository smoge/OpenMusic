(in-package :om)

;;;=================================
;;; THE PLAYER
;;;=================================
(defclass omplayer () 
  ((state :accessor state :initform :stop)    ; :play :pause :stop :record
   (loop-play :accessor loop-play :initform nil)
   (start-time :accessor start-time :initform 0)
   (stop-time :accessor stop-time :initform 0)
   (player-offset :accessor player-offset :initform 0)
   (ref-clock-time :accessor ref-clock-time :initform 0)
   ;;; CALLBACKS
   (callback-tick :initform 0.05 :accessor callback-tick :initarg :callback-tick)
   (caller :initform nil :accessor caller :initarg :caller)
   (callback-fun :initform nil :accessor callback-fun :initarg :callback-fun)
   (callback-process :initform nil :accessor callback-process)
   (stop-fun :initform nil :accessor stop-fun :initarg :stop-fun)
   ;;; SCHEDULING TASKS
   (events :initform nil :accessor events :initarg :events)
   (scheduling-process :initform nil :accessor scheduling-process)
   (scheduler-tick :initform 0.01 :accessor scheduler-tick :initarg :scheduler-tick)
   ;;; OBJECTS
   (play-list :initform nil :accessor play-list :initarg :play-list)
   ;;; ENGINES
   (engines :initform nil :accessor engines :initarg :engines)
   ))


;(defstruct player-task (object nil) (engine nil) (at 0) (interval nil))

(defmethod player-init ((self omplayer)) t)

(defmacro get-player-time (player)
  `(cond ((equal (state ,player) :play)
          (+ (player-offset ,player) (start-time ,player) (- (clock-time) (ref-clock-time ,player))))
         ((equal (state ,player) :pause)
          (+ (player-offset ,player) (start-time ,player)))
         (t 0)))

(defmethod idle-p ((self omplayer)) 
  (not (equal (state self) :play)))

(defmethod schedule-task ((player omplayer) task at)
  (push (cons at task) (events player))
  (setf (events player) (sort (events player) '< :key 'car)))

(defmethod unschedule-all ((player omplayer))
  (setf (events player) nil))

(defun get-my-play-list (engine play-list)
  (mapcar 'cadr (remove-if-not #'(lambda (x) (equal x engine)) play-list :key 'car)))



;;; THIS METHOD WHEN THE PLAYER HAS TO PLAY SEVERAL THINGS OR PREPARE THEM IN ADVANCE
;;; SAYS ENGINE TO PREPARE FOR PLAYING <INTERVAL> (optional) IN <OBJ> WITH< ENGINE> AT TIME <at>
(defmethod player-schedule ((player omplayer) obj engine &key (at 0) interval)
  (unless (find engine (engines player)) (push engine (engines player)))
  (push (list engine obj) (play-list player))
  (prepare-to-play engine player obj at interval))
  

(defmethod general-play ((player omplayer) &key (start-t 0) (end-t 3600000))
  (cond ((equal (state player) :play) 
         ;;; prolonge la dur�e de vie du player
         (setf (stop-time player) (max (stop-time player) end-t)))
        ((equal (state player) :pause)
         (general-continue player))
        (t 
         (setf (stop-time player) end-t)
           (when (callback-process player)
             (om-kill-process (callback-process player)))
           (when (scheduling-process player)
             (om-kill-process (scheduling-process player)))
           
           (om-with-priority 80000000
             (setf (scheduling-process player)
                   (om-run-process "player scheduling"
                                   #'(lambda ()
                                       (loop
                                        (loop while (and (events player) (>= (get-player-time player) (car (car (events player))))) do
                                              (funcall (cdr (pop (events player)))))
                                        (if (> (get-player-time player) (stop-time player)) (general-stop player))
                                        (sleep (scheduler-tick player))
                                        )))))
           
           (when (callback-fun player)
             (om-with-priority 10
               (setf (callback-process player)
                     (om-run-process "editor player callback"
                                     #'(lambda ()
                                         (loop 
                                          (funcall (callback-fun player) 
                                                   (caller player) 
                                                   (get-player-time player))
                                          (sleep (callback-tick player))
                                         )))
                     )))
           
           (mapcar #'player-start (engines player) 
                   (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
           
           (setf (state player) :play
                 (start-time player) start-t
                 (ref-clock-time player) (clock-time))
           
           ;(om-delayed-funcall stop-time #'player-stop player obj)
           )
         ))


(defmethod general-pause ((player omplayer))
  (mapcar #'player-pause (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  (when (equal (state player) :play)
    (setf (start-time player) (get-player-time player)
          (state player) :pause
          )))

(defmethod general-continue ((player omplayer))
  (mapcar #'player-continue (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  (setf (ref-clock-time player) (clock-time)
        (state player) :play
        ))

(defmethod general-stop ((player omplayer))
  (mapcar #'player-stop (engines player)
          (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
  (unschedule-all player)
  (setf (engines player) nil
        (play-list player) nil)
  (when (stop-fun player)
    (funcall (stop-fun player) (caller player)))
  (when (callback-process player)
    (om-kill-process (callback-process player))
    (setf (callback-process player) nil))
  (setf (state player) :stop
        (ref-clock-time player) (clock-time)
        (start-time player) 0)
  (when (scheduling-process player)
    (om-kill-process (scheduling-process player))
    (setf (scheduling-process player) nil))
  )

;;;=====================
;;; ENGINES METHODS
;;;=====================

;;; TO BE REDEFINED BY THE DIFFERENT ENGINES
;(defclass player-engine () ())
;(defmethod class-from-player-type ((type t)) 'player-engine)

(defmethod make-player-specific-controls ((self t) control-view) nil)


;;; SPECIFIES SOMETHING TO BE PLAYED ATHER A GIVEN DELAY (<at>) PAST TEH CALL TO PLAYER-start
;;; THE DEFAULT BEHAVIOUR IS TO SCHEDULE 'player-play' AT DELAY
(defmethod prepare-to-play ((engine t) (player omplayer) object at interval)
  (schedule-task player 
                        #'(lambda () 
                            (player-play-object engine object :interval interval))
                        at))

;;; PLAY (NOW)
(defmethod player-play-object ((engine t) object &key interval)
  (print (format nil "~A : play ~A - ~A" engine object interval)))


;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine t) &optional play-list)
  (print (format nil "~A : start" engine)))

;;; PAUSE (all)
(defmethod player-pause ((engine t) &optional play-list)
  (print (format nil "~A : pause" engine)))

;;; CONTINUE (all)
(defmethod player-continue ((engine t) &optional play-list)
  (print (format nil "~A : continue" engine)))

;;; STOP (all)
(defmethod player-stop ((engine t) &optional play-list)
  (print (format nil "~A : stop" engine)))




;;; PAUSE ONLY ONE OBJECT
;(defmethod player-pause-object ((engine t) object)
;  (print (format nil "~A : pause ~A - ~A" engine object)))

;;; RESTART ONLY ONE OBJECT
;(defmethod player-continue-object ((engine t) object)
;  (print (format nil "~A : continue ~A - ~A" engine object)))

;;; STOP ONLY ONE OBJECT
;(defmethod player-stop-object ((engine t) object)
;  (print (format nil "~A : stop ~A - ~A" engine object)))



;;;=================================
;;; GENERAL PLAYER: USED IN PATCH EDITORS
;;;=================================

(defparameter *general-player* (make-instance 'omplayer 
                                              ;;; :callback-fun 'general-player-callback
                                              :callback-tick 1.0
                                              :stop-fun 'general-player-stop
                                              ))

(defvar *play-boxes* nil)

;(defun general-player-callback (caller time)
;  (mapcar #'(lambda (box)
;              (om-invalidate-view (car (frames box))))
;          *play-boxes*))

(defun general-player-stop (caller)
  (mapcar #'(lambda (box)
              (setf (play-state box) nil)
              (om-invalidate-view (car (frames box))))
          *play-boxes*)
  (setf *play-boxes* nil))

(defmethod play-obj? ((self t)) (allowed-in-maq-p self))


(defmethod play-boxes ((boxlist list))
  (mapcar #'(lambda (box)
              (when (play-obj? (value box))
                (player-schedule *general-player*
                                 (value box) 
                                 (get-edit-param box 'player) :at (get-player-time *general-player*))
                (setf (play-state box) t)
                (push box *play-boxes*)
                ))
          boxlist)
  (when *play-boxes*
    (general-play *general-player* :end-t (loop for box in boxlist maximize (get-obj-dur (value box))))))


(defmethod stop-boxes ((boxlist list))
  (mapcar #'(lambda (box)
              (when (play-obj? (value box))
                (player-stop (get-edit-param box 'player) (list (value box)))
                (setf (play-state box) nil)
                (setf *play-boxes* (remove box *play-boxes*))
                ))
          boxlist)
  (unless *play-boxes* (general-stop *general-player*))
  )

(defmethod stop-all-boxes ()
  (general-player-stop nil)
  (stop-boxes *play-boxes*))


;;; DEPRECATED !!
;(defmethod play-from-box ((self list))
;  (let ((playlist (loop for box in self 
;                        when (and (play-obj? (value (object box))) 
;                                  (not (typep (value (object box)) 'sound)) 
;                                  (not (typep (value (object box)) 'faust-synth-console)))
;                        collect (object box)))
;        (sndplaylist (loop for box in self 
;                           when (typep (value (object box)) 'sound)
;                           collect (value (object box))))
;        (synthplaylist (loop for box in self 
;                           when (typep (value (object box)) 'faust-synth-console)
;                           collect (value (object box))))) ;;PAS PROPRE

;    (when playlist
;      (PlayAny t (make-instance 'listtoplay
;                                :thelist (loop for item in playlist
;                                               collect (value item))
;                                :params (loop for item in playlist
;                                              collect (edition-params item)))))
;    (when sndplaylist
;      (las-play/stop sndplaylist))
;    (when synthplaylist
;      (las-synth-preview-play/stop synthplaylist)
;      )))


;;;=================================
;;; AN EDITOR ASSOCIATED WITH A PLAYER
;;;=================================
(defclass play-editor-mixin ()
   ((player :initform nil :accessor player)
    (player-type :initform nil :accessor player-type)
    (loop-play :initform nil :accessor loop-play)
    (end-callback :initform nil :accessor end-callback)))

(defun init-editor-player (editor)
  (setf (player editor) (make-instance 'omplayer ;; (class-from-player-type (get-score-player editor))
                                     :caller editor
                                     :callback-fun 'play-editor-callback
                                     :stop-fun 'stop-editor-callback)))

(defmethod initialize-instance :after ((self play-editor-mixin) &rest initargs)
  (init-editor-player self))

(defmethod get-player-engine ((self play-editor-mixin)) t)
(defmethod get-player-engine ((self editorview)) (get-edit-param self 'player))


(defmethod reset-editor-player ((self play-editor-mixin))
  (init-editor-player self)
  (player-init (player self)))

(defmethod get-obj-to-play ((self play-editor-mixin)) nil)

(defmethod get-duration ((self play-editor-mixin)) 
  (get-obj-dur (get-obj-to-play self)))   ;;; = 0 if obj = NIL

(defmethod get-interval-to-play ((self play-editor-mixin))
  (let ((selection-pane (car (cursor-panes self)))
        (object (get-obj-to-play self)))
    (when (and selection-pane object)
      (cond ((and (cursor-interval selection-pane) 
                  (not (= (car (cursor-interval selection-pane)) (cadr (cursor-interval selection-pane)))))
             (cursor-interval selection-pane))
            ((and (cursor-pos selection-pane) (not (zerop (cursor-pos selection-pane))))
             (list (cursor-pos selection-pane) (get-duration self)))
            (t nil)))))

;;; THE USER PRESSES PLAY IN THE EDITOR
(defmethod editor-play ((self play-editor-mixin) )
  (setf (loop-play (player self)) (loop-play self))
  (let ((obj (get-obj-to-play self))
        (interval (get-interval-to-play self))
        (engine (get-player-engine self)))
    (setf (callback-fun (player self))
          #'(lambda (editor time)
              (handler-bind ((error #'(lambda (e) 
                                        (print e)
                                        (om-kill-process (callback-process (player self)))
                                        (abort e))))
                (play-editor-callback editor time)
                )))
    (mapcar #'(lambda (view) (start-cursor view)) (cursor-panes self))
    (player-schedule (player self) obj engine :at 0 :interval interval)
    (general-play (player self) 
                  :start-t (or (car interval) 0)
                  :end-t (or (cadr interval) (get-obj-dur obj)))))


(defmethod editor-pause ((self play-editor-mixin))
  (general-pause (player self)))

(defmethod editor-stop ((self play-editor-mixin))
  (general-stop (player self)))

(defmethod editor-play/stop ((self play-editor-mixin))
  (if (idle-p (player self))
      (editor-play self)
    (editor-stop self)))

;;; temp compatibility
(defmethod recording? ((self play-editor-mixin))
  (equal (state (player self)) :record))

;;; A REDEFINIR PAR LES SOUS-CLASSES
(defmethod cursor-panes ((self play-editor-mixin)) nil)

(defmethod play-editor-callback ((self play-editor-mixin) time)
  (mapcar #'(lambda (view) (update-cursor view time))
          (cursor-panes self)))

(defmethod stop-editor-callback ((self play-editor-mixin)) nil)






;;;===================================
; VIEW WITH CURSOR
;;;===================================

(defclass cursor-play-view-mixin (om-view-cursor-play) 
  ((cursor-mode  :initform :normal :accessor cursor-mode :initarg :cursor-mode)   ;; :normal ou :interval
   (cursor-interval :initform '(0 0) :accessor cursor-interval)
   (cursor-pos :initform 0 :accessor cursor-pos)))

(defmethod set-cursor-mode ((self cursor-play-view-mixin))
  (setf (cursor-mode self) (if (equal (cursor-mode self) :normal) :interval :normal))
  (om-invalidate-view self t))

(defmethod cursor-p ((self t)) 
  (om-beep-msg "!!! CURSOR-P DOES NOT EXIST ANYMORE!!!"))

;(defmethod get-obj-to-play ((self cursor-play-view-mixin))
;  (values (object (om-view-container self))
;          (car (cursor-interval self))
;          (cadr (cursor-interval self))
;          ))

(defmethod start-position ((self cursor-play-view-mixin)) 
  (or (cursor-pos self) 0))

(defmethod view-turn-pages-p ((self cursor-play-view-mixin)) t)

;--------------------
; INTERVAL SELECTION
;--------------------

(defmethod new-interval-cursor ((self cursor-play-view-mixin) where)
  (om-init-motion-functions self 'interval-select-action 'release-interval-select)
  (om-new-movable-object self (om-point-h where) 0 4 (h self) 'om-selection-rectangle))

(defmethod interval-select-action ((self cursor-play-view-mixin) pos)
 (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (om-v-scroll-position self) 
                                (max 4 (third rect)) (om-point-v (om-interior-size self))))))

(defmethod release-interval-select ((self cursor-play-view-mixin) pos)  
  (let ((rect (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
        (minpixel 2) position)
    (when rect
      (om-erase-movable-object self)
      (setf position (if (> (third rect) minpixel)
                         (list (car rect) (+ (car rect) (third rect)))
                       (car rect)))
      (if (listp position)
          (setf (cursor-interval self) (list (om-point-x (pixel2point self (om-make-point (car position) 0)))
                                             (om-point-x (pixel2point self (om-make-point (cadr position) 0)))))
        (progn
          (setf (cursor-interval self) nil)
          (setf (cursor-pos self) (max 0 (om-point-h (pixel2point self (om-make-point position 0)))))))
      (om-invalidate-view self))))

(defmethod draw-interval-cursor ((self cursor-play-view-mixin))
   (let* ((sys-etat (get-system-etat self))
          (interval (cursor-interval self))
          pixel-interval)
     (when interval
       (setq pixel-interval (list (om-point-h (point2pixel self (om-make-point (car interval) 0) sys-etat))
                             (om-point-h (point2pixel self (om-make-point (second interval) 0) sys-etat))))
       (om-with-focused-view self
         (draw-h-rectangle (list (car pixel-interval) 0 (second pixel-interval) (h self)) t))
       )))

(defmethod start-cursor ((self cursor-play-view-mixin))
  (let* ((dur (get-obj-dur (object (om-view-container self))))
         (range (rangex (panel (om-view-container self))))
         (xview (- (second range) (first range)))
         (start (start-position self))
         (dest (+ start xview))
         (at-pix (om-point-x (point2pixel self (om-make-point start 0) (get-system-etat self)))))
    (when (and (view-turn-pages-p self) (< dest dur))
      (scroll-play-view self at-pix))
    (om-erase-movable-cursor self)
    (om-new-movable-cursor self (start-position self) (start-position self) 4 (h self) 'om-cursor-line)))


(defmethod time2pixel ((self t) time)
  (om-point-x (point2pixel self (om-make-point time 0) (get-system-etat self))))

(defmethod update-cursor ((self cursor-play-view-mixin) time &optional y1 y2)
  (let* ((y (or y1 0))
         (h (if y2 (- y2 y1) (h self)))
         (pixel (time2pixel self time))
         (dur (get-obj-dur (object (om-view-container self))))
         range
         xview
         dest
        ;(pixel (xpoint2pixel self time (get-system-etat self)))
         )
    (when (and (view-turn-pages-p self)
               (> pixel (+ (w self) (om-h-scroll-position self)))
               (< time dur))
      (progn
        (setf range (rangex (panel (om-view-container self))))
        (setf xview (- (second range) (first range)))
        (setf dest (+ time xview))
        (if (> dest dur)
            (setf pixel (time2pixel self (- dur xview))))
        (scroll-play-view self pixel)))
    (om-update-movable-cursor self pixel y 4 h)))

(defmethod scroll-play-view ((self cursor-play-view-mixin) &optional at-pixel)
  (om-set-scroll-position self (om-make-point at-pixel 0)))
  


