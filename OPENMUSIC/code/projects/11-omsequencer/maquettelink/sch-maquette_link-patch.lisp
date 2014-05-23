(in-package :om)

;;;This patch acts on the sequencer queue when adding elements in a maquette
;;;Works only with CHORD (to test the system)
;;;If an element of the maquette is deleted, it won't be deleted of the sequencer queue. 
;;;Make the sequencer queue empty to restart from scratch : (setq sch::*om-sequencer-queue* nil)
(defvar *curplayer* nil)
(defvar *curmaq* nil)
(defvar *jump-ms* 2000)
(defvar *reschedule-ms* 1500)

(defun create-new-chord (xoff yoff)
  (let* ((x (om-make-point 10 0))
         (y (om-make-point 11 1))
         (self *curmaq*)
         (class (get-absmaqclass))
         (thename (mk-unique-name self (get-maq-obj-name class)))
         (new-patch (make-instance 'chord 
                                   :lmidic '(6000 7000 7300)))
         (pixsizex (max 20 (- (om-point-h y) (om-point-h x))))
         (pixsizey (max 10 (- (om-point-v y) (om-point-v x))))
         (maqpos (om-make-point xoff yoff))
         (y-size 2)
         (tempobj (omNG-make-tempobj new-patch maqpos thename))
         new-frame)
    (setf (slot-value tempobj 'extend) 500)
    (setf (slot-value tempobj 'sizey)  y-size)
    (setf new-frame (make-frame-from-callobj tempobj))
    (omG-add-element self new-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ADD functions (compile the one you want to use at each drop)
;;;ADD BASIC
(defmethod omNG-add-element ((self OMMaquette) elem) (print elem)
  (call-next-method) 
  (if (not sch::*om-sequencer-scheduler*) (sch::init-sequencer-scheduler))
  (print (list "Schedule" (value elem) "at" (offset elem)))
  (sch::schedule-sequencer-task
   (sch::make-om-task :id (sch::generate-task-id)
                      :event 
                      (cond ((= (free-store elem) 0)
                             #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)))
                            ((= (free-store elem) 1)
                             #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (setf (offset elem) (+ *reschedule-ms* (offset elem)))
                                 (let (task)
                                  (loop for item in sch::*om-sequencer-queue* do
                                         (if (eq (sch::om-task-object (cadr item)) (car (value elem)))
                                             (setq task (cadr item))))
                                  (when task
                                    (print (list "Re-schedule" (car (value elem)) "from" (sch::om-task-timestamp task) "to" (offset elem)))
                                    (sch::reschedule-sequencer-task task (offset elem))
                                    ;(sleep 0.3)
                                    ;(om-invalidate-view (om-view-container (car (frames elem))))
                                    ))))
                            ((= (free-store elem) 2)
                             #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 ;(sch::compute-tree sch::*treetest*)
                                 ))
                            ((= (free-store elem) 3)
                             #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (progn 
                                   (sch::om-jump-scheduler sch::*om-sequencer-scheduler* *jump-ms*)
                                   (setf (ref-clock-time *curplayer*) (sch::om-scheduler-ref-time sch::*om-sequencer-scheduler*)))))
                            ((= (free-store elem) 4)
                             #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (progn 
                                   (create-new-chord (om-random 1000 10000) 0))))
                            ((= (free-store elem) 5)
                             #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (progn 
                                   (delete-general *curmaq*)))))
                                          
                      :readyp t
                      :object (car (value elem))
                      :timestamp (offset elem))))

#|(delete-general *curmaq*) (reference (car (get-actives *curmaq*)))

;;;ADD RE-SCHEDULE
(defmethod omNG-add-element ((self OMMaquette) elem)
  (call-next-method)
  (if (not sch::*om-sequencer-scheduler*) (sch::init-sequencer-scheduler))
  (print (list "Schedule" (value elem) "at" (offset elem)))
  (sch::schedule-sequencer-task
   (sch::make-om-task :id (sch::generate-task-id)
                      :event #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (setf (offset elem) (+ *reschedule-ms* (offset elem)))
                                 (let (task)
                                  (loop for item in sch::*om-sequencer-queue* do
                                         (if (eq (sch::om-task-object (cadr item)) (car (value elem)))
                                             (setq task (cadr item))))
                                   (when task
                                     (print (list "Re-schedule" (car (value elem)) "from" (sch::om-task-timestamp task) "to" (offset elem)))
                                     (sch::reschedule-sequencer-task task (offset elem)))))
                      :readyp t
                      :object (car (value elem))
                      :timestamp (offset elem))))

;;;ADD COMPUTATION
(defmethod omNG-add-element ((self OMMaquette) elem)
  (call-next-method)
  (if (not sch::*om-sequencer-scheduler*) (sch::init-sequencer-scheduler))
  (print (list "Schedule" (value elem) "at" (offset elem)))
  (sch::schedule-sequencer-task
   (sch::make-om-task :id (sch::generate-task-id)
                      :event #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (sch::compute-tree sch::*treetest*))
                      :readyp t
                      :object (car (value elem))
                      :timestamp (offset elem))))

;;;ADD JUMP
(defmethod omNG-add-element ((self OMMaquette) elem)
  (call-next-method)
  (if (not sch::*om-sequencer-scheduler*) (sch::init-sequencer-scheduler))
  (print (list "Schedule" (value elem) "at" (offset elem)))
  (sch::schedule-sequencer-task
   (sch::make-om-task :id (sch::generate-task-id)
                      :event #'(lambda (self) 
                                 (print (format nil "STARTING TASK at ~A (delay of ~Ams)" 
                                                (sch::get-clock-time sch::*om-sequencer-scheduler*) 
                                                (- (sch::get-clock-time sch::*om-sequencer-scheduler*) (offset elem))))
                                 (push (list self 0 nil nil nil) *ms-list-to-play*)
                                 (player-start :midishare)
                                 (pop *ms-list-to-play*)
                                 (progn 
                                   (sch::om-jump-scheduler sch::*om-sequencer-scheduler* *jump-ms*)
                                   (setf (ref-clock-time *curplayer*) (sch::om-scheduler-ref-time sch::*om-sequencer-scheduler*))))
                      :readyp t
                      :object (car (value elem))
                      :timestamp (offset elem))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Re-schedule when drag
(defmethod make-move-after ((self MaquettePanel) dragged)
  (call-next-method)
  (setf *curmaq* self)
  (if (not sch::*om-sequencer-scheduler*) (sch::init-sequencer-scheduler))
  (let (task)
    (loop for item in sch::*om-sequencer-queue* do
          (if (eq (sch::om-task-object (cadr item)) (car (value (object (car dragged)))))
              (setq task (cadr item))))
    (when task
      (print (list "Re-schedule" (value (object (car dragged))) "from" (sch::om-task-timestamp task) "to" (offset (object (car dragged)))))
      (sch::reschedule-sequencer-task task (offset (object (car dragged)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Play/Stop
(defmethod editor-play/stop ((self maquetteeditor))
  (call-next-method)
  (if (eq (sch::om-scheduler-state sch::*om-sequencer-scheduler*) :stop)
      (sch::om-start-scheduler sch::*om-sequencer-scheduler*)
    (sch::om-stop-scheduler sch::*om-sequencer-scheduler*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Play that only starts cursor
(defmethod general-play ((player omplayer))
  (setq *curplayer* player)
  (let ((start-t (or (car (play-interval player)) 0))
        (end-t (or (cadr (play-interval player )) 3600000)))
    (cond ((equal (state player) :play)
           ;;; prolonge la durée de vie du player
           (setf (stop-time player) (max (stop-time player) end-t)))
        
          (t 
           (setf (stop-time player) end-t)
           (when (callback-process player)
             (om-kill-process (callback-process player)))
           (when (scheduling-process player)
             (om-kill-process (scheduling-process player)))
           
           (when (callback-fun player)
             (om-with-priority 10
               (setf (callback-process player)
                     (om-run-process "editor player callback"
                                     #'(lambda ()
                                         (loop 
                                          (funcall (callback-fun player) (caller player) (get-player-time player))
                                          (sleep (callback-tick player))
                                          ))))))
           (when (loop-play player) 
             (mapcar #'(lambda (pl) (player-set-loop pl start-t end-t)) 
                     (engines player)))
           (mapcar #'player-start (engines player) 
                   (mapcar #'(lambda (engine) (get-my-play-list engine (play-list player))) (engines player)))
           (setf (state player) :play
                 (start-time player) start-t
                 (ref-clock-time player) (clock-time))))))

;;;Init/Abort sequencer scheduler
;(sch::init-sequencer-scheduler)
;(sch::abort-sequencer-scheduler) 

;;;Change the scheduler type
;(setq sch::*om-sequencer-scheduler-type* sch::SCH_ASYNCHRONOUS)
;(setq sch::*om-sequencer-scheduler-type* sch::SCH_SYNCHRONOUS)

;;;Transport functions
;(sch::om-start-scheduler sch::*om-sequencer-scheduler*)
;(sch::om-stop-scheduler sch::*om-sequencer-scheduler*)
;(sch::om-pause-scheduler sch::*om-sequencer-scheduler*)
;(sch::om-continue-scheduler sch::*om-sequencer-scheduler*)

;;;Make the sequencer queue empty
;(setq sch::*om-sequencer-queue* nil)

(defmethod put-boxes-inmaquette ((self OMMaquette) time objs &optional connections)
   "Called by the evaluation of a omboxmaquette."
   (when objs
     (let* ((time-list (list! time))
            (time-list (corrige-metric-info self time-list))
            (objlist (list! objs))
            (deftime (default-from-list time-list))
            (facty (round 100 (length objlist)))
            (i 0) (old-time 0))
       (setf (boxes self) nil)
       (loop for item in objlist do
             (let* ((posi (if time-list (pop time-list) (+ old-time deftime))) newtemp)
               (if (boxtempobj-p item)
                   (progn (setf newtemp (clone item)) (incf i))
                 (progn
                   (setf newtemp (omNG-make-tempobj (clone item) (om-make-point 0 0)  (string+ "tempobj" (format () "~D" (incf i)))))
                   (setf (slot-value newtemp 'sizey) facty)
                   (setf (slot-value newtemp 'posy)  (* i facty))))
               (setf (slot-value newtemp 'offset) posi)
               (when newtemp
                 (omNG-add-element self newtemp))
               (setf old-time posi)))
       (setf (connec self) connections)
       (remk-connections (reverse (boxes self)) connections)
       )))

(defmethod omNG-box-value ((self OMBoxMaquette) &optional (num-out 0))
  (handler-bind ((error #'(lambda (c) 
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                          (om-report-condition c ))
                                               :size (om-make-point 300 200))
                              (om-abort)))))
    (cond
     ((and (equal (allow-lock self) "x") (value self))
      ;(setf (value self) (cons-copy-maquette-object (reference self) (boxes (reference self))))
      (value self))
     ((equal (allow-lock self) "o") (reference self))
     ((and (equal (allow-lock self) "&") (ev-once-p self)) (value self))
     (t (let* ((args  (mapcar #'(lambda (input)
                                  (omNG-box-value input)) (inputs self)))
               rep)
          (when (and (= 0 (mode self)) (third args))
            (setf (metricparam (params (reference self))) (third args)))
          (when (editorFrame (reference self))
            (om-close-window (window (editorFrame (reference self)))))
                
          (if (= (mode self) 0)
              (put-boxes-inmaquette (reference self) (first args) (second args) (nth 3 args)))

          (cons-maq-values (reference self) args)
                          
          (setf rep (cons-copy-maquette-object (reference self) (boxes (reference self))))
          (when (= (mode self) 1)
            (setf rep (append (list rep) 
                              (loop for out in (sort (find-class-boxes (boxes (reference self)) 'maq-OMout)  '< :key 'indice) 
                                    collect (if (connected? (car (inputs out)))
                                                (let ((con-obj (car (connected? (car (inputs out)))))
                                                      (num (cadr (connected? (car (inputs out))))))
                                                 ;(print (list (value con-obj) num))
                                                  (if (listp (value con-obj))
                                                      (nth (+ num 1) (value con-obj))
                                                    (value con-obj))
                                                  )
                                              (value (car (inputs out))))
                                    )))
            )
           
          (when (equal (allow-lock self) "&")
            (setf (ev-once-p self) t)
            (setf (value self) rep))
          (when (equal (allow-lock self) "x")
            (setf (value self) rep))
          (if (= (mode self) 1)
              (nth num-out rep)
            rep)
          )))))


(defmethod make-maq-tempobj ((self MaquettePanel) x y)
   "Add a new empty temporal maq to 'self'. This method is called when you make ALT+CLICK+DRAG in 'self'."
   (let* (;(class (get-absmaqclass))
          (thename (mk-unique-name self (get-maq-obj-name class)))
          (new-patch (make-instance class 
                                 :name thename :icon 265))
          (pixsizex (max 20 (- (om-point-h y) (om-point-h x))))
          (pixsizey (max 10 (- (om-point-v y) (om-point-v x))))
          (maqpos (get-offset/posy-from-pixel self  (om-make-point (om-point-h x) (om-point-v x))))
          (y-size (pixel2norme self 'y  pixsizey))
          (tempobj (omNG-make-tempobj new-patch maqpos thename))
          new-frame)
     (setf (slot-value tempobj 'extend) (pixel2norme  self 'x pixsizex))
     (setf (slot-value tempobj 'sizey)  y-size)
     (setf new-frame (make-frame-from-callobj tempobj))
     (omG-add-element self new-frame)))

;(omG-add-element self (make-frame-from-callobj tempobj))

;(make-maq-tempobj *curmaq* (om-make-point 10000 -6) (om-make-point 11000 -8))





(defmethod om-draw-contents ((self tempobjframe))
   (om-with-focused-view self
     (if (zerop (extend (object self)))
         (om-draw-picture self (get-impulsion-pict (car (value (object self)))) (om-make-point 0 0) (om-make-point (w self) (h self)))
       (if (showpict (object self))
           (progn
             (if *minipict-bg* 
                 (om-with-fg-color self (if (equal *minipict-bg* :white) *om-white-color* (colorframe (object self)))
                   (om-fill-rect 0 0 (w self) (h self))))
             (draw-editor-mode (get-obj-to-draw (object self)) self)
             )
         (let* ((durinx (norme2pixel (om-view-container self) 'x (extend (object self)))))
           (cond ((has-picture-p self)
                  (om-draw-picture self (thepict (pictu (object self))) (om-make-point 0 0) (om-make-point (w self) (h self)))
                  (draw-carre self t))
                 ((and (pictu (object self)) (pict-pathname (pictu (object self))))
                  (draw-lost-picture (pictu (object self)) self)
                  (draw-carre self t))
                 (t
                  (om-with-fg-color self (colorframe (object self))
                    (draw-frame self (object self) 0 0  durinx (h self)))
                  (if (active-mode self)
                      (draw-carre self t)
                    (draw-frame-shadows self))))
           )))
     (when (show-name (object self))
       (om-with-fg-color self *om-dark-gray-color*
       (om-with-font *om-default-font1b*
                     (om-draw-string 4 (- (h self) 5) (number-to-string (free-store (object self)))))))
     )
     (when (mute (object self)) 
       (let ((iconparams (get&corrige-icon 341))
             (size (om-make-point 16 16)))       ;;(om-make-point (round (w self) 4) (round (h self) 4))))
         (om-draw-icon (second iconparams) self (om-make-point 
                                               (- (round (w self) 2) (round (om-point-h size) 2)) 
                                               (- (round (h self) 2) (om-point-v size)))
                                               size)
                     ))
   (when (lock (object self)) 
     (let ((iconparams (get&corrige-icon 340))
           (size (om-make-point 16 16)))       ;;(om-make-point (round (w self) 4) (round (h self) 4))))
       (om-draw-icon (second iconparams) self (om-make-point 
                                               (- (round (w self) 2) (round (om-point-h size) 2)) 
                                               (round (h self) 2))
                                               size)
                     ))
   
   (when *maq-show-icons*
     (let ((iconparams (get&corrige-icon (icon (reference (object self)))))
           (size (om-make-point 10 10)))
       (om-draw-icon (second iconparams) self (om-make-point (- (w self) 16) 2) size)))

  (when (show-con? self)
    (call-next-method)))
