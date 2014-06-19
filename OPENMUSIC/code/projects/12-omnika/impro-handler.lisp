(in-package :om)


;;;=================================================================================;;;
;;;====================================SCENARIOS====================================;;;
;;;=================================================================================;;;
(setq *scenario-original* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                            (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                            (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4)
                            (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *scenario-degeu* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) 
                         (bb maj7 4) (eb maj7 4) (c maj7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) 
                         (c m7 4) (f 7 4) (bb maj7 4) (b 7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *scenario-subst1* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                        (c m7 4) (f 7 4) (b m7 2) (e 7 2) (bb m7 2) (eb 7 2) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                        (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (b m7 4) (e 7 4)
                        (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (a m7 2) (d 7 2) (g m7 4) (g m7 4))
      *scenario-subst2* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                        (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                        (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4)
                        (a m7 4) (d 7 4) (g 7 2) (gb 7 2) (f 7 2) (e 7 2) (eb 7 4) (d 7 4) (g m7 4) (g m7 4)))

;;;=================================================================================;;;
;;;====================================DATABASES====================================;;;
;;;=================================================================================;;;
(setq *db-path-solo1* "/Users/bouche/Documents/OpenMusic/OM/OPENMUSIC/code/projects/12-omnika/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
      *db-path-accomp1* "/Users/bouche/Documents/OpenMusic/OM/OPENMUSIC/code/projects/12-omnika/AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or")

;;;===================================================================================================;;;
;;;====================================BEATS TRANSLATION FUNCTIONS====================================;;;
;;;===================================================================================================;;;
(defun beats->midi (beatlist refbeatvalue deltachords start-time)
  (flat (preparetoplay :midi (beats->chseq beatlist refbeatvalue deltachords) start-time)))

(defun midi->schedlist (midi-evt-list handler)
  (let* ((l1 (loop for evt in midi-evt-list collect
                   (list (om-midi::midi-evt-date evt) 
                         'midi-send-handler-evt (list evt handler))))
         (dur (beat-dur handler))
         (tmin (* (ceiling (caar l1) dur) dur))
         (tmax (* (ceiling (caar (last l1)) dur) dur))
         (tlist (loop for i from tmin to tmax by dur collect 
                      (let ((time i))
                        (list time #'(lambda (hnd) (setf (beat-pos hnd) (floor time dur))) (list handler))))))
    (sort (append l1 tlist) '< :key #'car)))

(defun midi-send-handler-evt (evt handler)
  (midi-send-evt evt)
  (incf (play-pos handler)))

;;;================================================================================================;;;
;;;====================================IMPRO HANDLER DE LA MORT====================================;;;
;;;================================================================================================;;;
(defclass impro-handler ()
  ((name :initform "Improvizer-Handler" :accessor name :initarg :name :type string)
   ;;;Improvizer
   (rtimprovizer :initform nil :accessor rtimprovizer)
   (scenario :initform nil :accessor scenario :initarg :scenario :type list)
   (db-path :initform nil :accessor db-path :initarg :db-path :type (or null string))
   (beat-dur :initform 100 :accessor beat-dur :initarg :beat-dur :type integer)
   (beat-list :initform nil :accessor beat-list :type list)
   (beat-index :initform 0 :accessor beat-index :type integer)
   (beat-max :initform 0 :accessor beat-max :type integer)
   (beat-pos :initform 0 :accessor beat-pos :type integer)
   (empty-pos :initform 0 :accessor empty-pos :type integer)
   ;;;Handler
   (epsilon :initform 3 :accessor epsilon :initarg :epsilon :type integer)
   (player-scheduler :initform nil :accessor player-scheduler :type (or null sch::scheduler))
   (play-pos :initform 0 :accessor play-pos :type integer))
  (:documentation "impro-handler"))

(defun build-impro-handler (&key name scenario db-path beat-dur epsilon)
  (let ((handler (make-instance 'impro-handler 
                                :name (or name "Improvizer-Handler")
                                :scenario scenario
                                :db-path db-path
                                :beat-dur (or beat-dur 100)
                                :epsilon (or epsilon 3))))
    (setf (rtimprovizer handler) (load-realtimeImprovizer-fromSavedImprovizer (db-path handler))
          (beat-max handler) (length (expand_grid scenario)))
    (setf (player-scheduler handler) 
          (sch:make-scheduler :name "Improvizator" :tick 0.001))
    handler))


(defmethod proceed-impro-handler ((self impro-handler))
  (let* ((beat-index (beat-index self))
         (scenario-suffix (nthcdr beat-index (expand_grid (scenario self))))
         result-beats-list result-schedlist result-length tmpchseq)
    (when (< beat-index (beat-max self))
      (setq result-beats-list (improvize-loop-next-factor (rtimprovizer self)
                                                          scenario-suffix
                                                          (beat-dur self) 
                                                          beat-index)
            result-length (length result-beats-list)) 
      (setf (beat-list self) (append (nthcar beat-index (beat-list self)) result-beats-list) ;(nthcdr (+ beat-index result-length) (beat-list self)))
            (empty-pos self) (length (beat-list self))
            (beat-index self) (+ beat-index result-length))

      (when (eq self *test-solo-handler*)
          (setq tmpchseq (beats->chseq (beat-list self) (beat-dur self) 0))
          (om-send tmpchseq :solo))
      (when (eq self *test-accomp-handler*)
          (setq tmpchseq (beats->chseq (beat-list self) (beat-dur self) 0))
          (om-send tmpchseq :accomp))
      
      (when result-beats-list
        (setq result-schedlist (midi->schedlist (beats->midi result-beats-list (beat-dur self) 0 (* beat-index (beat-dur self))) self))
        (if (om-get-scheduler-queue (player-scheduler self))
            (mp:with-lock ((om-get-scheduler-lock (player-scheduler self)))
              (let* ((queue (om-get-scheduler-queue (player-scheduler self)))
                     (indx (or (position (caar result-schedlist) queue :test '<= :key 'car) (length queue)))
                     (queue-prefix (nthcar indx queue))
                     (queue-suffix (nthcdr indx queue))
                     evt notes-off)
                (loop for elem in queue-suffix do
                      (setq evt (caar (last elem)))
                      (if (and (eq (type-of evt) 'om-midi::midi-evt) (eq (om-midi:midi-evt-type evt) :keyoff))
                          (push elem notes-off)))
                (setq result-schedlist (sort (append result-schedlist notes-off) '< :key #'car))
                (om-set-scheduler-queue (player-scheduler self) (append (nthcar indx queue) result-schedlist))))
          (om-set-scheduler-queue (player-scheduler self) result-schedlist)))) t))

(defmethod (setf beat-pos) (new-beat-pos (handler impro-handler)) 
  ;(print (list "Beat-pos n°" new-beat-pos "/" (empty-pos handler)))
  (setf (slot-value handler 'beat-pos) new-beat-pos)
  (if (and (< (- (empty-pos handler) new-beat-pos) (epsilon handler))
           (< (empty-pos handler) (beat-max handler)))
      (progn
        (setf (beat-index handler) (empty-pos handler))
        (proceed-impro-handler handler))))

(defmethod (setf scenario) (new-scenario (handler impro-handler))
  (let* ((pos (beat-pos handler))
         ;(eps (epsilon handler))
         (switch-pos (or (position nil (mapcar 'equal (nthcdr pos (expand_grid (scenario handler))) (nthcdr pos (expand_grid new-scenario)))) -1)))
    (when (> switch-pos 0)
      (incf switch-pos pos)
      (setf (slot-value handler 'scenario) new-scenario
            (slot-value handler 'beat-index) switch-pos
            (slot-value handler 'beat-max) (length (expand_grid new-scenario)))
      (proceed-impro-handler handler))))

(defmethod init-impro-handler ((self impro-handler) &optional (start-pos 0))
  (let* ((i 0)
         (beat (nth i (beat-list self)))
         (dur (beat-dur self)))
  (loop while (and (<= (empty-pos self) start-pos) 1) do
        (proceed-impro-handler self))
  (om-set-scheduler-queue (player-scheduler self)
                          (midi->schedlist (beats->midi (beat-list self) dur 0 (* start-pos dur)) self))
  (player-scheduler self)))

(defmethod play-impro-handler ((self impro-handler) &optional (start-pos 0))
  (let* ((i 0)
         (beat (nth i (beat-list self)))
         (dur (beat-dur self)))
  (loop while (and (<= (empty-pos self) start-pos) 1) do
        (proceed-impro-handler self))
  (om-set-scheduler-queue (player-scheduler self)
                          (midi->schedlist (beats->midi (beat-list self) dur 0 (* start-pos dur)) self))
  (om-start-scheduler (player-scheduler self))))

(defmethod stop-impro-handler ((self impro-handler))
  (om-stop-scheduler (player-scheduler self)))


(defmethod play-chord-seq ((self chord-seq))
  (progn
    (push (list self 0 nil nil nil) *ms-list-to-play*)
    (player-start :midishare)
    (pop *ms-list-to-play*)))

#|
(when (eq self *test-solo-handler*)
          (setq tmpchseq (beats->chseq (beat-list self) (beat-dur self) 0))
          (loop for slot in (hcl:class-slots 'chord-seq)
                for slot-name = (slot-definite-name slot)
                for slot-value = (slot-value object slot-name) do
                (setf (slot-value *display-chord-seq* slot-name) slot-value)
                )
          (eval *display-chord-seq*))


;;;Démarrer un handler de solo + un handler d'accompagnement :
(progn 
  (setq *test-solo-handler* (build-impro-handler :name "TestNika" :scenario *scenario-original* :db-path *db-path-solo1* :beat-dur 330))
  (setq *test-accomp-handler* (build-impro-handler :name "TestNika1" :scenario *scenario-original* :db-path *db-path-accomp1* :beat-dur 330))
  (setq *test-solo-handler-scheduler* (init-impro-handler *test-solo-handler*))
  (setq *test-accomp-handler-scheduler* (init-impro-handler *test-accomp-handler*))
  (om-start-multiple-scheduler (list *test-solo-handler-scheduler* *test-accomp-handler-scheduler*)))

(setf (lmidic *display-chord-seq*) (list 6500 6800))


;;;Solo only
(progn
  (setq *test-solo-handler* (build-impro-handler :name "TestNika" :scenario *scenario-original* :db-path *db-path-solo1* :beat-dur 330))
  (setq *test-solo-handler-scheduler* (init-impro-handler *test-solo-handler*))
  (om-start-scheduler *test-solo-handler-scheduler*))

;;;Stopper ces mêmes bouzins :
(progn
  (stop-impro-handler *test-solo-handler*)
  (stop-impro-handler *test-accomp-handler*))

;;;Lancer un process qui s'amuse à changer de scénario du solo toutes les secondes, l'insolent. Bien penser à le tuer sinon il s'arrête jamais le con!
(setq *scenario-switcher* (mp:process-run-function "change scenar" nil #'(lambda (hnd)
                                                                           (loop
                                                                            (setf (scenario hnd) *scenario-subst1*)
                                                                            (mp::process-wait-with-timeout "jesuisfou" 1)
                                                                            (setf (scenario hnd) *scenario-subst2*)
                                                                            (mp::process-wait-with-timeout "jesuisfou" 1))) 
                                                   *test-solo-handler*))
(mp:process-kill *scenario-switcher*)
|#

;thread-beats pour clean les onset

 
