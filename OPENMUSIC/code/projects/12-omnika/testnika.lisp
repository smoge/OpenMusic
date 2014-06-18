(in-package :om)

;original :
(setq *scenario-original* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4)))

(setq *scenario* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (c maj7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (b 7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *database-path* "/Users/bouche/Documents/OpenMusic/OM/OPENMUSIC/code/projects/12-omnika/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or")

(defun init-testnika ()
  (setq *beat-dur* 330
        *start-indx* 0
        *rtimprovizer* (load-realtimeImprovizer-fromSavedImprovizer *database-path*)))

(defun run-testnika ()
  (setq *scenario-suffix* (nthcdr *start-indx* (expand_grid *scenario*))
        *result-beats* (improvize-loop-next-factor *rtimprovizer* *scenario-suffix* *beat-dur* *start-indx*)
        *result-chord-seq* (beats->chseq *result-beats* *beat-dur* 0))
  
  (incf *start-indx* (length *result-beats*)))

 
(defun play-test-nika ()
  (progn
    (push (list *result-chord-seq* 0 nil nil nil) *ms-list-to-play*)
    (player-start :midishare)
    (pop *ms-list-to-play*)))

(defun stop-testnika ()
  (player-stop :midishare))

;(init-testnika)
;(run-testnika)
;(play-test-nika)
;(stop-testnika)

;(midi-send-evt (caaar (preparetoplay :midi (beats->chseq *result-beats* *beat-dur* 0) 0)))
;(midi-send-evt (cadaar (preparetoplay :midi (beats->chseq *result-beats* *beat-dur* 0) 0)))

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

(loop for evt in '(1 2 3)
        for i from 330 by 330 collect
        (list evt 
              i))

;(let () (multiple-value-bind (lmidic lonset) (beats->mididata *result-beats* *beat-dur* 0) (print (length lmidic)) (print (length lonset))))

#|
But :
- savoir à partir de quelle date on aura plus rien à jouer
- 1ere date vide : beat d'index T
- Date de leture courante : index C

- lancer calcul avec startindx=T si :
   - T-C<=threshold
- lancer calcul avec startindx=C+Eps si :
   - Scénario modifié
   - Paramètre de génération change.
|#



;;;FAIRE EN CLASSE

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
   (epsilon :initform 4 :accessor epsilon :initarg :epsilon :type integer)
   (player-scheduler :initform nil :accessor player-scheduler :type (or null sch::scheduler))
   (play-pos :initform 0 :accessor play-pos :type integer))
  (:documentation "impro-handler"))

(defun build-impro-handler (&key name scenario db-path beat-dur epsilon)
  (let ((handler (make-instance 'impro-handler 
                                :name (or name "Improvizer-Handler")
                                :scenario scenario
                                :db-path db-path
                                :beat-dur (or beat-dur 100)
                                :epsilon (or epsilon 4))))
    (setf (rtimprovizer handler) (load-realtimeImprovizer-fromSavedImprovizer (db-path handler))
          (beat-max handler) (length (expand_grid scenario)))
    (setf (player-scheduler handler) 
          (sch:make-scheduler :name "Improvizator" :tick 0.001))
    handler))


(defmethod proceed-impro-handler ((self impro-handler)) (print "PROCEED")
  (let* ((beat-index (beat-index self))
         (scenario-suffix (nthcdr beat-index (expand_grid (scenario self))))
         result-beats-list result-schedlist result-length)
    (when (< beat-index (beat-max self))
      (setq result-beats-list (improvize-loop-next-factor (rtimprovizer self)
                                                          scenario-suffix
                                                          (beat-dur self) 
                                                          beat-index)
            result-length (length result-beats-list)) 
      (setf (beat-list self) (append (nthcar beat-index (beat-list self)) result-beats-list) ;(nthcdr (+ beat-index result-length) (beat-list self)))
            (empty-pos self) (length (beat-list self))
            (beat-index self) (+ beat-index result-length))
      (setq result-schedlist (midi->schedlist (beats->midi result-beats-list (beat-dur self) 0 (* beat-index (beat-dur self))) self))
      (when (om-get-scheduler-queue (player-scheduler self))
        (mp:with-lock ((om-get-scheduler-lock (player-scheduler self)))
          (let ((indx (or (position (caar result-schedlist) (sch::scheduler-queue (player-scheduler self)) :test '<= :key 'car) 
                          (length (sch::scheduler-queue (player-scheduler self))))))
            (setf (sch::scheduler-queue (player-scheduler self)) (append (nthcar indx (sch::scheduler-queue (player-scheduler self))) result-schedlist))))))
    t))

#|
(progn 
  (setq *test-impro-handler* (build-impro-handler :name "TestNika" :scenario *scenario* :db-path *database-path* :beat-dur 330))
  (play-impro-handler *test-impro-handler*))
(stop-impro-handler *test-impro-handler*)

(car (last (sch::scheduler-queue (player-scheduler *test-impro-handler*))))
(beat-pos *test-impro-handler*)
(empty-pos *test-impro-handler*)
(last (beats->midi (beat-list *test-impro-handler*) 330 0 0))
(length (om-inspect (nth 42 (beat-list *test-impro-handler*)))
(setf (scenario *test-impro-handler*) *scenario-original*)
(play-chord-seq (beats->chseq (list beat) (beat-dur self) 0)
|#

(defmethod (setf play-pos) (new-play-pos (handler impro-handler))
  (setf (slot-value handler 'play-pos) new-play-pos))

(defmethod (setf beat-pos) (new-beat-pos (handler impro-handler))
  (setf (slot-value handler 'beat-pos) new-beat-pos)
  (if (and (< (- (empty-pos handler) new-beat-pos) (epsilon handler))
           (< (empty-pos handler) (beat-max handler)))
      (progn
        (setf (beat-index handler) (empty-pos handler))
        (proceed-impro-handler handler))))

(defmethod (setf scenario) (new-scenario (handler impro-handler))
  (let ((switch-pos (or (position nil (mapcar 'equal (expand_grid (scenario handler)) (expand_grid new-scenario))) (length (expand_grid new-scenario)))))
    (setf (slot-value handler 'scenario) new-scenario
          (slot-value handler 'beat-index) switch-pos
          (slot-value handler 'beat-max) (length (expand_grid new-scenario)))
    (proceed-impro-handler handler)))




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

;(setf (player-process self) 
;        (mp:process-run-function "Improvizator" nil #'(lambda () 
;                                                        (let* ((i 0)
;                                                               (beat (nth i (beat-list self)))
;                                                               (dur (/ (beat-dur self) 1000.0)))
;                                                          (loop while beat do
;                                                                (play-chord-seq (beats->chseq (list beat) (beat-dur self) 0))
;                                                                (setq beat (nth (setf (play-pos self) (incf i)) (beat-list self)))
;                                                                (mp:process-wait-with-timeout "Zzzz" dur))))))

(defmethod play-chord-seq ((self chord-seq))
  (progn
    (push (list self 0 nil nil nil) *ms-list-to-play*)
    (player-start :midishare)
    (pop *ms-list-to-play*)))



;(proceed-impro-handler *test-impro-handler*)
;(length (beat-list *test-impro-handler*))
;(setf (beat-index *test-impro-handler*) 32)
;(setf (beat-pos *test-impro-handler*) 92)

(defmethod compare-scenario ((l1 list) (l2 list))
  (let* ((ln1 (length l1))
         (ln2 (length l2))
         (lnmax (max ln1 ln2))
         (i 0)
         (same t))
    (loop for elem1 in l1
          for elem2 in l2 do
          (setq same (and same (equal elem1 elem2)))
          (incf i))
    (if (= i lnmax) same)))






  

;thread-beats pour clean les onset
;beat-dur*beat-n + onset


