(in-package :om)

;original :
(setq *scenario-original* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4)))

(setq *scenario* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (c maj7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (b 7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *database-path* "/Users/bouche/Documents/OpenMusic/OM/OPENMUSIC/code/projects/12-omnika/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or")

(defun init-testnika ()
  (setq *scenario* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (c maj7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (b 7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
        *database-path* "/Users/bouche/Documents/OpenMusic/OM/OPENMUSIC/code/projects/12-omnika/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
        *beat-dur* 330
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
   ;(process :initform nil :accessor process :type (or null mp::process))
   (scenario :initform nil :accessor scenario :initarg :scenario :type list)
   (db-path :initform nil :accessor db-path :initarg :db-path :type (or null string))
   (beat-dur :initform 100 :accessor beat-dur :initarg :beat-dur :type integer)
   (epsilon :initform 3 :accessor epsilon :initarg :epsilon :type integer)
  ;(beat-trace :initform nil :type list)
   (index :initform 0 :accessor index :type integer)
   (rtimprovizer :initform nil :accessor rtimprovizer)
   (alarm :initform nil :accessor alarm :type boolean)
   (playlist :initform nil :accessor playlist :type list)
   (player-scheduler :initform nil :accessor player-scheduler :type (or null sch::scheduler))
   (play-pos :initform 0 :accessor play-pos :type integer)
   (empty-pos :initform 0 :accessor empty-pos :type integer))
  (:documentation "impro-handler"))

(defun build-impro-handler (&key name scenario db-path beat-dur epsilon)
  (let ((handler (make-instance 'impro-handler 
                                :name (or name "Improvizer-Handler")
                                :scenario scenario
                                :db-path db-path
                                :beat-dur (or beat-dur 100)
                                :epsilon (or epsilon 3))))
    (setf (rtimprovizer handler) (load-realtimeImprovizer-fromSavedImprovizer (db-path handler)))
    (setf (player-scheduler handler) 
          (sch:make-scheduler :name "Improvizator"))
    handler))

(defmethod proceed-impro-handler ((self impro-handler))
  (let* ((index (index self))
         (scenario-suffix (nthcdr index (expand_grid (scenario self))))
         result-beats-list result-length)
    (setq result-beats-list (improvize-loop-next-factor (rtimprovizer self)
                                                        scenario-suffix
                                                        (beat-dur self) 
                                                        index)
          result-length (length result-beats-list))    
    (setf (playlist self) (append (nthcar index (playlist self)) result-beats-list (nthcdr (+ index result-length) (playlist self)))
          (empty-pos self) (length (playlist self))
          (index self) (+ index result-length))
    (when (sch::scheduler-queue (player-scheduler self))
      (let ((indx (round (caar (last (sch::scheduler-queue (player-scheduler self)))) (beat-dur self))))
        (nconc (sch::scheduler-queue (player-scheduler self))
               (loop for elem in (nthcdr indx (playlist self))
                     for i from indx collect
                     (list (* (beat-dur self) i) 
                           #'(lambda (chseq handler) (play-chord-seq chseq) (incf (play-pos handler)))
                           (list (beats->chseq (list elem) (beat-dur self) 0) self))))))))

#|
(progn 
  (setq *test-impro-handler* (build-impro-handler :name "TestNika" :scenario *scenario* :db-path *database-path* :beat-dur 330))
  (play-impro-handler *test-impro-handler*))
;(stop-impro-handler *test-impro-handler*)

;(car (last (sch::scheduler-queue (player-scheduler *test-impro-handler*))))
|#



(defmethod (setf play-pos) (new-play-pos (handler impro-handler))
  (setf (slot-value handler 'play-pos) new-play-pos)
  (if (< (- (empty-pos handler) new-play-pos) (epsilon handler))
      (progn
        (setf (index handler) (empty-pos handler))
        (proceed-impro-handler handler))))

(defmethod (setf scenario) (new-scenario (handler impro-handler))
  (let ((switch-pos (position nil (mapcar 'equal (expand_grid (scenario handler)) (expand_grid new-scenario)))))
    (setf (slot-value handler 'scenario) new-scenario
          (slot-value handler 'index) switch-pos)
    (proceed-impro-handler handler)))

(defmethod play-impro-handler ((self impro-handler) &optional (start-pos 0))
  (let* ((i 0)
         (beat (nth i (playlist self)))
         (dur (beat-dur self)))
  (loop while (and (<= (empty-pos self) start-pos) (= (length (playlist self)) 0)) do
        (proceed-impro-handler self))
  (setf (sch::scheduler-queue (player-scheduler self))
        (loop for elem in (playlist self) 
              for i from 0 collect 
              (list (* dur i) 
                    #'(lambda (chseq handler) (play-chord-seq chseq) (incf (play-pos handler)))
                    (list (beats->chseq (list elem) dur 0) self))))
  (sch:start-scheduler (player-scheduler self))
  
  ))

(defmethod stop-impro-handler ((self impro-handler))
  (mp:process-stop (player-process self)))

;(setf (player-process self) 
;        (mp:process-run-function "Improvizator" nil #'(lambda () 
;                                                        (let* ((i 0)
;                                                               (beat (nth i (playlist self)))
;                                                               (dur (/ (beat-dur self) 1000.0)))
;                                                          (loop while beat do
;                                                                (play-chord-seq (beats->chseq (list beat) (beat-dur self) 0))
;                                                                (setq beat (nth (setf (play-pos self) (incf i)) (playlist self)))
;                                                                (mp:process-wait-with-timeout "Zzzz" dur))))))

(defmethod play-chord-seq ((self chord-seq))
  (progn
    (push (list self 0 nil nil nil) *ms-list-to-play*)
    (player-start :midishare)
    (pop *ms-list-to-play*)))




;(nth 0 (playlist *test-impro-handler*))
;(length (playlist *test-impro-handler*))
;(setf (scenario *test-impro-handler*) *scenario-original*)
;(play-chord-seq (beats->chseq (list beat) (beat-dur self) 0)


;(proceed-impro-handler *test-impro-handler*)
;(length (playlist *test-impro-handler*))
;(setf (index *test-impro-handler*) 32)
;(setf (play-pos *test-impro-handler*) 92)

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


