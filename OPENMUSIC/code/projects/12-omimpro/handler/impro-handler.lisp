(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Class
(defclass impro-handler ()
  ((name :initform "Improvizer-Handler" :accessor name :initarg :name :type string)
   ;;;Improvizer
   (rtimprovizer :initform nil :accessor rtimprovizer)
   (scenario :initform nil :accessor scenario :initarg :scenario :type list)
   (expanded-scenario :initform nil :accessor expanded-scenario :initarg :expanded-scenario :type list)
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
  (:documentation "
A handler for Improtek (Copyright 2013 (C) J.Nika).
This object can automate improvization generation based on the rtimprovizer class from Improtek."))

;;;Build an improvizer from a scenario and a database
(defun build-impro-handler (&key name scenario db-path beat-dur epsilon)
  (let ((handler (make-instance 'impro-handler 
                                :name (or name "Improvizer-Handler")
                                :scenario scenario
                                :expanded-scenario (expand_grid scenario)
                                :db-path db-path
                                :beat-dur (or beat-dur 330)
                                :epsilon (or epsilon 3))))
    (setf (rtimprovizer handler) (load-realtimeImprovizer-fromSavedImprovizer (db-path handler))
          (beat-max handler) (length (expanded-scenario handler))
          (player-scheduler handler) (sch:make-scheduler :name (format nil "~A player" (or name "Improvizer-Handler")) :tick 0.001))
    handler))

;;;Reactive set method for the beat-pos
(defmethod (setf beat-pos) (new-beat-pos (handler impro-handler)) 
  (setf (slot-value handler 'beat-pos) new-beat-pos)
  (when (and (< (- (empty-pos handler) new-beat-pos) (epsilon handler))
             (< (empty-pos handler) (beat-max handler)))
    (setf (beat-index handler) (empty-pos handler))
    (proceed-impro-handler handler)))

;;;Reactive set method for the scenario
(defmethod (setf scenario) (new-scenario (handler impro-handler))
  (let* ((new-expanded-scenario (expand_grid new-scenario))
         (pos (beat-pos handler))
         (switch-pos (position nil (mapcar 'equal (nthcdr pos (expanded-scenario handler)) (nthcdr pos new-expanded-scenario)))))
    (when switch-pos
      (incf switch-pos pos)
      (setf (slot-value handler 'scenario) new-scenario
            (slot-value handler 'expanded-scenario) new-expanded-scenario
            (slot-value handler 'beat-index) switch-pos
            (slot-value handler 'beat-max) (length new-expanded-scenario))
      (proceed-impro-handler handler))))

;;;Run the first generation step
(defmethod init-impro-handler ((self impro-handler) &optional (start-pos 0))
  (let* ((bdur (beat-dur self))
         (start-time (* start-pos bdur)))
    (loop while (and (<= (empty-pos self) start-pos) 1) do
          (proceed-impro-handler self))
    (om-set-scheduler-queue (player-scheduler self)
                            (midi->schedlist (beats->midi (beat-list self) bdur 0 start-time) self))
    (player-scheduler self)))
 

;;;Run one generation step
(defmethod proceed-impro-handler ((self impro-handler))
  (let* ((beat-index (beat-index self))
         (scenario-suffix (nthcdr beat-index (expanded-scenario self)))
         (bdur (beat-dur self))
         result-beats-list result-schedlist result-length tmp-chseq queue)
    ;;;When improvization is not over
    (when (< beat-index (beat-max self))
      ;;;Run improvization as far as possible
      (setq result-beats-list (improvize-loop-next-factor (rtimprovizer self)
                                                          scenario-suffix
                                                          bdur 
                                                          beat-index)
            result-length (length result-beats-list))
      ;;;Stretch the result beat list to the desired tempo
      (loop for bt in result-beats-list do
            (setf (MidiSet bt) (timestretch (MidiSet bt) (/ bdur (duration bt)))
                  (duration bt) bdur))
      ;;;Add the generated beat list to the handler beat-list, set the new empty position and the next generation index
      (setf (beat-list self) (append (nthcar beat-index (beat-list self)) result-beats-list)
            (empty-pos self) (length (beat-list self))
            (beat-index self) (+ beat-index result-length))
      ;;;Display hack, only for Jerome's demo
      (setq tmp-chseq (beats->chseq (beat-list self) bdur 0))
      (when (eq self *test-solo-handler*)
        (copy-chseq-data tmp-chseq *solo-chord*))
      (when (eq self *test-accomp-handler*)
        (copy-chseq-data tmp-chseq *accomp-chord*))
      ;;;When the generation gave a non-null result
      (when result-beats-list
        ;;;Turn the result beat list into a scheduling list
        (when (setq result-schedlist (midi->schedlist (beats->midi result-beats-list bdur 0 (* beat-index bdur)) self))
          ;;;Set the player-scheduler queue accrding to new values
          (mp:with-lock ((om-get-scheduler-lock (player-scheduler self)))
            ;;;If the player scheduler has a non-null queue, cut the non-processed queue at the right place, keeping only the note-off midi events, and mix with the new queue + sort.
            (if (setq queue (om-get-scheduler-queue (player-scheduler self)))
                (let* ((indx (or (position (caar result-schedlist) queue :test '<= :key 'car) (length queue)))
                       (queue-prefix (nthcar indx queue))
                       (queue-suffix (nthcdr indx queue))
                       evt notes-off)
                  (loop for elem in queue-suffix do
                        (setq evt (caar (last elem)))
                        (if (and (eq (type-of evt) 'om-midi::midi-evt) (eq (om-midi:midi-evt-type evt) :keyoff))
                            (push elem notes-off)))
                  (setq result-schedlist (sort (append result-schedlist notes-off) '< :key #'car))
                  (om-set-scheduler-queue (player-scheduler self) (append (nthcar indx queue) result-schedlist)))
              ;;;If the player scheduler has no queue, set it to the result queue
              (om-set-scheduler-queue (player-scheduler self) result-schedlist)))))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tools
;;;Translate a beat-list to a midi-evts-list
(defun beats->midi (beatlist refbeatvalue deltachords start-time)
  (flat (preparetoplay :midi (beats->chseq beatlist refbeatvalue deltachords) start-time)))

;;;Translate a midi-evts-list to a scheduler list (i.e. midi on/off events + set beat-pos events)
(defun midi->schedlist (midi-evt-list handler)
  (if midi-evt-list
      (let* ((l1 (loop for evt in midi-evt-list collect
                       (list (om-midi::midi-evt-date evt) 
                             'midi-send-handler-evt (list evt handler))))
             (dur (beat-dur handler))
             (tmin (* (ceiling (caar l1) dur) dur))
             (tmax (* (ceiling (caar (last l1)) dur) dur))
             (tlist (loop for i from tmin to tmax by dur collect 
                          (let ((time i))
                            (list time #'(lambda (hnd) (setf (beat-pos hnd) (floor time dur))) (list handler))))))
        (sort (append l1 tlist) '< :key #'car))))

;;;Function to play a midi event and increase the play position
(defun midi-send-handler-evt (evt handler)
  (midi-send-evt evt)
  (if (eq (om-midi:midi-evt-type evt) :keyon) (incf (play-pos handler))))

;;;Copy a chord-seq data to an other
(defmethod copy-chseq-data ((self chord-seq) cs)
  (let* ((class (class-of self))
         (slot-definitions (class-effective-slots class))
         (slot-names (mapcar 'slot-definition-name slot-definitions)))
    (dolist (slot slot-names)
      (when (not (eq 'associated-box slot))
        (setf (slot-value cs slot)
              (slot-value self slot))
        (notify-change cs)
        (refresh-maquette *display-maquette*)))
    t))

;;;Play an impro handler
(defmethod play-impro-handler ((self impro-handler) &optional (start-pos 0))
  (let* ((i 0)
         (beat (nth i (beat-list self)))
         (dur (beat-dur self)))
  (loop while (and (<= (empty-pos self) start-pos) 1) do
        (proceed-impro-handler self))
  (om-set-scheduler-queue (player-scheduler self)
                          (midi->schedlist (beats->midi (beat-list self) dur 0 (* start-pos dur)) self))
  (om-start-scheduler (player-scheduler self))))

;;;Stop an impro handler
(defmethod stop-impro-handler ((self impro-handler))
  (om-stop-scheduler (player-scheduler self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Démo Jérôme
;;;Solo + Accomp
(defun start-full-impro ()
  (setq *test-solo-handler* (build-impro-handler :name "Nika SOLO" :scenario *scenario-original* :db-path *db-path-solo1* :beat-dur 330))
  (setq *test-accomp-handler* (build-impro-handler :name "Nika ACCOMP" :scenario *scenario-original* :db-path *db-path-accomp1* :beat-dur 330))
  (setq *test-solo-handler-scheduler* (init-impro-handler *test-solo-handler*))
  (setq *test-accomp-handler-scheduler* (init-impro-handler *test-accomp-handler*))
  (om-start-multiple-scheduler (list *test-solo-handler-scheduler* *test-accomp-handler-scheduler*))
  (editor-play (editor (editorframe *display-maquette*))))

(defun pause-full-impro ()
  (om-pause-multiple-scheduler (list *test-solo-handler-scheduler* *test-accomp-handler-scheduler*))
  (editor-pause (editor (editorframe *display-maquette*))))

(defun continue-full-impro ()
  (om-continue-multiple-scheduler (list *test-solo-handler-scheduler* *test-accomp-handler-scheduler*))
  (editor-play (editor (editorframe *display-maquette*))))

(defun stop-full-impro ()
  (stop-impro-handler *test-solo-handler*)
  (stop-impro-handler *test-accomp-handler*)
  (editor-stop (editor (editorframe *display-maquette*))))

;;;Solo only
(defun start-solo-impro ()
  (setq *test-solo-handler* (build-impro-handler :name "TestNika" :scenario *scenario-original* :db-path *db-path-solo1* :beat-dur 330))
  (setq *test-solo-handler-scheduler* (init-impro-handler *test-solo-handler*))
  (om-start-scheduler *test-solo-handler-scheduler*)
  (editor-play (editor (editorframe *display-maquette*))))

(defun pause-solo-impro ()
  (om-pause-scheduler *test-solo-handler-scheduler*)
  (editor-pause (editor (editorframe *display-maquette*))))

(defun continue-solo-impro ()
  (om-continue-scheduler *test-solo-handler-scheduler*)
  (editor-play (editor (editorframe *display-maquette*))))

(defun stop-solo-impro ()
  (stop-impro-handler *test-solo-handler*)
  (editor-stop (editor (editorframe *display-maquette*))))

;;;Accomp only
(defun start-accomp-impro ()
  (setq *test-accomp-handler* (build-impro-handler :name "TestNika1" :scenario *scenario-original* :db-path *db-path-accomp1* :beat-dur 330))
  (setq *test-accomp-handler-scheduler* (init-impro-handler *test-accomp-handler*))
  (om-start-scheduler *test-accomp-handler-scheduler*)
  (editor-play (editor (editorframe *display-maquette*))))

(defun pause-accomp-impro ()
  (om-pause-scheduler *test-accomp-handler-scheduler*)
  (editor-pause (editor (editorframe *display-maquette*))))

(defun continue-accomp-impro ()
  (om-continue-scheduler *test-accomp-handler-scheduler*)
  (editor-play (editor (editorframe *display-maquette*))))

(defun stop-accomp-impro ()
  (stop-impro-handler *test-accomp-handler*)
  (editor-stop (editor (editorframe *display-maquette*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Display Hack functions
;;;Refresh maquette display
(defmethod refresh-maquette ((self ommaquette))
 (loop for b in (boxes self) do
       (omNG-box-value b))
 (let ((panel (editorframe self)))
   (when panel
     (update-after-mark panel))))

;;;réinitialiser les chord-seq du display
(defun reinit-chord-display ()
  (copy-chseq-data (make-instance 'chord-seq :lmidic (list (om-random 6000 7000))) *solo-chord*)
  (copy-chseq-data (make-instance 'chord-seq :lmidic (list (om-random 6000 7000))) *accomp-chord*))


 
