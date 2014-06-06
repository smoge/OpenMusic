(in-package :om)

;original :
;((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))

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

(defun beats->mididata (beatlist refbeatvalue deltachords)
  (let* (chords beatvalue
                lonset 
         ;last-note 
                (beats (loop for beat in beatlist
                             for eventlist = (MidiSet beat)
                             for label = (HarmLabel beat)
                             for onset = 0 then (+ onset beatvalue)
                             do (setf beatvalue              ; beatvalue of previous beat for shifting onset 
                                      (if (and (listp label) (third label)) (* refbeatvalue (third label)) 
                                        refbeatvalue))    ;if label does not contain informations on the number of beats,
                                                     ;the chords has the same duration as refbeatvalue
                             ;;;;;;do (format *om-stream* "~a ~a~%" (HarmLabel beat) beatvalue)
                             append (loop for event in eventlist
                                          collect (list (first event) (+ onset (second event)) (third event) (fourth event) 
                                                 ;(1+ 
                                                        (fifth event)      ;)
                                                        )))))
    (setf chords (make-quanti-chords beats deltachords)
          lonset (mapcar 'offset chords))
    (values chords lonset)))

;(init-testnika)
;(run-testnika)
;(play-test-nika)

;(let () (multiple-value-bind (lmidic lonset) (beats->mididata *result-beats* *beat-dur* 0) (print lmidic) (print lonset)))

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

(defstruct (impro-handler)
  (name "Improvizer-Handler" :type string)
  (scheduler nil :type (or null sch::scheduler))
  (scenario nil :type list)
  (db-path nil :type (or null string))
  (beat-dur 100 :type integer)
  ;(beat-trace nil :type list)
  (index 0 :type integer)
  (rtimprovizer nil :type integer)
  (alarm nil :type boolean)
  (playlist nil :type list)
  (play-pos 0 :type integer)
  (empty-pos 0 :type integer))

(defun build-impro-handler (&key name scenario db-path beat-dur)
  (let ((handler (make-impro-handler :name (or name "Improvizer-Handler")
                                     :scheduler (sch:make-scheduler)
                                     :scenario scenario
                                     :db-path db-path
                                     :beat-dur (or beat-dur 100))))
    (setf (impro-handler-rtimprovizer handler) (load-realtimeImprovizer-fromSavedImprovizer (impro-handler-db-path handler)))))

(defmethod proceed-impro-handler ((self impro-handler))
  (let* ((beat-dur (impro-handler-beat-dur self))
         (index (impro-handler-index self))
         (scenario-suffix (nthcdr index (expand_grid (impro-handler-scenario self))))
         result-beat-list)
    (setq result-beat-list (improvize-loop-next-factor (impro-handler-rtimprovizer self)
                                                       beat-dur 
                                                       index))
    (setf (impro-handler-index self) (+ index (length result-beat-list))) 
    ;;;TODO : mettre la result-beat-list au bon endroit, soit a la fin soit à partir de tel endroit

    )
  )



;thread-beats pour clean les onset 
;beat-dur*beat-n + onset

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

