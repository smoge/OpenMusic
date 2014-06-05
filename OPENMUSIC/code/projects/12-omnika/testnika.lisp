(in-package :om)

;original :
;((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))


(setq *scenario* '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (c maj7 4) (d 7 4) (g m7 4) (g m7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (b 7 4) (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4))
      *database-path* "/Users/bouche/Documents/OpenMusic/OM/OPENMUSIC/code/projects/12-omnika/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
      *beat-dur* 330)

(setq *start-indx* 0)

(setq *rtimprovizer* (load-realtimeImprovizer-fromSavedImprovizer *database-path*))

(progn
  (setq *scenario-suffix* (nthcdr *start-indx* (expand_grid *scenario*)))
  
  (setq *result-beats* (improvize-loop-next-factor *rtimprovizer* *scenario-suffix* *beat-dur* *start-indx*))

  (setq *result-chord-seq* (beats->chseq *result-beats* *beat-dur* 0))

;(play *result-chord-seq*)

  (progn
    (push (list *result-chord-seq* 0 nil nil nil) *ms-list-to-play*)
    (player-start :midishare)
    (pop *ms-list-to-play*))
  (incf *start-indx* (length *result-beats*)))


;stop
(player-stop :midishare)


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


(let ()
  (multiple-value-bind (lmidic lonset
    (beats->mididata *result-beats* *beat-dur* 0)
    (print (list (length lmidic) (length lonset))))
  )

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