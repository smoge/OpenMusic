;Tune.lisp
;by Marc Chemillier 2011 (additional code by J. Nika)
;(formerly in Antescofo.lisp)

(in-package :om)

(defclass* tune ()
  (
   (MidiFixedBuff :initform path_midibuff :initarg :MidiFixedBuff :accessor MidiFixedBuff)  
   (tunedir :initform path_tunessavedir :initarg :tunedir :accessor tunedir)
   (tunename :initform "Reveeveille" :initarg :tunename :accessor tunename)
   (grid :initform '((a m7 4) (b m7 4) (g m7 4) (a m7 4)) :initarg :grid :accessor grid)
   (chapters :initform '(1) :initarg :chapters :accessor chapters); Jérôme 13/02/13 : idxs of the measures at the beginning of the chapters
   (oracletable :initform (let ((table (make-hash-table)))          ;;;Marc 12/5/2012 pairs (channel oracle): channel 3 -> channel 14
                            (loop for i from 3 to 14 for o = (NewImprovizer) do (setf (gethash i table) o (max-continuity o) 1000))  ;;;(if (<= i 5) 1000 8)))
                            table)
                :initarg :oracletable :accessor oracletable)
   (improtable :initform (make-hash-table :test 'equal) :initarg :improtable :accessor improtable) ;Jérôme 29/04/2013 : save calculated impros on oracles in oracletable 
                                                                                                   ;(so that a new impro is calculated only if the associated oracle has changed)
   (grammar :initform bluesgrammar :initarg :grammar :accessor grammar)
   (voicings :initform hermetovoicings :initarg :voicings :accessor voicings)
   (beatduration :initform 400 :initarg :beatduration :accessor beatduration)
   (NbBeatsPerMeasure :initform 4 :initarg :NbBeatsPerMeasure :accessor NbBeatsPerMeasure) 

   (double :initform nil :initarg :double :accessor double)          ;Marc 27/2/2012
   ))


(setf *current-tune* (make-instance 'tune))

;Jérôme 13/02/13 : add chapters
(defmethod info_tune ((self tune))
  (setf nb_beats (loop for chord in (grid self) sum (third chord))
        tempo (round (* 1000 (/ 60 (beatduration self))))
        s_chapters (format nil "~{ ~A~}" (loop for c in (chapters self) collect (format nil "~D" c))))
  (format nil "~s ~a ~a ~a ~a"s_chapters (tunename self) nb_beats tempo (NbBeatsPerMeasure self))
  )

; Functions called in "HandleMaxMessages.lisp"
; --------------------------------------------

;message from Max: "/set-tune": -> LoadImprotek.lisp

(defmethod! generate-grid ((self tune) beatdur)
  (loop for x in (simplegrid self)
        collect (make-instance 'beat :HarmLabel x) into beatlist
        finally (save-for-antescofo2 beatlist                    ;MARC 15/11/2011: 'generate-grid' writes for the second Antescofo with "mnote2"
                                     beatdur
                                     (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "grid"))
                                                    :name (format nil "~a-grid.txt" (tunename self))))))

(defmethod simplegrid ((self tune)) 
  (let ((multibeatgrid (grid self)))
    (when (double self) (setf multibeatgrid (doublegrid multibeatgrid)))              ;Marc 27/2/2012
    (multibeatgrid->labels multibeatgrid)))

(defun multibeatgrid->labels (multibeatgrid) (loop for x in multibeatgrid append (make-list (third x) :initial-element (nthcar 2 x))))

;message from Max: "/load-midibuff-in-oraclechan"

;Jerome 29/04/2013 : ex "load-realtime-data"
(defmethod! load-midibuff-in-oraclechan ((self tune) num_oracle)

(if (probe-file (MidiFixedBuff self))   ;Jerome 28/01/2012, WARNING: this test is necessary to avoid OM crash !!!
    (let* ((res (midi-to-beatlist (MidiFixedBuff self))) (defaultbeatdur) (beatlist))
      (format *om-stream* "~%===== LOADING MIDI ORACLE ~a ... =====~%" num_oracle)
      (if res
        (progn (setf defaultbeatdur (second res) beatlist (first res))
               (setf (beatduration self) defaultbeatdur)
               (let ((o (NewImprovizer beatlist defaultbeatdur)))
                 (setf (gethash num_oracle (oracletable self)) o (max-continuity o) 1000))  ;;;(if (<= i 5) 1000 8)))
               (gethash num_oracle (oracletable self))
               ;Jerome 29/04/2013
               ;The oracle has been modified. The associated impro in improtable has to be deleted because it is not up to date.
               (remhash num_oracle (improtable self))
                     )
        (print "Error : empty MidiFixedBuff, no oracle is built")))
  (print "Error : no MidiFixedBuff for current tune")
))

;message from Max:  "/set-max-continuity-oraclechan"

;message from Max: "/gen-impro-polychan-oracletable"

;Jerome 29/04/2013 : ex "generate-impro-for-antescofo"
(defmethod! generate-polychan-impro-for-antescofo ((self tune))
  (let ((oraclelist1 (loop for k from 8 to 14 collect (gethash k (oracletable self))))
        (oraclelist2 (loop for k from 3 to 7 collect (gethash k (oracletable self))))
        (impro1) (impro2))

    (format *om-stream* "~%===== GENERATING A MIX OF MULTI ORACLES FOR COMPING ANTESCOFO \"mnote2\" ... =====~%")
    (setf impro2 (generate-improbeatlist-from-oraclelist self oraclelist2 3))

    (format *om-stream* "~%===== GENERATING A MIX OF MULTI ORACLES FOR SOLO ANTESCOFO ... =====~%")
    (setf impro1 (generate-improbeatlist-from-oraclelist self oraclelist1 8))

    (let* ((dir1 (append (pathname-directory (tunedir self)) (list (tunename self) "new")))
           (dir2 (append (pathname-directory (tunedir self)) (list (tunename self) "new2")))
           (l (mapcar #'(lambda (x) (format nil "~2,'0d" x)) (cdddr (reverse (multiple-value-list (get-decoded-time))))))
           (filename1 (format nil "~a-new-~a.~a.~a-~ah~a.txt" (tunename self) (first l) (second l) (third l) (fourth l) (fifth l))) 
           (filename2 (format nil "~a-new2-~a.~a.~a-~ah~a.txt" (tunename self) (first l) (second l) (third l) (fourth l) (fifth l))))
                               ;Marc 26/4/12 Back to a dated name --> Max chooses the last impro in the folder "new" or "new2"
                                              ;thanks to (format nil "~2,'0d" x) which writes 01, 02, etc. instead of 1, 2
                                 ; Jerome 04/03/13: year first

      (if impro1 (save-for-antescofo impro1 (beatduration self) (make-pathname :directory dir1 :name filename1))
        (print "Solo impro is empty, no file for antescofo 1"))
      (if impro2 (save-for-antescofo2 impro2 (beatduration self) (make-pathname :directory dir2 :name filename2))
        (print "Comping impro is empty, no file for antescofo 2 \"mnote2\"")))))

;Generate impros for each oracle in 'oraclelist' and merge them with consecutive channels starting from 'refchannel'
;The result is a mix of impros in a beatlist
(defmethod generate-improbeatlist-from-oraclelist ((self tune) (oraclelist list) refchannel)          
  (loop for i from refchannel 
        for oracle in oraclelist 
        ;Jerome 29/04/2013
        for saved-impro = (gethash i (improtable self))
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i)
                      ;Jerome 29/04/2013
                      (if saved-impro 
                          (progn (format *om-stream* "Unchanged oracle -> previous impro loaded~%")
                            saved-impro)
                        (if (<= (maxetat oracle) 1) 
                             (progn (format *om-stream*  "Empty oracle~%") nil)
                           (progn (when (/= (beatduration self) (RefTempo oracle)) 
                                    (SetReftempo oracle (beatduration self)))     ;'oracle N' is adapted to the beat duration of the tune
                             ;(ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))))))
                             ;Jerome 29/04/2013
                             (setf (gethash i (improtable self)) (ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self)))))))

                      with mix = nil
                      
                      do (if (null mix) (setf mix impro)
                           (add-voice-to-beatsmix impro mix i (beatduration self)))
                      finally return (progn (format *om-stream* "-----------------------~%") 
                                       (thread-Beats mix (beatduration self)))))
  
#|
(defmethod generate-improbeatlist-from-oraclelist ((self tune) (oraclelist list) refchannel)          
  (loop for i from refchannel 
        for oracle in oraclelist                 
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i)
                      (if (<= (maxetat oracle) 1) 
                        (progn (format *om-stream*  "Empty oracle~%") nil)
                        (progn (when (/= (beatduration self) (RefTempo oracle)) 
                                 (SetReftempo oracle (beatduration self)))     ;'oracle N' is adapted to the beat duration of the tune
                          (ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self)))))
        with mix = nil
       
        do (if (null mix) (setf mix impro)
             (add-voice-to-beatsmix impro mix i (beatduration self)))
        finally return (progn (format *om-stream* "-----------------------~%") 
                         (thread-Beats mix (beatduration self)))))
|#






; REVIEW JEROME 15/05/2013 : A METTRE DANS BEATLIST.LISP ?
(defun add-voice-to-beatsmix (beatsvoice beatsmix chan beatdur)
  (loop for x in beatsvoice for y in beatsmix 
        do (setf (MidiSet y) (sort (append (remove chan (MidiSet y) :test #'(lambda (chan 5uple) (= (fifth 5uple) chan)))
                                           (loop for z in (MidiSet x) collect (append (nthcar 4 z) (list chan)))) 
                                   #'<= :key #'second))
        finally return (thread-Beats beatsmix beatdur)))


#|
ADDING A VOICE MANUALY
----------------------
(setf *current-tune* Allthethingsyouare_tune)
(tunename *current-tune*)
(setf beatdur (beatduration *current-tune*))

(setf (gethash 3 (oracletable *current-tune*)) (load-improvizer "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Allthethings-bass.or")) 
(setf (gethash 5 (oracletable *current-tune*)) (load-improvizer "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Allthethings-accords.or"))

(setf beatsmix (generate-improbeatlist-from-oraclelist *current-tune* (loop for k from 3 to 7 collect (gethash k (oracletable *current-tune*))) 3)) 

;Adding a voice on channel 7 (chord voicings, cf. "HarmoArrang/Substitution.lisp"):
(setf voicings-multibeats (PlayVoicings basicvoicings (grid *current-tune*) (beatduration *current-tune*)))
(setf voicings (loop for x in voicings-multibeats  
                     append (cons x                                       ;1rst beat of a chord: MidiSet with the whole duration 
                                  (make-list (1- (third (HarmLabel x)))   ;+ adding as many beats as the chord duration with empty MidiSet
                                               :initial-element (make-instance 'beat :HarmLabel (HarmLabel x))))))

(setf newbeatsmix (add-voice-to-beatsmix voicings beatsmix 7 (beatduration *current-tune*)))

;SAVING THE RESULT INTO FILE "buffer-for-antescofo.txt"
;"/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/buffer-for-antescofo.txt"
(save-for-antescofo2 newbeatsmix (beatduration *current-tune*))

;Check the result as a piano-roll in Intuem:
(save-as-midi-with-tempo (beats->chseq newbeatsmix (beatduration *current-tune*) 0) (beatduration *current-tune*))
(save-as-midi-with-tempo (beats->chseq voicings-multibeats (beatduration *current-tune*) 0) (beatduration *current-tune*))



|#


;message from Max: "/save-oraclechan"
;Jerome 29/04/2013 : ex "save-oracle"
(defmethod! save-oraclechan ((self tune) num-oracle)
            (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
              (save-improvizer 
               (gethash num-oracle (oracletable self)) 
               (format nil "~a/~a" 
                       path_dir_live_oracles
                       (format nil "~a-OrChan~D-~a.~a.~a-~ah~a.or" (tunename self) num-oracle (first l) (second l) (third l) (fourth l) (fifth l))))))

;message from Max: "/set-start-region-oraclechan"   -> voir Improvizer.lisp

;message from Max: "/load-saved-oracle-in-oraclechan"

(defmethod! load-saved-oracle-in-oraclechan ((self tune) s num_oracle)            ;Jerome 11 mars 2013
  (if (probe-file s)
      (progn 
        (setf (gethash num_oracle (oracletable self)) (load-improvizer s))
        ;Jerome 29/04/2013
        ;The oracle has been modified. The associated impro in improtable has to be deleted because it is not up to date.
        (remhash num_oracle (improtable self)))
        (print "Impro oracle not found")
))




;message from Max: "/reset-oraclechan"
;Jerome 29/04/2013 : ex "reset-liveoracle"
(defmethod! reset-oraclechan ((self tune) num_oracle) 
  (setf (gethash num_oracle (oracletable self)) (NewImprovizer))
  ;Jerome 29/04/2013
  ;The oracle has been modified. The associated impro in improtable has to be deleted because it is not up to date.
  (remhash num_oracle (improtable self))
  (when (member num_oracle '(3 4 5)) (setf (max-continuity (gethash num_oracle (oracletable self))) 1000))
  (gethash num_oracle (oracletable self)))

;Jerome 29/04/2013
(defmethod! set-max-continuity-oraclechan ((self tune) num_oracle max-cont)
  (setf (max-continuity (gethash num_oracle (oracletable self))) max-cont)
  ;The oracle has been modified. The associated impro in improtable has to be deleted because it is not up to date.
  (remhash num_oracle (improtable self))
)

;Jerome 29/04/2013
(defmethod! reset-tabou-oraclechan ((self tune) num_oracle)
  (ResetTabou (gethash num_oracle (oracletable self)))
  ;The oracle has been modified. The associated impro in improtable has to be deleted because it is not up to date.
  (remhash num_oracle (improtable self))
)


#|

; Tutorial based on musical data from "Styles/Garner.lisp"

(setf *current-tune* Garnerloop_tune)
(tunename *current-tune*)
(setf (beatduration *current-tune*) 236)    ;BPM=254
(setf (double *current-tune*) t)       ;impros played half tempo in Max BPM=127, thus computed with a double grid (BPM=254)

(setf (gethash 1 (oracletable *current-tune*)) garnerrightoracle) 
(setf (gethash 2 (oracletable *current-tune*)) garnerleftoracle)

(simplegrid *current-tune*)           ;when (double *current-tune*) = t ---> (simplegrid *current-tune*) is doubled

(setf (max-continuity garnerrightoracle) 1000
      (max-continuity garnerleftoracle) 1000)

(setf impro (mix-impro-multi-oracle *current-tune* (list (gethash 1 (oracletable *current-tune*)) 
                                                         (gethash 2 (oracletable *current-tune*))) 1)) 

(pgmout 4 3) (pgmout 4 4) (pgmout 4 5)
(play (merger (beats->chseq impro 236 0)
              (beats->chseq (make-clocks 32 236 2) 236 0)))

(play (merger (beats->chseq (mix-impro-multi-oracle *current-tune* (list (gethash 1 (oracletable *current-tune*)) 
                                                         (gethash 2 (oracletable *current-tune*))) 1) 
                            236 0)
              (beats->chseq (make-clocks 32 236 2) 236 0)))

(Stop-Player *general-player*)

(setf (maxpolyphony *current-tune*) 3)

(midi-to-beatlist (MidiFixedBuff *current-tune*))
(load-realtime-data *current-tune* 1)
|#





; CREE HARMO SUR LA GRILLE PAR SUBSTITUTIONS
(defmethod! generate-offline-harmos ((self tune) maxbeatdur)     ;;;;;'maxbeatdur'????? ---> en principe DEJA dans (beatduration self)?????
  (loop for i from 1 to 1    ;;;;;;;;;;;;10
        do (let* ((labels (GridLabel (Rewrite (grammar self) (InitMultiplebeatGrid (grid self)) 4)))
                  (harmo (PlayVoicings (voicings self) labels maxbeatdur)))     ;500
             (save-for-antescofo (multipleBeats->singleBeats harmo maxbeatdur)
                                 maxbeatdur
                                 (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "harmo"))
                                                :name (format nil "~a-harmo~a.txt" (tunename self) i)))))
  (format *om-stream* "==== end of impro =====~%"))

;!!!!!!!!!!!!!!!!!!!!!!!!! DOES NOT WORK !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun multipleBeats->singleBeats (beatlist beatdur)
  (loop for beat in beatlist for evts = (MidiSet beat)
        append (if (null (cddr (HarmLabel beat)))  
                 (list beat)
                 (loop for i from 1 to (third (HarmLabel beat)) for new-beat = (clone-object beat) 
                       do (setf (HarmLabel new-beat) (nthcar 2 (HarmLabel beat))
                                (MidiSet new-beat)
                                (loop for evt in evts       ; notes are dispatched in successive beats according to their onset
                                      when (and (>= (MEOnset evt) (* (1- i) beatdur)) 
                                                (< (MEOnset evt) (* i beatdur)))
                                      collect (list (MEPitch evt) (mod (MEOnset evt) beatdur) (MEDur evt) (MEVel evt) (MEChannel evt))))   
                                                                                                  ;bug 8/3/2012: MEVel instead of 60
                       collect new-beat))))




;=============================================================================================================================================================================================================
;NOUVEAU
;=============================================================================================================================================================================================================

(defmethod! generate-accomp-current-voicing-oracle ((self tune) length_impro)
             
    ;====START-REGION !!!!
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
             (setf (max-continuity *current-VoicingOracle*) 1000)
             (set-start-region *current-VoicingOracle* (list 0 (1-(NbEvent?  *current-VoicingOracle*))))
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
             
             
             (loop for i from 1 to 2
                   do (progn (format *om-stream* "-----------------------~%~a~a~%" (tunename self) i)
                        (save-for-antescofo (thread-Beats (ImprovizeOnHarmGrid *current-VoicingOracle* (* length_impro (length (simplegrid self))) (simplegrid self))
                                                          (beatduration self))
                                            (beatduration self)
                                            (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "harmo"))
                                                           :name (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
                                                                   (format nil "~a-accomp~a.~a.~a-~a-~a.~a.txt" 
                                                                           (tunename self) (second l) (third l) (first l) (fourth l) (fifth l) i))))))
             (format *om-stream* "==== accomp. generated =====~%"))






