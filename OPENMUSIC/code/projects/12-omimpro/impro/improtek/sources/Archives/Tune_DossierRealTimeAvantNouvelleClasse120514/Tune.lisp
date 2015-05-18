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
   (alternategrid :initform nil :initarg :alternategrid :accessor alternategrid)    ;Marc 8/5/2013 for playing interesting voicings
                                                      ;when the 'grid' for improvizing is simplified
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

   ;(double :initform nil :initarg :double :accessor double)          ;Marc 27/2/2012 -> supprime 8/5/2013

   ;Jerome 26/10/13 : New fields for real time improvisation (move ?)
   ;-------------------------------------------------------------------
   (RealTimeSystem :initform nil :initarg :RealTimeSystem :accessor RealTimeSystem) 
   (ThreadRcvLearning :initform nil :initarg :ThreadRcvLearning :accessor ThreadRcvLearning) 
   (ThreadRcvNavigOrder :initform nil :initarg :ThreadRcvNavigOrder :accessor ThreadRcvNavigOrder)
   (ThreadProcessLearning :initform nil :initarg :ThreadProcessLearning :accessor ThreadProcessLearning)
   (ThreadProcessNavig :initform nil :initarg :ThreadProcessNavig  :accessor ThreadProcessNavig )
   (ServerRcvLearning :initform nil :initarg :ServerRcvLearning :accessor ServerRcvLearning) 
   (ServerRcvNavigOrder :initform nil :initarg :ServerRcvNavigOrder :accessor ServerRcvNavigOrder)
   ;-------------------------------------------------------------------
   
   ))

(setf *current-tune* (make-instance 'tune))

;Jérôme 13/02/13 : add chapters
(defmethod info_tune ((self tune))
  (setf nb_beats (loop for chord in (grid self) sum (third chord))
        tempo (round (* 1000 (/ 60 (beatduration self))))
        s_chapters (format nil "~{ ~A~}" (loop for c in (chapters self) collect (format nil "~D" c))))
  (format nil "~s ~a ~a ~a ~a"s_chapters (tunename self) nb_beats tempo (NbBeatsPerMeasure self))
  )

;Marc 26/10/13
(defmethod substitution ((self tune)) (setf (alternategrid self) (grid self)))

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


;Marc 26/10/13
(defmethod simplegrid ((self tune)) 
  (let ((multibeatgrid (grid self)))
    ;(when (double self) (setf multibeatgrid (doublegrid multibeatgrid)))              ;Marc 27/2/2012 -> supprime 8/5/2013
    (expandgrid multibeatgrid)))
(defun expandgrid (multibeatgrid) (loop for x in multibeatgrid append (make-list (third x) :initial-element (nthcar 2 x))))

(defun multibeatgrid->labels (multibeatgrid) (expandgrid multibeatgrid))
#|
(defmethod simplegrid ((self tune)) 
  (let ((multibeatgrid (grid self)))
    (when (double self) (setf multibeatgrid (doublegrid multibeatgrid)))              ;Marc 27/2/2012
    (multibeatgrid->labels multibeatgrid)))

(defun multibeatgrid->labels (multibeatgrid) (loop for x in multibeatgrid append (make-list (third x) :initial-element (nthcar 2 x))))
|#


;message from Max: "/load-midibuff-in-oraclechan"

;Jerome 29/04/2013 : ex "load-realtime-data"
(defmethod! load-midibuff-in-oraclechan ((self tune) num_oracle)
(if (probe-file (MidiFixedBuff self))   ;Jerome 28/01/2012, WARNING: this test is necessary to avoid OM crash !!!
    (let* ((res (midi-to-beatlist (MidiFixedBuff self))) (defaultbeatdur) (beatlist))
      (format *om-stream* "~%===== LOADING MIDI ORACLE ~a ... =====~%" num_oracle)
      (if res
        (progn (setf defaultbeatdur (second res) beatlist (first res))
               

               ;Jerome 26/10/13
               ;????? CHANGER LE TEMPO DE LA TUNE PAR CELUI CALCULE SUR LE MIDIBUFF CHARGE DANS UN ORACLE ????????
               ;(setf (beatduration self) defaultbeatdur)
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


;MAJ 26/10/13
;Generate impros for each oracle in 'oraclelist' and merge them with consecutive channels starting from 'refchannel'
;The result is a mix of impros in a beatlist
(defmethod generate-improbeatlist-from-oraclelist ((self tune) (oraclelist list) refchannel)          
  (loop for i from refchannel 
        for oracle in oraclelist 
        ;Jerome 29/04/2013
        for saved-impro = (gethash i (improtable self))
        ;Marc 8/5/13
        with gridforimprovizing = nil     ;;;;;;Marc 9/8/13
        with gridforvoicings = (substitution self)
        with substgrid = nil
        
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i)
                      ;Jerome 29/04/2013
                      ;Marc 8/5/2013 conditions added for playing voicings on channels 6 and 7
                      (if (and saved-impro (not (member i '(6 7))))
                          (progn (format *om-stream* "Unchanged oracle -> previous impro loaded~%")
                            saved-impro)
                        
                        (cond 
                         ;Non empty oracle
                         ((> (maxetat oracle) 1)    
                          (when (/= (beatduration self) (RefTempo oracle)) 
                            (SetReftempo oracle (beatduration self)))     ;'oracle N' is adapted to the beat duration of the tune
                             ;(ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))))))
                             ;Jerome 29/04/2013
                             ; TODO : IL FAUT DEJA LE SAUVEGARDER DANS LA TABLE AVEC LE BON CANAL !!!! OU BIEN REVOIR L'ARCHITECTURE DE CETTE FONCTION !
                             ; PB : ON A POUR L'INSTANT UN PATE DANS LA TABLE POUR LE CANAL 8... POURQUOI ?????
                          (setf gridforimprovizing (if (= i 4) (expandgrid gridforvoicings) (simplegrid self))
                                (gethash i (improtable self)) (ImprovizeOnHarmGrid oracle (length gridforimprovizing) gridforimprovizing)))
                         
                              ;Marc 8/5/2013  -> if maxetat <= 1 => empty oracle, compute voicings on channel 6-7
                         
                         ; !!!!!!!!!!!!!! JEROME 040314 !!!!!!!!!!!!!!!!!!
                         ((= i 12)  
                         ;((= i 7)
                          (format *om-stream*  "Voicings~%")
                          (setf (gethash i (improtable self)) (PlayVoicings (voicings self) gridforvoicings (beatduration self))))
                              ;((= i 7)                 
                              ; (format *om-stream*  "Substituted voicings~%")
                              ; (setf substgrid ;gridforvoicings)
                              ;       (Jazzchordlist->MultibeatLabels (substitute-by-transition (MultibeatLabels->Jazzchordlist gridforvoicings))))
                              ; (PrintWord substgrid)
                              ; (setf (gethash i (improtable self)) (PlayVoicings basicvoicings substgrid (beatduration self))))
                         (t (format *om-stream*  "Empty oracle~%") nil))))
        
        with mix = nil
        
        do (setf impro (replace-channel impro i)
                 mix (add-voice-to-beatsmix impro mix i (beatduration self))) 

        finally return (progn (format *om-stream* "-----------------------~%" ) 
                         (thread-Beats mix (beatduration self)))))


; REVIEW JEROME 15/05/2013 : A METTRE DANS BEATLIST.LISP ?
;Marc 8/5/2013
(defun replace-channel (beatsvoice chan &optional oldchan)    ;warning: names 'set-channel','change-channel' already used in OM
  (loop for x in beatsvoice for new-x = (clone x) 
        do (loop for z in (MidiSet new-x) when (or (null oldchan) (= (fifth z) oldchan)) do (setf (fifth z) chan))
        collect new-x))

(defun add-voice-to-beatsmix (beatsvoice beatsmix chan beatdur)
  (cond ((null beatsmix) beatsvoice)
        ((null beatsvoice) beatsmix)
        (t (loop for y in beatsmix for new-y = (clone y) for x in beatsvoice 
                 do (setf (MidiSet new-y) (sort (append (remove chan (MidiSet new-y) :key 'fifth) (MidiSet x))
                                                #'<= :key #'second))
                 collect new-y into new-beatsmix
                 finally return (thread-Beats new-beatsmix beatdur)))))


#|
; Sauvegarde avant MAJ 26/10/13
;====================

;---------------
;Version Jerome
;---------------
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
                             ;Jerome 01/07/2013
                            (setf (gethash i (improtable self)) (ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))))))
                      )
        with mix = nil
        
        do (if (null mix) 
               (progn
                 (loop for beat in impro do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list i))))) 
                 (setf mix impro))
             (add-voice-to-beatsmix impro mix i (beatduration self)))
        
        finally return (progn (format *om-stream* "-----------------------~%") 
                         (thread-Beats mix (beatduration self)))))

; REVIEW JEROME 15/05/2013 : A METTRE DANS BEATLIST.LISP ?
(defun add-voice-to-beatsmix (beatsvoice beatsmix chan beatdur)
  (loop for x in beatsvoice for y in beatsmix 
        do (setf (MidiSet y) (sort (append (remove chan (MidiSet y) :test #'(lambda (chan 5uple) (= (fifth 5uple) chan)))
                                           (loop for z in (MidiSet x) collect (append (nthcar 4 z) (list chan)))) 
                                   #'<= :key #'second))
        finally return (thread-Beats beatsmix beatdur)))




;---------------
;Version Marc
;---------------

(defmethod generate-improbeatlist-from-oraclelist ((self tune) (oraclelist list) refchannel)          
  (loop for i from refchannel 
        for oracle in oraclelist 
        ;Jerome 29/04/2013
        for saved-impro = (gethash i (improtable self))
        ;Marc 8/5/13
        with gridforimprovizing = nil     ;;;;;;Marc 9/8/13
        with gridforvoicings = (substitution self)
        with substgrid = nil
        
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i)
                      ;Jerome 29/04/2013
                      (if (and saved-impro (not (member i '(6 7))))
                          (progn (format *om-stream* "Unchanged oracle -> previous impro loaded~%")
                            saved-impro)
                        ;Marc 8/5/2013 conditions added for playing voicings on channels 6 and 7

                        (cond ((> (maxetat oracle) 1)    ;non empty oracle
                               (when (/= (beatduration self) (RefTempo oracle)) 
                                 (SetReftempo oracle (beatduration self)))     ;'oracle N' is adapted to the beat duration of the tune
                             ;(ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))))))
                             ;Jerome 29/04/2013
                             ; TODO : IL FAUT DEJA LE SAUVEGARDER DANS LA TABLE AVEC LE BON CANAL !!!! OU BIEN REVOIR L'ARCHITECTURE DE CETTE FONCTION !
                             ; PB : ON A POUR L'INSTANT UN PATE DANS LA TABLE POUR LE CANAL 8... POURQUOI ?????
                               (setf gridforimprovizing (if (= i 4) (expandgrid gridforvoicings) (simplegrid self))
                                     (gethash i (improtable self)) (ImprovizeOnHarmGrid oracle (length gridforimprovizing) gridforimprovizing)))

                              ;Marc 8/5/2013  -> if maxetat <= 1 => empty oracle, compute voicings on channel 6-7
                              
                              ((= i 12)                 
                               (format *om-stream*  "Voicings~%")
                               (setf (gethash i (improtable self)) (PlayVoicings (voicings self) gridforvoicings (beatduration self))))
                              ;((= i 7)                 
                              ; (format *om-stream*  "Substituted voicings~%")
                              ; (setf substgrid ;gridforvoicings)
                              ;       (Jazzchordlist->MultibeatLabels (substitute-by-transition (MultibeatLabels->Jazzchordlist gridforvoicings))))
                              ; (PrintWord substgrid)
                              ; (setf (gethash i (improtable self)) (PlayVoicings basicvoicings substgrid (beatduration self))))
                              (t (format *om-stream*  "Empty oracle~%") nil)
                              
                              )))

        with mix = nil
        
        do (setf impro (replace-channel impro i)
                 mix (add-voice-to-beatsmix impro mix i (beatduration self)))
        finally return (progn (format *om-stream* "-----------------------~%") 
                         (thread-Beats mix (beatduration self)))))


; REVIEW JEROME 15/05/2013 : A METTRE DANS BEATLIST.LISP ?
;Marc 8/5/2013
(defun replace-channel (beatsvoice chan &optional oldchan)    ;warning: names 'set-channel','change-channel' already used in OM
  (loop for x in beatsvoice for new-x = (clone x) 
        do (loop for z in (MidiSet new-x) when (or (null oldchan) (= (fifth z) oldchan)) do (setf (fifth z) chan))
        collect new-x))

(defun add-voice-to-beatsmix (beatsvoice beatsmix chan beatdur)
  (cond ((null beatsmix) beatsvoice)
        ((null beatsvoice) beatsmix)
        (t (loop for y in beatsmix for new-y = (clone y) for x in beatsvoice 
                 do (setf (MidiSet new-y) (sort (append (remove chan (MidiSet new-y) :key 'fifth) (MidiSet x))
                                                #'<= :key #'second))
                 collect new-y into new-beatsmix
                 finally return (thread-Beats new-beatsmix beatdur)))))


|#






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

(defmethod end-realtime ((self tune))
  ; TODO : KILL THREADS !!!!!!
  (if (ServerRcvLearning self)
      (progn
        (om-stop-osc-server (ServerRcvLearning self))
        (setf (ServerRcvLearning self) nil)))

  (if (ServerRcvNavigOrder self)
      (progn
        (om-stop-osc-server (ServerRcvNavigOrder self))
        (setf (ServerRcvNavigOrder self) nil))))


(defmethod launch-realtime ((self tune) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer) (port_rcv_learn integer) (port_rcv_navig_order integer))
  (end-realtime self)
  (setf (RealTimeSystem self) 
        (make-instance 'AEDPCS 
                       :sharedData (let ((improv (NewRealTimeImprovizer)))
                                     (setf (max-continuity improv) 1000)
                                     improv)
                       :LearnMethodOnSharedData #'learn-realtime 
                       :GenerateMethodOnSharedData #'(lambda (realTimeImpro NavigationOrder) 
                                                       (navigate-realtime 
                                                        realTimeImpro NavigationOrder 
                                                        beatduration host_send port_send_ante numAntescofo))))

  (setf (ThreadRcvLearning self) (bordeaux-threads:make-thread #'(lambda () 
                                                                   (setf (ServerRcvLearning self)
                                                                         (om-start-osc-server port_rcv_learn host_send 
                                                                                              #'(lambda (message host) 
                                                                                                  (handle-evts-to-learn-from-max (RealTimeSystem self) (om-decode-msg-or-bundle message)) nil)))) :name 'L-filler)
        (ThreadRcvNavigOrder self) (bordeaux-threads:make-thread #'(lambda () 
                                                                     (setf (ServerRcvNavigOrder self)
                                                                           (om-start-osc-server port_rcv_navig_order host_send 
                                                                                                #'(lambda (message host) 
                                                                                                    (handle-navig-order-from-max (RealTimeSystem self) (om-decode-msg-or-bundle message)) nil)))) :name 'G-filler)
        (ThreadProcessLearning self) (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer)
        (ThreadProcessNavig self) (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))






  



#|
  (let* (
         (L-filler (bordeaux-threads:make-thread #'(lambda () 
                                                     (om-start-osc-server port_rcv_learn host_send 
                                                                          #'(lambda (mess host) 
                                                                              (handle-evts-to-learn-from-max (RealTimeSystem self) (om-decode-msg-or-bundle mess)) nil))) :name 'L-filler))
         (G-filler (bordeaux-threads:make-thread #'(lambda () 
                                                     (om-start-osc-server port_rcv_navig_order host_send 
                                                                          #'(lambda (mess host) 
                                                                              (handle-navig-order-from-max (RealTimeSystem self) (om-decode-msg-or-bundle mess)) nil))) :name 'G-filler))
         (L-processer (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer))
         (G-processer (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))))
|#



#|
(defmethod handle-evts-to-learn-from-max ((self AEDPCS) (message list))
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  (if (= *print_received_from_Max* 1) 
      (progn
        (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
        (format *om-stream* "Type : ~a~%" (type-of message))
        (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
        (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
        (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))
  (let (evts-to-learn (eval (read-from-string (nth 1 message))))
    (if (string= (car message) "/learnforTR")
        (L-fillEventsToLearnChannelWithEvent (RealTimeSystem self) evts-to-learn))))


;VIEUX TEST
;----------
(defmethod write-in-list ((self list) (i integer))
  (nconc self (list i) ))
(defmethod read-in-list ((self list) (i integer)) 
  (format *om-stream* "---->Reading ~D : ~D ~%~%" (- i 1) (nth (- i 1) self)))
(setf PCS (make-instance 'AEDPCS :sharedData '(0 1 2 3 4 5 6 7 8 9) :LearnMethodOnSharedData 'write-in-list :GenerateMethodOnSharedData 'read-in-list ))


(setf thread 
      (bordeaux-threads:make-thread #'(lambda () 
                                        (om-start-osc-server 7410 "127.0.0.1" 
                                                             #'(lambda (message host) 
                                                                 (handle-evts-to-learn-from-max PCS (om-decode-msg-or-bundle message)) nil))) :name 'L-filler))
|#








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


;JEROME 26/10/13 POUR TR
(defmethod! generate-grid-param ((self tune) beatdur)
            
  ;from "generate-grid"
            (let* ((harmgrid (simplegrid self))
                   (gridlength (list-length harmgrid))
                   (beatlist (loop for x in harmgrid collect (make-instance 'beat :HarmLabel x)))
                   (beatduration (beatduration self))
                   (absolute-path-filename (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "realtime"))
                                                          :name (format nil "~a-grid-param.txt" (tunename self))))
                   (suffixes "")
                   (midicodes1 "")
                   (midicodes2 "")
                   (suffix nil)
                   (midicode nil)
                   (j 0))
              
  ;from "save-for-antescofo2"
              (ensure-directories-exist absolute-path-filename)
              (with-open-file 
                  (ss absolute-path-filename 
                      :direction :output :if-exists :supersede)
                (setf beatlist (transfer-syncopated-event beatlist beatduration))
                (setf beatlist (add-grid-to-beatlist beatlist beatduration))
                
                (loop with c-beatlist = (mapcar #'clone-object beatlist)
                      for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
                      do (progn 
                           (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second)) 
                           (setf j 0)
                           (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                                 do 
                                 (progn 
                                   (incf j)
                                   (setf midicode (format nil "(~d, \"~a ~a 0.0.~a ~a\")" 
                                                         i 
                                                         (MEPitch event)     
                                                         (MEVel event) 
                                                         (round (* (/ (MEDur event) beatduration) 480))
                                                         (MEChannel event)))
                                   (if (= j 1)
                                       (setf midicodes1 (concatenate 'string midicodes1 midicode))
                                     (if (= j 2)
                                       (setf midicodes2 (concatenate 'string midicodes2 midicode))
                                     ))))
                                 (setf suffix (format nil "(~d,\"\'~a\")" i harmgrid))
                                 (setf suffixes (concatenate 'string suffixes suffix))
                                 ; JEROME 01/09/13
                                 ; POUR POUVOIR FAIRE IMPROS SUR PLUSIEURS GRILLES DE SUITES : CIRCULAIRE !
                                 ;(pop harmgrid)
                                 ;(if harmgrid (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                                 (setf harmgrid (rotate harmgrid))
                                 (if nextbeat (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                      ;(format ss "EVENT 1.0~%")
                      ;(format ss "group {~%")
                      (format ss "$suffixes := MAP{")
                      ;QUOTE AU DEBUT ??
                      (format ss "~a}~%~%" suffixes)
                      (format ss "$midicodes1 := MAP{")
                      (format ss "~a}~%~%" midicodes1)
                      (format ss "$midicodes2 := MAP{")
                      (format ss "~a}~%~%" midicodes2)
                      (format ss "$gridlength := ~d~%" gridlength)
                      ;(format ss "}~%")
                      )))
            
#|
(setf tune Jaime_tune)
(generate-grid-param tune (beatduration tune))
|#

(defmethod string-group-for-antescofo ((self beat) (beatduration integer) (numAntescofo integer))
  (let ((c-beat (clone-object self))
        (midicode nil)
        (midicodes ""))
    (setf (MidiSet c-beat) (sort (MidiSet c-beat) #'<= :key #'second)) 
    
    (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet c-beat)) for event in (MidiSet c-beat)
          do 
          (progn 
            ; 06/08/13 : RAJOUTÉ "@local" pour synchronisation évènements arrivés potentiellement trop tard
            ; (cf Delainegatif.asco.txt)
            (setf midicode (format nil "    ~a   mnote~D ~a ~a \"0.0.~a\" ~a @local~%" 
                                   (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))
                                   numAntescofo
                                   (MEPitch event)     
                                   (MEVel event) 
                                   (round (* (/ (MEDur event) beatduration) 480))
                                   (MEChannel event)))
            (setf midicodes (concatenate 'string midicodes midicode))))
    (format nil "group{~%~a}" midicodes)))





