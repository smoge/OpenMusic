;Antescofo.lisp
;by Marc Chemillier 2011 (additional code by J. Nika)

(in-package :om)

;----------------------------------------------------------------
;IMPROVISING WITH 'ANTESCOFO' IN MAX


#|
;SAVE AN IMPROVIZER
 
(save-oracle *current-tune* 8)

|#


;Jérôme 13/02/13 : add chapters
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

;!!!!!!!!!!!!!!   NOT IN USE ANYMORE
   (oraclechan3 :initform (NewImprovizer nil 536 :max-continuity 1000) :initarg :oraclechan3 :accessor oraclechan3)  ;Marc Uzeste 16/3/12
   (oraclechan4 :initform (NewImprovizer nil 536 :max-continuity 1000) :initarg :oraclechan4 :accessor oraclechan4)
   (oraclechan5 :initform (NewImprovizer nil 536 :max-continuity 1000) :initarg :oraclechan5 :accessor oraclechan5)
   (oraclechan6 :initform (NewImprovizer) :initarg :oraclechan6 :accessor oraclechan6)
   (oraclechan7 :initform (NewImprovizer) :initarg :oraclechan7 :accessor oraclechan7)
   (oraclechan8 :initform (NewImprovizer) :initarg :oraclechan8 :accessor oraclechan8)
;!!!!!!!!!!!!!   NOT IN USE ANYMORE

   (oracle :initform (NewImprovizer) :initarg :oracle :accessor oracle)   ;;;;;not needed anymore !!!!!!!!!!!!!!!!!!!!!!!!!!!
   (liveoracle :initform (NewImprovizer) :initarg :liveoracle :accessor liveoracle);;;;;not needed anymore

   (grammar :initform bluesgrammar :initarg :grammar :accessor grammar)
   (voicings :initform hermetovoicings :initarg :voicings :accessor voicings)
   (beatduration :initform 400 :initarg :beatduration :accessor beatduration)
   (NbBeatsPerMeasure :initform 4 :initarg :NbBeatsPerMeasure :accessor NbBeatsPerMeasure) 

   (maxpolyphony :initform 1 :initarg :maxpolyphony :accessor maxpolyphony)        ;Marc 12/2/2012 !!!!!!! - NOT IN USE WITH oracletable - !!!!!!!!!!!!
   (double :initform nil :initarg :double :accessor double)          ;Marc 27/2/2012
   ))




(defmethod simplegrid ((self tune)) 
  (let ((grid (grid self)))
    (when (double self) (setf grid (doublegrid grid)))              ;Marc 27/2/2012
    (loop for x in grid append (make-list (third x) :initial-element (nthcar 2 x)))))


;Jérôme 13/02/13 : add chapters
(defmethod info_tune ((self tune))
  (setf nb_beats (loop for chord in (grid self) sum (third chord))
        tempo (round (* 1000 (/ 60 (beatduration self))))
        s_chapters (format nil "~{ ~A~}" (loop for c in (chapters self) collect (format nil "~D" c))))
  (format nil "~s ~a ~a ~a ~a"s_chapters (tunename self) nb_beats tempo (NbBeatsPerMeasure self))
  )
#|
(setf testtune (make-instance 'tune))
(info_tune testtune)

(setf chapters '(1 5 10))
(format nil "Test :~{ ~A~}." (loop for c in chapters collect (format nil "~D" c)))
|#
#|
(defmethod info_tune ((self tune))
  (setf nb_beats (loop for chord in (grid self) sum (third chord))
        tempo (round (* 1000 (/ 60 (beatduration self)))))
  
  (format nil "~a ~a ~a ~a" (tunename self) nb_beats tempo (NbBeatsPerMeasure self))
  )
|#


(setf *current-tune* (make-instance 'tune))


;======ATTENTION DEPLACE DANS "CommunicationMax.lisp"=======
;(defmethod! load-realtime () (load-realtime-data-and-generate-impros *current-tune*))
;(defmethod! reset-live () (reset-liveoracle *current-tune*))


;MARC 15/11/2011: 'generate-grid' writes for the second Antescofo with "mnote2" instead of "mnote"

(defmethod! generate-grid ((self tune) beatdur)
  (loop for x in (simplegrid self)
        collect (make-instance 'beat :HarmLabel x) into beatlist
        finally (save-for-antescofo2 beatlist
                                     beatdur
                                     (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "grid"))
                                                    :name (format nil "~a-grid.txt" (tunename self))))))


#|

(defmethod! generate-grid ((self tune) beatdur)
  (let* ((emptybeatfromfile '(((c maj7) nil) ((d maj7) nil) ((f maj7) nil) ((a maj7) nil) ((c m7) nil) ((d m7) nil) ((f m7) nil) ((a m7) nil) ((c 7) nil) ((d 7) nil) ((f 7) nil) ((a 7) nil)))
         (emptybeatlist (make-beat-list emptybeatfromfile)) 
         (o (NewImprovizer emptybeatlist)))
    (save-for-antescofo2 (thread-Beats (ImprovizeOnHarmGrid o (length (simplegrid self)) (simplegrid self)))
                        beatdur
                        (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "grid"))
                                       :name (format nil "~a-grid.txt" (tunename self))))))


; Tutorial based on musical data from "Garner.lisp"

(setf *current-tune* Garnerloop_tune)
(tunename *current-tune*)
(setf (beatduration *current-tune*) 236)    ;BPM=254
(setf (double *current-tune*) t)       ;impros played half tempo in Max BPM=127, thus computed with a double grid (BPM=254)

(setf (oracle *current-tune*) garnerrightoracle)    ;!!!!!!!!!!!!!!!!!!!!!!!!!
(setf (liveoracle *current-tune*) garnerleftoracle) ;!!!!!!!!!!!!!!!!!!!

(simplegrid *current-tune*)           ;when (double *current-tune*) = t ---> (simplegrid *current-tune*) is doubled

(setf (max-continuity garnerrightoracle) 5
      (max-continuity garnerleftoracle) 1000)

(setf impro (mix-poly-impro *current-tune*))

(pgmout 4 3) (pgmout 4 4) (pgmout 4 5)
(play (merger (beats->chseq impro 236 0)
              (beats->chseq (make-clocks 32 236 2) 236 0)))

(Stop-Player *general-player*)

(setf (maxpolyphony *current-tune*) 3)

(midi-to-beatlist (MidiFixedBuff *current-tune*))
(load-realtime-data-and-generate-impros *current-tune* 1)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Marc 25/4/12 Oracles on channels 3-8  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! reset-liveoracle ((self tune) num_oracle) 
  (setf (gethash num_oracle (oracletable self)) (NewImprovizer))
  (when (member num_oracle '(3 4 5)) (setf (max-continuity (gethash num_oracle (oracletable self))) 1000))
  (gethash num_oracle (oracletable self)))

;  (case num_oracle 
;    (3 (setf (oraclechan3 self) (NewImprovizer nil 536 :max-continuity 1000)))
;    (4 (setf (oraclechan4 self) (NewImprovizer nil 536 :max-continuity 1000)))
;    (5 (setf (oraclechan5 self) (NewImprovizer nil 536 :max-continuity 1000)))
;    (6 (setf (oraclechan6 self) (NewImprovizer)))
;    (7 (setf (oraclechan7 self) (NewImprovizer)))
;    (8 (setf (oraclechan8 self) (NewImprovizer)))))


(defmethod! load-realtime-data ((self tune) num_oracle)

(if (probe-file (MidiFixedBuff self))   ;Jerome 28/01/2012, WARNING: this test is necessary to avoid OM crash !!!
    (let* ((res (midi-to-beatlist (MidiFixedBuff self))) (defaultbeatdur) (beatlist))
      (format *om-stream* "~%===== LOADING MIDI ORACLE ~a ... =====~%" num_oracle)
      (if res
        (progn (setf defaultbeatdur (second res) beatlist (first res))
               (setf (beatduration self) defaultbeatdur)
               (let ((o (NewImprovizer beatlist defaultbeatdur)))
                 (setf (gethash num_oracle (oracletable self)) o (max-continuity o) 1000))  ;;;(if (<= i 5) 1000 8)))

               ;(setf (gethash num_oracle (oracletable self)) (NewImprovizer beatlist defaultbeatdur))
               ;(when (member num_oracle '(3 4 5)) (setf (max-continuity (gethash num_oracle (oracletable self))) 1000))
               (gethash num_oracle (oracletable self))

               ;(case num-oracle 
               ;  (3 (setf (oraclechan3 self) (NewImprovizer beatlist defaultbeatdur :max-continuity 1000)))
               ;  (4 (setf (oraclechan4 self) (NewImprovizer beatlist defaultbeatdur :max-continuity 1000)))
               ;  (5 (setf (oraclechan5 self) (NewImprovizer beatlist defaultbeatdur :max-continuity 1000)))
               ;  (6 (setf (oraclechan6 self) (NewImprovizer beatlist defaultbeatdur)))
               ;  (7 (setf (oraclechan7 self) (NewImprovizer beatlist defaultbeatdur)))
               ;  (8 (setf (oraclechan8 self) (NewImprovizer beatlist defaultbeatdur)))
               ;)
                     )
        (print "Error : empty MidiFixedBuff, no oracle is built")))
  (print "Error : no MidiFixedBuff for current tune")
))


;Jerome 11 mars 2013
(defmethod! load-saved-oracle-in-oraclechan ((self tune) s num_oracle) 
  (if (probe-file s)
      (progn (setf (gethash num_oracle (oracletable self)) (load-improvizer s)))
        (print "Impro oracle not found")
))


;-------------------------------------------------------------------------------------
;JEROME 03/07/12 : TEST INTEGRATION FONCTION FLORIANE
(defmethod! load-realtime-data-with-substitution ((self tune) num_oracle)

(if (probe-file (MidiFixedBuff self))   ;Jerome 28/01/2012, WARNING: this test is necessary to avoid OM crash !!!
    (let* ((res (learn-and-substitute path_labelized (MidiFixedBuff self))) (defaultbeatdur) (beatlist))
      (format *om-stream* "~%===== LOADING MIDI ORACLE ~a ... =====~%" num_oracle)
      (if res
        (progn (setf defaultbeatdur (second res) beatlist (first res))
               (setf (beatduration self) defaultbeatdur)
               (let ((o (NewImprovizer beatlist defaultbeatdur)))
                 (setf (gethash num_oracle (oracletable self)) o (max-continuity o) 1000)) 
               (gethash num_oracle (oracletable self))
               )
        (print "Error : empty MidiFixedBuff, no oracle is built")))
  (print "Error : no MidiFixedBuff for current tune")
))







(defmethod! generate-multi-oracle ((self tune))
  (let ((or-table (oracletable self)) (impro1) (impro2)
        (dir1 (append (pathname-directory (tunedir self)) (list (tunename self) "new")))
        (dir2 (append (pathname-directory (tunedir self)) (list (tunename self) "new2")))
        (l (mapcar #'(lambda (x) (format nil "~2,'0d" x)) (cdddr (reverse (multiple-value-list (get-decoded-time)))))))

    (format *om-stream* "~%===== GENERATING A MIX OF MULTI ORACLES FOR COMPING ANTESCOFO \"mnote2\" ... =====~%")
    (setf impro-mnote2 (mix-impro-multi-oracle self (loop for k from 3 to 7 collect (gethash k or-table)) 3))

    (format *om-stream* "~%===== GENERATING A MIX OF MULTI ORACLES FOR SOLO ANTESCOFO ... =====~%")
    (setf impro-mnote (mix-impro-multi-oracle self (loop for k from 8 to 14 collect (gethash k or-table)) 8))

    (if impro-mnote 
        (save-for-antescofo impro-mnote
                            (beatduration self) 
                            (make-pathname :directory dir1
                                           :name (format nil "~a-new-~a.~a.~a-~ah~a.txt" 
                                                         (tunename self)   ;Marc 26/4/12 Back to a dated name
                                                                ;---> Max chooses the last impro in the folder "new"
                                                 ;thanks to (format nil "~2,'0d" x) which writes 01, 02, etc. instead of 1, 2
                                                         (first l) (second l) (third l) (fourth l) (fifth l)))); Jerome 04/03/12
      (print "Solo impro is empty, no file for antescofo 1"))
    (if impro-mnote2
        (save-for-antescofo2 impro-mnote2
                             (beatduration self) 
                             (make-pathname :directory dir2
                                            :name (format nil "~a-new2-~a.~a.~a-~a-~a.txt" (tunename self)
                                                          (first l) (second l) (third l) (fourth l) (fifth l)))); Jerome 04/03/12
      (print "Comping impro is empty, no file for antescofo 2 \"mnote2\""))))


(defmethod mix-impro-multi-oracle ((self tune) (oraclelist list) refchannel)          
  (loop for i from refchannel 
        for o in oraclelist                 
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i) 
                      (if (<= (maxetat o) 1) 
                        (progn (format *om-stream*  "Empty oracle~%") nil)
                        (progn (when (/= (beatduration self) (RefTempo o)) 
                                 (SetReftempo o (beatduration self)))     ;'oracle N' is adapted to the beat duration of the tune
                          (ImprovizeOnHarmGrid o (length (simplegrid self)) (simplegrid self)))))
        with mix = nil
       
        do (loop for x in impro 
                 do (setf (MidiSet x)       ;fix channels: 3-7 for oracles on "mnote2", 8-14 for oracles on "mnote"
                          (loop for 5uple in (MidiSet x) collect (append (nthcar 4 5uple) (list i)))))
        (if (null mix) (setf mix impro)
          (loop for x in impro for y in mix 
                do (setf (MidiSet y) (sort (append (MidiSet y) (MidiSet x)) #'<= :key #'second))))
        finally return (progn (format *om-stream* "-----------------------~%") (thread-Beats mix (beatduration self)))))



#|
(defmethod mix-impro-multi-oracle ((self tune))          
  (loop for i from 1 to 4 
        for o = (let ((o (case i (1 (oracle1 self)) (2 (oracle2 self)) (3 (oracle3 self)) (4 (oracle4 self)))))
                  (when (/= (beatduration self) (RefTempo o)) (SetReftempo o (beatduration self)))
                   o)                 ;'oracle N' is adapted to the beat duration of the tune
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i) 
                      (ImprovizeOnHarmGrid o (length (simplegrid self)) (simplegrid self)))
        with mix = nil
        do (loop for x in impro 
                 do (setf (MidiSet x)          ;fix channels: 3, 4, 5, 6 for oracles 1 to 4
                          (loop for 5uple in (MidiSet x) collect (append (nthcar 4 5uple) (list (+ 2 i))))))
        (if (null mix) (setf mix impro)
          (loop for x in impro for y in mix 
                do (setf (MidiSet y) (sort (append (MidiSet y) (MidiSet x)) #'<= :key #'second))))
        finally return (thread-Beats mix (beatduration self))))
  |#  




(defmethod! save-oracle ((self tune) num-oracle)
            (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
              (save-improvizer 
               (gethash num-oracle (oracletable self)) 
               (format nil "~a/~a" 
                       path_dir_live_oracles
                       (format nil "~a-OrChan~D-~a.~a.~a-~ah~a.or" (tunename self) num-oracle (first l) (second l) (third l) (fourth l) (fifth l))))))

                      
                         
#|
(om-inspect (oracle1 *current-tune*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TESTS UZESTE 16 mars 2012;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! load-realtime-data ((self tune) num-oracle)

(if (probe-file (MidiFixedBuff self))   ;Jerome 28/01/2012, WARNING: this test is necessary to avoid OM crash !!!
    (let* ((res (midi-to-beatlist (MidiFixedBuff self))) (defaultbeatdur) (beatlist) (impro) (maxbeatdur) (neworacle))
      (format *om-stream* "~%===== LOADING MIDI ORACLE ~a ... =====~%" num-oracle)
      (when res
        (setf defaultbeatdur (second res) beatlist (first res))
        (setf (beatduration self) defaultbeatdur)
        (setf maxbeatdur defaultbeatdur)
        (if (= num-oracle 1) (setf (oracle1 self) (NewImprovizer beatlist defaultbeatdur))
          (setf (oracle2 self) (NewImprovizer beatlist defaultbeatdur)))))
  (print "Error : no MidiFixedBuff for current tune")
))

(defmethod! generate-oracle1-oracle2 ((self tune))
  (let* ((impro))
    (format *om-stream* "~%===== GENERATING A MIX OF ORACLE 1 AND 2 ... =====~%")

    (setf impro (mix-oracle1-oracle2 self))
    (save-for-antescofo impro
                        (beatduration self) 
                        (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "new"))
                                       :name (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
                                               (format nil "~a-new.oracle1-oracle2.txt" (tunename self)))))))

(defmethod mix-oracle1-oracle2 ((self tune))          
  (when (/= (beatduration self) (RefTempo (oracle1 self))) (SetReftempo (oracle1 self) (beatduration self))) ;'oracle' is adapted to the beat duration of the tune
  (when (/= (beatduration self) (RefTempo (oracle1 self))) (SetReftempo (oracle1 self) (beatduration self)))
  (let ((impro1 (ImprovizeOnHarmGrid (oracle1 self) (length (simplegrid self)) (simplegrid self)))
        (impro2 (ImprovizeOnHarmGrid (oracle2 self) (length (simplegrid self)) (simplegrid self)))
        (impro))
    (loop for x in impro1 do (setf (MidiSet x)          ;change channels
                                  (loop for 5uple in (MidiSet x) collect (append (nthcar 4 5uple) (list 3)))))  
    (loop for x in impro1 for y in impro2
          do (setf (MidiSet x) 
                   (sort (append (MidiSet x) 
                                 (loop for 5uple in (MidiSet y) collect (append (nthcar 4 5uple) (list 4)))) 
                         #'<= :key #'second)))     
    (setf impro (thread-Beats impro1 (beatduration self)))
))


|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Marc 3/3/2012 bug in Max: "midibuff.mid" erased as soon as 'init' in "On"

(defmethod! load-realtime-data-and-generate-impros ((self tune) length_impro)    ;;;;???????????   length_impro

(if (probe-file (MidiFixedBuff self))   ;Jerome 28/01/2012, WARNING: this test is necessary to avoid OM crash !!!
    (progn

      (format *om-stream* "~%===== LOADING MIDI AND GENERATING N POLYPHONIES ... =====~%")
      (let* ((res (midi-to-beatlist (MidiFixedBuff self))) (defaultbeatdur) (beatlist) (impro) (maxbeatdur) (neworacle))
        (when res

          (setf defaultbeatdur (second res) beatlist (first res))

          (setf (beatduration self) defaultbeatdur)
          (setf maxbeatdur defaultbeatdur)
          (setf neworacle (NewImprovizer beatlist defaultbeatdur))
          (setf (liveoracle self) (add-improvizer neworacle (liveoracle self)))
          (set-start-point (liveoracle self) 1)
          (format *om-stream* "-----------------------~%~a~%" (tunename self))

          (setf impro (mix-poly-impro self (liveoracle self)))
          (when (double self) (setf impro (halftempo impro (beatduration self)) maxbeatdur (* defaultbeatdur 2)))
          (save-for-antescofo impro
                              maxbeatdur
                              (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "new"))
                                             :name (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
                                                     (format nil "~a-new.~avoices.txt" (tunename self) (maxpolyphony self)))))))
      (format *om-stream* "==== end of impro =====~%"))
  (print "Error : no MidiFixedBuff for current tune")
))

(defmethod mix-poly-impro ((self tune) (oracle oracle))             
  (when (/= (beatduration self) (RefTempo oracle))         ;Marc 27/2/2012
    (SetReftempo oracle (beatduration self)))              ;'oracle' is adapted to the beat duration of the tune
  (let ((impro (ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))))
    (loop for x in impro do (setf (MidiSet x)          
                                  (loop for 5uple in (MidiSet x) collect (append (nthcar 4 5uple) (list 3))))) ;channel 3
    (loop for i from 2 to (maxpolyphony self)        ;mix other impros  
          do (set-start-point oracle         
                              (* i (floor (/ (maxetat oracle) (* 2 (maxpolyphony self))))))    
          (loop for x in impro       
                for x1 in (ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))
                do (setf (MidiSet x) 
                         (sort (append (MidiSet x) 
                                       (loop for 5uple in (MidiSet x1) collect (append (nthcar 4 5uple) (list (+ 2 i))))) 
                               #'<= :key #'second))))     
    (setf impro (thread-Beats impro (beatduration self)))
))

(defmethod! generate-offline-impros ((self tune) maxbeatdur)
  (let* ((impro))
    (setf (beatduration self) maxbeatdur)
    (set-start-point (oracle self) 1)
    (format *om-stream* "-----------------------~%~a~%" (tunename self))

    (setf impro (mix-poly-impro self (oracle self)))
    (when (double self) (setf impro (halftempo impro (beatduration self)) maxbeatdur (* maxbeatdur 2)))
    (save-for-antescofo impro
                        maxbeatdur
                        (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "new"))
                                       :name (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
                                               (format nil "~a-new.~avoices.txt" (tunename self) (maxpolyphony self)))))))

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



#|
(defmethod! generate-offline-impros ((self tune) beatdur)
  (loop for i from 1 to 10
        do (save-for-antescofo (thread-Beats (ImprovizeOnHarmGrid (oracle self) (length (simplegrid self)) (simplegrid self)))
                               beatdur
                               (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "linear"))
                                              :name (format nil "~a-linear~a.txt" (tunename self) i))))
  (format *om-stream* "==== end of impro =====~%"))
|#


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









(defun save-for-antescofo (beatlist beatduration &optional absolute-path-filename)  ;&optional slower-ratio)   
  (when (null absolute-path-filename)                                               ; slower-ratio = 2 -> beat = half notes instead of quarter notes
    (setf absolute-path-filename path_bufferAntescofo))

  (ensure-directories-exist absolute-path-filename)
  (with-open-file 
      (ss absolute-path-filename 
          :direction :output :if-exists :supersede)
    ;(format ss "BPM ~a~%" (float (/ 60000 beatduration)))
    ;Since tempo may change during impro, it should not be included in the impro files
    ;(but adapted during performance by just modifying the tempo value of Antescofo through the [tempo $1] message using a float in BPM
    (setf beatlist (transfer-syncopated-event beatlist beatduration))   ;MARC 2011/6/27
    (setf beatlist (add-grid-to-beatlist beatlist beatduration))
    (loop with c-beatlist = (mapcar #'clone-object beatlist)
          for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
          do (progn 
               (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second))   ;BUG 2011/6/14 'sort' is needed for deltatimes
               ;(if (or (null slower-ratio) (= (mod i slower-ratio) 1)) 
               (format ss "NOTE  60 1.0 beat~a~%" i)     ;'beat' is used as a cue to allow the placement at the current beat when loading a file in Antescofo 
               (format ss "	GFWD for-killall {~%")  ;Marc 1/3/2012 "killall" need GFWD in Antescofo
               ;  (format ss "NOTE  0 1.0~%"))    ;a silence is not an event (= not activated by 'nextevent' in Antescofo)
               (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                     do (format ss "    ~a   mnote ~a ~a 0.0.~a ~a~%" 
                                (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))   ; deltatime in floats
                                (MEPitch event)     
                                (MEVel event) 
                                (round (* (/ (MEDur event) beatduration) 480))   ; durations in ticks (= 1/480th of a beat)
                                (MEChannel event)))
               (format ss "    }~%")
))))


;MARC 15/11/2011
; writes with "mnote2" instead of "mnote" -> for the second Antescofo with fixed tempo (accompaniment)

(defun save-for-antescofo2 (beatlist beatduration &optional absolute-path-filename)    ;;; ATTENTION = save-for-antescofo2
  (when (null absolute-path-filename)                                              
    (setf absolute-path-filename path_bufferAntescofo))

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
               (format ss "NOTE  60 1.0 beat~a~%" i) 
               (format ss "	GFWD for-killall {~%")
               (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                     do (format ss "    ~a   mnote2 ~a ~a 0.0.~a ~a~%"        ;;;;;; MNOTE2 instead of MNOTE  (for a second Antescofo devoted to comping)
                                (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))   ; deltatime in floats
                                (MEPitch event)     
                                (MEVel event) 
                                (round (* (/ (MEDur event) beatduration) 480))   ; durations in ticks (= 1/480th of a beat)
                                (MEChannel event)))
               (format ss "    }~%")
))))



(defun add-grid-to-beatlist (beatlist beatduration)   
  (loop for beat in beatlist for c-beat = (clone-object beat)
        do (setf (MidiSet c-beat) (append (make-grid-event-for-beat c-beat beatduration) (MidiSet c-beat))) 
        collect c-beat))

(defun transfer-syncopated-event (beatlist beatduration)
  (loop with c-beatlist = (mapcar #'clone-object beatlist)
        for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil))
        do (let ((transferredevent (if (null nextbeat) nil
                                     (loop for event in (MidiSet nextbeat)      ; syncopated events must be transferred to the previous beat
                                           when (< (MEOnset event) 0) do (setf (MidiSet nextbeat) ;BUG 2011/5/31 delete does NOT make physical modif. here!
                                                                               (delete event (MidiSet nextbeat)))
                                           and collect (progn (setf (MEOnset event) (+ (MEOnset event) beatduration)) event)))))
             (setf (MidiSet beat) (append (MidiSet beat) transferredevent)))
        finally return (progn (loop for evt in (MidiSet (first c-beatlist)) do (setf (MEOnset evt) (max (MEOnset evt) 0)))
                         c-beatlist)))

;WARNING: 'beats' must have the same duration (whereas it might not be the case in all examples, see ImprotekTutorial)

;(save-for-antescofo (thread-Beats beats12) 682)

; DELTATIME IN MAX: (http://www.cycling74.com/docs/max5/vignettes/core/maxtime_syntax.html)
;    - makenote object: duration can be a "Tempo-Relative Time Value" represented in bars/beats/units as a single value, 
;      with three integers separated by periods, for example 2.3.240, which is 2 measures (bars), 3 beats, and 240 ticks 
;      (ticks represent 1/480th of a quarter note).
; DELTATIME IN 'ANTESCOFO': 
;    - deltatimes between 'events' are represented as floats indicating the number of beats or a fraction of a beat.
;    - 'events' of the "score language" (i.e. NOTE 60 1.0.) are passed each time the space key is pressed in Max.
;    - 'actions' activated by the 'events' are notes to be played, which are coded by a deltime and a "send message" received by a Max r-object "mnote", 
;      which activates a makenote object by sending it pitch velocity duration (tempo-relative time) and channel.
;    The initial tempo is given by an event named BPM. In Max, user activating each event (for instance by tapping the space key) must fit the initial tempo value.
;    Then one can modify the tempo provided variations are done smoothly.




