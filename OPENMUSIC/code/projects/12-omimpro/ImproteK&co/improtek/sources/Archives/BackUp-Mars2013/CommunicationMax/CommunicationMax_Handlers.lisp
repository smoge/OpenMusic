; J. Nika, Nov. 2011
;
; Initialisation et manipulation des objets *current_* (tune, liveoracle, MeloHarmOracle, VoicingOracle) en réponse aux messages OSC reçus de Max.
; (Application des fonctions définies dans les sections concernées de la librairie aux objets *current_*) 
;
;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================
(in-package :om)



;-------------------------------------------------------------------------------------
;JEROME 03/07/12 : TEST INTEGRATION FONCTION FLORIANE APPEL A LA FONCTION DANS ANTESCOFO.LISP
(defun load-realtime_buffer_without_gen_with_substitution (num_oracle) 
(if (probe-file (MidiFixedBuff *current-tune*))
  (progn (load-realtime-data-with-substitution *current-tune* num_oracle) (print "MidiBuff loaded"))
  (print "Error : No MidiFixedBuff for *current-tune*")
  )
)




;--------------------------------------------------------------------------------------
;EXPERIENCE UZESTE 16/03/12 2 ORACLES 

(defun load-realtime_buffer_without_gen (num_oracle) 
(if (probe-file (MidiFixedBuff *current-tune*))
  (progn (load-realtime-data *current-tune* num_oracle) (print "MidiBuff loaded"))
  (print "Error : No MidiFixedBuff for *current-tune*")
  )
)


(defun gen_poly_o1_o2 () 
(generate-oracle1-oracle2 *current-tune*)
)


;--------------------------------------------------------------------------------------
;Test Marc 4 oracles 25/4/12

(defun gen_poly_oracletable () 
(generate-multi-oracle *current-tune*)
)


;--------------------------------------------------------------------------------------


(defmethod set-start-region-oraclechan ((oracle Improvizer) begin end)
  (set-start-region oracle (list (floor (* begin (/ (max 0 (1- (maxetat oracle))) 127))) 
                                 (floor (* end (/ (max 0 (1- (maxetat oracle))) 127))))))


 











; Initialization of *current-tune*
; Response to OSC message "/set_tune"
;--------------------------------------------------------------------------------------------------------------------
(defun tune_initialization (s) 
(print s)
(setf req_tune (gethash s *available-grids*))
(setf *current-tune* (clone req_tune))                        ;;;;;;;;;; MARC 5/3/2012
(generate-grid *current-tune* (beatduration *current-tune*))
)


;;;;;Marc 24/1/13
; Initialization of improvisation oracle
; Response to OSC message "/load-and-set-impro_oracle"
;--------------------------------------------------------------------------------------------------------------------
(defun impro_oracle_initialization (s num_oracle) 
  (if (probe-file s)
      (progn (setf (gethash num_oracle (oracletable *current-tune*)) (load-improvizer s)))
        (print "Impro oracle not found")
))


;;;;;MODIFICATION JEROME 28/01/12
; Initialization of harmonization oracle
; Response to OSC message "/set_MeloHarmOracle"
;--------------------------------------------------------------------------------------------------------------------
(defun meloharm_oracle_initialization (s) 
  (if (probe-file s)
      (progn (setf *current-MeloHarmOracle* (load-improvizer s)))
        (print "MeloHarm Oracle not found")
))

;;;;;MODIFICATION JEROME 28/01/12
; Initialization of arrangement oracle
; Response to OSC message "/set_VoicingOracle"
;--------------------------------------------------------------------------------------------------------------------
(defun voicings_oracle_initialization (s) 
  (if (probe-file s)
      (progn (setf *current-VoicingOracle* (load-improvizer s)))
    (print "MeloHarm Oracle not found")
  ;(generate-accomp-current-voicing-oracle *current-tune* 2)
))


;;;;;;;;;; MODIFICATION JEROME 28/01/12
; Generation of 10 impros inspired by *current-song*
; Response to OSC message "/load-realtime_buffer"
;--------------------------------------------------------------------------------------------------------------------
(defun load-realtime_buffer (length_impro) 
(if (probe-file (MidiFixedBuff *current-tune*))
  (progn (load-realtime-data-and-generate-impros *current-tune* length_impro) (print "MidiBuff loaded. Impro created."))
  (print "Error : No MidiFixedBuff for *current-tune*")
  )
)


;;;;;;;;;; NOUVEAU JEROME 28/01/12 - TODO
;;;; A VOIR AVEC MARC : arguments de load-realtime-data-and... : tune, alors que pourrait reservir si s'appliquait à un path de fichier midi...

; Generation of 10 impros inspired by the loaded midifile
; Response to OSC message "/load-realtime_midifile"
;--------------------------------------------------------------------------------------------------------------------
(defun load-realtime_midifile (length_impro path_midifile) 
(if (probe-file path_midifile)
  (print "Midifile loaded. Impro created.")
  (print "Error : No Midifile")
  )
)


; Generation of 10 grids adapted to *current-song* using substitution rules
; Response to OSC message "/generate-harmos-grid-subs"
;--------------------------------------------------------------------------------------------------------------------
(defun generate-harmos-grid-subs ()
  (generate-offline-harmos *current-tune* (beatduration *current-tune*))
)


; Reset liveoracle
; Response to OSC message "/reset-melody-oracle"
;--------------------------------------------------------------------------------------------------------------------
(defun reset-live (num_oracle) 
  (reset-liveoracle *current-tune* num_oracle)
  )




;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================


;;;;; MODIFICATION JEROME 28/01/12
; Harmonization of an oraclized melody
; Response to OSC message " /load-real-time_harmonized-impros_midifile"
;--------------------------------------------------------------------------------------------------------------------
(defun loadrealtime-and-harmo-impro-melody-midi (path_melotoharm length_impro)

(if (probe-file path_melotoharm)
    (progn
  (format *om-stream* "~%===== LOADING MIDI AND GENERATING AN IMPRO ... =====~%")
  (setf midifromfile (evts-from-midifile path_melotoharm)
;         (defaultbeatdur (round (/ (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile))))) 2)))   ;;;;;;MARC 2011/6/27 "/ 2" deleted!!!!!
;         (stretchedmidifromfile (timestretch (check-midifile-evts midifromfile) 0.5))
;         (beatlist (make-beat-list (clocked-evts->beats stretchedmidifromfile))))
        defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile))))))
        original_beatlist (make-beat-list (clocked-evts->beats midifromfile))
        (beatduration *current-tune*) defaultbeatdur
        beatdur defaultbeatdur)

  (setf res1 (check-clocks midifromfile) 
        clocksfromfile (first res1) quintuplesfromfile (second res1)) 
  
 
  (loop for i from 0 to (1- (length original_beatlist)) do (learn-event (liveoracle *current-tune*) (nth i original_beatlist)))

    
  


 
   ;====START-REGION !!!!
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
   ; (setf (max-continuity (liveoracle *current-tune*)) 1000)
    (set-start-region (liveoracle *current-tune*) (list 0 (1-(NbEvent?  (liveoracle *current-tune*)))))
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------
    ;---------------------------------------------------------------

 
;(setf impro_beatlist (thread-Beats (ImprovizeOnHarmGrid (liveoracle *current-tune*) (length (simplegrid *current-tune*)) (simplegrid *current-tune*)) (beatduration *current-tune*)))
(setf impro_beatlist (thread-Beats (ImprovizeOnHarmGrid (liveoracle *current-tune*) (* length_impro (length (simplegrid *current-tune*))) (simplegrid *current-tune*)) (beatduration *current-tune*)))
  




  (format *om-stream* "==== melody impro generated =====~%")
  ;(setf labels (mapcar 'harmLabel beatlist))


;(setf buffer_beatlist-beatdur (midi-to-beatlist path_melotoharm))
;(setf beatlist (first buffer_beatlist-beatdur) beatdur (second buffer_beatlist-beatdur)) 
 

  
  
  ;HARMONISATION AVEC "Current-Oracles"
  (setf harmo_chseq (harmonize-current_oracles impro_beatlist beatdur))




  ;(format *om-stream* "Labels : ~a~%" (nth 1 harmo_chseq))
  (progn (pgmout 4 1) (pgmout 4 2) 
    (setf mix (merger (beats->chseq impro_beatlist beatdur 0)
                      ;(harmonize-hermeto beatlist beatdur))))
                      (nth 0 harmo_chseq))))
                      
  

  ;ECRITURE FORMAT BEATLIST
  ;On découpe avec même clock,tempo,... que mélodie importée
  (setf Meloharmo (chord-seq->mf-info mix))



  (progn
   ;découper avec horloge précédemment obtenue
    (setf quintuples Meloharmo)
    (setf beatsfrommix2 (quintuples->beats clocksfromfile quintuples))
    (setf beatsfrommix3 (cut-beat-events clocksfromfile beatsfrommix2))           
    (setf beatsfrommix4 (set-relative-time-beat clocksfromfile beatsfrommix3))
    (setf beatsfrommix (mapcar #'list beatsfrommix4))
    
    ;entrelacer avec liste labels, puis puis make beat list
    (setf beats_w_labels (mapcar #'cons (nth 1 harmo_chseq ) beatsfrommix)))
    (setf mixMeloHarmo-beatlist (make-beat-list beats_w_labels))
    (setf beatlist-MeloHarmo1 (thread-Beats mixMeloHarmo-beatlist beatdur))
    
    ;-----------------------------------------------------------------------------------------------------------------------
    ;===============ECRITURE DANS LE MIDI ET DANS ANTESCOFO DES LABELS CALCULÉS PAR ORACLES SUR LE CANAL 15
    ;A CETTE ETAPE DANS BEATLIST HARMO :
    ;- Midiset :
    ;          - le solo capté (donc solo + grille canal 16
    ;          - mixé avec arrangement qu'on vient de calculer
    ;(- harm labels : ceux provenant de l'harmonisation ou de la lecture du canal 16 provenant du solo ? nou) 
    ;
    ; =====>>>>> ICI RAJOUTER DANS CANAL 15 LES LABELS PROVENANT DU CALCUL
    (setf beatlist-MeloHarmo (annote_chords_generated_harmo beatlist-MeloHarmo1))
    ;-----------------------------------------------------------------------------------------------------------------------

    ;(om-inspect beatlist-Meloharmo)

    ;======================
    ;(list-length beatsfrommix)
    ;(list-length (nth 1 harmo_chseq ))
    ;(list-length beatlist-MeloHarmo)


    ;(play mix)
    ;(Stop-Player *general-player*)
    ;(play (beats->chseq beatlist-MeloHarmo beatdur 10))
    ;(Stop-Player *general-player*)
    ;=======================
    
 


  ;SAUVEGARDE ANTESCOFO
    (format *om-stream* "==== IMPRO CALCULEE, SAUVEGARDE ANTESCOFO... =====~%")
    
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (setf absolute_path_filename (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "HarmonizedPhrases"))
                                                  :name (format nil "~a-~a__~a-~a_~a-~a__~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.txt" 
                                                                (tunename *current-tune*)
                                                                (max-continuity (liveoracle *current-tune*))
                                                                (name *current-MeloHarmOracle*)
                                                                (max-continuity *current-MeloHarmOracle*)
                                                                (name *current-VoicingOracle*)
                                                                (max-continuity *current-VoicingOracle*)
                                                                date
                                                                month
                                                                year
                                                                hour
                                                                minute
                                                                second))
            absolute_path_filename_midi (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "HarmonizedPhrases"))
                                                  :name (format nil "~a-~a__~a-~a_~a-~a__~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d";enlever le .mid 
                                                                (tunename *current-tune*)
                                                                (max-continuity (liveoracle *current-tune*))
                                                                (name *current-MeloHarmOracle*)
                                                                (max-continuity *current-MeloHarmOracle*)
                                                                (name *current-VoicingOracle*)
                                                                (max-continuity *current-VoicingOracle*)
                                                                date
                                                                month
                                                                year
                                                                hour
                                                                minute
    


                                                            second))))
;VERSION SANS LE NOM DES ORACLES UTILISES
;    (multiple-value-bind
;        (second minute hour date month year day-of-week dst-p tz)
;        (get-decoded-time)
;      (setf absolute_path_filename (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "HarmonizedPhrases"))
;                                                  :name (format nil "~a-HarmOraclePhrase_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.txt" 
;                                                                (tunename *current-tune*)
;                                                                date
;                                                                month
;                                                                year
;                                                                hour
;                                                                minute
;                                                                second))
;            absolute_path_filename_midi (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "HarmonizedPhrases"))
;                                                  :name (format nil "~a-HarmOraclePhrase_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d";enlever le .mid 
;                                                                (tunename *current-tune*)
;                                                                date
;                                                                month
;                                                                year
;                                                                hour
;                                                                minute
;                                                                second))))





    
    (ensure-directories-exist absolute_path_filename)
    
    ;(save-for-antescofo beatlist-MeloHarmo beatdur path_harmomelAntescofo)
    (save-for-antescofo beatlist-MeloHarmo beatdur absolute_path_filename)
    
    (format *om-stream* "==== OK !!! Tempo à ~a =====~%" beatdur)
    


    
    ;SAUVEGARDE MIDI
    (format *om-stream* "==== ECRITURE EN MIDI... =====~%")
    ;ProblemeHermeto : commenter ligne ci-dessous
    (save-as-midi-with-tempo (beats->chseq beatlist-MeloHarmo beatdur 0) beatdur absolute_path_filename_midi)
    (format *om-stream* "=========== OK !!! ===========~%")
    







    (setf comp_harmo_grid 
          (loop for i from 0 to (list-length (nth 1 harmo_chseq))
                collect (list (nth i (expand_grid (grid *current-tune*))) (nth i (nth 1 harmo_chseq)))
                )
    )
    (setf melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats impro_beatlist beatdur))))
    


  

    ;(print comp_harmo_grid)





;(setf mess '()) (setf indic '("/comparate-grid-harmo"))
 ;   (setf mess
 ;         (loop for i from 1 to (list-length comp_harmo_grid)
 ;               collect
 ;               (format nil "set 0 ~a ~a, set 1 ~a ~a" i (nth 0 (nth i comp_harmo_grid)) i (nth 1 (nth i comp_harmo_grid)))
  ;              append mess))
  ;  (setf m (append indic mess))
  ;  (osc-send m host_server prtSnd)



    (setf mess '()) (setf indic '("/comparate-grid-harmo"))
    (setf mess
          (loop for i from 1 to (list-length comp_harmo_grid)
                collect
                (format nil "set ~a 1 ~a, set ~a 0 ~a, set ~a 2 ~a" i (nth i melolist) i (nth 0 (nth i comp_harmo_grid)) i (nth 1 (nth i comp_harmo_grid)))
                append mess))
    (setf m (append indic mess))
    (osc-send m host_server prtSnd)
)

(print "No midi file found")
)

)





;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================

;;;;;; MODIFICATION JEROME 28/01/2012
; PLUS UTILISE, A REVOIR
; Harmonization of a monophonic midi file
; Response to OSC message "/harmo-melody-midi"
;--------------------------------------------------------------------------------------------------------------------
(defun harmo-melody-midi (path_melotoharm)
  
(if (probe-file path_melotoharm)
    (progn

  ;IMPORTER MIDIBUFF

  (setf midifromfile (evts-from-midifile path_melotoharm)) 
  (setf (res1 (check-clocks midifromfile))
        (clocksfromfile (first res1) quintuplesfromfile (second res1))
        (defaultbeatdur (round (om-mean (x->dx (mapcar 'first clocksfromfile)))))
        )
  
  
  (setf beatsfromfile2 (quintuples->beats clocksfromfile quintuplesfromfile))
  (setf beatsfromfile3 (cut-beat-events clocksfromfile beatsfromfile2))           
  (setf beatsfromfile4 (set-relative-time-beat clocksfromfile beatsfromfile3))
  (setf beatsfromfile (label-chord-beat beatsfromfile4))
  
  (setf beatlist (make-beat-list beatsfromfile) beatdur defaultbeatdur) ; ??????
  (setf labels (mapcar 'harmLabel beatlist))


;(setf buffer_beatlist-beatdur (midi-to-beatlist path_melotoharm))
;(setf beatlist (first buffer_beatlist-beatdur) beatdur (second buffer_beatlist-beatdur)) 
 

  
  
  ;HARMONISATION AVEC "Current-Oracles"
  (setf harmo_chseq (harmonize-current_oracles beatlist beatdur))



  ;(format *om-stream* "Labels : ~a~%" (nth 1 harmo_chseq))
  (progn (pgmout 4 1) (pgmout 4 2) 
    (setf mix (merger (beats->chseq (thread-Beats beatlist) beatdur 0)
                      ;(harmonize-hermeto beatlist beatdur))))
                      (nth 0 harmo_chseq))))
                      
  

  ;ECRITURE FORMAT BEATLIST
  ;On découpe avec même clock,tempo,... que mélodie importée
  (setf Meloharmo (chord-seq->mf-info mix))



  (progn
   ;découper avec horloge précédemment obtenue
    (setf quintuples Meloharmo)
    (setf beatsfrommix2 (quintuples->beats clocksfromfile quintuples))
    (setf beatsfrommix3 (cut-beat-events clocksfromfile beatsfrommix2))           
    (setf beatsfrommix4 (set-relative-time-beat clocksfromfile beatsfrommix3))
    (setf beatsfrommix (mapcar #'list beatsfrommix4))
    
    ;entrelacer avec liste labels, puis puis make beat list
    (setf beats_w_labels (mapcar #'cons (nth 1 harmo_chseq ) beatsfrommix)))
    (setf mixMeloHarmo-beatlist (make-beat-list beats_w_labels))
    (setf beatlist-MeloHarmo1 (thread-Beats mixMeloHarmo-beatlist beatdur))


    ;-----------------------------------------------------------------------------------------------------------------------
    ;===============ECRITURE DANS LE MIDI ET DANS ANTESCOFO DES LABELS CALCULÉS PAR ORACLES SUR LE CANAL 15
    ;A CETTE ETAPE DANS BEATLIST HARMO :
    ;- Midiset :
    ;          - le solo capté (donc solo + grille canal 16
    ;          - mixé avec arrangement qu'on vient de calculer
    ;(- harm labels : ceux provenant de l'harmonisation ou de la lecture du canal 16 provenant du solo ? nou) 
    ;
    ; =====>>>>> ICI RAJOUTER DANS CANAL 15 LES LABELS PROVENANT DU CALCUL
    (setf beatlist-MeloHarmo (annote_chords_generated_harmo beatlist-MeloHarmo1))
    ;-----------------------------------------------------------------------------------------------------------------------
    ;(om-inspect beatlist_Meloharmo)
    ;======================
    ;(list-length beatsfrommix)
    ;(list-length (nth 1 harmo_chseq ))
    ;(list-length beatlist-MeloHarmo)


    ;(play mix)
    ;(Stop-Player *general-player*)
    ;(play (beats->chseq beatlist-MeloHarmo beatdur 10))
    ;(Stop-Player *general-player*)
    ;=======================
    
 


  ;SAUVEGARDE ANTESCOFO
    (format *om-stream* "==== IMPRO CALCULEE, SAUVEGARDE ANTESCOFO... =====~%")
    
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (setf absolute_path_filename (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "HarmonizedPhrases"))
                                                  :name (format nil "~a-HarmoPhrase_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.txt" 
                                                                (tunename *current-tune*)
                                                                date
                                                                month
                                                                year
                                                                hour
                                                                minute
                                                                second))
            absolute_path_filename_midi (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "HarmonizedPhrases"))
                                                  :name (format nil "~a-HarmoPhrase_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d" 
                                                                (tunename *current-tune*)
                                                                date
                                                                month
                                                                year
                                                                hour
                                                                minute
                                                                second))))
    
    (ensure-directories-exist absolute_path_filename)
    (ensure-directories-exist absolute_path_filename_midi)
    
    (save-for-antescofo beatlist-MeloHarmo beatdur absolute_path_filename)
    
    (format *om-stream* "==== OK !!! Tempo à ~a =====~%" beatdur)
    
    
;SAUVEGARDE MIDI
    (format *om-stream* "==== ECRITURE EN MIDI... =====~%")
    (save-as-midi-with-tempo mix beatdur absolute_path_filename_midi)
    (format *om-stream* "=========== OK !!! ===========~%")
)
(print "No midi file found")
    )
)
