; J. Nika, Nov. 2011
;
; Fonctions réalisant la construction des couples d'oracles d'harmonisation et d'arrangement à partir de fichiers MIDI formatés.

(in-package :om)

; Deplacé dans Work_directory_Global Jérôme 9/01/12
;(defparameter channel_melo 2)
;(defparameter channel_harmo 1)


;/!\ list of beats -> (beatlist_melo beatlist_harmo)
(defun separate_melo-harmo ( list_of_beats beatdur_list_of_beats ) 
(setf beats-melo
      (loop for beat in list_of_beats          
            collect 
            (list (first beat) 
                  (loop for midi_quintuple1 in (second beat) if (= (fifth midi_quintuple1) channel_melo) collect midi_quintuple1 )
                  )))
;===================================
;(setf beatlist-melo (thread-Beats (make-beat-list beats-melo) beatdur_list_of_beats))
(setf beatlist-melo (make-beat-list beats-melo))
;(play (beats->chseq beatlist-melo beatdur 0))

(setf beats-harmo
      (loop for beat in list_of_beats          
            collect 
            (list (first beat) 
                  (loop for midi_quintuple2 in (second beat) if (= (fifth midi_quintuple2) channel_harmo) collect midi_quintuple2 )
                  )))
;(setf beatlist-harmo (thread-Beats (make-beat-list beats-harmo) beatdur_list_of_beats))
(setf beatlist-harmo (make-beat-list beats-harmo))
;(play (beats->chseq beatlist-harmo beatdur 0))
(list beatlist-melo beatlist-harmo))
#|
; JEROME 15/05/2013 : y a t il toujours besoin d'une fonction séparant les pistes ? A l'époque on avait juste un oracle live ou tout était mélangé...
; Mais si, peut toujours servir, dans ce cas faire une liste de canaux en deuxieme argument et en sortie on aura seulement les beatlists correspondant à ces canaux
(defun extract-channels-beatlist (beatlist list-wanted-channels)
  (let* ((list-beats (first beatlist)))
    (loop for beat in list-beats
          ))
|#

;----------
;(oracles-from-midi_Melo-Harmo path_pourOracle)
;(om-inspect (load-improvizer save_path_oracle_melo_harmo))
;(om-inspect (gethash oracle_melo_harm_name *available-MeloHarmOracles*))
;(om-inspect (load-improvizer save_path_oracle_voicings))
;(om-inspect (gethash oracle_melo_harm_name *available-VoicingsOracles*))
;---------

;ORACLES A LA MAIN
;(setf path_file "Users/Nika/Desktop/Uzeste_propre/Solos_ch2/Jaime_solo_ch2.mid")
;(oracles-from-midi_Melo-Harmo path_file)

;;;;;;;;MODIFICATION JEROME 28/01/2012
(defun learn-oracles-from-buffer-melo-harmo ( path_file )
(if (probe-file path-file)
    (progn
      (setf beats_beatdur (midi-to-beats path_file))
      (format *om-stream* "========= FICHIER MIDI IMPORTE ========= ~%")
      (setf beats (first beats_beatdur) beat_duration (second beats_beatdur) )
      
      (setf Melo-Harmo (separate_melo-harmo beats beat_duration))
      (format *om-stream* "========= MELO ET HARMO SEPARES ========= ~%")
      (setf beatlist_melo (first Melo-Harmo) beatlist_harmo (second Melo-Harmo))
      
      (setf melobeats_Melo (beats->melobeats beatlist_melo))
      (format *om-stream* "========= MELO CONVERTIE BEATLIST ========= ~%")
;(setf melobeatlist_Melo (make-melobeatlist melobeats_Melo))
      (setf oracleMelo
            (loop with o1 = (NewImprovizer) for i from 0 to (1- (length melobeats_Melo)) do (learn-event o1 (nth i melobeats_Melo)) finally return o1))
      (format *om-stream* "========= ORACLE MELO OK ========= ~%")
;(setf melobeats_Harmo beatlist_harmo)
;(setf melobeats_Harmo (make-beats-from-voicings Harmo))
;(setf oracleHarmo
 ;     (loop with o = (NewImprovizer) for i from 0 to (1- (length melobeats_Harmo)) do (learn-event o (nth i melobeats_Harmo)) finally return o))

      (setf oracleHarmo
            (loop with o2 = (NewImprovizer) for i from 0 to (1- (length beatlist_harmo)) do (learn-event o2 (nth i beatlist_harmo)) finally return o2))
      (format *om-stream* "========= ORACLE HARMO OK ========= ~%")


      (setf (RefTempo oracleMelo) beat_duration (RefTempo oracleHarmo) beat_duration)
      (setf (max-continuity oracleMelo) 1000 (max-continuity oracleHarmo) 1000)
      (set-start-region oracleMelo (list 0 (1-(NbEvent? oracleMelo))))
      (set-start-region oracleHarmo (list 0 (1-(NbEvent? oracleHarmo))))

      (format *om-stream* "========= START-REGION BEATDURATION ET MAX CONT UPDATED ========= ~%")


;(setf *current-MeloHarmOracle* oracleMelo *current-VoicingOracle* oracleHarmo)

      (multiple-value-bind 
          (second minute hour date month year day-of-week dst-p tz)
          (get-decoded-time)
        (setf oracle_melo_harm_name (format nil "~a-OracleMeloHarm_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.or" 
                                            (tunename *current-tune*)
                                            date
                                            month
                                            year
                                            hour
                                            minute
                                            second)
              oracle_voicings_name (format nil "~a-OracleVoicing_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.or" 
                                           (tunename *current-tune*)
                                           date
                                           month
                                           year
                                           hour
                                           minute
                                           second)
              )
        )
      
      (format *om-stream* "========= NOMS CREES ========= ~%")
      
      
      (setf save_path_oracle_melo_harmo (format nil "~a/~a" path_dir_oracle_melo_harmo oracle_melo_harm_name) 
            save_path_oracle_voicings (format nil "~a/~a" path_dir_oracle_voicings oracle_voicings_name))
      (format *om-stream* "========= PATH CREES ========= ~%")
      
      
      
      (ensure-directories-exist save_path_oracle_melo_harmo)
      (ensure-directories-exist save_path_oracle_voicings)
      (format *om-stream* "========= ENSURE DIRECTORIES OK ========= ~%")
      
      
;(progn
      
      (save-improvizer oracleMelo save_path_oracle_melo_harmo)
      (save-improvizer oracleHarmo save_path_oracle_voicings)
      (format *om-stream* "========= IMPROVIZERS SAVED ========= ~%")
      
;)
      
      
; JEROME REVIEW 14/05 : NON ! MESSAGE ET TOUT A FAIRE DANS FICHIER COMMUNICATION AVEC MAX ! PAS DANS LA FONCTION ELLE MEME ENFIN !!!!!
;ATTENTION FAIRE TEST : QUE SI SERVEUR OUVERT !!!!
;A CHANGER : ENVOYER TOUTE LA LISTE, COMME CA ON PEUT Y AJOUTER UN CLEAR EN ARRIVANT DANS MAX ET DONC TRAITER COMME LES GRILLES
#|
      (setf message '("/info-available_MeloHarmOracles"))
      (setf m (append message (list oracle_melo_harm_name)))
      (osc-send m host_server prtSnd)
      
      
      (setf message '("/info-available_VoicingsOracles"))
      (setf m (append message (list oracle_voicings_name)))
      (osc-send m host_server prtSnd)
      
      (format *om-stream* "=========MESSAGES ENVOYES ========= ~%")
|#

      
;====================TEMPORAIRE !!!!!===================
      (setf oracleMelo nil oracleHarmo nil)
      )
  (print "Error : no 'melo-harmo' midi file to create oracles")
)

)




(defun oracle-voicing-from-midi_Harmo ( path_file )
(setf beats_beatdur (midi-to-beats path_file))
(setf beats (first beats_beatdur) beat_duration (second beats_beatdur) )

(setf Melo-Harmo (separate_melo-harmo beats beat_duration))
(setf beatlist_harmo (second Melo-Harmo))



(setf oracleHarmo
      (loop with o2 = (NewImprovizer) for i from 0 to (1- (length beatlist_harmo)) do (learn-event o2 (nth i beatlist_harmo)) finally return o2))

(setf (RefTempo oracleHarmo) beat_duration)


(setf (max-continuity oracleHarmo) 1000)

(setf *current-VoicingOracle* oracleHarmo)
(set-start-region oracleHarmo (list 0 (1-(NbEvent? oracleHarmo))))

(multiple-value-bind 
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
  (setf oracle_voicings_name (format nil "~a-OracleVoicing_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.or" 
                                     (tunename *current-tune*)
                                     date
                                     month
                                     year
                                     hour
                                     minute
                                     second)
        )
  )


(setf save_path_oracle_voicings (format nil "~a/~a" path_dir_oracle_voicings oracle_voicings_name))

(ensure-directories-exist save_path_oracle_voicings)



(progn

(save-improvizer oracleHarmo save_path_oracle_voicings)

(setf (gethash oracle_voicings_name *available-VoicingsOracles*) (load-improvizer save_path_oracle_voicings)))


; JEROME REVIEW 15/05 : NON A FAIRE DANS HANDLING MESSAGES FROM MAX ! PAS DANS LA FONCTION !
;ATTENTION FAIRE TEST : QUE SI SERVEUR OUVERT !!!!
(setf message '("/info-available_VoicingsOracles"))
(setf m (append message (list oracle_voicings_name)))
(osc-send m host_server prtSnd)


)



; JEROME REVIEW 15/05 : INUTILE ? Il y a une fonction dans beatlist.lisp (add-improvizer) qui en concatène 2. (Mais avec fausse relation entre fin du 1er et début du second)
(defun concatenate-improvizers (improvizers_list)
  (let* ((improvizer (make-instance 'Improvizer
                                    :comparateur 'CompareEvents
                                    :lrsMode t
                                    )))
    
    (loop for i from 0 to (1- (length improvizers_list)) 
          do (loop for j from 0 to (maxetat (nth i improvizers_list))
                   do (learn-event improvizer (elt (vectext (nth i improvizers_list)) j)))
          )

    improvizer)
; QUE METTRE COMME REF TEMPO ? COMME DURATION DANS CHAQUE ETAT ?
  )

;POUR TEST
;---------
;(setf p1 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/Jaime_solo.or" p2 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/Dicidenbas_solo.or")
;(setf o1 (load-improvizer p1) o2 (load-improvizer p2))
;(om-inspect (concatenate-improvizers (list o1 o2))) 


#|

(setf p1 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/ItDontMeanAThing_basse.or" 
      p2 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/ItDontMeanAThing_solo.or"
      p3 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/SongForMyFather_basse.or"
      p4 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/SongForMyFather_solo.or"
      p5 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/StolenMoments_basse.or"
      p6 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/StolenMoments_solo.or"
      )
(setf o1 (load-improvizer p1) o2 (load-improvizer p2) o3 (load-improvizer p3) o4 (load-improvizer p4) o5 (load-improvizer p5) o6 (load-improvizer p6) )
(setf big_oracle_meloharmo (concatenate-improvizers (list o1 o2 o3 o4 o5 o6))) 
;(om-inspect big_oracle_meloharmo)
(save-improvizer big_oracle_meloharmo "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/MegaOracleRealBook_melo2.or" )


;(om-inspect (load-improvizer  "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/MegaOracleRealBook4.or" ))

(setf pp1 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/ItDontMeanAThing_accomp+basse.or" 
      pp2 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/SongForMyFather_accomp+basse.or"
      pp3 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/StolenMoments_accomp+basse.or"
      )
(setf oo1 (load-improvizer pp1) oo2 (load-improvizer pp2) oo3 (load-improvizer pp3))
(setf big_oracle_voicings (concatenate-improvizers (list oo1 oo2 oo3))) 
;(om-inspect big_oracle_voicings)
(save-improvizer big_oracle_voicings "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/MegaOracleRealBook_accomp2.or" )



(setf p1 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/AliceInWonderland_solo.or" 
      p2 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/AuPrivave_solo.or"
      p3 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/BluesForAlice_solo.or"
      )
(setf o1 (load-improvizer p1) o2 (load-improvizer p2) o3 (load-improvizer p3))
(setf big_oracle_meloharmo (concatenate-improvizers (list o1 o2 o3))) 
;(om-inspect big_oracle_meloharmo)
(save-improvizer big_oracle_meloharmo "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/MegaOracleJose_melo.or" )


;(om-inspect (load-improvizer  "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_melo_harmo/MegaOracleRealBook4.or" ))

(setf pp1 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/AliceInWonderland_accomp.or" 
      pp2 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/AuPrivave_accomp.or"
      pp3 "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/BluesForAlice_accomp.or"
      )
(setf oo1 (load-improvizer pp1) oo2 (load-improvizer pp2) oo3 (load-improvizer pp3))
(setf big_oracle_voicings (concatenate-improvizers (list oo1 oo2 oo3))) 
;(om-inspect big_oracle_voicings)
(save-improvizer big_oracle_voicings "Users/Nika/Desktop/Antescofo~_Max_UB/_Corpus/Oracles_voicings/MegaOracleJose_accomp.or" )


|#


















