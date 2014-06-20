(in-package :om)

; HandleMaxMessages.lisp
;------------------------------------------------------------------------------------------------------------
; Handle messages received from the Max/MSP interface
;
; J�r�me Nika - March 11th 2013
;------------------------------------------------------------------------------------------------------------

(defparameter *print_received_from_Max* 1)

(defun handle-messages-from-max (message)
  (setf m message)  ; ?????
   ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
  ;JEROME 16/07/13 : BRICOLAGE AVANT HOMOGENEISATION MESSAGES VENANT DU PATCH MAX / DIRECTEMENT D'ANTESCOFO
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
  (if (= *print_received_from_Max* 1) 
      (progn
        (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
        (format *om-stream* "Type : ~a~%" (type-of message))
        (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
        (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
        (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))
  
 
  (cond
   
   ;Dans tune.lisp
   ( (string= (car message) "/launchTR")
     (launch-realtime *current-tune* (beatduration *current-tune*) host_server 3008 1 7415 7416)
     )


   ;*********ARRETER PROCESSUS TR ***********
   ;=====================================KILL TR==================================
   ; TEST !
   ; Kill process (MultithreadsTR.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/killTR")
     (end-realtime *current-tune*)
     )

   ;*********MODIFICATION DU RESET POUR RELANCER LES WAITS !!!!  ***********
   ;!!!!!!! A REMETTRE COMME AVANT APRES EXPERIENCE !!! !!!!!!!!!!!!!!!!!!!!!!!
   ; POUR L'INSTANT DANS ANTESCOFO TR !!!
   ;=====================================KILL TR==================================
   ;toOM11 - Calls "reset-oraclechan" (Tune.lisp)
   ;-----------------------------------------------------------------------------------
   ;Jerome 29/04/2013 : ex "reset-liveoracle"
   ;JEROME 01/09/13 : ATOMICITY DANS L'APPEL ICI... BONNE IDEE ???? LE FAIRE PLUS BAS !!!!!

   ;( (string= (car message) "/reset-oraclechan") (sys:setup-atomic-funcall 
   ;                                               'reset-realtime-oraclechan 
   ;                                               *current-tune* 
   ;                                               (nth 1 message) 
   ;                                               host_server 3008 
   ;                                               1) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd ))
   ;( (string= (car message) "/reset-oraclechan") (reset-realtime-oraclechan *current-tune* (nth 1 message) host_server 3008 1) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd ))
   ( (string= (car message) "/reset-oraclechan") 
     ;(reset-oraclechan *current-tune* (nth 1 message)) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd )
     (progn
       ; /!\ Ghost of channel 14 is used for the realtime improvizer;
       (if (and (= (nth 1 message) 14) (RealTimeSystem *current-tune*)) 
           (let ((RTimprov (NewRealTimeImprovizer)))
                                     (setf (max-continuity RTimprov) 1000)
                                     (setf (sharedData (RealTimeSystem *current-tune*)) RTimprov))
         (reset-oraclechan *current-tune* (nth 1 message)))
         (print "Oracle empty")
         ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd )))



 ;=====================================NAVIGATION TR UNE ETAPE==================================
   ; TEST !
   ; REMOUVZEUTOUDOU
   ;toOM21 - Calls "Improvize-play-next-state" (Improvizer.lisp)
   ;---------------------------------------------------------------------
  #|
   VOIR COMMENT MARCHE L'OSC !!!! 
   AVEC TRAITEMENT HABITUEL ON OBTIENT :
    RECEIVED_FROM_MAX : (#(0 0 0 0 0 0 0 1) (/antescofo/improvize_next_step ((eb maj7) (eb maj7) (db maj7) (db maj7))))
    Type : cons
    Car : #(0 0 0 0 0 0 0 1), de type : (simple-array (unsigned-byte 8) (8))
    Cdr : ((/antescofo/improvize_next_step ((eb maj7) (eb maj7) (db maj7) (db maj7)))), de type : cons
    nth 1 : (/antescofo/improvize_next_step ((eb maj7) (eb maj7) (db maj7) (db maj7))), de type : cons
|#
   ;****Premiers pas dans le TR : beat par beat****
   ((string= (car message) "/antescofo/improvize_next_step")
     (progn
       ;1)JOUER DANS OM
       ;--------------
       ;(Improvize-play-next-state (gethash 8 (oracletable *current-tune*)) (eval (read-from-string (nth 1 message))) (beatduration *current-tune*))
       ;2)JOUER DANS MAX
       ;---------------
       (setf beatforAntescofo 
             (Improvize-groupAnte-next-state (gethash 8 (oracletable *current-tune*)) (eval (read-from-string (nth 1 message))) (beatduration *current-tune*) 1))
       (format *om-stream* "~a" beatforAntescofo)
       (osc-send (list "/OM/improbeat" beatforAntescofo) host_server 3008) ;/!\/!\/!\/!\/!\ : PORT ANTESCOFO
       ))



   ;toOM7 - Calls "set-current-tune" (LoadImprotek.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/set-current-tune") ; Jerome 29/04/2013 ex message "set-tune" (changed in Max)
     (set-current-tune (car (cdr message))) (print "Grid loaded") ( osc-send '("/executed_request" "Grid : loaded") host_server prtSnd ) )

   ;toOM1 - Calls "load-midibuff-in-oraclechan" (Tune.lisp)
   ;-------------------------------------------------------------------
   ( (string= (car message) "/load-midibuff-in-oraclechan") 
     (if (probe-file (MidiFixedBuff *current-tune*))
         ;Jerome 29/04/2013 : ex "load-realtime-data"
         (progn 
           (load-midibuff-in-oraclechan *current-tune*  (nth 1 message))
           (print "Sequence learnt") ( osc-send '("/executed_request" "Sequence learnt") host_server prtSnd ) )
       (progn (print "Error : no MidiFixedBuff for current tune") ( osc-send '("/error" "Error : no MidiFixedBuff for current tune") host_server prtSnd ))))

   ;toOM2 - Calls "set-max-continuity-oraclechan" (Tune.lisp)
   ;----------------------------------------------------
   ( (string= (car message) "/set-max-continuity-oraclechan") 
     ;(setf (max-continuity (gethash (nth 2 message) (oracletable *current-tune*))) (car (cdr message)))
     ;Jerome 29/04/2013
     
     (progn
       ; /!\ Ghost of channel 14 is used for the realtime improvizer
       (if (= (nth 2 message) 14)
           (setf (max-continuity (sharedData (RealTimeSystem *current-tune*))) (car (cdr message)))
         (set-max-continuity-oraclechan *current-tune* (nth 2 message) (car (cdr message))))
       (print "New max-continuity in current oracle") ( osc-send '("/executed_request" "New max-continuity in liveoracle") host_server prtSnd )))

   ;toOM3 - Calls "generate-polychan-impro-for-antescofo " (Antescofo.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/generate-polychan-impro-for-antescofo") ;Jerome 29/04/2013 : ex "gen-polychan-oracletable" (changed in Max)
     (generate-polychan-impro-for-antescofo  *current-tune*) ;Jerome 29/04/2013 : ex "generate-impro-for-antescofo"
     (print "Impro generated") ( osc-send '("/executed_request" "Impro generated") host_server prtSnd ))

   ;Jerome 17/05/2013
   ;toOM10 - Calls "harmonize&arrange&saveAntescofo2-improchan" (HarmonizationNew.lisp) 
   ; Ex "/load-midibuff-and-gen-harmonized-impro"
   ;-------------------------
   ( (string= (car message) "/harmonize&arrange&saveAntescofo2-improchan") 
     (if (gethash (nth 1 message) (improtable *current-tune*))
         (progn (harmonize&arrange&saveAntescofo2-improchan *current-tune* (nth 1 message) *current-meloharmoracle* *current-voicingoracle*) 
           (print "Impro harmonized and arranged") ( osc-send '("/executed_request"  "Impro harmonized and arranged") host_server prtSnd ) )
       (progn (print "Error : No saved impro to harmonize") ( osc-send '("/error" "No saved impro to harmonize") host_server prtSnd ))
       ))

   ;toOM4 - Calls "save-oraclechan" (Tune.lisp)
   ;Jerome 29/04/2013 : ex "save-oracle"
   ;---------------------------------------------------------------------
   ( (string= (car message) "/save-oraclechan") 
     (progn
       ; /!\ Ghost of channel 14 is used for the realtime improvizer
       (if (= (nth 1 message) 14)
           (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
              (save-improvizer 
               (sharedData (RealTimeSystem *current-tune*))
               (format nil "~a/~a" 
                       path_dir_live_oracles
                       (format nil "~a-RealTimeOr-~a.~a.~a-~ah~a.or" (tunename *current-tune*) (first l) (second l) (third l) (fourth l) (fifth l)))))
         (save-oraclechan *current-tune* (nth 1 message)))
         (print "Oracle saved") ( osc-send '("/executed_request" "Oracle saved") host_server prtSnd )))

   ;toOM6 - Calls "set-start-region-oraclechan" (Improvizer.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/set-start-region-oraclechan") 
     (progn
     ; /!\ Ghost of channel 14 is used for the realtime improvizer
     (if (= (nth 3 message) 14)
         (setf (start-region (sharedData (RealTimeSystem *current-tune*))) 
               (list (floor (* (nth 1 message) (/ (max 0 (1- (maxetat (sharedData (RealTimeSystem *current-tune*))))) 127))) 
                     (floor (* (nth 2 message) (/ (max 0 (1- (maxetat (sharedData (RealTimeSystem *current-tune*))))) 127)))))
       (set-start-region-oraclechan (gethash (nth 3 message) (oracletable *current-tune*)) (nth 1 message) (nth 2 message) 0 127))
     (print "New start region") ( osc-send '("/executed_request" "New start region") host_server prtSnd )))


   ;toOM8 - Calls "load-saved-oracle-in-oraclechan" (Tune.lisp)
   ;----------------------------------------------------------------------------
   ;( (string= (car message) "/load-saved-oracle-in-oraclechan") 
   ;  (impro_oracle_initialization (nth 1 message) (nth 2 message)) (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ))
   ( (string= (car message) "/load-saved-oracle-in-oraclechan") 
     (if (probe-file (car (cdr message)))
         (progn
           ; /!\ Ghost of channel 14 is used for the realtime improvizer
           (if (= (nth 1 (cdr message)) 14)
               (if (probe-file (nth 0 (cdr message)))
                   (setf (sharedData (RealTimeSystem *current-tune*)) (load-realtimeImprovizer-fromSavedImprovize (nth 0 (cdr message))))
                 (print "Impro oracle not found"))
             (load-saved-oracle-in-oraclechan *current-tune* (nth 0 (cdr message)) (nth 1 (cdr message)))) 
           (print "Impro oracle loaded") 
           (osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ))
       (progn (print "Error : Impro oracle not found") ( osc-send '("/error" "Impro oracle not found") host_server prtSnd ))))

   ; TODO
   ;toOM9 - Calls "learn-oracles-from-buffer-melo-harmo" (HarmonizationTools.lisp)
   ;-------------------------
   ( (string= (car message) "/learn-oracles-from-buffer-melo-harmo") ;Jerome 29/04/2013 : ex "/create-oracles-from-buffer-melo-harmo" (changed in Max)
     (if (probe-file path_midibuff_melo-harmo)
         (progn (learn-oracles-from-buffer-melo-harmo path_midibuff_melo-harmo) (print "Buffer 'oraclized'") ( osc-send '("/executed_request" "Buffer 'oraclized'") host_server prtSnd ))
       (progn (print "Error : No midibuff_melo_harmo") ( osc-send '("/error" "No midibuff_melo_harmo") host_server prtSnd ))))


   ;toOM12- Calls "/reset-tabou-oraclechan" (Tune.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/reset-tabou-oraclechan") 
     (progn
       ; /!\ Ghost of channel 14 is used for the realtime improvizer
       (if (= (nth 1 message) 14)
           (ResetTabou (sharedData (RealTimeSystem *current-tune*)))
         (reset-tabou-oraclechan *current-tune* (nth 1 message)))
       (print "Reset tabou in current oracle") ( osc-send '("/executed_request" "Reset tabou in current oracle") host_server prtSnd ) ))
   ;Jerome 29/04/2013 : ex "ResetTabou (Improvizer.lisp)"

   ;toOM13 - Calls "stop-osc-server" (StartServerMax.lisp)
   ;--------------------------------------------------------------------
   ( (string= (car message) "/stop-osc-server") (stop-osc-server) (print "Server stopped")) ;Jerome 29/04/2013 : ex "kill-server"


   ;toOM14 - Calls "load-saved-oracle-in-meloharm-oracle" (LoadImprotek.lisp)
   ;-------------------------
   ( (string= (car message) "/load-saved-oracle-in-MeloharmOracle") 
     (if (probe-file (car (cdr message)))
         ;Jerome 29/04/2013 : ex "voicings_oracle_initialization
         (progn (load-saved-oracle-in-meloharm-oracle (car (cdr message))) (print "MeloHarm oracle loaded") ( osc-send '("/executed_request" "MeloHarm oracle loaded") host_server prtSnd ) )
       (progn (print "Error : MeloHarm oracle not found") ( osc-send '("/error" "MeloHarm oracle not found") host_server prtSnd ))))

   ;toOM15 - Harmo/Arrangement, calls "max-continuity" (Improvizer.lisp)
   ;---------------------------------------
   ( (string= (car message) "/set-max-continuity-MeloharmOracle") 
     (setf (max-continuity *current-meloharmoracle*) (car (cdr message))) (print "New max-continuity in meloharmoracle") ( osc-send '("/executed_request" "New max-continuity in meloharmoracle") host_server prtSnd ) )


   ;toOM16 - Calls "load-saved-oracle-in-meloharm-oracle" (LoadImprotek.lisp)
   ;-------------------------
   ( (string= (car message) "/load-saved-oracle-in-VoicingsOracle ") 
     (if (probe-file (car (cdr message)))
         ;Jerome 29/04/2013 : ex "voicings_oracle_initialization
         (progn (load-saved-oracle-in-voicings-oracle (car (cdr message))) (print "Voicings oracle loaded") ( osc-send '("/executed_request" "Voicings oracle loaded") host_server prtSnd ) )
       (progn (print "Error : Voicings oracle not found") ( osc-send '("/error" "Voicings oracle not found") host_server prtSnd ))))

   ;toOM17 - Harmo/Arrangement, calls "max-continuity" (Improvizer.lisp)
   ;---------------------------
   ( (string= (car message) "/set-max-continuity-VoicingsOracle") 
     (setf (max-continuity *current-voicingoracle*) (car (cdr message))) (print "New max-continuity in voicingsoracle") ( osc-send '("/executed_request" "New max-continuity in voicingsoracle") host_server prtSnd ) )

   ;TODO
   ;toOM18 - Calls "harmo-melody-midi" (HarmonizationHandlers.lisp)
   ; HARMONISATION
   ;---------------------------------------------------------------
   ( (string= (car message) "/harmo-melody-midi") 
     (if (probe-file (car (cdr message)))
                     (progn (harmo-melody-midi (car (cdr message))) (print "Harmonisation OK") ( osc-send '("/executed_request" "Harmonisation OK") host_server prtSnd ) )
                     (progn (print "Error : no midi file") ( osc-send '("/error" "No midi file") host_server prtSnd ))))

   ;toOM19 - Calls "load-midibuff-and-gen-harmonized-impro" (HarmonizationHandlers.lisp) 
   ;TODO : group with "toOM10"
   ;-------------------------------------------------
   ( (string= (car message) "/load-real-time_harmonized-impros_midifile") 
     (if (probe-file (car (cdr message)))
         (progn (load-midibuff-and-gen-harmonized-impro (car (cdr message)) (nth 2 message)) 
           (print "Harmonization oracle melody from midifile OK") ( osc-send '("/executed_request"  "Harmonization oracle melody from midifile OK") host_server prtSnd ) )
       (progn (print "Error : no midi file") ( osc-send '("/error" "No midi file") host_server prtSnd ))))

   ;toOM20 - Calls "learn-oracles-from-buffer-melo-harmo" (HarmonizationTools.lisp)
   ; TODO : Group with "toOM9" 
   ;-------------------------
   ( (string= (car message) "/create-oracles-from-midifile-melo-harmo") 
     (if (probe-file path_midibuff_melo-harmo)
         (progn (learn-oracles-from-buffer-melo-harmo (car (cdr message))) (print "Midi file 'oraclized'") ( osc-send '("/executed_request" "Midi file 'oraclized'") host_server prtSnd ))
       (progn (print "Error : No midibuff_melo_harmo") ( osc-send '("/error" "No midibuff_melo_harmo") host_server prtSnd ))))

   ;Unknown messages
   ;-----------------
   ( t (print "OM : UNKNOWN MESSAGE FROM MAX") ( osc-send '("/error" "OM : UNKOWN MESSAGE FROM MAX") host_server prtSnd))))