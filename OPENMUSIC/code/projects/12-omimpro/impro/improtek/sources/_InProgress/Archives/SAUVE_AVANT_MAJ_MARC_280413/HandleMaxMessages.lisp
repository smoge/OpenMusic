(in-package :om)

; HandleMaxMessages.lisp
;------------------------------------------------------------------------------------------------------------
; Handle messages received from the Max/MSP interface
;
; Jérôme Nika - March 11th 2013
;------------------------------------------------------------------------------------------------------------

(defun handle-messages-from-max (message)
  (setf m message)  
  (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
  (format *om-stream* "Type : ~a~%" (type-of message))
  (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
  (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
  
  
  (cond

   ;toOM1 - Calls "load-realtime-data" (Antescofo.lisp -> Tune.lisp ?)
   ;-------------------------------------------------------------------
   ( (string= (car message) "/load-midibuff-in-oraclechan") 
     (if (probe-file (MidiFixedBuff *current-tune*))
         (progn (load-realtime-data *current-tune*  (nth 1 message)) (print "Sequence learnt") ( osc-send '("/executed_request" "Sequence learnt") host_server prtSnd ) )
       (progn (print "Error : no MidiFixedBuff for current tune") ( osc-send '("/error" "Error : no MidiFixedBuff for current tune") host_server prtSnd ))))

   ;toOM2 - Calls "max-continuity" (Oracle.lisp)
   ;----------------------------------------------------
   ( (string= (car message) "/set-max-continuity-oraclechan") 
     (setf (max-continuity (gethash (nth 2 message) (oracletable *current-tune*))) (car (cdr message)))
     (print "New max-continuity in current oracle") ( osc-send '("/executed_request" "New max-continuity in liveoracle") host_server prtSnd ))

   ;toOM3 - Calls "generate-multi-oracle" (Antescofo.lisp -> Tune.lisp ?)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/gen-impro-polychan-oracletable") (generate-multi-oracle *current-tune*) (print "Impro generated") ( osc-send '("/executed_request" "Impro generated") host_server prtSnd ))

   ;toOM4 - Calls "save-oracle" (Antescofo.lisp -> Tune.lisp ?)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/save-oraclechan") (save-oracle *current-tune* (nth 1 message)) (print "Oracle saved") ( osc-send '("/executed_request" "Oracle saved") host_server prtSnd ))

   ;toOM5 - Calls "generate-offline-harmos" (Antescofo.lisp -> Tune.lisp ?)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/gen-harmos-grid-subs") 
     (generate-offline-harmos *current-tune* (beatduration *current-tune*)) (print "10 harmos-subs generated") ( osc-send '("/executed_request" "10 harmo-subs generated") host_server prtSnd) )

   ;toOM6 - Calls "set-start-region-oraclechan" (Improvizer.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/set-start-region-oraclechan") 
     (set-start-region-oraclechan (gethash (nth 3 message) (oracletable *current-tune*)) (nth 1 message) (nth 2 message) 0 127) (print "New start region") ( osc-send '("/executed_request" "New start region") host_server prtSnd ) )

   ;toOM7 - Calls "tune-initialization" (LoadImprotek.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/set-tune") (tune-initialization (car (cdr message))) (print "Grid loaded") ( osc-send '("/executed_request" "Grid : loaded") host_server prtSnd ) )

   ;toOM8 - Calls "load-saved-oracle-in-oraclechan" (Antescofo.lisp -> Tune.lisp ?)
   ;----------------------------------------------------------------------------
   ;( (string= (car message) "/load-saved-oracle-in-oraclechan") 
   ;  (impro_oracle_initialization (nth 1 message) (nth 2 message)) (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ))
   ( (string= (car message) "/load-saved-oracle-in-oraclechan") 
     (if (probe-file (car (cdr message)))
         (progn (load-saved-oracle-in-oraclechan *current-tune* (nth 0 (cdr message)) (nth 1 (cdr message))) (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ) )
       (progn (print "Error : Impro oracle not found") ( osc-send '("/error" "Impro oracle not found") host_server prtSnd ))))

   
   ;toOM9 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/create-oracles-from-buffer-melo-harmo") 
     (if (probe-file path_midibuff_melo-harmo)
         (progn (oracles-from-midi_melo-harmo path_midibuff_melo-harmo) (print "Buffer 'oraclized'") ( osc-send '("/executed_request" "Buffer 'oraclized'") host_server prtSnd ))
       (progn (print "Error : No midibuff_melo_harmo") ( osc-send '("/error" "No midibuff_melo_harmo") host_server prtSnd ))))

   ;toOM10 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/load-real-time_harmonized-impros_buffer") 
     (if (probe-file path_midibuff)
         (progn (loadrealtime-and-harmo-impro-melody-midi path_midibuff (nth 1 message)) 
           (print "Harmonization oracle melody from buffer OK") ( osc-send '("/executed_request"  "Harmonization oracle melody from buffer OK") host_server prtSnd ) )
       (progn (print "Error : no midi buff") ( osc-send '("/error" "No midibuff") host_server prtSnd ))
       ))

   ;toOM11 - Calls "reset-liveoracle" (Antescofo.lisp -> Tune.lisp ?)
   ;-----------------------------------------------------------------------------------
   ( (string= (car message) "/reset-oraclechan") (reset-liveoracle *current-tune* (nth 1 message)) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd ))

   ;toOM12- Calls "ResetTabou" (Improvizer.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/reset-tabou-oraclechan") 
     (ResetTabou (gethash (nth 1 message) (oracletable *current-tune*))) (print "Reset tabou in current oracle") ( osc-send '("/executed_request" "Reset tabou in liveoracle") host_server prtSnd ) )

   ;toOM13 - Calls "kill-server" (LoadImprotek.lisp)
   ;--------------------------------------------------------------------
   ( (string= (car message) "/stop-osc-server") (kill-server) (print "Server stopped"))


   ;toOM14 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/load-saved-oracle-in-MeloharmOracle") 
     (if (probe-file (car (cdr message)))
         (progn (meloharm_oracle_initialization (car (cdr message))) (print "MeloHarm oracle loaded") ( osc-send '("/executed_request" "MeloHarm oracle loaded") host_server prtSnd ) )
       (progn (print "Error : MeloHarm oracle not found") ( osc-send '("/error" "MeloHarm oracle not found") host_server prtSnd ))))

   ;toOM15 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/set-max-continuity-MeloharmOracle") 
     (setf (max-continuity *current-meloharmoracle*) (car (cdr message))) (print "New max-continuity in meloharmoracle") ( osc-send '("/executed_request" "New max-continuity in meloharmoracle") host_server prtSnd ) )


   ;toOM16 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/load-saved-oracle-in-VoicingsOracle ") 
     (if (probe-file (car (cdr message)))
         (progn (voicings_oracle_initialization (car (cdr message))) (print "Voicings oracle loaded") ( osc-send '("/executed_request" "Voicings oracle loaded") host_server prtSnd ) )
       (progn (print "Error : Voicings oracle not found") ( osc-send '("/error" "Voicings oracle not found") host_server prtSnd ))))

   ;toOM17 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/set-max-continuity-VoicingsOracle") 
     (setf (max-continuity *current-voicingoracle*) (car (cdr message))) (print "New max-continuity in voicingsoracle") ( osc-send '("/executed_request" "New max-continuity in voicingsoracle") host_server prtSnd ) )

   ;toOM18 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/harmo-melody-midi") 
     (if (probe-file (car (cdr message)))
                     (progn (harmo-melody-midi (car (cdr message))) (print "Harmonisation OK") ( osc-send '("/executed_request" "Harmonisation OK") host_server prtSnd ) )
                     (progn (print "Error : no midi file") ( osc-send '("/error" "No midi file") host_server prtSnd ))))

   ;toOM19 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/load-real-time_harmonized-impros_midifile") 
     (if (probe-file (car (cdr message)))
         (progn (loadrealtime-and-harmo-impro-melody-midi (car (cdr message)) (nth 2 message)) 
           (print "Harmonization oracle melody from midifile OK") ( osc-send '("/executed_request"  "Harmonization oracle melody from midifile OK") host_server prtSnd ) )
       (progn (print "Error : no midi file") ( osc-send '("/error" "No midi file") host_server prtSnd ))))

   ;toOM20 - Harmo/Arrangement
   ;-------------------------
   ( (string= (car message) "/create-oracles-from-midifile-melo-harmo") 
     (if (probe-file path_midibuff_melo-harmo)
         (progn (oracles-from-midi_melo-harmo (car (cdr message))) (print "Midi file 'oraclized'") ( osc-send '("/executed_request" "Midi file 'oraclized'") host_server prtSnd ))
       (progn (print "Error : No midibuff_melo_harmo") ( osc-send '("/error" "No midibuff_melo_harmo") host_server prtSnd ))))

   ;Unknown messages
   ;-----------------
   ( t (print "OM : UNKNOWN MESSAGE FROM MAX") ( osc-send '("/error" "OM : UNKOWN MESSAGE FROM MAX") host_server prtSnd))))