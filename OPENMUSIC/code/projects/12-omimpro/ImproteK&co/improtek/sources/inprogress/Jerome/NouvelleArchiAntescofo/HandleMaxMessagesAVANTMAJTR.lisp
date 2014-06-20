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

   ;toOM7 - Calls "set-current-tune" (LoadImprotek.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/set-current-tune") ; Jerome 29/04/2013 ex message "set-tune" (changed in Max)
     (set-current-tune (car (cdr message))) (print "Grid loaded") ( osc-send '("/executed_request" "Grid : loaded") host_server prtSnd ) )

   ;toOM1 - Calls "load-midibuff-in-oraclechan" (Tune.lisp)
   ;-------------------------------------------------------------------
   ( (string= (car message) "/load-midibuff-in-oraclechan") 
     (if (probe-file (MidiFixedBuff *current-tune*))
         ;Jerome 29/04/2013 : ex "load-realtime-data"
         (progn (load-midibuff-in-oraclechan *current-tune*  (nth 1 message)) (print "Sequence learnt") ( osc-send '("/executed_request" "Sequence learnt") host_server prtSnd ) )
       (progn (print "Error : no MidiFixedBuff for current tune") ( osc-send '("/error" "Error : no MidiFixedBuff for current tune") host_server prtSnd ))))

   ;toOM2 - Calls "set-max-continuity-oraclechan" (Tune.lisp)
   ;----------------------------------------------------
   ( (string= (car message) "/set-max-continuity-oraclechan") 
     ;(setf (max-continuity (gethash (nth 2 message) (oracletable *current-tune*))) (car (cdr message)))
     ;Jerome 29/04/2013
     (set-max-continuity-oraclechan *current-tune* (nth 2 message) (car (cdr message)))
     (print "New max-continuity in current oracle") ( osc-send '("/executed_request" "New max-continuity in liveoracle") host_server prtSnd ))

   ;toOM3 - Calls "generate-polychan-impro-for-antescofo " (Antescofo.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/generate-polychan-impro-for-antescofo") ;Jerome 29/04/2013 : ex "gen-polychan-oracletable" (changed in Max)
     (generate-polychan-impro-for-antescofo  *current-tune*) ;Jerome 29/04/2013 : ex "generate-impro-for-antescofo"
     (print "Impro generated") ( osc-send '("/executed_request" "Impro generated") host_server prtSnd ))

   ;toOM4 - Calls "save-oraclechan" (Tune.lisp)
   ;Jerome 29/04/2013 : ex "save-oracle"
   ;---------------------------------------------------------------------
   ( (string= (car message) "/save-oraclechan") (save-oraclechan *current-tune* (nth 1 message)) (print "Oracle saved") ( osc-send '("/executed_request" "Oracle saved") host_server prtSnd ))

   ;toOM6 - Calls "set-start-region-oraclechan" (Improvizer.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/set-start-region-oraclechan") 
     (set-start-region-oraclechan (gethash (nth 3 message) (oracletable *current-tune*)) (nth 1 message) (nth 2 message) 0 127) (print "New start region") ( osc-send '("/executed_request" "New start region") host_server prtSnd ) )


   ;toOM8 - Calls "load-saved-oracle-in-oraclechan" (Tune.lisp)
   ;----------------------------------------------------------------------------
   ;( (string= (car message) "/load-saved-oracle-in-oraclechan") 
   ;  (impro_oracle_initialization (nth 1 message) (nth 2 message)) (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ))
   ( (string= (car message) "/load-saved-oracle-in-oraclechan") 
     (if (probe-file (car (cdr message)))
         (progn (load-saved-oracle-in-oraclechan *current-tune* (nth 0 (cdr message)) (nth 1 (cdr message))) (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ) )
       (progn (print "Error : Impro oracle not found") ( osc-send '("/error" "Impro oracle not found") host_server prtSnd ))))

   ;toOM9 - Calls "harmonize&arrange&saveMixAsMidi-midifile" (HarmonizationNew.lisp) 
   ;-------------------------
   ( (string= (car message) "/harmonize&arrange&saveMixAsMidi-midifile") 
     (if (probe-file (nth 0 (cdr message)))
         (progn (harmonize&arrange&saveMixAsMidi-midifile (nth 0 (cdr message)) (nth 1 (cdr message)) *current-meloharmoracle* *current-voicingoracle* (nth 2 (cdr message)) (nth 3 (cdr message)) (nth 4 (cdr message))) 
           (print "Impro harmonized and arranged") ( osc-send '("/executed_request"  "Impro harmonized and arranged") host_server prtSnd ) )
       (progn (print "Error : No saved impro to harmonize") ( osc-send '("/error" "No saved impro to harmonize") host_server prtSnd ))))
   
   ;Jerome 17/05/2013
   ;toOM10 - Calls "harmonize&arrange&saveAntescofo2-improchan" (HarmonizationNew.lisp) 
   ; Ex "/load-midibuff-and-gen-harmonized-impro"
   ;-------------------------
   ( (string= (car message) "/harmonize&arrange&saveAntescofo2-improchan") 
     (if (gethash (nth 1 message) (improtable *current-tune*))
         (progn (harmonize&arrange&saveAntescofo2-improchan *current-tune* (nth 1 message) *current-meloharmoracle* *current-voicingoracle* (nth 2 message) (nth 3 message)) 
           (print "Impro harmonized and arranged") ( osc-send '("/executed_request"  "Impro harmonized and arranged") host_server prtSnd ) )
       (progn (print "Error : No saved impro to harmonize") ( osc-send '("/error" "No saved impro to harmonize") host_server prtSnd ))
       ))

   ;toOM11 - Calls "reset-oraclechan" (Tune.lisp)
   ;-----------------------------------------------------------------------------------
   ;Jerome 29/04/2013 : ex "reset-liveoracle"
   ( (string= (car message) "/reset-oraclechan") (reset-oraclechan *current-tune* (nth 1 message)) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd ))

   ;toOM12- Calls "/reset-tabou-oraclechan" (Tune.lisp)
   ;---------------------------------------------------------------------
   ( (string= (car message) "/reset-tabou-oraclechan") 
     (reset-tabou-oraclechan *current-tune* (nth 1 message)) (print "Reset tabou in current oracle") ( osc-send '("/executed_request" "Reset tabou in current oracle") host_server prtSnd ) )
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
   ( (string= (car message) "/load-saved-oracle-in-VoicingsOracle") 
     (if (probe-file (car (cdr message)))
         ;Jerome 29/04/2013 : ex "voicings_oracle_initialization
         (progn (load-saved-oracle-in-voicings-oracle (car (cdr message))) (print "Voicings oracle loaded") ( osc-send '("/executed_request" "Voicings oracle loaded") host_server prtSnd ) )
       (progn (print "Error : Voicings oracle not found") ( osc-send '("/error" "Voicings oracle not found") host_server prtSnd ))))

   ;toOM17 - Harmo/Arrangement, calls "max-continuity" (Improvizer.lisp)
   ;---------------------------
   ( (string= (car message) "/set-max-continuity-VoicingsOracle") 
     (setf (max-continuity *current-voicingoracle*) (car (cdr message))) (print "New max-continuity in voicingsoracle") ( osc-send '("/executed_request" "New max-continuity in voicingsoracle") host_server prtSnd ) )


   ;toOM20 - Calls "learn&save-harmo&voicings-improvizers-from-midifile" (HarmonizationNew.lisp)
   ;The first word of the message is "midibuff" if the current midibuff is the input for learning, or the absolute path of a midifile
   ;---------------------------------------------------------------------------------------------------------------------------------------
   ( (string= (car message) "/learn&save-harmo&voicings-improvizers-from-midifile") 
     (let ((input nil))
       (if (string= (nth 0 (cdr message)) "midibuff")
           (setf input (MidiFixedBuff *current-tune*))
         (setf input (nth 0 (cdr message))))
       (if (probe-file input)
             (progn (learn&save-harmo&voicings-improvizers-from-midifile 
                     input (nth 1 (cdr message)) (eval (read-from-string (nth 2 (cdr message)))) (eval (read-from-string (nth 3 (cdr message))))) 
               (print "Harmo/Arrang corpus learnt from midifile") 
               ( osc-send '("/executed_request" "Harmo/Arrang corpus learnt from midifile") host_server prtSnd ))
         (progn (print "Error : Cannot find the midifile") ( osc-send '("/error" "Cannot find the midifile") host_server prtSnd )))))

   ;Unknown messages
   ;-----------------
   ( t (print "OM : UNKNOWN MESSAGE FROM MAX") ( osc-send '("/error" "OM : UNKOWN MESSAGE FROM MAX") host_server prtSnd))))