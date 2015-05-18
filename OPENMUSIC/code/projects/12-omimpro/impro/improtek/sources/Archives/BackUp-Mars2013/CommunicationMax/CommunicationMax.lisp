; J. Nika, Nov. 2011
;
; Gère le serveur pour la communication avec Max, et définit la liste des messages pouvant être reçus et les fonctions à appeler.
;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================


(in-package :om)


(let ((file-lib-improtek (make-pathname :directory (reverse (cdr (reverse (pathname-directory *load-pathname*)))) :name "Improtek")))
  (if (om-standalone-p) (load file-lib-improtek) (compile&load file-lib-improtek)))



;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================
; DEFINITION VARIABLES GLOBALES
;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================

(defparameter prtRcv 7413)
(defparameter prtSnd 7414)
(defparameter host_server "127.0.0.1") 
(defvar *server* nil)


;(send-available_grids host_server prtRcv prtSnd)

;(osc-send '("/hello" "Communication with OM : OK") host_server prtSnd)

;(osc-send '("/hello" "Communication with OM : OK") host_server 7400)


;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================
; COMMUNICATION OSC AVEC MAX
;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================



;;;;;;;;;;;;;;;;;MODIFICATION JEROME 28/01/12
;INTERPRETE LES DIFFERENTS MESSAGES VENUS DE MAX ET LANCE EXECUTION DES ORDRES ADAPTES
;--------------------------------------------------------------------------------------
(defun handle-messages-from-max (message)

(setf m message)  

  (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
  (format *om-stream* "Type : ~a~%" (type-of message))
  (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
  (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))

  
  (cond
   ( (string= (car message) "Test?") (print "Test?") ( osc-send '("TEST OK !" 1) host_server prtSnd ))
   ( (string= (car message) "/set-tune") (tune_initialization (car (cdr message))) (print "Grid loaded") ( osc-send '("/executed_request" "Grid : loaded") host_server prtSnd ) )
   

;;GESTION ABSENCE ORACLE Marc 24/1/13
   ( (string= (car message) "/load-and-set-impro_oracle") 
  (if (probe-file (car (cdr message)))
      (progn (impro_oracle_initialization (nth 0 (cdr message)) (nth 1 (cdr message))) (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ) )
    (progn (print "Error : Impro oracle not found") ( osc-send '("/error" "Impro oracle not found") host_server prtSnd ))
))




; ORACLE 7 PASSE PAR SUBSITUTIONS FLORIANE
;--------------------------------------------------------------------------------------
;EXPERIENCE UZESTE 16/03/12 2 ORACLES ---> Marc 26/4/12 4 oracles    'gen_poly_oracletable' instead of 'gen_poly_o1_o2'
;JEROME 03/07/12 : TEST INTEGRATION FONCTION FLORIANE APPEL A LA FONCTION DANS ANTESCOFO.LISP

   ( (string= (car message) "/load-realtime_buffer_without_gen") 
     (if (probe-file (MidiFixedBuff *current-tune*))
         (if (= 7 (nth 1 message))
             (progn (load-realtime_buffer_without_gen_with_substitution 7) (print "Sequence learnt and Florianized") ( osc-send '("/executed_request" "Sequence learnt") host_server prtSnd ) )
         (progn (load-realtime_buffer_without_gen (nth 1 message)) (print "Sequence learnt") ( osc-send '("/executed_request" "Sequence learnt") host_server prtSnd ) ))
     (progn (print "Error : no MidiFixedBuff for current tune") ( osc-send '("/error" "Error : no MidiFixedBuff for current tune") host_server prtSnd ))
     )
     )

;;;;;;;;;;;;;;;;;;;;;;;;Marc 24/1/13

   ( (string= (car message) "/load-and-set-impro_oracle") (impro_oracle_initialization (nth 1 message) (nth 2 message)) 
     (print "Impro oracle loaded") ( osc-send '("/executed_request" "Impro oracle loaded") host_server prtSnd ))



   ( (string= (car message) "/gen_poly_oracletable") (gen_poly_oracletable) (print "Impro generated") ( osc-send '("/executed_request" "Impro generated") host_server prtSnd ))

   ( (string= (car message) "/reset-melody-oracle") (reset-live (nth 1 message)) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd ))

   ( (string= (car message) "/save-oracle") (save-oracle *current-tune* (nth 1 message)) (print "Oracle saved") ( osc-send '("/executed_request" "Oracle saved") host_server prtSnd ))


   ( (string= (car message) "/set-liveoracle_max-continuity") 
     (setf (max-continuity (gethash (nth 2 message) (oracletable *current-tune*))) (car (cdr message)))

     ;(case (nth 2 message) 
     ;            (3 (setf (max-continuity (oraclechan3 *current-tune*)) (car (cdr message))))
     ;            (4 (setf (max-continuity (oraclechan4 *current-tune*)) (car (cdr message))))
     ;            (5 (setf (max-continuity (oraclechan5 *current-tune*)) (car (cdr message))))
     ;            (6 (setf (max-continuity (oraclechan6 *current-tune*)) (car (cdr message))))
     ;            (7 (setf (max-continuity (oraclechan7 *current-tune*)) (car (cdr message))))
     ;            (8 (setf (max-continuity (oraclechan8 *current-tune*)) (car (cdr message))))
     ;                ) 
     (print "New max-continuity in current oracle") ( osc-send '("/executed_request" "New max-continuity in liveoracle") host_server prtSnd ) )


   ( (string= (car message) "/reset-liveoracle_tabou") (ResetTabou (gethash (nth 1 message) (oracletable *current-tune*)))
     (print "Reset tabou in current oracle") ( osc-send '("/executed_request" "Reset tabou in liveoracle") host_server prtSnd ) )

;--------------------------------------------------------------------------------------








;;GESTION ABSENCE DE BUFFER
   ( (string= (car message) "/load-realtime_buffer") 
     (if (probe-file (MidiFixedBuff *current-tune*))
         (progn (load-realtime_buffer (nth 1 message)) (print "Impro generated") ( osc-send '("/executed_request" "Impro generated") host_server prtSnd ) )
       (progn (print "Error : no MidiFixedBuff for current tune") ( osc-send '("/error" "Error : no MidiFixedBuff for current tune") host_server prtSnd ))
       ))


;;TODO
   ( (string= (car message) "/load-realtime_midifile") 
     (if (probe-file (MidiFixedBuff *current-tune*))
         (progn (load-realtime_buffer (nth 1 message)) (print "TODO") ( osc-send '("/error" "TODO") host_server prtSnd ) )
       (progn (print "Error : TODO") ( osc-send '("/error" "Error : TODO") host_server prtSnd ))
       ))


   ( (string= (car message) "/generate-harmos-grid-subs") (generate-harmos-grid-subs) (print "10 harmos-subs generated") ( osc-send '("/executed_request" "10 harmo-subs generated") host_server prtSnd) )
   
   ;;GESTION ABSENCE DE BUFFER
   ( (string= (car message) "/harmo-melody-buffer") 
     (if (probe-file path_midibuff)
         (progn (harmo-melody-midi path_midibuff) (print "Harmonisation buffer OK") ( osc-send '("/executed_request" "Harmonisation buffer OK") host_server prtSnd ) )
       (progn (print "Error : no midibuff") ( osc-send '("/error" "No midibuff") host_server prtSnd ))
       ))
   
   ;;GESTION ABSENCE DE BUFFER
   ( (string= (car message) "/harmo-melody-midi") 
     (if (probe-file (car (cdr message)))
                     (progn (harmo-melody-midi (car (cdr message))) (print "Harmonisation OK") ( osc-send '("/executed_request" "Harmonisation OK") host_server prtSnd ) )
                     (progn (print "Error : no midi file") ( osc-send '("/error" "No midi file") host_server prtSnd ))
    ))
                     
   
;   ( (string= (car message) "/reset-melody-oracle") (reset-live) (print "Oracle empty") ( osc-send '("/executed_request" "Oracle empty") host_server prtSnd ))
;;;;;;;;;;;;;;;;; Marc 26/4/12 see above

   ( (string= (car message) "/stop-osc-server") (kill-server) (print "Server stopped"))

   ;;GESTION ABSENCE DE BUFFER
   ( (string= (car message) "/loop-phrase") 
     (if (probe-file path_midibuff)
         (progn (loop_phrase path_midibuff) (print "Midibuff saved for Antescofo") ( osc-send '("/executed_request" "Midibuff saved for Antescofo") host_server prtSnd ))
       (progn (print "Error : no midi buff") ( osc-send '("/error" "No midibuff") host_server prtSnd ))
))
   
;;GESTION ABSENCE DE BUFFER
( (string= (car message) "/create-oracles-from-buffer-melo-harmo") 
  (if (probe-file path_midibuff_melo-harmo)
      (progn (oracles-from-midi_melo-harmo path_midibuff_melo-harmo) (print "Buffer 'oraclized'") ( osc-send '("/executed_request" "Buffer 'oraclized'") host_server prtSnd ))
    (progn (print "Error : No midibuff_melo_harmo") ( osc-send '("/error" "No midibuff_melo_harmo") host_server prtSnd ))
))

;;GESTION ABSENCE DE BUFFER
   ( (string= (car message) "/create-oracles-from-midifile-melo-harmo") 
     (if (probe-file path_midibuff_melo-harmo)
         (progn (oracles-from-midi_melo-harmo (car (cdr message))) (print "Midi file 'oraclized'") ( osc-send '("/executed_request" "Midi file 'oraclized'") host_server prtSnd ))
       (progn (print "Error : No midibuff_melo_harmo") ( osc-send '("/error" "No midibuff_melo_harmo") host_server prtSnd ))
))



;;GESTION ABSENCE ORACLE
( (string= (car message) "/load-and-set-meloharm_oracle") 
  (if (probe-file (car (cdr message)))
      (progn (meloharm_oracle_initialization (car (cdr message))) (print "MeloHarm oracle loaded") ( osc-send '("/executed_request" "MeloHarm oracle loaded") host_server prtSnd ) )
    (progn (print "Error : MeloHarm oracle not found") ( osc-send '("/error" "MeloHarm oracle not found") host_server prtSnd ))
))

;;GESTION ABSENCE ORACLE
   ( (string= (car message) "/load-and-set-voicings_oracle") 
     (if (probe-file (car (cdr message)))
         (progn (voicings_oracle_initialization (car (cdr message))) (print "Voicings oracle loaded") ( osc-send '("/executed_request" "Voicings oracle loaded") host_server prtSnd ) )
    (progn (print "Error : Voicings oracle not found") ( osc-send '("/error" "Voicings oracle not found") host_server prtSnd ))
))



   ;;GESTION ABSENCE DE BUFFER
   ( (string= (car message) "/load-real-time_harmonized-impros_buffer") 
     (if (probe-file path_midibuff)
         (progn (loadrealtime-and-harmo-impro-melody-midi path_midibuff (nth 1 message)) (print "Harmonization oracle melody from buffer OK") ( osc-send '("/executed_request"  "Harmonization oracle melody from buffer OK") host_server prtSnd ) )
       (progn (print "Error : no midi buff") ( osc-send '("/error" "No midibuff") host_server prtSnd ))
))
       
   
;;GESTION ABSENCE DE BUFFER
( (string= (car message) "/load-real-time_harmonized-impros_midifile") 
  (if (probe-file (car (cdr message)))
      (progn (loadrealtime-and-harmo-impro-melody-midi (car (cdr message)) (nth 2 message)) (print "Harmonization oracle melody from midifile OK") ( osc-send '("/executed_request"  "Harmonization oracle melody from midifile OK") host_server prtSnd ) )
       (progn (print "Error : no midi file") ( osc-send '("/error" "No midi file") host_server prtSnd ))
))



;   ( (string= (car message) "/set-liveoracle_max-continuity") (setf (max-continuity (liveoracle *current-tune*)) (car (cdr message))) (print "New max-continuity in liveoracle") ( osc-send '("/executed_request" "New max-continuity in liveoracle") host_server prtSnd ) )
; Marc 30/4/2012 see above



   ( (string= (car message) "/set-meloharmoracle_max-continuity") (setf (max-continuity *current-meloharmoracle*) (car (cdr message))) (print "New max-continuity in meloharmoracle") ( osc-send '("/executed_request" "New max-continuity in meloharmoracle") host_server prtSnd ) )

   ( (string= (car message) "/set-voicingsoracle_max-continuity") (setf (max-continuity *current-voicingoracle*) (car (cdr message))) (print "New max-continuity in voicingsoracle") ( osc-send '("/executed_request" "New max-continuity in voicingsoracle") host_server prtSnd ) )

   ( (string= (car message) "/set-distance-min-navigation") (setf *min_distance_melobeats* (car (cdr message))) (print "New min distance for navigation") ( osc-send '("/executed_request" "New min distance for navigation") host_server prtSnd ) )


   ( (string= (car message) "/set-start-region") 
     (set-start-region-oraclechan (gethash (nth 3 message) (oracletable *current-tune*)) (nth 1 message) (nth 2 message))
    
     ;(case (nth 3 message) 
     ;           (3 (set-start-region-oraclechan (oraclechan3 *current-tune*) (nth 1 message) (nth 2 message)))
     ;           (4 (set-start-region-oraclechan (oraclechan4 *current-tune*) (nth 1 message) (nth 2 message)))
     ;           (5 (set-start-region-oraclechan (oraclechan5 *current-tune*) (nth 1 message) (nth 2 message)))
     ;           (6 (set-start-region-oraclechan (oraclechan6 *current-tune*) (nth 1 message) (nth 2 message)))
     ;           (7 (set-start-region-oraclechan (oraclechan7 *current-tune*) (nth 1 message) (nth 2 message)))
     ;           (8 (set-start-region-oraclechan (oraclechan8 *current-tune*) (nth 1 message) (nth 2 message)))
     ;           )
 (print "New start region") ( osc-send '("/executed_request" "New start region") host_server prtSnd ) )


   ;( (string= (car message) "/set-path-save") (define_paths_save (car (cdr message))) (print "Path save defined") ( osc-send '("/executed_request" "Path_save_defined") host_server prtSnd ) )

   ;( (string= (car message) "/set-path-corpus") (define_paths_corpus (car (cdr message))) (print "Path corpus defined") ( osc-send '("/executed_request" "Path_corpus_defined") host_server prtSnd ) )

   ( t (print "OM : UNKNOWN MESSAGE FROM MAX") ( osc-send '("/error" "OM : UNKOWN MESSAGE FROM MAX") host_server prtSnd))
   )
  )



; LISTE DE FONCTIONS ENVOYANT INFOS A MAX
;----------------------------------------
;(defun send-available_grids (server_host portRcv portSnd)
;  (setf grid_list '()  message '("/info-available_grid"))
;  (setf grid_list (loop for e being the hash-key of *available-grids*; using (hash-value v)
;        collect e append grid_list))
;  (setf m (append message grid_list))
;  (osc-send m server_host portSnd)
;)


(defun send-available_grids (server_host portRcv portSnd)
  (setf grid_list '()  message '("/info-available_grid"))
  (setf grid_list (loop for e being the hash-key of *available-grids* using (hash-value v)
        collect (info_tune v) append grid_list))
  (setf m (append message grid_list))
  (osc-send m server_host portSnd)
)








; FERME LE SERVEUR
;---------------------------------
(defun kill-server ()
  (om-stop-osc-server *server*)
  (setf *server* nil))




; LANCE L'ATTENTE DE MESSAGES PROVENANT DE MAX
;---------------------------------------------
(defun start-communication-max (server_host portRcv portSnd)
  
  ;Announce
  (osc-send '("/hello" "Communication with OM : OK") server_host portSnd)

  ;Ouvrir serveur qui écoute MAX
  (when *server* (kill-server))
 
  (setf *server* 
        (om-start-osc-server portRcv server_host 
                             #'(lambda (mess host) 
                                 (handle-messages-from-max (om-decode-msg-or-bundle mess)) nil
                                 )
                             )
        )

  ;Send available grids
  ;(setf grid_list '()  message '("/info-available_grid"))
  ;(setf grid_list (loop for e being the hash-key of *available-grids*; using (hash-value v)
  ;      collect e append grid_list))
  ;(setf m (append message grid_list))
  ;(osc-send m server_host portSnd)
  (send-available_grids server_host portRcv portSnd)

  
  )



;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================
; MAIN 
;===========================================================================================================================================================================================================
;===========================================================================================================================================================================================================

(start-communication-max host_server prtRcv prtSnd)



; (kill-server)













