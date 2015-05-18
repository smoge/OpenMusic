(in-package :om)
;=========
;= CLASS =
;=========
(defclass* realTime-improvizer (improvizer)
   (

    ;IMPRO
    ;-----
    (CurrentImproIdx :initform 0 :initarg :CurrentImproIdx :accessor CurrentImproIdx)
    ;For "real-time" navigation, Jerome 22/07/13
    ;"true" only if last step in navigation succesfully called "find-prefix-label-match"
    ;(in general, next steps will be quicker
    (nextPrefixImpactFound :initform nil :initarg :nextPrefixImpactFound :accessor nextPrefixImpactFound)
    (improTrace :initform (make-hash-table :test '=) :initarg :improTrace :accessor improTrace)

    ;MULTITHREADS
    ;------------
    (nameRunningThreads :initform '() :initarg :nameRunningThreads :accessor nameRunningThreads)
    (lock-for-reading-and-writing :initform (mp:make-lock) :initarg :lock-for-reading-and-writing :accessor lock-for-reading-and-writing)
    ))


;==========================
;= INITIALIZE & GET TOOLS =
;==========================
; SEULE DIFFERENCE EST L'IMPROTRACE / NEWIMPROVIZER !!!
; !!! TODO !!!
; REECRIRE PROPREMENT CONSTRUCTUER DE NEWIMPROVIZER (make-instance ??) ET FAIRE UNE SURCHARGE ICI !!!
(defmethod NewRealtimeImprovizer (&optional beatlist beatdur &key max-continuity)
  (let* ((realTime-improvizer (make-instance 'realTime-improvizer
                                   :comparateur 'CompareEvents
                                   :lrsMode t)))
    (when beatlist (loop for i from 0 to (1- (length beatlist)) do (learn-event realTime-improvizer (nth i beatlist))))
    (when beatdur (setf (RefTempo realTime-improvizer) beatdur))
    (when max-continuity (SetmaxCont realTime-improvizer max-continuity))
    (set-start-point realTime-improvizer 0)
    (setf (gethash 0 (improtrace realTime-improvizer)) 0)
    realTime-improvizer))

(defmethod reset-navigation-params ((self realTime-improvizer))
  (call-next-method)
  (setf (CurrentImproIdx self) 0
        (improTrace self) (make-hash-table :test #'=)
        (nextPrefixImpactFound self) nil)
  (setf (gethash 0 (improtrace self)) 0))

(defmethod StateIdx-at-ImproIdx-in-traceimpro ((self realTime-improvizer) (ImproIdx integer))
  (gethash ImproIdx (improtrace self)))


;===================================
;=  OVERLOADED IMPROVIZER METHODS  =
;=     (keep the improtrace)       =
;===================================
(defmethod Improvize-navigate-one-step ((self realTime-improvizer)  &optional (harmgrid nil))
  ; Provient de la méthode de la classe "improvizer"
  (let* ((time-call (get-internal-real-time))
         (index (CurrentStateIdx self))
         (nextindex (Improvize-next-idx self harmgrid)))
    (setf (PrevStateIdx self) index)
    (setf (CurrentStateIdx self) nextindex)
    (format *om-stream* "Time computation : ~5,2F~%~%" (/ (- (get-internal-real-time) time-call) internal-time-units-per-second)) 
    ; Propre à la classe "realTime-improvizer" (pour improtrace)
    (setf (CurrentImproIdx self) (1+ (CurrentImproIdx self)))
    (setf (gethash (CurrentImproIdx self) (improTrace self)) (CurrentStateIdx self))))

;===================================
;=  OVERLOADED IMPROVIZER METHODS  =
;=  (with locks to make W and R atomic) =
;===================================
;-----NAVIGATION
(defmethod Improvize-navigate-one-step-when-unlocked ((self realTime-improvizer)  &optional (harmgrid nil))
  (mp:with-lock ((lock-for-reading-and-writing self))
    (Improvize-navigate-one-step self harmgrid)))

(defmethod Improvize-next-state ((self realTime-improvizer)  &optional (harmgrid nil))
  (let ((run-function (if (lock-for-reading-and-writing self)
                          'Improvize-navigate-one-step-when-unlocked
                        'Improvize-navigate-one-step))
        (navigation-process (mp:process-run-function
                             "Navigating one step" 
                             nil
                             run-function 
                             self 
                             harmgrid)))
    (mp:kill-process navigation-process)
    (if (> (CurrentStateIdx self) 0) 
        (TransposeClonedBeat (otext self (CurrentStateIdx self)) (- (CurrentTranspo self)))
      (null-beat self))))
  





;-----APPRENTISSAGE

#|
; LOCK-FOR-READING-AND-WRITING SUR LEARN-EVENT OU AJOUTER OBJET ???
(defmethod learn-event ((self realTime-improvizer) (event event))
(call-next-method))
;06/08/2013
;*#*#*#*#*#*#*#*# ATOMIC !!! *#*#*#*#*#*#*#*#
;VERSION AVEC LOCK-FOR-READING-AND-WRITING POUR NE PAS AVOIR DE PROBLEME AVEC ERITURE ET LECTURE SIMULTANEE !!!!!
;*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
;????? LOCK-FOR-READING-AND-WRITING POUR L'APPRENTISSAGE D'UN EVENEMENT (+ court) ou de toute la liste ? (+ sûr)
(defmethod learn-event-when-unlocked ((self realTime-improvizer) (event event))
  (mp:with-lock ((lock-for-reading-and-writing self))
    (learn-event self event)))
|#

(defmethod ajouter-objet ((self realTime-improvizer) (event event))
(call-next-method))
(defmethod ajouter-objet ((self realTime-improvizer) (event t))
(call-next-method))
;06/08/2013
;*#*#*#*#*#*#*#*# ATOMIC !!! *#*#*#*#*#*#*#*#
;VERSION AVEC LOCK-FOR-READING-AND-WRITING POUR NE PAS AVOIR DE PROBLEME AVEC ERITURE ET LECTURE SIMULTANEE !!!!!
;*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
;????? LOCK-FOR-READING-AND-WRITING POUR L'APPRENTISSAGE D'UN EVENEMENT (+ court) ou de toute la liste ? (+ sûr)
(defmethod ajouter-objet-when-unlocked ((self realTime-improvizer) (event event))
  (mp:with-lock ((lock-for-reading-and-writing self))
    (learn-event self event)))

(defmethod learn-event-list ((self realTime-improvizer) (events list))
  (let ((run-function (if (lock-for-reading-and-writing self)
                          'ajouter-objet-when-unlocked
                        'ajouter-objet)))
    ; ATTENTION !!! RIEN N'ASSURE QUE SERA APPRIS DANS LE BON ORDRE !!!!!!!
    (loop for event in events do 
          (mp:process-run-function
           "Learning AN EVENT"
           nil
           run-function
           self
           event))))
;BESOIN DE RETOURNER ??
;self))



;===================================
;=   SPECIFIC NAVIGATION METHODS   =
;= (with "nextPrefixImpactFound")  =
;===================================
;VERSION "SEQUENTIELLE" : on n'envoie que lorsqu'on a tout le fragment.
; (pour gérer notes tenues, évènements syncopés...)
(defmethod Improvize-send-groupsAnte-loop-next-factor ((self realTime-improvizer) &optional (harmgrid nil) (beatduration integer) (beatIdxInImpro integer) (host t) (port_ante2 integer) (numAntescofo integer))
  
  #|
  ; Ne pas confondre
  ; --> cas où au début on attaque directement sur ImproIdx > 1 (par exemple parce qu'on attendait que la mémoire soit déjà un peu remplie)
  ; --> cas où on revient en arrière sur une partie d'impro déjà calculée.
  ; /!\
  ; !!!! PROBLEME SI ON NE CALCULE PAS A PARTIR DU BEAT 1 !!!
  ; VOUDRA ACCEDER A DES ELEMENTS DE TRACE QUI N'EXISTENT PAS !!!
  (if (and
       (not (= (+1 (CurrentImproIdx self)) beatIdxInImpro))
       (StateIdx-at-ImproIdx-in-traceimpro self (- beatIdxInImpro 1)))
      (progn
        (setf (CurrentImproIdx self) (- beatIdxInImpro 1) )
        (setf (CurrentStateIdx self) (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))))
  |#

  (if (< beatIdxInImpro 2) 
      ; /!\ TODO : TROUVER CE BUG !!!
      ;(reset-navigation-params self)
      (format *om-stream* "I FEEL LIKE RESETING !!~%")
    (if (not (= beatIdxInImpro (1+ (CurrentImproIdx self))))
        (progn
          (setf (CurrentImproIdx self) (- beatIdxInImpro 1))
          (if (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))
            ; --> cas où on revient en arrière sur une partie d'impro déjà calculée.
              (setf (CurrentStateIdx self) (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))))))
                  ; --> sinon cas où au début on attaque directement pour la 1ere fois sur un ImproIdx > 1 
                  ; (par exemple parce qu'on attendait que la mémoire soit déjà un peu remplie)
  
  (let* ((beatlistImpro (Improvize-by-fragments self (list-length harmgrid) harmgrid))
         ;[In non r-t version :] ALWAYS DONE BEFORE CALLING "save-for-antescofo"
         ;In particular in "generate-improbeatlist-from-oraclelist" (Tune.lisp)
         (beatlistImpro (thread-Beats beatlistImpro beatduration))
         ;[In non r-t version :] DONE IN "save-for-antescofo"
         (beatlistImpro (transfer-syncopated-event beatlistImpro beatduration))
         (beatlistImpro (add-grid-to-beatlist beatlistImpro beatduration))
         (beatforAntescofo nil)
         (beatIdx beatIdxInImpro))

    ; BONNE IDEE ?????
    ; je ne sais toujours pas, mais remonté avant le let...
    ;(if (< beatIdxInImpro 2) (reset-navigation-params self))
    
    ; Serveur existe et ouvert ?
    (if (and (boundp '*server*) (not (null *server*)))
         ; Traduction en group antescofo et envoi
        (loop for beat in beatlistImpro do 
              (progn
                (setf beatforAntescofo (string-group-for-antescofo beat beatduration numAntescofo))
                (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
                (incf beatIdx)))
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))




; TRES INSPIRE DE IMPROVIZE (improvizer.lisp) !!!
; MODULARISER ET FAIRE APPEL A DES MEMES FONCTIONS !!!
; -----------------------------------------------------
(defmethod Improvize-by-fragments ((self realTime-improvizer) (length integer) &optional (harmgrid nil))
  (let ((impro nil) (current-grid-suffix nil) (next-grid-suffix nil)
        ;(run-function (if (lock-for-reading-and-writing self)
        ;                  'Improvize-next-state-when-unlocked
        ;                'Improvize-next-state))
        )  
    (format *om-stream* "-----------------------~%")
    (when (null (veclrs self)) (setf (bestSuffixMode self) nil) (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))  
    (when harmgrid (setf next-grid-suffix harmgrid))

    ;===========!!!===========
    ; diff 1/2 avec Improvize !
    ;===========!!!===========    
    (setf (nextPrefixImpactFound self) nil)

    (loop for i from 1 to length
          for label =  (pop harmgrid) 
          do
          (setf current-grid-suffix next-grid-suffix)
          ;Display info : /!\ Current-transpo is managed in "Improvize-one-step". 
          ;Transpositions here (label and current-grid-suffix) are only used to display info.
          (when label
            (format *om-stream* "~%----label=~a, " (FormatLabel label))
            ;(if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", oracle=~a, " (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))))))        
            (if (not (= (CurrentTranspo self) 0)) (format *om-stream* "oracle=~a, " (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))) )))
          (if (= *print_info_navig* 1) (format *om-stream* "i = ~D, current grid suffix = ~a~%" i (TransposeGrid current-grid-suffix (CurrentTranspo self))))

          ;Update for next navigation step
          (setf next-grid-suffix harmgrid)

          
          ;06/08/2013
          ;*#*#*#*#*#*#*#*# LOCK-FOR-READING-AND-WRITING !!! *#*#*#*#*#*#*#*#
          ;One navigation step : /!\ (CurrentStateIdx self) is modified
          collect (Improvize-next-state self current-grid-suffix)
          ;===========!!!===========
          ; diff "3/2" avec Improvize ----> LOCK-FOR-READING-AND-WRITING POUR NE PAS AVOIR DE PROBLEME AVEC ECRITURE ET LECTURE SIMULTANEE !!!!!
          ;===========!!!=========== 
          ;collect (mp:process-run-function
          ;         "Navigating one step" 
          ;         nil
          ;         run-function 
          ;         self 
          ;         current-grid-suffix)

          ;===========!!!===========
          ; diff 2/2 avec Improvize !
          ;===========!!!===========
          while (not (nextPrefixImpactFound self))
          )))


;===================================
;=           MULTITHREADS          =
;===================================

;SOLUTION TEMPORAIRE : stocker les messages reçus de Max dans variables surveillées par les "waits" ci-dessous
;--------------------------------------------------------------------------------------------------------------
(defvar *last-received-messagefornavigation* nil)
;(suffixe beatIdxInImpro)
;pour l'instant on utilise la beatduration de la *current-tune*, il faudra ensuite envoyer le tempo calculé par Antescofo pour plus de realtime
(defvar *last-received-messageforlearning* nil)
;juste la liste d'évènements midi

;*********NAVIGATION TR***********
;Calls "Improvize-send-groupsAnte-loop-next-factor" (AntescofoTR.lisp)
;*********************************
(defmethod navigate-real-time ((self realTime-improvizer) (messagefornavigation-symbol t) (beatduration integer) (host t) (port_ante integer) (numAntescofo integer))
  (loop
   ; EN PRATIQUE QUOI ATTENDRE ?
   (mp:process-wait "Waiting for a grid suffix to navigate" 
                    (lambda () 
                      ;/!\ SOLUTION TEMPORAIRE POUR NE PAS NAVIGUER SUR UN ORACLE VIDE
                      (if (otext self 1) 
                      ;/!\ ------------------------------------------------------------    
                          (symbol-value messagefornavigation-symbol))))
  (let* ((message (symbol-value messagefornavigation-symbol))
         (suffixgrid (nth 1 message))
         (beatIdxInImpro (nth 0 message)))
    ;SUFFIT POUR QUE CELA NE SE DECLENCHE PAS PLUSIEURS FOIS ???
    (setf (symbol-value messagefornavigation-symbol) nil)
    (Improvize-send-groupsAnte-loop-next-factor self suffixgrid beatduration beatIdxInImpro host port_ante numAntescofo))))


;*********APPRENTISSAGE TR***********
; Calls functions in MidiTR.lisp (midievtsList-to-beatlist)
;************************************
(defmethod learn-realtime ((self realTime-improvizer) (messageforlearning-symbol t))
  (loop
   ; EN PRATIQUE QUOI ATTENDRE ?
   (mp:process-wait "Waiting for a midi events list" (lambda () (symbol-value messageforlearning-symbol)))
  (let ((midievts-list (symbol-value messageforlearning-symbol)))
    ;SUFFIT POUR QUE CELA NE SE DECLENCHE PAS PLUSIEURS FOIS ???
    ; --->>>>>> NON /!\ !!!! 
    ; SOLUTION TEMPORAIRE ET TRES MOCHE : ce "if midievts-list (progn..." :
    ; (et le "(if beatlist (progn..." plus bas
    (if midievts-list 
        (progn
          (format *om-stream* "Received midi to learn...")
          (setf (symbol-value messageforlearning-symbol) nil)
          (let* ((res (midievtsList-to-beatlist midievts-list))
                 (beatlist (first res))
                 (defaultbeatdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le même defaultbeatdur, mais ensuite ???
      ; quand differents tempi donnés par Antescofo ????? 
      ; LE PROBLEME SE POSAIT DEJA AVANT NON ?????????
      ;-----------------------------------------------
            (if beatlist 
                (progn
                  (format *om-stream* "... indeed I learn !!!~%")
                  (setf (RefTempo self) defaultbeatdur)
                  (learn-event-list self beatlist)))))))))
  

;*********LANCER CES PROCESS D'ATTENTE SUR IMPROVIZER***********
(defmethod launch-realtime ((self realTime-improvizer) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer))
  ;(setf (lock-for-reading-and-writing self) (mp:make-lock))
  (let* ((learner-name 
          (format nil "Learner~{~D~}" (cdddr (reverse (multiple-value-list (get-decoded-time))))))
         (navigator-name
          (format nil "Navigator~{~D~}" (cdddr (reverse (multiple-value-list (get-decoded-time))))))
         (learner
          (mp:process-run-function learner-name ()
                                   'learn-realtime
                                   self
                                   '*last-received-messageforlearning*))
         (navigator
          (mp:process-run-function navigator-name ()
                                   'navigate-real-time
                                   self
                                   '*last-received-messagefornavigation*
                                   beatduration
                                   host_send
                                   port_send_ante
                                   numAntescofo)))
  (push learner-name (nameRunningThreads self))
  (push navigator-name (nameRunningThreads self))))

(defmethod kill-realtime ((self realTime-improvizer))
  (loop for process-name in (nameRunningThreads self) do
        (mp:process-kill (mp:find-process-from-name process-name)))
  (setf (nameRunningThreads self) '()
        (lock-for-reading-and-writing self) nil))




#|
; TEMPORAIREMENT DEPLACE DANS "AntescofoTR"
; A METTRE ENSUITE DANS "Tune.lisp"
;=========================================

;(( UTILITAIRES POUR UTILISER PLUS FACILEMENT AVEC ORACLETABLE DE LA CLASSE TUNE))      
(defmethod launch-realtime-on-oraclechan ((self tune) (numoraclechan integer) (host_send t) (port_send_ante2 integer))
  (launch-realtime (gethash numoraclechan (oracletable *current-tune*))
                                 (beatduration *current-tune*)
                                 host_send
                                 port_send_ante2))
(defmethod kill-realtime-on-oraclechan ((self tune) (numoraclechan integer))
  (kill-realtime (gethash numoraclechan (oracletable *current-tune*))))
|#
     



#|
; VERSION AVEC DELAI ET PITCH NEGATIFS 
;(SYNCOPES ET NOTES TENUES NON PRISES EN COMPTE, CAR PLUS TRAVAIL SUR TOUTE UNE BEATLIST AVEC THREAD-BEATS, TRANSFER-SYNCOPATED-EVENTS,... MAIS BEAT PAR BEAT).
;---------------------------------------------------------------------------------


; TODO : TRAITER BEATS VIDES !!!!!
;A RAJOUTER DANS IMPROVIZER.LISP (AU DESSUS DE Improvize-next-state) ?
;---------------------------------------------------------------------------------
;Jerome, 22/07/2013
;Jerome, 26/03/2013 : One step in navigation : modification of current/previous states fields
; RETURNS a group for antescofo2 corresponding to the beat found in the navigation
;Jerome, 25/01/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;---------------------------------------------------------------------------------
(defmethod Improvize-groupAnte-next-state ((self realTime-improvizer)  &optional (harmgrid nil) (beatduration integer) (numAntescofo integer))
(let ((next-beat (Improvize-next-state self harmgrid)))
  (string-group-for-antescofo next-beat beatduration numAntescofo)))


;TODO : revoir les arguments... en particuliers les "optionals"
(defmethod Improvize-send-groupsAnte2-loop-next-factor ((self realTime-improvizer) &optional (harmgrid nil) (beatduration integer) (beatIdxInImpro integer) (host t) (port_ante2 integer))
(let ((beatforAntescofo nil)
      (beatIdx beatIdxInImpro))
  ;Jerome 23/07/2013
  (setf (nextPrefixImpactFound self) nil)
  (if (and (boundp '*server*) (not (null *server*)))
      (loop while (not (nextPrefixImpactFound self)) do
          (progn
            (setf beatforAntescofo (Improvize-groupAnte2-next-state self harmgrid beatduration))
            (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
            (incf beatIdx)
            (pop harmgrid))) ; ???????????????????????????????????
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))
|#


