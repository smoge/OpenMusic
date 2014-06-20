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
    ;MAJCONT 15/10/13 : plus juste un entier (indice) mais une liste (indiceassoci� continuit�associ�e)
    (improTrace :initform (make-hash-table :test '=) :initarg :improTrace :accessor improTrace)

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
    ;MAJCONT 15/10/13
    ;(setf (gethash 0 (improtrace realTime-improvizer)) 0)
    (setf (gethash 0 (improtrace realTime-improvizer)) '(0 0))
    realTime-improvizer))

 

(defmethod reset-navigation-params ((self realTime-improvizer))
  (call-next-method)
  (setf (CurrentImproIdx self) 0
        (improTrace self) (make-hash-table :test #'=)
        (nextPrefixImpactFound self) nil)
  ;MAJCONT 15/10/13
  (setf (gethash 0 (improtrace self)) '(0 0)))

 

(defmethod StateIdx-at-ImproIdx-in-traceimpro ((self realTime-improvizer) (ImproIdx integer))
;MAJCONT 15/10/13  
;(gethash ImproIdx (improtrace self)))
(nth 0 (gethash ImproIdx (improtrace self))))

;MAJCONT 15/10/13
(defmethod Continuity-at-ImproIdx-in-traceimpro ((self realTime-improvizer) (ImproIdx integer))  
(nth 1 (gethash ImproIdx (improtrace self))))


(defmethod load-realtimeImprovizer-fromSavedImprovizer ((name t))
  (let* ((loadedimprovizer (load-improvizer name))
         (newRealtimeImprovizer (NewRealtimeImprovizer)))
    (loop for sl in 
          (mapcar #'slot-definition-name (class-slots (class-of loadedimprovizer))) do
          (setf (slot-value newRealtimeImprovizer sl) 
                (slot-value loadedimprovizer sl)))      
    newRealtimeImprovizer))

 
;===================================
;=  OVERLOADED IMPROVIZER METHODS  =
;=     (keep the improtrace)       =
;===================================
(defmethod Improvize-navigate-one-step ((self realTime-improvizer)  &optional (harmgrid nil))
  (call-next-method)  
 
  (if *print-navig-basics* (format *om-stream* "ImproBeat num ~D : " (CurrentImproIdx self)))
  ;26/10/13 NON !!!! PARALLELISME !!!! ICI DES QUE CALCULE INCREMENTE !!!
  (setf (CurrentImproIdx self) (1+ (CurrentImproIdx self)))
  ;MAJCONT 15/10/13  
  ;(setf (gethash (CurrentImproIdx self) (improTrace self)) (CurrentStateIdx self)))
  ;---- Mauvaise Syntaxe !!
  ;(setf (nth 0 (gethash (CurrentImproIdx self) (improTrace self))) (CurrentStateIdx self))
  ;(setf (nth 1 (gethash (CurrentImproIdx self) (improTrace self))) (continuity self))
  ;----
  (setf (gethash (CurrentImproIdx self) (improTrace self)) (list (CurrentStateIdx self) (continuity self))))
 

;===================================
;=  OVERLOADED IMPROVIZER METHODS  =
;===================================

;------NAVIGATION
(defmethod Improvize-next-state ((self realTime-improvizer)  &optional (harmgrid nil))
  (Improvize-navigate-one-step self harmgrid)
  (if (> (CurrentStateIdx self) 0) 
      (TransposeClonedBeat (otext self (CurrentStateIdx self)) (- (CurrentTranspo self)))
    (null-beat self)))


(defmethod Improvize-next-idx ((self realTime-improvizer)  &optional (harmgrid nil))
  (let* ((index (CurrentStateIdx self)) (previndex (PrevStateIdx self)) (nextindex nil)
         (harmgrid-current-transp (TransposeGrid harmgrid (CurrentTranspo self)))
         (label-current-transp (car harmgrid-current-transp))
         (links nil))

    ;*print-navig-basics* defini dans LoadImproteK   (setf *print_info_navig* t)
    (when *print-navig-basics* (display-infolabel self harmgrid))
    
    (cond
     
     ;Starting point
     ((and (zerop index) (setf nextindex (find-prefix-labels-match self harmgrid-current-transp)))
      (format *om-stream* "Starting point : ~a ~%" nextindex))
     
     ;Navigation
     (t
      ;Update navigation mode
      (if (check-continuity self) 
          (setf (NavigationMode self) 'continuity)  
        (if (available-suppleance self index) 
            (setf (NavigationMode self) 'suppleance)
          (setf (NavigationMode self) 'continuity)))
      
      ; MODE CONTINUITY
      (when (eq (NavigationMode self) 'continuity)
        (setf links (flink self index)
              nextindex (choose-factor-link self links label-current-transp))
        (if nextindex
            (progn
              (if *print-navig-basics* (format *om-stream* "c : ~a ~%" nextindex))    
              ;For "real-time" navigation, Jerome 22/07/13 :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              ;(in general, next steps will be quicker)
              (setf (nextPrefixImpactFound self) nil))
          (if (available-suppleance self index) 
              (setf (NavigationMode self) 'suppleance)
            (setf (NavigationMode self) 'nothing))))
      ;MODE SUPPLEANCE
      (when (eq (NavigationMode self) 'suppleance)
        (setf nextindex (continuations-by-suppleance self index label-current-transp)) 
        (if (and nextindex (/= nextindex previndex))
            (progn 
              (if *print-navig-basics* (format *om-stream* "--->s : ~a ~%" nextindex))
              (setf (continuity self) 0)
              ;For "real-time" navigation, Jerome 22/07/13 :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              ;(in general, next steps will be quicker)
              (setf (nextPrefixImpactFound self) nil))
          (progn
            (setf (NavigationMode self) 'nothing)
            (if *print-navig-basics* (format *om-stream*  "~a, " (NavigationMode self))))))
      ;MODE NOTHING
      (when (eq (NavigationMode self) 'nothing)
          (setf nextindex (find-prefix-labels-match self harmgrid-current-transp))
        (if nextindex 
            (progn 
              (if *print-navig-basics* (format *om-stream* "new : ~a " nextindex))
              (if (or (null harmgrid) (zerop (CurrentTranspo self))) 
                  (if *print-navig-basics* (format *om-stream* " ~%")) (if *print-navig-basics* (format *om-stream* "transpo=~a ~%" (CurrentTranspo self))))
              (setf (continuity self) 0)
              ;For "real-time" navigation, Jerome 22/07/13 :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              ;(in general, next steps will be quicker)
              (setf (nextPrefixImpactFound self) T))
          (progn               
            (if *print-navig-basics* (format *om-stream* "~a~%" 'empty))
            (setf nextindex 0))))))
    nextindex))

 

(defmethod learn-event-list ((self realTime-improvizer) (events list))
  ;(format *om-stream* "(START learning!)~%")
   (loop for event in events do 
         (progn
         (ajouter-objet self event)
         ;(format *om-stream* "** Learnt one => maxetat = ~D~%" (maxetat self))
         ))
   ;(format *om-stream* "(FINISHED learning!)~%")
   ;----------
   ;(om-inspect self)
   ;---------- 
   ;self
) ; RETOURNER SELF ...? 

 
;===================================
;=   SPECIFIC NAVIGATION METHODS   =
;= (with "nextPrefixImpactFound")  =
;===================================
;VERSION "SEQUENTIELLE" : on n'envoie que lorsqu'on a tout le fragment.
; (pour g�rer notes tenues, �v�nements syncop�s...)
(defmethod Improvize-send-groupsAnte-loop-next-factor ((self realTime-improvizer) &optional (harmgrid nil) (beatduration integer) (beatIdxInImpro integer) (host t) (port_ante2 integer) (numAntescofo integer))
  #|
  ; Ne pas confondre
  ; --> cas o� au d�but on attaque directement sur ImproIdx > 1 (par exemple parce qu'on attendait que la m�moire soit d�j� un peu remplie)
  ; --> cas o� on revient en arri�re sur une partie d'impro d�j� calcul�e.
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

  (format *om-stream* "~%~%****** Launching new navigation beginning at idx ~D in impro~%" beatIdxInImpro)
 
  (if (< beatIdxInImpro 2) 
      ; NE RIEN FAIRE DU TOUT EN FAIT SI ???
      ; /!\ TODO : TROUVER CE BUG !!!
      ;(reset-navigation-params self)
      ;(format *om-stream* "I FEEL LIKE RESETING !!~%")
    (if (not (= beatIdxInImpro (1+ (CurrentImproIdx self))))
        (progn
          (setf (CurrentImproIdx self) (- beatIdxInImpro 1))
          (if (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))
            ; --> cas o� on revient en arri�re sur une partie d'impro d�j� calcul�e.
              (progn 
                (format *om-stream* "---> GOING BACK TO STATE ~D IN ORACLE~%" (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                (setf (CurrentStateIdx self) (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                ;MAJCONT 15/10/13
                (setf (continuity self) (Continuity-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))

                )))))
                  ; --> sinon cas o� au d�but on attaque directement pour la 1ere fois sur un ImproIdx > 1 
                  ; (par exemple parce qu'on attendait que la m�moire soit d�j� un peu remplie)
  
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
    ; je ne sais toujours pas, mais remont� avant le let...
    ;(if (< beatIdxInImpro 2) (reset-navigation-params self))
    
    ; Serveur existe et ouvert ?
    (if (and (boundp '*server*) (not (null *server*)))
         ; Traduction en group antescofo et envoi
        (progn
          (loop for beat in beatlistImpro do 
                (progn
                  (setf beatforAntescofo (string-group-for-antescofo beat beatduration numAntescofo))
                  (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
                  (incf beatIdx)))
          (format *om-stream* "Generated from beat ~D to ~D ~%" beatIdxInImpro beatIdx))
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))


 

; TRES INSPIRE DE IMPROVIZE (improvizer.lisp) !!!
; MODULARISER ET FAIRE APPEL A DES MEMES FONCTIONS !!!
; -----------------------------------------------------
(defmethod Improvize-by-fragments ((self realTime-improvizer) (length integer) &optional (harmgrid nil))
  (let ((impro nil) (current-grid-suffix nil) (next-grid-suffix nil))  
    (format *om-stream* "-----------------------~%")
    (when (null (veclrs self)) (setf (bestSuffixMode self) nil) (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))  
    (when harmgrid (setf next-grid-suffix harmgrid))

    ;===========!!!===========
    ; diff 1/2 avec Improvize !
    ;===========!!!===========    
    (setf (nextPrefixImpactFound self) nil)

    ; JEROME 03/09/13
    ;===========***********!!!***********===========
    ;TEST "min ..." du coup faut-il faire qqch
    ; avec le booleen "nextPrefix..." ?
    ;===========***********!!!***********===========
    (loop for i from 1 to (min length (maxetat self)) ; avant : juste length
          for label =  (pop harmgrid) 
          do
          (setf current-grid-suffix next-grid-suffix)
          ;Display info : /!\ Current-transpo is managed in "Improvize-one-step". 
          ;Transpositions here (label and current-grid-suffix) are only used to display info.
          
          ;; AFFICHAGE DEJA FAIT DANS "Improvize-next-idx" (improvizer.lisp)
          ;(when label
            ;(format *om-stream* "~%----label=~a, " (FormatLabel label))
            ;;(if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", oracle=~a, " (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))))))        
            ;(if (not (= (CurrentTranspo self) 0)) (format *om-stream* "oracle=~a, " (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))) )))
          ;;(if (= *print_info_navig* 1) (format *om-stream* "i = ~D, current grid suffix = ~a~%" i (TransposeGrid current-grid-suffix (CurrentTranspo self))))
          
          ;Update for next navigation step
          (setf next-grid-suffix harmgrid)

          
          ;06/08/2013
          ;*#*#*#*#*#*#*#*# LOCK !!! *#*#*#*#*#*#*#*#
          ;One navigation step : /!\ (CurrentStateIdx self) is modified
          collect (Improvize-next-state self current-grid-suffix)

          ;===========!!!===========
          ; diff 2/2 avec Improvize !
          ;===========!!!===========
          while (not (nextPrefixImpactFound self))
          )))

 
;===================================
;=           MULTITHREADS          =
;===================================
;*********NAVIGATION TR***********
;Calls "Improvize-send-groupsAnte-loop-next-factor" (AntescofoTR.lisp)
;*********************************
(defmethod navigate-realtime ((self realTime-improvizer) (NavigationOrder list) (beatduration integer) (host t) (port_ante integer) (numAntescofo integer))
  (let* ((suffixgrid (nth 1 NavigationOrder))
         (beatIdxInImpro (nth 0 NavigationOrder)))
    (if (otext self 10) 
        (progn
          ;(format *om-stream* "******YA DES TRUCS -> JE NAVIGUE******~%")
          (Improvize-send-groupsAnte-loop-next-factor self suffixgrid beatduration beatIdxInImpro host port_ante numAntescofo))
      ;(format *om-stream* "******YA PAS DE TRUCS -> JE NAVIGUE APS******~%")
      )))
 

;*********APPRENTISSAGE TR***********
; Calls functions in MidiTR.lisp (midievtsList-to-beatlist)
;************************************
(defmethod learn-realtime ((self realTime-improvizer) (midievts-list list))
  (let* ((res (midievtsList-to-beatlist midievts-list))
         (beatlist (first res))
         (defaultbeatdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le m�me defaultbeatdur, mais ensuite ???
      ; quand differents tempi donn�s par Antescofo ????? 
      ; LE PROBLEME SE POSAIT DEJA AVANT NON ?????????
      ;-----------------------------------------------
    ;(format *om-stream* "~%~%ENTER LEARNING PHASE !!!~%")
    (if beatlist 
        (progn
          ;;(format *om-stream* "... indeed I learn !!!~%")
          (setf (RefTempo self) defaultbeatdur)
                  ;------> TEST (cf defmethod learn-event-list)
          (learn-event-list self beatlist)
          (format *om-stream* "~%~%++++++ Learn beatlist of length : ~D -----> New maxetat : ~D~%" (list-length beatlist) (maxetat self))))
    ;(format *om-stream* "LEAVE LEARNING PHASE !!!~%~%~%")
    ))
   

(defmethod handle-evts-to-learn-from-max ((self AEDPCS) (message list))
  ;(format *om-stream* "~% *********************** handle-evts-to-learn-from-max RECEIVED_FROM_MAX : ~a~%" message)
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  
  ;(if (= *print_received_from_Max* 1) 
  ;    (progn
  ;      (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
  ;      (format *om-stream* "Type : ~a~%" (type-of message))
  ;      (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
  ;      (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
  ;      (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))

    (if (string= (car message) "/learnforTR")
          (L-fillEventsToLearnChannelWithEvent self (eval (read-from-string (nth 1 message))))))

 

(defmethod handle-navig-order-from-max ((self AEDPCS) (message list))
  ;(format *om-stream* "~% *********************** handle-navig-order-from-max RECEIVED_FROM_MAX : ~a~%" message)
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  #|
  (if (= *print_received_from_Max* 1) 
      (progn
        (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
        (format *om-stream* "Type : ~a~%" (type-of message))
        (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
        (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
        (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))
  |#
  (let ((idx (nth 1 message)) 
        (gridsuff (eval (read-from-string (nth 2 message)))))
    (if (string= (car message) "/antescofo/improvize_next_steps")
        (G-fillNavigationOrdersChannelWithEvent self (list idx gridsuff)))))

 

#|
(setf message '("/learnforTR" "'( (84 5486 623 47 8) (12 5530 108 100 16) (5 5530 101 101 16) (12 5786 108 100 16) (5 5849 101 101 16) (12 6094 108 100 16) (5 6168 101 101 16) (80 6096 295 53 8) (12 6435 108 100 16) (7 6487 101 102 16) (79 6406 335 62 8) (7 6806 101 102 16) )"))

(format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
(format *om-stream* "Type : ~a~%" (type-of message))
(format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
(format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
(format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))

(setf evts-to-learn (eval (read-from-string (nth 1 message))))
(type-of evts-to-learn)

(let (evts-to-learn (eval (read-from-string (nth 1 message))))
    (if (string= (car message) "/learnforTR")
        (progn
          (format *om-stream* "$*$*$*$*$* /Indeed learnforTR ===>~%$*$*$*$*$* Giving ~a as argument to L-Fillevents...~%" evts-to-learn)
          )))
|#



#|
;ATTENTION !!! DANS TUNE !!!

(defmethod launch-realtime ((self tune) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer) (port_rcv_learn integer) (port_rcv_navig_order integer))
  (setf (RealTimeSystem self) 
        (make-instance 'AEDPCS 
                       :sharedData (NewRealTimeImprovizer) 
                       :LearnMethodOnSharedData 'learn-realtime 
                       :GenerateMethodOnSharedData #'(lambda (realTimeImpro NavigationOrder) 
                                                       (navigate-realtime 
                                                        realTimeImpro NavigationOrder 
                                                        beatduration host_send port_send_ante numAntescofo))))
  
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
         (G-processer (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer))
         )
  ))

|#




;?????????
(defmethod kill-realtime ((self realTime-improvizer))
  ;(loop for process-name in (nameRunningThreads self) do
  ;      (mp:process-kill (mp:find-process-from-name process-name)))
  ;(setf (nameRunningThreads self) '()
        ;(lock self) nil
        );)




#|

; S'INSPIRER DE CA POUR LE KILL ???????

(defun start-udp-server (&key (function #'identity) (arguments nil)
                              (announce nil)
                              address (service 0)
                              (process-name (format nil "~S UDP server" service))
                              (loop-time 1)
                              (max-buffer-size +max-udp-message-size+)
                              (multicast nil)
                              (mcast-interface 0)
                              (mcast-ttl 1 mcast-ttl-p)
                              (mcast-loop t mcast-loop-p))
  "Something like START-UP-SERVER"
  (let* ((socket (open-udp-socket :local-address address
                                  :local-port service
                                  :read-timeout loop-time
                                  :errorp t
                                  :reuse-address multicast))
         (socket-fd (socket-datagram-socket socket)))
    (announce-server-started announce socket-fd nil)
    ;; multicast support (4.1)
    (when multicast
      (mcast-join socket-fd address :interface mcast-interface :errorp t)
      (when mcast-ttl-p
        (setf (mcast-ttl socket-fd) mcast-ttl))
      (when mcast-loop-p
        (setf (mcast-loop socket-fd) mcast-loop)))
    ;; start a thread
    (let ((process (mp:process-run-function process-name nil
                                            #'udp-server-loop
                                            arguments ; additional arguments for function
                                            socket function max-buffer-size)))
      (setf (getf (mp:process-plist process) 'socket) socket)
      process)))

(defun stop-udp-server (process &key wait)
  (let ((socket (getf (mp:process-plist process) 'socket)))
    (mp:process-kill process)
    (prog1 (zerop (close-datagram socket))
      (when wait
        (mp:process-wait "Wait until UDP server process be killed"
                         #'(lambda () (not (mp:process-alive-p process))))))))

|#




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

