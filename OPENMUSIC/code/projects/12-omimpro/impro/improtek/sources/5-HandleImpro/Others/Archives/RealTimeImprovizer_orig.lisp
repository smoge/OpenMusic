;RealTimeImprovizer.lisp
;J. Nika

(in-package :om)

;=========
;= CLASS =
;=========
(defclass* realTime-improvizer (improvizer)
   (
    (CurrentImproIdx :initform 0 :initarg :CurrentImproIdx :accessor CurrentImproIdx)
    (StartingPointNextPhaseFound? :initform nil :initarg :StartingPointNextPhaseFound? :accessor StartingPointNextPhaseFound?) 
    ;"true" only if last step in navigation succesfully called "find-prefix-label-match"
    (improTrace :initform (make-hash-table :test '=) :initarg :improTrace :accessor improTrace)
    ))

 

;==========================
;= INITIALIZE & GET TOOLS =
;==========================
; SEULE DIFFERENCE EST L'IMPROTRACE / NEWIMPROVIZER !!!
; !!! TODO !!!
; REECRIRE PROPREMENT CONSTRUCTUER DE NEWIMPROVIZER (make-instance ??) ET FAIRE UNE SURCHARGE ICI !!!
;---
;GENERICITE !!!!!!!!!! NewRealTimeImprovizer ou plutot NewRealTimeImprovizerONBEATS ???
;CHOISIR UN PEU CAR ICI SI PAS D'ARGS -> NewRealTimeImprovizer, SI ARGS -> NewRealTimeImprovizerONBEATS
;===> FONCTION BATARDE !!!
;;;;;
(defmethod NewRealtimeImprovizer (&optional memory-list beatdur &key max-continuity)
  (let* ((realTime-improvizer (make-instance 'realTime-improvizer
                                   :comparateur 'CompareEvents
                                   :lrsMode t)))
    (when memory-list (loop for i from 0 to (1- (length memory-list)) do (learn-event realTime-improvizer (nth i memory-list))))
    (when beatdur (setf (RefTempo realTime-improvizer) beatdur))
    (when max-continuity (SetmaxCont realTime-improvizer max-continuity))
    (set-start-point realTime-improvizer 0)
    (setf (gethash 0 (improtrace realTime-improvizer)) '(0 0))
    realTime-improvizer))

(defmethod reset-navigation-params ((self realTime-improvizer))
  (call-next-method)
  (setf (CurrentImproIdx self) 0
        (improTrace self) (make-hash-table :test #'=)
        (StartingPointNextPhaseFound? self) nil)
  (setf (gethash 0 (improtrace self)) '(0 0)))

(defmethod StateIdx-at-ImproIdx-in-traceimpro ((self realTime-improvizer) (ImproIdx t))
  (nth 0 (gethash ImproIdx (improtrace self))))

(defmethod Continuity-at-ImproIdx-in-traceimpro ((self realTime-improvizer) (ImproIdx t))  
  (nth 1 (gethash ImproIdx (improtrace self))))

(defmethod Improvizer->RealtimeImprovizer ((self improvizer))
  (let ((newRealtimeImprovizer (NewRealtimeImprovizer)))
    (loop for sl in 
          (mapcar #'slot-definition-name (class-slots (class-of self))) do
          (setf (slot-value newRealtimeImprovizer sl) 
                (slot-value self sl)))      
    newRealtimeImprovizer))

(defmethod load-realtimeImprovizer-fromSavedImprovizer ((name t))
  (let ((loadedimprovizer (load-improvizer name)))
    (Improvizer->RealtimeImprovizer loadedimprovizer)))

 
;===================================
;=  OVERLOADED IMPROVIZER METHODS  =
;=     (keep the improtrace)       =
;===================================
(defmethod Improvize-navigate-one-step ((self realTime-improvizer)  &optional (harmgrid nil))
  (call-next-method)  
  (if *print-navig-basics* (format *om-stream* "ImproIdx ~D : " (CurrentImproIdx self)))
  ;26/10/13 NON !!!! PARALLELISME !!!! ICI DES QUE CALCULE INCREMENTE !!!
  (setf (CurrentImproIdx self) (1+ (CurrentImproIdx self)))
  (setf (gethash (CurrentImproIdx self) (improTrace self)) (list (CurrentStateIdx self) (continuity self))))


;===================================
;=  OVERLOADED IMPROVIZER METHODS  =
;===================================

;------NAVIGATION
(defmethod Improvize-next-state ((self realTime-improvizer)  &optional (scenario nil))
  (Improvize-navigate-one-step self scenario)
  (if (> (CurrentStateIdx self) 0) 
      (TransposeClonedBeat (otext self (CurrentStateIdx self)) (- (CurrentTranspo self)))
    (null-beat self)))


(defmethod Improvize-next-idx ((self realTime-improvizer)  &optional (scenario nil))
  (let* ((index (CurrentStateIdx self)) (previndex (PrevStateIdx self)) (nextindex nil)
         (scenario-current-transp (TransposeGrid scenario (CurrentTranspo self)))
         (label-current-transp (car scenario-current-transp))
         (links nil))
    (when *print-navig-basics* (display-infolabel self scenario))
    
    (cond
     
     ;Starting point
     ((and (zerop index) (setf nextindex (find-prefix-labels-match self scenario-current-transp)))
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
              ;For "real-time" navigation :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              (setf (StartingPointNextPhaseFound? self) nil))
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
              ;For "real-time" navigation :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              (setf (StartingPointNextPhaseFound? self) nil))
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
              ;For "real-time" navigation :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              (setf (StartingPointNextPhaseFound? self) T))
          (progn               
            (if *print-navig-basics* (format *om-stream* "~a~%" 'empty))
            (setf nextindex 0))))))
    nextindex))

 


 
;===================================
;=   SPECIFIC NAVIGATION METHODS   =
;= (with "StartingPointNextPhaseFound?")  =
;===================================

;/!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\
;/!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\
;/!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\
; MEGA MODULARISER CI-DESSOUS !!!!! IL Y A 1000 DIMENSIONS DIFFERENTES TRAITEES DANS CETTE FONCTION : LE CALCUL EN LUI-MÊME, LE COTE "TRACE ET VERIFIER SI ON EST SUR PARTIE DEJA TRAITEE OU SUR PARTIE JAMAIS ENCORE CALCULEE"--> RECATION DIFFERENTES,
; PLUS APRES TOUT CA LE FORMATAGE ANTESCOFO
; PUIS L'ENVOI OSC !!!!!!!
;/!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\
;/!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\
;/!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\ ---- /!\


;VERSION "SEQUENTIELLE" : on n'envoie que lorsqu'on a tout le fragment.
; (pour gérer notes tenues, évènements syncopés...)
(defmethod Improvize&send-groupsAnte-loop-next-factor ((self realTime-improvizer) &optional (scenario nil) (beatduration t) (beatIdxInImpro t) (hostsend t) (portsend t) (adresssend simple-base-string) (numVoice t))
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
  (format *om-stream* "~%~%****** Launching new navigation beginning at idx ~D in impro~%" beatIdxInImpro)
  (go-backwards-with-improtrace? self beatIdxInImpro)

#|
  (if (< beatIdxInImpro 2) 
      ; NE RIEN FAIRE DU TOUT EN FAIT SI ???
      ; /!\ TODO : TROUVER CE BUG !!!
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ;/!\ ---- /!\ ---- /!\ ;/!\ ---- 
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ;/!\ ---- /!\ ---- /!\ ;/!\ ----
      ; C'est normal que les "if" soient imbriqués ainsi ou bien c'est dû au fait d'avoir """temporairement""" commenté réaction au cas "<2" ???????? ; 
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ;/!\ ---- /!\ ---- /!\ ;/!\ ---- 
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ;/!\ ---- /!\ ---- /!\ ;/!\ ---- 
      ;(reset-navigation-params self)
      (format *om-stream* "I FEEL LIKE RESETING !!~%")
    (if (not (= beatIdxInImpro (1+ (CurrentImproIdx self))))
        (progn
          (setf (CurrentImproIdx self) (- beatIdxInImpro 1))
          (if (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))
            ; --> cas où on revient en arrière sur une partie d'impro déjà calculée.
              (progn 
                (format *om-stream* "---> GOING BACK TO STATE ~D IN ORACLE~%" (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                (setf (CurrentStateIdx self) (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                ;MAJCONT 15/10/13
                (setf (continuity self) (Continuity-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))))))))
                  ; --> sinon cas où au début on attaque directement pour la 1ere fois sur un ImproIdx > 1 
                  ; (par exemple parce qu'on attendait que la mémoire soit déjà un peu remplie)

|#
  
#|;---------------------------------------------------------------------------------------------------------------------------
; ----- VERSION ORIGINALE : AVANT MODIF POUR GENERICITE LE 7/01/15

  (let* ((beatlistImpro (Improvize-next-phase self scenario beatduration beatIdxInImpro)); hostsend portsend adresssend))
         ;[In non r-t version :] ALWAYS DONE BEFORE CALLING "save-for-antescofo"
         ;In particular in "generate-improbeatlist-from-oraclelist" (Tune.lisp)
         (beatlistImpro (thread-Beats beatlistImpro beatduration))
         ;[In non r-t version :] DONE IN "save-for-antescofo"
         (beatlistImpro (transfer-syncopated-event beatlistImpro beatduration))
         (beatlistImpro (add-scenario-to-beatlist beatlistImpro beatduration))
         (beatforAntescofo nil)
         ;(beatIdx beatIdxInImpro)
         )
    ;;;; dans "AntescofoTR.lisp"
    (send-beatlist-as-string-group-for-antescofo beatlistImpro beatduration beatIdxInImpro hostsend portsend adresssend numVoice)

    ; BONNE IDEE ?????
    ; je ne sais toujours pas, mais remonté avant le let...
    ;(if (< beatIdxInImpro 2) (reset-navigation-params self))
    
    ; Serveur existe et ouvert ?
    ;(if (and (boundp '*server*) (not (null *server*)))
    ;     ; Traduction en group antescofo et envoi
    ;    (progn
    ;      (loop for beat in beatlistImpro do 
    ;            (progn
    ;              (setf beatforAntescofo (string-group-for-antescofo beat beatduration numVoice)); "string-group...." = celui dans "AntescofoTR.lisp" ???
    ;              (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
    ;        ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
    ;              (incf beatIdx)))
    ;      (format *om-stream* "Generated from beat ~D to ~D ~%" beatIdxInImpro beatIdx))
    ;  (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))

    )
|#
;---------------------------------------------------------------------------------------------------------------------------
; ----- VERSION MODIF POUR GENERICITE LE 7/01/15
  (let* ((GeneratedImpro (Improvize-next-phase self scenario beatduration beatIdxInImpro)); hostsend portsend adresssend))
         (ProcessedGeneratedImpro (FormatOutputSequence GeneratedImpro beatduration)))
    ;;;; dans "AntescofoTR.lisp"
    ;(send-beatlist-as-string-group-for-antescofo beatlistImpro beatduration beatIdxInImpro hostsend portsend adresssend numVoice)
    ;((sequence list) beatIdxInImpro hostsend portsend adresssend numVoice) NON &optional beatduration)
    (osc-send-sequence-fragment ProcessedGeneratedImpro beatIdxInImpro hostsend portsend adresssend numVoice); beatduration)

    ; BONNE IDEE ?????
    ; je ne sais toujours pas, mais remonté avant le let...
    ;(if (< beatIdxInImpro 2) (reset-navigation-params self))
    
    ; Serveur existe et ouvert ?
    ;(if (and (boundp '*server*) (not (null *server*)))
    ;     ; Traduction en group antescofo et envoi
    ;    (progn
    ;      (loop for beat in beatlistImpro do 
    ;            (progn
    ;              (setf beatforAntescofo (string-group-for-antescofo beat beatduration numVoice)); "string-group...." = celui dans "AntescofoTR.lisp" ???
    ;              (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
    ;        ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
    ;              (incf beatIdx)))
    ;      (format *om-stream* "Generated from beat ~D to ~D ~%" beatIdxInImpro beatIdx))
    ;  (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))

    )
;---------------------------------------------------------------------------------------------------------------------------
)


;;; ENVOYER A MAX
;---------------------------------------------------------------------------------------------------------------------------
; ----- NOUVEAU POUR GENERICITE LE 7/01/15
;---------------------------------------------------------------------------------------------------------------------------
; GENERAL : A METTRE DANS LE FUTUR "EVENT.LISP
(defmethod osc-send-sequence-fragment ((sequence list) beatIdxInImpro hostsend portsend adresssend numVoice); &optional beatduration)
  ;(when (beatduration)
     ; (osc-send-sequence-fragment-of sequence (car sequence) beatIdxInImpro hostsend portsend adresssend numVoice beatduration)
    (osc-send-sequence-fragment-of sequence (car sequence) beatIdxInImpro hostsend portsend adresssend numVoice)
   ; )
)
;cas général
(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent t) beatIdxInImpro hostsend portsend adresssend numVoice); &optional beatduration) 
sequence)
;---------------------------------------------------------------------------------------------------------------------------
; PARTICULIER : A REDEFINIR A CHAQUE FOIS DANS LE FICHIER DEFINISSANT UN TYPE DE CONTENU
;---------------------------------------------------------------------------------------------------------------------------
; A METTRE DANS LE FUTUR "BEAT.LISP"



; POUR L'INSTANT DANS ANTESCOFO.LISP

;;;; 21/02/15
;;;; PROBLEME !!!!!! BEATDURATION POUR THREAD BEAT DOIT ÊTRE FAIT DANS FORMATOUTPUT... SURCHARGE POUR BEAT ! DONC SE DEBROUILLER DANS LA SUCCESSION DES APPELS POUR QUE FORMATTED DE RESULT IMPRO RENTRE DANS OSCSEND...



; (defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent beat) &optional beatduration beatIdxInImpro hostsend portsend adresssend numVoice) [...] )
;---------------------------------------------------------------------------------------------------------------------------


;;; FORMATER LA SEQUENCE GENEREE
;---------------------------------------------------------------------------------------------------------------------------
; ----- NOUVEAU POUR GENERICITE LE 7/01/15
;---------------------------------------------------------------------------------------------------------------------------
; GENERAL : A METTRE DANS LE FUTUR "EVENT.LISP"
(defmethod FormatOutputSequence ((sequence list) &optional beatduration)
  (when beatduration
      (FormatOutputSequenceOf sequence (car sequence) beatduration)
    (FormatOutputSequenceOf sequence (car sequence))))
;cas général
(defmethod FormatOutputSequenceOf ((sequence list) (whencontent t) &optional beatduration) sequence)
;---------------------------------------------------------------------------------------------------------------------------
; PARTICULIER : A REDEFINIR A CHAQUE FOIS DANS LE FICHIER DEFINISSANT UN TYPE DE CONTENU
;---------------------------------------------------------------------------------------------------------------------------
; A METTRE DANS LE FUTUR "BEAT.LISP"
(defmethod FormatOutputSequenceOf ((sequence list) (whencontent beat) &optional beatduration)
 (let* ((ProcessedBeatsSequence sequence)         
        ;[In non r-t version :] ALWAYS DONE BEFORE CALLING "save-for-antescofo"
        ;In particular in "generate-improbeatlist-from-oraclelist" (Tune.lisp)
        (ProcessedBeatsSequence (thread-Beats ProcessedBeatsSequence beatduration))
        ;[In non r-t version :] DONE IN "save-for-antescofo"
        (ProcessedBeatsSequence (transfer-syncopated-event ProcessedBeatsSequence beatduration))
        (ProcessedBeatsSequence (add-scenario-to-beatlist ProcessedBeatsSequence beatduration))
        ;;;;;;;;;;;;;  (beatforAntescofo nil) ;; ????????????????
        ;(beatIdx beatIdxInImpro)
        ) 
   ProcessedBeatsSequence))
;---------------------------------------------------------------------------------------------------------------------------

#|

;-- "hack" jean

(defmethod process-list ((l list) (type t))
  (print "n'importe"))

(defmethod process-list ((l list) (type (eql 'nika)))
  (print "i am jerome nika"))

(defclass nika () ())
(defclass niko (nika) ())


(let ((list (list (make-instance 'nika) 2 3)))
  (process-list list (type-of (car list))))

;-- Equivalent version précédente

(defmethod process-list ((l list) (type t))
  (print "n'importe"))

(defmethod process-list ((l list) (type nika))
  (print "i am jerome nika"))

(defclass nika () ())
(defclass niko (nika) ())


(let ((list (list (make-instance 'niko) 2 3)))
  (process-list list (car list)))

|#



;VERSION "SEQUENTIELLE" : CALCULE tout un ""fragment"", ainsi on n'ENVERRA A ANTESCOFO (fonction du dessus "Improvize&send-groupsAnte-loop-next-factor")  que lorsqu'on a tout le fragment.
; (pour gérer notes tenues, évènements syncopés...)
(defmethod Improvize-next-phase ((self realTime-improvizer) (scenario t) (beatduration t) (beatIdxInImpro t)) ;(hostsend t) (portsend t) (adresssend simple-base-string)) ;(numVoice t))
  (format *om-stream* "~%~%****** Launching new navigation beginning at idx ~D in impro~%" beatIdxInImpro)
  (go-backwards-with-improtrace? self beatIdxInImpro)
  
  (let* ((beatlistImpro (Improvize-navigate-one-phase self (list-length scenario) scenario))
         ;;;;[In non r-t version :] ALWAYS DONE BEFORE CALLING "save-for-antescofo" ------>>>>>> DONC FAIT PAR DEFAUT ICI !
         ;In particular in "generate-improbeatlist-from-oraclelist" (Tune.lisp)

         (beatlistImpro (thread-Beats beatlistImpro beatduration))

         ;;;;[In non r-t version :] DONE IN "save-for-antescofo" ------>>>>>> HERE DONE IN "Improvize&send-groupsAnte-loop-next-factor"
         ;(beatlistImpro (transfer-syncopated-event beatlistImpro beatduration))
         ;(beatlistImpro (add-scenario-to-beatlist beatlistImpro beatduration))
         ;(beatforAntescofo nil)
         ;(beatIdx beatIdxInImpro)
         )
    
    ;PLUS LA CAR MODULARISE
    ;(send-beatlist-as-string-group-for-antescofo beatlistImpro beatduration beatIdxInImpro hostsend portsend adresssend simple-base-string numVoice)
    beatlistImpro))



; Quand generation query, teste si sur partie déjà calculée (donc réécriture) 
; ou si nouvelle partie, et mise à jour des états... du RealTimeImprovizer
; si nécessaire
; --------------------------------------------------------------------------
(defmethod go-backwards-with-improtrace? ((self realTime-improvizer) (beatIdxInImpro t))
  #|
  ; Cékoissa ? que du vieux ou du à reregarder ?
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
      ; NE RIEN FAIRE DU TOUT EN FAIT SI ???
      ; /!\ TODO : TROUVER CE BUG !!!
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!
      ; C'est normal que les "if" soient imbriqués ainsi ou bien c'est dû au fait d'avoir """temporairement""" commenté réaction au cas "<2"
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!
      ;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!;/!\ ---- /!\ ---- /!\ ---- /!
      ;(reset-navigation-params self)
      (format *om-stream* "I FEEL LIKE RESETING !!~%")
    (if (not (= beatIdxInImpro (1+ (CurrentImproIdx self))))
        (progn
          (setf (CurrentImproIdx self) (- beatIdxInImpro 1))
          (if (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))
            ; --> cas où on revient en arrière sur une partie d'impro déjà calculée.
              (progn 
                (format *om-stream* "---> GOING BACK TO STATE ~D IN ORACLE~%" (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                (setf (CurrentStateIdx self) (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                ;MAJCONT 15/10/13
                (setf (continuity self) (Continuity-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))))))))
                  ; --> sinon cas où au début on attaque directement pour la 1ere fois sur un ImproIdx > 1 
                  ; (par exemple parce qu'on attendait que la mémoire soit déjà un peu remplie)
  )


 

; TRES INSPIRE DE IMPROVIZE (improvizer.lisp) !!!
; MODULARISER ET FAIRE APPEL A DES MEMES FONCTIONS !!!
; -----------------------------------------------------
(defmethod Improvize-navigate-one-phase ((self realTime-improvizer) (length t) &optional (scenario nil))
  (let ((impro nil) (current-scenario-suffix nil) (next-scenario-suffix nil))  
    (format *om-stream* "-----------------------~%")
    (when (null (veclrs self)) (setf (bestSuffixMode self) nil) (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))  
    (when scenario (setf next-scenario-suffix scenario))

    ;===========!!!===========
    ; diff 1/2 avec Improvize !
    ;===========!!!===========    
    (setf (StartingPointNextPhaseFound? self) nil)

    ; JEROME 03/09/13
    ;===========***********!!!***********===========
    ; TEST "min ..." du coup faut-il faire qqch
    ; avec le booleen "nextPrefix..." ?
    ;===========***********!!!***********===========
    (loop for i from 1 to (min length (maxetat self)) ; avant : juste length
          for label =  (pop scenario) 
          do
          (setf current-scenario-suffix next-scenario-suffix)
          (setf next-scenario-suffix scenario)
          collect (Improvize-next-state self current-scenario-suffix)
          ;===========!!!===========
          ; diff 2/2 avec Improvize !
          ;===========!!!===========
          while (not (StartingPointNextPhaseFound? self))
          )))

 
;===================================
;=           MULTITHREADS          =
;===================================
;*********NAVIGATION TR***********
;Calls "Improvize-send-groupsAnte-loop-next-factor" (AntescofoTR.lisp)
;*********************************


; GENERIQUE OK ... -> VOIR Improvize&send-groupsAnte-loop-next-factor
(defmethod navigate-realtime ((self realTime-improvizer) (NavigationOrder list) (beatduration t) (hostsend t) (portsend t) (adresssend simple-base-string) &optional numVoice)
  (let* ((SuffixScenario (nth 1 NavigationOrder))
         (beatIdxInImpro (nth 0 NavigationOrder)))
    (if (otext self 10)
        (let ((VoiceNum 1))
          (when numVoice (setf VoiceNum numVoice))
          (Improvize&send-groupsAnte-loop-next-factor self suffixgrid beatduration beatIdxInImpro hostsend portsend adresssend numAnte))
      )))
 






;*********APPRENTISSAGE TR***********
; Calls functions in MidiTR.lisp (midievtsList-to-beatlist)
; Or in AudioBeat.lisp
;************************************
#|  
;-------------------------------------------------------------
; !!!!!!!!! VERSION MIDI !!!!!!!!!
;-------------------------------------------------------------
(defmethod learn-realtime ((self realTime-improvizer) (midievts-list list))
  (let* ((res (midievtsList-to-beatlist midievts-list))
         (beatlist (first res))
         (defaultbeatdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le même defaultbeatdur, mais ensuite ???
      ; quand differents tempi donnés par Antescofo ????? 
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

;-------------------------------------------------------------
; !!!!!!!!! EQUIVALENT AUDIO DEFINI DANS AUDIOBEAT.LISP !!!!!!!!!
;-------------------------------------------------------------
(defmethod learn-realtime ((self realTime-improvizer) (evtsfrombuffer-list list))
  (let* ((res (evtsfrombuffer-list-to-audiobeatlist evtsfrombuffer-list))
         (audiobeatlist (first res))
         (defaultbeatdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le même defaultbeatdur, mais ensuite ???
      ; quand differents tempi donnés par Antescofo ????? 
      ; LE PROBLEME SE POSAIT DEJA AVANT NON ?????????
      ;-----------------------------------------------
    ;(format *om-stream* "~%~%ENTER LEARNING PHASE !!!~%")
    (if audiobeatlist 
        (progn
          ;;(format *om-stream* "... indeed I learn !!!~%")
          (setf (RefTempo self) defaultbeatdur)
                  ;------> TEST (cf defmethod learn-event-list)
          (learn-event-list self audiobeatlist)
          (format *om-stream* "~%~%++++++ Learn audiobeatlist of length : ~D -----> New maxetat : ~D~%" (list-length audiobeatlist) (maxetat self))))
    ;(format *om-stream* "LEAVE LEARNING PHASE !!!~%~%~%")
    ))

|#

 

(defmethod kill-realtime ((self realTime-improvizer))
  ;(loop for process-name in (nameRunningThreads self) do
  ;      (mp:process-kill (mp:find-process-from-name process-name)))
  ;(setf (nameRunningThreads self) '()
        ;(lock self) nil
        );)

