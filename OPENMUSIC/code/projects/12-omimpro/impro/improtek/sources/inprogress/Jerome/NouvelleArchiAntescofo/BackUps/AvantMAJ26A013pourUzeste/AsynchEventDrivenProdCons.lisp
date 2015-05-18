;(require-library 'bordeaux-threads)
(in-package :om)

;-------------------------------------------------------------------------
;http://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;-------------------------------------------------------------------------


;AsyncEvtDrivenProdConsSystem
(defclass AEDPCS ()
           (
            (running :initform t :initarg :running :accessor running)
            ;SharedData
            (sharedData :initform nil :initarg :sharedData :accessor sharedData)
            (LearnMethodOnSharedData :initform 'learnInAEDPCS :initarg :LearnMethodOnSharedData :accessor LearnMethodOnSharedData)
            (GenerateMethodOnSharedData :initform 'generateWithAEDPCS :initarg :GenerateMethodOnSharedData :accessor GenerateMethodOnSharedData)
            ;Locks and CV on sharedData
            (lock-on-SD :initform (bordeaux-threads:make-lock "lock-on-SD") :initarg :lock-on-SD :accessor lock-on-SD)
            (CV-on-SD :initform (bordeaux-threads:make-condition-variable) :initarg :CV-on-SD :accessor CV-on-SD)
            ;Events to learn channel
            (L-EventsToLearn :initform () :initarg :L-EventsToLearn :accessor L-EventsToLearn)
            (lock-on-L :initform (bordeaux-threads:make-lock "lock-on-L") :initarg :lock-on-L :accessor lock-on-L)
            (CV-on-L :initform (bordeaux-threads:make-condition-variable) :initarg :CV-on-L :accessor CV-on-L)
            ;Orders for navigation channel
            (G-NavigationOrders :initform () :initarg :G-NavigationOrders :accessor G-NavigationOrders)
            (lock-on-G :initform (bordeaux-threads:make-lock "lock-on-G") :initarg :lock-on-L :accessor lock-on-G)
            (CV-on-G :initform (bordeaux-threads:make-condition-variable) :initarg :CV-on-L :accessor CV-on-G)))



  
(defmethod! L-fillEventsToLearnChannelWithEvent ((self AEDPCS) (evt t))
            ;;(format *om-stream* "(L-fillEventsToLearnChannel asks L... )~%")
            (bordeaux-threads:acquire-lock (lock-on-L self) t)
            ;;(format *om-stream* "(L-fillEventsToLearnChannel LOCK L )~%")
            ;Produire et remplir le canal
            ;(push evt (self L-EventsToLearn))
            ;;(format *om-stream* "L-GONNAFILLWITH ~a : ~%" evt)
            (setf (L-EventsToLearn self) (append (L-EventsToLearn self) (list evt)))
            ;;(format *om-stream* "Now (L-EventsToLearn self) = ~a : ~%" (L-EventsToLearn self))
            (bordeaux-threads:condition-notify (CV-on-L self))
            ;(format *om-stream* "(L-fillEventsToLearnChannel NOTIFY CVL, after setting L-EventsToLearn = ~a)~%" (L-EventsToLearn self))
            (bordeaux-threads:release-lock (lock-on-L self))
            ;;(format *om-stream* "(L-fillEventsToLearnChannel UNLOCK L)~%")
            )


(defmethod! G-fillNavigationOrdersChannelWithEvent ((self AEDPCS) (evt t))
          ;;(format *om-stream* "(G-fillNavigationOrdersChannel asks G...)~%")
          (bordeaux-threads:acquire-lock (lock-on-G self) t)
          ;;(format *om-stream* "(G-fillNavigationOrdersChannel LOCK G)~%")
          ;Produire et remplir le canal
          ;(push i G-NavigationOrders)
                      ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ; BRICOLAGE !
            ; ICI ON OUBLIE TOUT POUR NE GARDER QUE LE DERNIER INDICE (POUR NE PAS LANCER TOUS LES CALCULS CONTIGUS PRECEDENTS)
            ; -> DONC TRES PARTICULIER A CETTE UTILISATION, PAS GENERIQUE
            ; ET PUIS TOUT CA DEVRAIT ETRE GERE DANS UNE METHODE PROPRE A CHAQUE CLASSE, COMME POUR GENERATE-EVENT...
          ;(setf (G-NavigationOrders self) (append (G-NavigationOrders self) (list evt)))
          (setf (G-NavigationOrders self) (list evt))
          
          (bordeaux-threads:condition-notify (CV-on-G self))
          ;(format *om-stream* "(G-fillNavigationOrdersChannel NOTIFY CVG, after setting G-NavigationOrders = ~a )~%" (G-NavigationOrders self))
          (bordeaux-threads:release-lock (lock-on-G self))
          ;;(format *om-stream* "(G-fillNavigationOrdersChannel UNLOCK G)~%")
          )

(defmethod! L-processLearning ((self AEDPCS))
  (let ((OwnsLockSD nil) (OwnsLockL nil))
    ;(loop for i from 1 to 50 do
    (loop while (running self) do
        (progn
          (if (not OwnsLockL)
              (progn
                ;;(format *om-stream* "(L-processLearning asks L...)~%")
                (setf OwnsLockL (bordeaux-threads:acquire-lock (lock-on-L self) t))
                ;;(format *om-stream* "(L-processLearning LOCK L)~%")
                ))
          
          ;Traiter les évènements
          ;Vider le canal
          (if OwnsLockL
              (progn
                (if (not OwnsLockSD)
                    (progn
                      ;;(format *om-stream* "(L-processLearning asks SD...)~%")
                      (setf OwnsLockSD (bordeaux-threads:acquire-lock (lock-on-SD self) t))
                      ;;(format *om-stream* "(L-processLearning LOCK SD)~%")
                      ))
                (loop while (car (L-EventsToLearn self)) do
                        (learn-event self (pop (L-EventsToLearn self))))
                (bordeaux-threads:condition-notify (CV-on-SD self)) 
                ;(format *om-stream* "(L-processLearning NOTIFY CVSD, after setting sharedData = ~a)~%" (sharedData self))
                (bordeaux-threads:release-lock (lock-on-SD self))
                (setf OwnsLockSD nil)
                ;;(format *om-stream* "(L-processLearning UNLOCK SD)~%")
                ))

          ;Si le canal est vide -> CV-wait
          (loop while (not (L-EventsToLearn self)) do
                ;"the caller must always test on waking that there is threading to be done, instead of assuming that it can go ahead."
                (progn 
                  ;;(format *om-stream* "(L-processLearning sleeps waiting L CVL...)~%")
                  ;VU LA FONCTION, SUREMENT PAS BESOIN, c'est plutôt le suivant qui doit provoquer le deadlock
                  ;**********************************************************************************
                  (if OwnsLockSD 
                      (progn
                        (bordeaux-threads:release-lock (lock-on-SD self))
                        (setf OwnsLockSD nil)
                        ;;(format *om-stream* "(L-processLearning and so releases SD that it held...)~%")
                        ))
                  ;**********************************************************************************
                  (bordeaux-threads:condition-wait (CV-on-L self) (lock-on-L self))))
          (setf OwnsLockL t)
          ;;(format *om-stream* "(L-processLearning WOKE UP => LOCK L)~%")  
          ;**********************************************************************************
          ;;(format *om-stream* "(L-processLearning WOKE UP => ask SD...)~%") 
          (bordeaux-threads:acquire-lock (lock-on-SD self))
          ;;(format *om-stream* "(L-processLearning LOCK SD)~%") 
          (setf OwnsLockSD t)
          ;**********************************************************************************  
          ;(sleep 0.2)
          ;(sleep 0.05)
          ))
    ;(bordeaux-threads:release-lock (lock-on-L self))
    ;(bordeaux-threads:release-lock (lock-on-SD self))
    ))

(defmethod! G-processNavigation ((self AEDPCS))
  (let ((OwnsLockSD nil) (OwnsLockG nil))
    ;(loop for i from 1 to 40 do
    (loop while (running self) do
          (progn
            (if (not OwnsLockG)
                (progn
                  ;;(format *om-stream* "(G-processNavigation asks G...)~%")
                  (setf OwnsLockG (bordeaux-threads:acquire-lock (lock-on-G self) t))
                  ;;(format *om-stream* "(G-processNavigation LOCK G)~%")
                  ))
            ;Traiter les évènements
            ;Vider le canal
            (if (not OwnsLockSD)
                (progn   
                  ;;(format *om-stream* "(G-processNavigation asks SD...)~%")
                  (setf OwnsLockSD (bordeaux-threads:acquire-lock (lock-on-SD self) t))
                  ;;(format *om-stream* "(G-processNavigation LOCK SD)~%")
                  ))
              
            ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ; BRICOLAGE !
            ; ICI ON OUBLIE TOUT POUR NE GARDER QUE LE DERNIER INDICE (POUR NE PAS LANCER TOUS LES CALCULS CONTIGUS PRECEDENTS)
            ; -> DONC TRES PARTICULIER A CETTE UTILISATION, PAS GENERIQUE
            ; ET PUIS TOUT CA DEVRAIT ETRE GERE DANS "GENERATE EVENT" POUR QUE CELA PUISSE ETRE DEFINI A CHAQUE FOIS
            ;=====>> REFAIRE GENERATE-EVENT PRENANT POUR ARGUMENT LA liste ET PAS JUSTE SA TETE
            (loop while (car (G-NavigationOrders self)) do
                  (generate-event self (pop (G-NavigationOrders self))))
            
            ;;(format *om-stream* "(G-processNavigation sleeps waiting SD CVSD...)~%")
            ;**********************************************************************************
            (if OwnsLockG 
                (progn
                  (bordeaux-threads:release-lock (lock-on-G self))
                  (setf OwnsLockG nil)
                  ;;(format *om-stream* "(G-processNavigation  and so releases G that it held...)~%")
                  ))
            ;**********************************************************************************
            (bordeaux-threads:condition-wait (CV-on-SD self) (lock-on-SD self))
            (setf OwnsLockSD t)
            ;;(format *om-stream* "(G-processNavigation WOKE UP => LOCK SD )~%")
            ;**********************************************************************************
            (if (not OwnsLockG)
                (progn
                  ;;(format *om-stream* "(G-processNavigation WOKE UP => ask G...)~%") 
                  (setf OwnsLockG (bordeaux-threads:acquire-lock (lock-on-G self)))
                  ;;(format *om-stream* "G-processNavigation LOCK G)~%")
                  )) 
            ;**********************************************************************************  
            
            ;Si le canal est vide -> CV-wait
            (loop while (not (car (G-NavigationOrders self))) do
                (progn
                  ;(format *om-stream* "(G-processNavigation sleeps waiting G CVG)~%")
                  ;**********************************************************************************
                  (if OwnsLockSD 
                      (progn
                        (bordeaux-threads:release-lock (lock-on-SD self))
                        (setf OwnsLockSD nil)
                        ;;(format *om-stream* "(G-processNavigation  and so releases SD that it held...)~%")
                        ))
                  ;**********************************************************************************
                  (bordeaux-threads:condition-wait (CV-on-G self) (lock-on-G self))))
            (setf OwnsLockG t)
            ;;(format *om-stream* "(G-processNavigation WOKE UP => LOCK G)~%")
            ;**********************************************************************************
            (if (not OwnsLockSD)
                (progn
                  ;;(format *om-stream* "(G-processNavigation WOKE UP => ask SD...)~%") 
                  (setf OwnsLockSD (bordeaux-threads:acquire-lock (lock-on-SD self)))
                  ;;(format *om-stream* "G-processNavigation LOCK SD)~%")
                  ))
            
            ;**********************************************************************************  
            ;(sleep 0.2)
            ))
    ;(bordeaux-threads:release-lock (lock-on-G self))
    ;(bordeaux-threads:release-lock (lock-on-SD self))
    ))

#|
(defmethod learn-event ((self AEDPCS) (event-to-learn integer))
            (setf (sharedData self) (append (sharedData self) (list event-to-learn))))

(defmethod generate-event ((self AEDPCS) (GenerationOrder integer))
            (format *om-stream* "~%=====================---->Reading ~D : ~D %=====================~%~%" (- GenerationOrder 1) (nth (- GenerationOrder 1) (sharedData self))))
|#



(defmethod learn-event ((self AEDPCS) (event-to-learn t))
            (apply (LearnMethodOnSharedData self) (list (sharedData self) event-to-learn )))
(defmethod generate-event ((self AEDPCS) (GenerationOrder t))
   (apply (GenerateMethodOnSharedData self) (list (sharedData self) GenerationOrder)))



#|
(setf PCS (make-instance 'AEDPCS :sharedData '(0) :LearnMethodOnSharedData 'write-in-list :GenerateMethodOnSharedData 'read-in-list ))
(defmethod write-in-list ((self list) (i integer))
  (nconc self (list i) ))
(defmethod read-in-list ((self list) (i integer)) 
  (format *om-stream* "---->Reading ~D : ~D ~%~%" (- i 1) (nth (- i 1) self)))


(let* (
      (L-filler (bordeaux-threads:make-thread #'(lambda () (loop for i from 1 to 50 do
                                                                 (progn
                                                                   (L-fillEventsToLearnChannelWithEvent PCS i)
                                                                   (sleep 0.2)
                                                                   )
                                                            )) :name 'L-filler))
      (G-filler (bordeaux-threads:make-thread #'(lambda () (loop for i from 1 to 40 do
                                                                 (progn
                                                                   (G-fillNavigationOrdersChannelWithEvent PCS i)
                                                                   (sleep 0.3)
                                                                   )
                                                            )) :name 'G-filler))
      (L-processer (bordeaux-threads:make-thread #'(lambda () (L-processLearning PCS)) :name 'L-processer))
      (G-processer (bordeaux-threads:make-thread #'(lambda () (G-processNavigation PCS)) :name 'G-processer))
      )
;(sleep 2)
;(setf (running PCS) nil)

)

|#


