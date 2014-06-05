;===================================
;TOUT MAINTENANT DANS REAL TIME IMPROVIZER !!
;===================================




(in-package :om)

;SOLUTION TEMPORAIRE : stocker les messages reçus de Max dans variables surveillées par les "waits" ci-dessous
;--------------------------------------------------------------------------------------------------------------
(defvar *last-received-messagefornavigation* nil)
;(suffixe beatIdxInImpro)
;pour l'instant on utilise la beatduration de la *current-tune*, il faudra ensuite envoyer le tempo calculé par Antescofo pour plus de realtime
(defvar *last-received-messageforlearning* nil)
;juste la liste d'évènements midi

;*********NAVIGATION TR***********
;Calls "Improvize-send-groupsAnte2-loop-next-factor" (AnstescofoTR.lisp)
;*********************************
(defmethod navigate-real-time ((self improvizer) (messagefornavigation-symbol t) (beatduration integer) (host t) (port_ante2 integer))
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
    (Improvize-send-groupsAnte2-loop-next-factor self suffixgrid beatduration beatIdxInImpro host port_ante2))))


;*********APPRENTISSAGE TR***********
; Calls functions in MidiTR.lisp (midievtsList-to-beatlist)
;************************************
(defmethod learn-real-time ((self improvizer) (messageforlearning-symbol t))
  (loop
   ; EN PRATIQUE QUOI ATTENDRE ?
   (mp:process-wait "Waiting for a midi events list" (lambda () (symbol-value messageforlearning-symbol)))
  (let ((midievts-list (symbol-value messageforlearning-symbol)))
    ;SUFFIT POUR QUE CELA NE SE DECLENCHE PAS PLUSIEURS FOIS ???
    (setf (symbol-value messageforlearning-symbol) nil)
    (let* ((res (midievtsList-to-beatlist midievts-list))
           (beatlist (first res))
           (defaultbeatdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le même defaultbeatdur, mais ensuite ???
      ; quand differents tempi donnés par Antescofo ????? 
      ; LE PROBLEME SE POSAIT DEJA AVANT NON ?????????
      ;-----------------------------------------------
      (setf (RefTempo self) defaultbeatdur)
      (learn-event-list self beatlist)))))


;*********LANCER CES PROCESS D'ATTENTE SUR ORACLE QUAND ON CHARGE UNE NOUVELLE TUNE***********
(defmethod launch-realtime-on-improvizer ((self improvizer) (beatduration integer) (host_send t) (port_send_ante2 integer))
(let ((learner
       (mp:process-run-function "Real time learner" ()
                                'learn-real-time
                                self
                                '*last-received-messageforlearning*))

      (navigator
       (mp:process-run-function "Real time navigator" ()
                                'navigate-real-time
                                self
                                '*last-received-messagefornavigation*
                                beatduration
                                host_send
                                port_send_ante2)))))
           
(defmethod launch-realtime-on-oraclechan ((self tune) (numoraclechan integer) (host_send t) (port_send_ante2 integer))
  (launch-realtime-on-improvizer (gethash numoraclechan (oracletable *current-tune*))
                                 (beatduration *current-tune*)
                                 host_send
                                 port_send_ante2))

#|
(defun kill-real-time-process ()
  (mp:process-kill (mp:process-name-to-process "Real time learner"))
  (mp:process-kill (mp:process-name-to-process "Real time navigator")))
|#
           