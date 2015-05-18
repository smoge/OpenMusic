;Audiobeat.lisp
;Jérôme Nika (1/2/2015)
(in-package :om)

;-----------------------------------------------------
;------------------ DEFINITION CLASSE ----------------
;-----------------------------------------------------

(defclass* Audiobeat (event)
  (
   (Label :initform '() :initarg :Label :accessor Label)
   (IdxInBuffer :initform -1 :initarg :IdxInBuffer :accessor IdxInBuffer)
   (DatesInBuffer :initform '(0 1) :initarg :DatesInBuffer :accessor DatesInBuffer)
   (OrigClass1_idx :initform 0 :initarg :OrigClass1_idx :accessor OrigClass1_idx)
   (OrigClass1_Val :initform 0.033 :initarg :OrigClass1_Val :accessor OrigClass1_Val)
   (Transpo :initform 0 :initarg :Transpo :accessor Transpo)
   (TranspoRMSratio :initform 1.0 :initarg :TranspoRMSratio :accessor TranspoRMSratio)
   (InitBeatdur :initform 500 :initarg :InitBeatdur :accessor InitBeatdur)
   (Feature :initform nil :initarg :Feature :accessor Feature)  
   (ListClass1_Val :initform '(0.0362822643434120 0.0542105138304180 0.0680410208165630 0.0839446341509650 0.101298833670700 0.122496311207598 0.144265965814417) :initarg :ListClass1_Val :accessor ListClass1_Val)
   (AudioFile :initform "path" :initarg :AudioFile :accessor AudioFile)
   ))


(defmethod NewAudiobeat (Beatlabel Idx Dates &optional Transp Beatdur Path ClassValInBuffer ClassIdxInBuffer Transporatio)
  (let* ((audiobeat (make-instance 'Audiobeat
                                   :Label Beatlabel
                                   :IdxInBuffer Idx
                                   :DatesInBuffer Dates
                                   )))
    (when Transp (setf (Transpo Audiobeat) Transp))
    (when Beatdur (setf (InitBeatdur Audiobeat) Beatdur))
    (when Path (setf (Audiofile Audiobeat) Path))
    (when ClassValInBuffer (setf (OrigClass1_Val Audiobeat) ClassValInBuffer))
    (when ClassIdxInBuffer (setf (OrigClass1_idx Audiobeat) ClassIdxInBuffer))
    (when Transporatio (setf (TranspoRMSratio Audiobeat) Transporatio))
    audiobeat))


;-----------------------------------------------------
;------------------ GENERATION -----------------------
;-----------------------------------------------------
;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedBeat ((self beat) int)
;- eligible-beat? ((self beat) (label list))
;- CompareEvents ((Event1 beat) (event2 beat))TranspoRMSratio
;- clone-object ((self beat))0.033
;
; when using label objects instead of simple lists ('garnerlabel'...):OrigClass1_Val
;- TransposeLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))

;--> continuations-by-suppleance, find-beat-label-match, choose-factor-link uses these funcOrigClass1_Valtions only


(defmethod CompareEvents ((event1 Audiobeat) (event2 Audiobeat)) 
  (and ;TranspoRMSratio
   (or 
    (equal (Label event1) (Label event2))
       (equalLabel (Label event1) (Label event2)))
   )
  )

; Change name : eligible-event?
(defmethod eligible-beat? ((self Audiobeat) (label list)) 
  (and 
   (or (null label) 
       (equalLabel label (Label self)))))

; Change name : TransposeClonedEvent
#|
(defmethod TransposeClonedBeat ((self Audiobeat) int)
   (let ((ClonedAudiobeat (clone-object self))
         
         )
     (setf (Label ClonedAudiobeat) (TransposeLabel (Label ClonedAudiobeat) int))
     (setf (Transpo ClonedAudiobeat) (+ (Transpo ClonedAudiobeat) int))
     ClonedAudiobeat))
|#
(defmethod TransposeClonedBeat ((self Audiobeat) delta)
   (let* (
          (ClonedAudiobeat (clone-object self))
          (origclassidx (OrigClass1_idx ClonedAudiobeat))
          (currentclassidx (+ origclassidx (Transpo ClonedAudiobeat)))
          (destclassidx (+ currentclassidx delta))
          (int 0)
          )

     (if (< destclassidx 0)
         (setf destclassidx 0)
       (if (> destclassidx 6)
           (setf destclassidx 6)))
     (setf int (- destclassidx currentclassidx))
     
     (setf (Label ClonedAudiobeat) (TransposeLabel (Label ClonedAudiobeat) int))
     (setf (Transpo ClonedAudiobeat) (+ (Transpo ClonedAudiobeat) int))
     (setf (TranspoRMSratio ClonedAudiobeat) 
           (/ 
            (nth destclassidx (ListClass1_Val ClonedAudiobeat))  
            (nth origclassidx (ListClass1_Val ClonedAudiobeat))))
     ClonedAudiobeat))

; Deep-copy... à déplacer dans event
(defmethod clone-object ((self t))
  (let ((cloned-object (make-instance (class-of self))))
    (loop for sl in 
          (mapcar #'slot-definition-name (class-slots (class-of self))) do
          (setf (slot-value cloned-object sl) 
                (slot-value self sl)))      
    cloned-object))



;---------------------------- ((s

; Change name : ScenarioFromImprovizer
(defmethod HarmGridFromImprovizer ((self Improvizer))
  (if (equal (type-of (otext self 1)) 'beat)
      (loop for i from 1 to (maxetat self) collect (harmlabel (otext self i)))
    (if (equal (type-of (otext self 1)) 'melobeat)
        (loop for i from 1 to (maxetat self) collect (melosignature (otext self i)))
      (if (equal (type-of (otext self 1)) 'Audiobeat)
        (loop for i from 1 to (maxetat self) collect (Label (otext self i)))
      ))))

; Change name : empty-event?
(defmethod empty-beat? ((self Audiobeat))
  (or
   (null (IdxInBuffer self))
   (< (IdxInBuffer self) 0)
   ))



;-----------------------------------------------------
;------------------ APPRENTISSAGE --------------------
;-----------------------------------------------------
#|

Comparaison pour apprentissage est fait dans Oracle.lisp :

(defmethod transition ((self pythie) (etat integer) (objet t))
  (let ((arrows (gethash  etat (hashtransition self))))
    (loop for arrow in arrows
          if (funcall (comparateur self) objet (otext self arrow))
          do (return arrow)
          finally (return nil))))

Tout repose donc sur "(funcall (comparateur self) objet (otext self arrow))".
Comparateur défini comme méthode DE LA CLASSE PYTHIE... En pratique on choisissait comparateur = CompareEvent et 
on redéfinissait CompareEvent pour chaque classe... Pourquoi pas plus simplement définir directement comparateur 
sur la classe de "l'objet" ?

|#


;-----------------------------------------------------
;------------------ FOR REAL TIME --------------------
;-----------------------------------------------------

; === METHODS FOR REALTIME ===

(defmethod PostProcessSequenceOf ((sequence list) (whencontent audiobeat) &optional beatduration) sequence)

#|
(defmethod send-to-max-interface-sequence-fragment-of ((sequence list) (whencontent audiobeat) &optional beatduration beatIdxInImpro hostsend portsend adresssend numAntescofo)
  (let ((beatIdx beatIdxInImpro))
    (if (and (boundp '*server*) (not (null *server*)))
         ; Traduction en group antescofo et envoi
        (progn
          (loop for audiobeat in sequence do 
                (progn
                  (setf beatforAntescofo (beat-as-string-group-fo ((sr-antescofo beat beatduration numAntescofo))
                ;(osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) hostsend portsend)
                  (osc-send (list adresssend beatIdx beatforAntescofo) hostsend portsend)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
                  (incf beatIdx)))
          (format *om-stream* "Generated from beat ~D to ~D ~%" beatIdxInImpro beatIdx))
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))
|#

#|

;EQUIVALENT MIDI DEFINI A L'ORIGINE DANS REALTIMEIMPROVIZER.LIS
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
 ((s


; === TOOLS FOR REALTIME ===

; Infos received from Max : " '( '(Beatlabel Idx Dates &optional Transp Beatdur Path) '(...) ... )  "
(defun evtsfrombuffer-list-to-audiobeatlist (l)
  (let ((meanbeatdur 0) (cpt 0) (audiobeatlist '()))
    (loop for i in l do 
          (progn
            (setf audiobeatlist
                  (append 
                   audiobeatlist
                   (NewAudiobeat 
                    (nth 0 i) 
                    (nth 1 i)  
                    (nth 2 i)  
                    (if (nth 3 i) (nth 3 i))
                    (if (nth 4 i) (nth 4 i)) 
                    (if (nth 5 i) (nth 5 i)))))

            (if (nth 4 i) 
                (progn
                  (setf meanbeatdur (+ meanbeatdur (nth 4 i)))
                  (setf cpt (+ cpt 1))))
            ))
    (if (> cpt 0) (setf meanbeatdur (/ meanbeatdur cpt)))
    (list audiobeatlist meanbeatdur)))

|#



