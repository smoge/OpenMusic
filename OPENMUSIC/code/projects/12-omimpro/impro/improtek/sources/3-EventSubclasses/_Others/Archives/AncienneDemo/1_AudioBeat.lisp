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
;(defmethod clone-object ((self t))
;  (let ((cloned-object (make-instance (class-of self))))
;    (loop for sl in 
;          (mapcar #'slot-definition-name (class-slots (class-of self))) do
;          (setf (slot-value cloned-object sl) 
;                (slot-value self sl)))      
;    cloned-object))



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


;==============================================================================================================



(defmethod ImprovizeOnProfileScenario ((self improvizer) (length integer) numVoice (scenario list))
  (let* ((beatlist (ImprovizeProfile self ;(1- length) 
                              length numVoice scenario))
         (beat1 (first beatlist)))
;    (when (or (null (Midiset beat1))
;              (> (MEOnset (first (Midiset beat1))) 0))
;      (setf (Midiset beat1) (cons '(60 0 100 1 11) (Midiset beat1))))
    beatlist))

(defmethod ImprovizeOnProfile ((self improvizer) (length integer) numVoice (harmGrid list))
  (ImprovizeOnProfileScenario self length numVoice harmGrid))
                     
(defmethod reset-navigation-params ((self improvizer))
  (setf (CurrentTranspo self) 0
          (continuity self) 0 
          (NavigationMode self) 'continuity 
          (CurrentStateIdx self) 0
          (PrevStateIdx self) -1))


;================= MODIFICATION IMPROVIZE ================= 
; GENERATION OF A WHOLE IMPROVISATION (reset navigation parameters at the beginning)
;------------------------------------
;Jerome, 24/07/2013 : "reset-navigation-param" isolated in a method
;Jerome, 26/03/2013 : Separated in two different functions
; - "Improvize-one-step" : 1 step in the navigation
; - "Improvize" : loop for the whole grid calling "Improvize-one-step"
;Jerome, 25/1/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;transpo added by M.C., generecity on labels added 10/2/2012
;------------------------------------------------------------
(defmethod ImprovizeProfile ((self improvizer) (length integer) numVoice &optional (harmgrid nil))
  (let ((impro nil) (current-grid-suffix nil) (next-grid-suffix nil) (state nil))  
    (format *om-stream* "-----------------------~%")
    (when (null (veclrs self)) (setf (bestSuffixMode self) nil) (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))  
    (when harmgrid (setf next-grid-suffix harmgrid))
    
  ;Reset navigation info in (self Improvizer)
  ;Jerome, 24/07/2013 : "reset-navigation-param" isolated in a method
  (reset-navigation-params self)
    
  ; Navigation through the whole grid
  (loop for i from 1 to length
        for label =  (pop harmgrid) 
        do
        (setf current-grid-suffix next-grid-suffix)
        
        ;;;;;;;;;;;Marc 23/7/13 -> Jerome 26/10/13          
        ; -> Navigation info displayed at a lower level : Improvize-next-idx

          ;Update for next navigation step
        (setf next-grid-suffix harmgrid)
        (setf state (Improvize-next-state self current-grid-suffix))
          ;One navigation step : /!\ (CurrentStateIdx self) is modified
        (osc-send-sequence-fragment (list state) i "127.0.0.1" 7657 "/modify" numVoice)
        
        collect state
        )))


(defmethod save-impro ((self list) (name t))
  (WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
  t)

(defmethod load-impro ((name t))
  (WITH-OPEN-FILE (in name :direction :input  );:if-does-not-exist) ;:nil)
    (eval (read in)) (eval (read in))))

(defun osc-send-saved-old-impro (path_impro numVoice shift)
  (let ((impro (load-impro path_impro)))
       (loop for j from 0 to (- (list-length impro) 1) do
             
             (osc-send-sequence-fragment (list (nth j impro)) (+ j shift) "127.0.0.1" 7657 "/modify" numVoice)
             (sleep 0.1)
             (format *om-stream* "Pos impro ~a : Idx ~a ; Label ~a ; RMStransf ~a ~%" j 
                     (IdxInBuffer (nth j impro))
                     (Label (nth j impro))
                     (TranspoRMSratio (nth j impro))
                     )
             )
             ))




