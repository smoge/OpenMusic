(in-package :om)

;J.Nika (after M. Chemillier and G. Assayag)
;--------------------------------------------------------------------------------



(defclass* improvizer (pythie)
   (
    (name :initform "improvizer" :initarg :name :accessor name)
    (context :initform () :initarg :context :accessor context)
    (continuity :initform 0 :initarg :continuity :accessor continuity)
    ;+++++++++++++
    ;Current info navigation, Jerome 20/02/13
    (NavigationMode :initform 'continuity :initarg :NavigationMode :accessor NavigationMode)
    (CurrentStateIdx :initform 0 :initarg :CurrentStateIdx :accessor CurrentStateIdx)
    (PrevStateIdx :initform -1 :initarg :PrevStateIdx :accessor PrevStateIdx)
    ;+++++++++++++
    (max-continuity :initform 8 :initarg :max-continuity :accessor max-continuity)
    (start-region :initform '(0 100) :initarg :start-region :accessor start-region)
    (fwsuffix :initform T :initarg :fwsuffix :accessor fwsuffix)
    (bwsuffix :initform T :initarg :bwsuffix :accessor bwsuffix)
    (bestSuffixMode :initform nil :initarg :bestSuffixMode :accessor bestSuffixMode)    ;;;;;;2/5/2012 nil
    (useEstrada :initform nil :initarg :useEstrada :accessor useEstrada)
    (useHindemith :initform nil :initarg :useHindemith :accessor useHindemith)
    (RefHarmGrid :initform nil :initarg :RefHarmGrid :accessor RefHarmGrid)
    (HarmGridLength :initform 48 :initarg :HarmGridLength :accessor HarmGridLength)
    (Beats/Measure :initform 4 :initarg :Beats/Measure :accessor Beats/Measure)
    (RefTempo :initform 536 :initarg :RefTempo :accessor RefTempo) ; beat duration in ms
    (CurrentTranspo :initform 0 :initarg :CurrentTranspo :accessor CurrentTranspo)    ; value beetwen -3 and +3
    
    ;+++++++++++++
    ;New controls, Jerome 20/02/13 -----> ADD THIS CONTROLS IN MAX INTERFACE !!!
    (LengthFactorsFromGrid :initform '(1 100) :initarg :start-region :accessor LengthFactorsFromGrid)
    (bestTranspoMode :initform T :initarg :bestTranspoMode :accessor bestTranspoMode)
    (firstWithoutTranspoMode :initform nil :initarg :firstWithoutTranspoMode :accessor firstWithoutTranspoMode)
    (AuthorizedTranspos :initform '(-4 -3 -2 -1 1 2 3 4) :initarg :AuthorizedTranspos :accessor AuthorizedTranspos)
    (randomPrefixOccurrenceMode :initform nil :initarg :randomPrefixOccurrenceMode :accessor randomPrefixOccurrenceMode)
    ;+++++++++++++

    (tabou-mode :initform nil :initarg :tabou-mode :accessor tabou-mode)     ;;;;;;; added by M.C. 11/5/2012: PB after the first impro, 
    (tabou :initform (make-hash-table :test '=) :initarg :tabou :accessor tabou)  ;;;     --> it becomes difficult to find 'matches' 
    (feature  :initform nil :initarg :feature :accessor feature)               ;;;;;;; added by M.C. 15/8/0212, list of 'features' as MIDI codes

    (hashlabeltranspo :initform (make-hash-table :test 'equal) :initarg :hashlabeltranspo :accessor hashlabeltranspo)     ;;;;;;;Marc 13/8/2013 essai pour eviter attente "nothing"
    
    ))

(defmethod set-LengthFactorsFromGrid ((self improvizer) (interval list))
  (setf (LengthFactorsFromGrid self) interval))


(defclass* event ()
  (
   (MidiSet :initform () :initarg :MidiSet :accessor MidiSet)      ; liste 5uples (midi onset dur vel can)
   (duration :initform 500 :initarg :duration :accessor duration)
   (data :initform nil :initarg :data :accessor data)
   ))


(defclass* beat (event)
  (
   (HarmLabel :initform () :initarg :HarmLabel :accessor HarmLabel)
   (RelHarmLabel :initform () :initarg :RelHarmLabel :accessor RelHarmLabel)
   (NumBeat :initform 1 :initarg :NumBeat :accessor NumBeat) ; in the measure
   (RefHarmLabel :initform 1 :initarg :RefHarmLabel :accessor RefHarmLabel) ; label before substitution
   (StartPhrase :initform () :initarg :StartPhrase :accessor StartPhrase) ; booleen
   (QMidiSet :initform () :initarg :QMidiSet :accessor QMidiSet) ; quantized midiset
   (Qdivision :initform 1 :initarg :Qdivision :accessor Qdivision)
   (Density :initform 1 :initarg :Density :accessor Density)
   (feature :initform nil :initarg :feature :accessor feature)               ;;;;;;; added by M.C. 15/8/0212
                                                               ;= nil (not integer) if there is no feature, = MIDI code if there is one
   ))


(defclass* CrossEvent (event)
  (
   ))



;--------------------------------------------------------------------------------

(defmethod set-start-region ((self improvizer) (region list))
  (setf (start-region self) region))

; A DEPLACER CAR LIE A L'INTERFACE (Sliders...)
(defmethod set-start-region-oraclechan ((self Improvizer) begin end begin_slider end_slider)
  (set-start-region self (list (floor (* begin (/ (max begin_slider (1- (maxetat self))) end_slider))) 
                                 (floor (* end (/ (max begin_slider (1- (maxetat self))) end_slider))))))

(defmethod set-start-point ((self improvizer) int)
  (setf (start-region self) (list int (maxetat self))))

(defmethod set-suffix-law ((self improvizer) (suffixes list))
  (setf (bwsuffix self) (first suffixes)
        (fwsuffix self) (second suffixes)))
        
(defmethod set-best-suffix-mode ((self improvizer) (value symbol))
  (setf (bestSuffixMode self) value))

(defmethod set-Estrada-Hind ((self improvizer) (value list))
  (setf (useEstrada self) (first value)
        (useHindemith self) (second value)))



;--------------------------------------------------------------------------------


(defmacro MEPitch (event)
  `(first ,event))

(defmacro MEOnset (event)
  `(second ,event))

(defmacro MEDur (event)
  `(third ,event))

(defmacro MEVel (event)
  `(fourth ,event))

(defmacro MEChannel (event)
  `(fifth ,event))

;--------------------------------------------------------------------------------

(defvar *current-improvizer*)

;modified by M.C. + J.N. for 'start-region'
(defmethod NewImprovizer (&optional beatlist beatdur &key max-continuity)
  (let* ((improvizer (make-instance 'Improvizer
                                   :comparateur 'CompareEvents
                                   :lrsMode t)))
    (when beatlist (loop for i from 0 to (1- (length beatlist)) do (learn-event improvizer (nth i beatlist))))
    (when beatdur (setf (RefTempo improvizer) beatdur))
    (when max-continuity (SetmaxCont improvizer max-continuity))
    (set-start-point improvizer 0)
    improvizer))

(defmethod NbEvent? ((self improvizer))
  (maxetat self))

(defmethod SetmaxCont ((self improvizer) (cont integer))
  (setf (continuity self) 0)
  (setf (max-continuity self) cont))

;Marc 12/5/2012
(defmethod ResetTabou ((self improvizer)) (setf (tabou self) (make-hash-table :test '=)))


;Marc 11/2/2012 ---> change (RefTempo improvizer) to adapt to live performance
(defmethod SetReftempo ((self improvizer) newbeatdur)     
  (loop for i from 1 to (maxetat self) for beat = (otext self i)
        do
        (setf (MidiSet beat) (timestretch (MidiSet beat) (/ newbeatdur (RefTempo self)))
              (duration beat) newbeatdur))
  (setf (RefTempo self) newbeatdur)                                 ; beat duration in ms
  self)
        
;formerly in "Beatlist.lisp"
(defun timestretch (5uples coef)
  (loop for x in 5uples for y = (clone x)
        when (= (length y) 5) do (setf (MEOnset y) (om-round (* coef (MEOnset y))) 
                                       (MEDur y) (if (clock-event-from-midi? y) (MEDur y) (om-round (* coef (MEDur y)))))
        collect y))



;--------------------------------------------------------------------------------
;;; APPRENTISSAGE



(defmethod learn-event ((self improvizer) (event event))
;   (setf *current-improvizer* self)
   (ajouter-objet self event)
   )

;by M.C.
(defmethod learn-event ((self improvizer) (event t))   ; for genericity, when event is a non 'Beat' event (t is for other types)
;   (setf *current-improvizer* self)
   (ajouter-objet self event)
   )

(defmethod learn-event-list ((self improvizer) (events list))
;   (setf *current-improvizer* self)
   (loop for event in events 
         do (ajouter-objet self event) )   
   self)


; Jerome, 10/04/13
;Jerome, 19/02/13 : set-start-point. Correction of the (bug when NewImprovizer called with no arguments and sequence learnt in a second time : "start-region = (0 maxetat_initial) = (0 0) !)
;by M.C., comparison with learnt object is done by the function 'transition' (see Pythie class in Oracle.lisp)
(defmethod ajouter-objet ((self improvizer) (objet t))
  (let ((m (maxetat self)) (Pi1))
    (when (>= (1+ m) (length (vectext self)))
      (setf (vectext self) (adjust-array (vectext self) (+ 500 (length (vectext self)))))
      (when (lrsmode self) (setf (veclrs self) (adjust-array (veclrs self) (+ 500 (length (veclrs self)))))))
    
    (creer-etat self (1+ m))
    ;Jerome, 19/02/13
    (set-start-point self 0)
    (setf (otext self (1+ m)) objet)
    (setf (transition self m objet) (1+ m))
    (loop for etat = (suppleance self m) then (suppleance self etat)
          with Pi1 = m
          while (and (> etat -1)
                     (null (transition self etat objet)))   ; no arrow
          do (setf (transition self etat objet) (1+ m)      ; => add arrow + follow link
                   Pi1 etat)
          finally
          (let ((sp (if (= etat -1)  0 (transition self etat objet)))) ; suffix link -> 0 or target
            ;(format *om-stream* " label=~a SUPPL: ~a => ~a " (harmlabel objet) (1+ m) sp)
            (when sp 
              (setf (suppleance self (1+ m)) sp)
              (when (lrsMode self)
                (setf (lrs self (1+ m)) (LenghrepeatedSuffix self Pi1 sp))))))
    self))


#|
(defmethod learn-beat-list ((self improvizer) (beats list) (StartBeatNb integer))
  ;(print StartBeatNb)
  (decf StartBeatNb)
  (let ((beat-list (make-beat-list beats)))
    (when beat-list (setf (StartPhrase (first beat-list)) T))
    (loop for beat in beat-list  do 
          (setf (Numbeat beat) (mod StartBeatNb (Beats/Measure self))
                (RefHarmLabel beat) (nth (mod StartBeatNb (HarmGridLength self)) (RefHarmGrid self))
                (duration beat) (RefTempo self) )
          (set-qMidiSet beat)
          (setf (density beat) (beat-density beat))
          (learn-event self beat)
          (incf StartBeatNb))  
    self))

(defmethod learn-beats-objects ((self improvizer) (beats list) (StartBeatNb integer))
  (print StartBeatNb)
  (decf StartBeatNb)
  (let ((beat-list beats))
    (when beat-list (setf (StartPhrase (first beat-list)) T))
    (loop for beat in beat-list  do 
          (setf (Numbeat beat) (mod StartBeatNb (Beats/Measure self))
                (RefHarmLabel beat) (nth (mod StartBeatNb (HarmGridLength self)) (RefHarmGrid self))
                (duration beat) (RefTempo self) )
          (set-qMidiSet beat)
          (setf (density beat) (beat-density beat))
          (learn-event self beat)
          (incf StartBeatNb))  
    self))

(defmethod! Oraclize ( (events list) &optional (estrada nil) (hindemith nil))
   (let ((self (NewImprovizer)))
     (setf (useEstrada self) estrada
           (useHindemith self) hindemith)
     (loop for event in events do (learn-event self event))
     self))
|#


;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)
(defmethod CompareEvents ((Event1 beat) (event2 beat)) 
  (and ;(not (empty-beat? event1))
   ;(not (empty-beat? event2))
   ;(= (density event1) (density event2))
   (or (equal (harmlabel event1) (harmlabel event2))   ;;(RefHarmlabel event1) (RefHarmlabel event2))
       (equalLabel (harmlabel event1) (harmlabel event2)) )))



#|
;not used in beat mode
(defmethod CompareEvents ((Event1 CrossEvent) (Event2 CrossEvent))
  (or (and (= (length (MidiSet event1)) (length (MidiSet event2)))
           (loop for ME1 in (MidiSet event1)
                 for ME2 in (MidiSet event2)
                 always (= (mod (abs (MEPitch ME1)) 12) (mod (abs (MEPitch ME2)) 12))))
      (and 
       (> (length (MidiSet event1)) 2)
       (> (length (MidiSet event2)) 2)
       (or (useEstrada *current-improvizer*) (useHindemith *current-improvizer*))
       (let ((ch1 (mapcar #'(lambda (e) (abs (first e)))  (MidiSet event1)))
             (ch2 (mapcar #'(lambda (e) (abs (first e)))  (MidiSet event2))))
         (or
          (and (useEstrada *current-improvizer*)
               (<= (dist-estrada (om* ch1 100) (om* ch2 100)) 1))
          (and (useHindemith *current-improvizer*)
               (eq (hindem  ch1) (hindem  ch2))))))))
|#

;/!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\
;Jerome 20/03/2013
;Jerome 18/05/2013
(defmethod HarmGridFromImprovizer ((self Improvizer))
  (if (equal (type-of (otext self 1)) 'beat)
      (loop for i from 1 to (maxetat self) collect (harmlabel (otext self i)))
    (if (equal (type-of (otext self 1)) 'melobeat)
        (loop for i from 1 to (maxetat self) collect (melosignature (otext self i))))))
  
;(defmethod HarmGridFromImprovizer ((self Improvizer))
;      (loop for i from 1 to (maxetat self) collect (harmlabel (otext self i))));
;
;/!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\!\ /!\ /!\ /!\ /!\!\ /!\ /!\ /!\ /!\!\ /!\ /!\ /!\ /!\!\ /!\ /!\ /!\ /!\
;--------------------------------------------------------------------------------
;;; MORRIS PRATT FOR GENERATION
; Jerome 20/03/2013
;Original Morris&Pratt algorithm : overloaded method
;---------------------------------------------------
;/!\ Arguments : 1)Improvizer 2)grid (list)
;Returns the list of idxs where the whole grid is found
;---------------------------------------------------
(defmethod MorrisPratt ((memory Improvizer) (grid list))
  (MorrisPratt (append (list '(nil)) (HarmGridFromImprovizer memory)) grid)) ;/!\ append nil because the first state (idx=0) of an oracle is empty !

;Overloaded method finding prefixes for the navigation
;-----------------------------------------------------
;/!\ Arguments : 1)Improvizer 2)grid (list)
;-----------------------------------------------------
(defmethod MorrisPratt_prefixes ((memory Improvizer) (grid list))
  (MorrisPratt_prefixes (append (list '(nil)) (HarmGridFromImprovizer memory)) grid)) ; /!\ append nil because the first state (idx=0) of an oracle is empty !



;--------------------------------------------------------------------------------
;;; GENERATION

;by M.C.
(defmethod ImprovizeOnHarmGrid ((self improvizer) (length integer) (harmGrid list))
  (let* ((beatlist (Improvize self ;(1- length) 
                              length harmgrid))
         (beat1 (first beatlist)))
;    (when (or (null (Midiset beat1))
;              (> (MEOnset (first (Midiset beat1))) 0))
;      (setf (Midiset beat1) (cons '(60 0 100 1 11) (Midiset beat1))))
    beatlist))

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
(defmethod Improvize ((self improvizer) (length integer) &optional (harmgrid nil))
  (let ((impro nil) (current-grid-suffix nil) (next-grid-suffix nil))  
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
          ;One navigation step : /!\ (CurrentStateIdx self) is modified
        collect (Improvize-next-state self current-grid-suffix)
        )))



;Jerome, 26/03/2013 : One step in navigation : modification of current/previous states fields
; PLAYS the corresponding beat
;Jerome, 25/01/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;---------------------------------------------------------------------------------
(defmethod Improvize-play-next-state ((self improvizer)  &optional (harmgrid nil) (beatduration integer))  
  ; -> Navigation info displayed at a lower level : Improvize-next-idx
  (let ((beat (Improvize-next-state self harmgrid)))
    (if beat
        (progn
          (Stop-Player *general-player*)
          (play (beats->chseq (list beat) beatduration 0)))
      (format *om-stream* "End of impro !~%"))))


;Jerome, 26/03/2013 : One step in navigation : modification of current/previous states fields
; RETURNS the corresponding beat
;Jerome, 25/01/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;---------------------------------------------------------------------------------
(defmethod Improvize-next-state ((self improvizer)  &optional (harmgrid nil))
  (Improvize-navigate-one-step self harmgrid)
  (if (> (CurrentStateIdx self) 0) 
      (TransposeClonedBeat (otext self (CurrentStateIdx self)) (- (CurrentTranspo self)))
    (null-beat self))
  )


;Jerome, 26/03/2013 : One step in navigation : modification of current/previous states fields
;Jerome, 25/01/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;---------------------------------------------------------------------------------
(defmethod Improvize-navigate-one-step ((self improvizer)  &optional (harmgrid nil))
  (let* ((time-call (get-internal-real-time))
         (index (CurrentStateIdx self))
         (nextindex (Improvize-next-idx self harmgrid)))
    (setf (PrevStateIdx self) index)
    (setf (CurrentStateIdx self) nextindex)
    ;(if *print-navig-basics* 
    ;    (format *om-stream* "Time computation : ~5,2F~%~%" (/ (- (get-internal-real-time) time-call) internal-time-units-per-second)))
    ))

;Jerome, 22/07/2013 : Improvizer field "nextPrefixImpact" for real-time navigation added
;Jerome, 26/03/2013 : One step in navigation : next index in oracle (no modification of current/previous states fields)
;Jerome, 25/01/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;---------------------------------------------------------------------------------
(defmethod Improvize-next-idx ((self improvizer)  &optional (harmgrid nil))
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
              (if *print-navig-basics* (format *om-stream* "c : ~a ~%" nextindex)) 
          (if (available-suppleance self index) 
              (setf (NavigationMode self) 'suppleance)
            (setf (NavigationMode self) 'nothing))))
      ;MODE SUPPLEANCE
      (when (eq (NavigationMode self) 'suppleance)
        (setf nextindex (continuations-by-suppleance self index label-current-transp)) 
        (if (and nextindex (/= nextindex previndex))
            (progn 
              (if *print-navig-basics* (format *om-stream* "--->s : ~a ~%" nextindex))
              (setf (continuity self) 0))
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
              (setf (continuity self) 0))
          (progn               
            (if *print-navig-basics* (format *om-stream* "~a~%" 'empty))
            (setf nextindex 0))))))
    nextindex))

; *print_info_navig* defini dans LoadImproteK   (setf *print_info_navig* 1)
(defmethod display-infolabel ((self improvizer) harmgrid)
  (when harmgrid
    (format *om-stream* "label=~a" (FormatLabel (car harmgrid)))
    (if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", oracle=~a, " (FormatLabel (TransposeLabel (car harmgrid) (- (CurrentTranspo self))))))
    (if (= *print_info_navig* 1) (format *om-stream* " current grid suffix = ~a~%" (TransposeGrid harmgrid (CurrentTranspo self))))))


#|
;==============================================================
;IMPROVIZING WITH MORRIS & PRATT
; Jerome 25/1/2013
;==============================================================

;Example : D'ici d'en bas
;------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 1000)


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(10 40))


(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(play impro)


;Example : D'ici d'en bas : PLAY STEP BY STEP
;-------------------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))
(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 4)
;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(4 10))
; EVALUATE SEVERAL TIMES THESE TWO LINES
;-----------------------------------------
(Improvize-play-next-state oracle_solo grid beatduration)
(pop grid)
;-----------------------------------------




;Example : "J'aime pour la vie counterpoint"
;-------------------------------------------
(setf tune Jaime_tune)
(setf beatduration Jaimesolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp2 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp3 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration))

(setf (max-continuity oracle_solo) 1
      (max-continuity oracle_accomp) 10
      (max-continuity oracle_accomp2) 50
      (max-continuity oracle_accomp3) 100)

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(setf (LengthFactorsFromGrid oracle_solo) '(1 4)
      (LengthFactorsFromGrid oracle_accomp) '(10 20)
      (LengthFactorsFromGrid oracle_accomp2) '(10 40)
      (LengthFactorsFromGrid oracle_accomp3) '(10 50))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid)
      impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid) grid)
      impro_accomp2 (ImprovizeOnHarmGrid oracle_accomp2 (length grid) grid)
      impro_accomp3 (ImprovizeOnHarmGrid oracle_accomp3 (length grid) grid))

(setf impro (merger 
             (beats->chseq impro_solo beatduration 0) 
             (merger 
              (beats->chseq impro_accomp beatduration 0) 
              (merger 
               (beats->chseq impro_accomp2 beatduration 0) 
               (beats->chseq impro_accomp3 beatduration 0)))))

(pgmout 3 3)
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




;Example : "J'aime pour la vie" + "Cantelope Island"
;-------------------------------------------
(setf tune3 Jaime_tune)
(setf grid3 (expand_grid (grid tune3)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) (beatduration tune3)))
(setf oracle_accomp (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(4 10))
(set-LengthFactorsFromGrid oracle_accomp '(1 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid3) grid3))
(setf impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid3) grid3))

(setf impro (merger (beats->chseq impro_solo (beatduration tune3) 0) (beats->chseq impro_accomp (beatduration tune3) 0)))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)



;Example : Cantelope
;-------------------
(setf tune CantelopeIsland_tune)
(setf beatduration (beatduration tune))
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))
(setf (max-continuity oracle_solo) 1000)
(setf (start-region oracle_solo) '(33 100))


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(10 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




===========Save===========
(my-save-as-midi impro beatduration) 
|#




;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedBeat ((self beat) int)
;- eligible-beat? ((self beat) (label list))
;- CompareEvents ((Event1 beat) (event2 beat))
;- clone-object ((self beat))
;when using label objects instead of simple lists ('garnerlabel'...):
;- TransposeLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))

;--> continuations-by-suppleance, find-beat-label-match, choose-factor-link uses these functions only


; MARC 10/08/11 fixed bug concerning negative MIDI codes
(defmethod TransposeClonedBeat ((self beat) int)
   (let ((Clonedbeat (clone-object self)))           ; cloned beat needed not to modify beats in the Oracle
     (setf (HarmLabel Clonedbeat) (TransposeLabel (HarmLabel Clonedbeat) int))
     (loop for evt in (MidiSet Clonedbeat)  ; only for notes, does not  apply if it remains other kind of event like clicks (onset 248)
           when (= (length evt) 5)          ; but in newer version (MidiSet beat) only contains real notes (not other events like clocks)
           do (let ((note (+ (abs (MEPitch evt)) int)))
              (if (< note 0) (incf note 12))       ; pb MIDI pitch  limits 0-127
              (if (> note 127) (decf note 12))
              (setf (MEPitch evt) (* note (if (>= (MEPitch evt) 0) 1  -1)))))  ;BUG 2011/5/29: transposition error for negative pitch (prolongation)
     Clonedbeat))


 
(defmethod TransposeClonedBeat ((self t) int) self)   ; for genericity, when self is a non 'Beat' event (t is for other types)




(defmethod available-suppleance ((self improvizer) (index integer))
  (or (and (bwsuffix self)
           (not (zerop (suppleance self index))))
      (and (fwsuffix self)
           (suppleance-> self index))))


(defmethod ImprovizeByContinuation ((self Improvizer) (nbevents integer) (context list))
   (let ((start-etat
          (loop for suffixe on context
                for etat = (factor-p self context)
                while (null etat)
                finally return (if suffixe etat 0))))
     (Improvize self nbevents :start-etat start-etat)))
     
;--------------------------------------------------------------------------------


(defmethod check-continuity ((self improvizer))
  (if (<= (continuity self) (max-continuity self))
    (true (incf (continuity self)))
    (false (setf (continuity self) 0))))



(defmethod null-beat ((self improvizer))    ;Marc 27/2/2012 pb null improvizer
  (when (otext self 1) (make-instance (class-of (otext self 1)))))

(defmethod empty-beat? ((self beat))
  (null (midiset self)))



;MARC 10/2/2012 generic function that works with 'beats', but also with objects of specific classes ('melobeats', 'relativechords', ...)

(defmethod eligible-beat? ((self beat) (label list)) 
  (and ;(not (empty-beat? self))                      ;MARC 10/2/2012 why controling that the beat is not empty (= silence)?????
       (or (null label) (equalLabel label (;RefHarmLabel 
                                           harmlabel self)))))

(defmethod eligible-beat? ((self t) (label list)) (null label))   ; for genericity, when self is a non 'Beat' event (t is for other types)

;;;;;;;;;;;;Marc 12/5/12 + 15/8/12  -> 'continuations-by-suppleance'
(defmethod reduce-eligible-beats ((self improvizer) list-of-choices)
  (remove nil list-of-choices :test #'(lambda (x y) (not (and (eligible-index? y self) 
                                                              (eligible-feature? (otext self y) self))))))

(defmethod eligible-index? (index (self improvizer))
  (and ;(or (not (tabou-mode self)) (not (gethash index (tabou self))))
       (>= index (first (start-region self))) (<= index (second (start-region self)))))

(defmethod eligible-feature? ((self beat) (o improvizer))
  (if (null (feature o)) t 
    (if (integerp (feature self))
        (member (abs (feature self)) (feature o))  ;'features' are MIDI codes, thus 'abs' is needed for prolongation
        nil)))     ;'feature' = nil when the beat has no feature, thus it should be rejected if the oracle looks for features

(defmethod eligible-feature? ((self t) (o improvizer)) t)      ;;;;;;;;;;for genericity




(defmethod beat-density ((self beat))
  (length (remove-duplicates (mapcar #'second (qMidiSet self)))))

(defmethod continuations-by-suppleance ((self improvizer) index label)
   (let (back-cont forw-cont (best-suffix-state -1) (max-suffix 0))
     (when (bwSuffix self)
       (setf back-cont
             (loop for supp = (suppleance self index) then (suppleance self supp)
                   with previous = index
                   with suffix-length = 0
                   while (and supp (> supp 0))
                   do 
                   ;(print (lrs self previous)) 
                   (when (bestSuffixMode self) (setf suffix-length (lrs self previous)  previous supp))
                   append (loop for cont in (flink self  supp)
                                if (eligible-beat? (otext self cont) label)
                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))
     (when (fwSuffix self)
       (setf forw-cont 
             (loop for supp = (suppleance-> self index) then (suppleance-> self supp)
                   with suffix-length = 0
                   while supp
                   do
                   (when (bestSuffixMode self) (setf suffix-length (lrs self supp)))
                   ;(print (list '-> (lrs self supp)))
                   ;(when (> suffix-length max-suffix) (setf best-suffix-state supp max-suffix suffix-length))
                   append (loop for cont in (flink self supp)
                                if (eligible-beat? (otext self cont) label)

                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))   

     (if (bestSuffixMode self)    ;MARC 24/4/2012 bestSuffixMode = nil
                                  ;since bestSuffixMode = t => TOO RESTRICTED: always the same path in the oracle
       
       (nth-random (loop for cont in (append back-cont forw-cont)
                         if (= (second cont) max-suffix) collect (first cont)))
       (let ((chosen-index (nth-random (reduce-eligible-beats self                          ;;;;MARC 11/5/12
                                                              (mapcar 'first (append back-cont forw-cont))    ))))
         (when chosen-index (setf (gethash chosen-index (tabou self)) t))
         chosen-index)
       )))

;================================================================================== FIND-PREFIX-LABEL-MATCH ================================================================================
;SEARCH FOR A BEAT WITHOUT CONTINUITY

; Jerome 20/02/13
;Collect the prefixes of a grid (list) found in an improvizer
;------------------------------------------------------------------------------------------------------------------------------------
;Prefixes filtered according to :
;   - the length (in (LengthFactorsFromGrid [Improvizer]))
;   - "tabous"
;Returns a 2 elements list :
; (nth 0 ) -> List of (prefix_length list_of_idxs_beginning)
; (nth 1 ) -> length of the longest prefix (used in find-prefix-label-match to compare the results of thee different transpositions)
;------------------------------------------------------------------------------------------------------------------------------------
(defmethod select-matching-prefixes ((self improvizer) (grid list))
  (let* ((MP (MorrisPratt_prefixes self grid)) (found-grid-prefixes (nth 0 MP)) (length_longest (nth 1 MP)) (length_longest (nth 1 MP)) (selected-prefixes nil))
    
    (list 
     (setf selected-prefixes 
           (loop for len being the hash-key of found-grid-prefixes using (hash-value idxs)
                 if (and (reduce-eligible-beats self idxs) 
                         (and (>= len (nth 0 (LengthFactorsFromGrid self))) 
                              (<= len (nth 1 (LengthFactorsFromGrid self))))) 
                 collect (list len (reduce-eligible-beats self idxs))
                 if (= *print_info_find* 1) do (format *om-stream* "---->Found prefix(es) : Length = ~D, Idxs =~a ------> FILTERED : idxs =~a~%" len idxs (reduce-eligible-beats self idxs) )
                 ))
     (if selected-prefixes length_longest 0))))

; Jerome 20/02/13
;Display select-matching-prefixes results
;-----------------------------------------
(defun print-select-matching-prefixes (list)
(format *om-stream* "~%Length longest prefix before filtering : ~D~%" (nth 1 list))
(format *om-stream* "Selected prefixes after filtering (LengthFactorsFromGrid and tabous):~%")
(loop for l in (nth 0 list) do
      (format *om-stream* "Length = ~D -> ~D occurence [idxs = ~a]~%" (nth 0 l) (list-length (nth 1 l)) (nth 1 l))))

; Jerome 20/02/13
;Get an index in the Improvizer where a prefix of the grid begins, according to the navigation modes in the Improvizer class
;--------------------------------------------------------------------------------------------------------------------------- ------------------------------
; Replaces "find-beat-label-match"
; -- Transpositions are managed by (bestTranspoMode [Improvizer]) and (firstWithoutTranspoMode [Improvizer])
; -- For all the transpositions, the prefixes are selected and filtered in select-matching-prefixes (above).
; -- The length is randomly chosen among those returned by select-matching-prefixes
;   ==> TO SELECT A PRECISE LENGTH N : (setf (LengthFactorsFromGrid [Improvizer] '(N N)))
; -- (randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;    (randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;--------------------------------------------------------------------------------------------------------------------------- ------------------------------
(defmethod find-prefix-labels-match ((self improvizer) (grid list)) 
  (if (= *print_info_find* 1) (format *om-stream* "~%~%=========================================FIND-PREFIX-LABELS-MATCH  of  ~a, length in ~a=========================================~%" grid (LengthFactorsFromGrid self))) 
  (if (= (maxetat self) 1) 1  

    (let* ((Transpos (AuthorizedTranspos self))
           (intervalTranspo 
            (if (FirstWithoutTranspoMode self) ;(firstWithoutTranspoMode self) = t : first search without transposition
                (append '(0) (rotate Transpos (random (list-length Transpos)))) 
              (rotate (append '(0) Transpos) (random (1+ (list-length Transpos))))))) 
      
          
      ;(bestTranspoMode self) = t : all the transpositions in (AuthorizedTranspos self) are compared to chose the transposition giving the longest prefix
      (if (bestTranspoMode self)
          (let* ((best_transp_len 0) (best_transp_idxs nil) (best_delta 0)
                 (cur_transp_selected_prefixes_info nil) (cur_transp_max_length nil) (cur_transp_selected_prefixes nil) (cur_transp_chosen_prefix nil) (cur_transp_idxs nil) )
            (loop for delta in (om- intervalTranspo (CurrentTranspo self)) 
                  do 
                  (if (= *print_info_find* 1) 
                      (format *om-stream* "[Searching for BEST transpo, with transpo = ~D -> transp-grid = ~a]~%" delta (TransposeGrid grid delta)))
                  (setf cur_transp_selected_prefixes_info (select-matching-prefixes self (TransposeGrid grid delta)))
                  (setf cur_transp_max_length (nth 1 cur_transp_selected_prefixes_info)) (setf cur_transp_selected_prefixes (nth 0 cur_transp_selected_prefixes_info))
                  (setf cur_transp_chosen_prefix (nth-random cur_transp_selected_prefixes)) 
                  (setf cur_transp_len (nth 0 cur_transp_chosen_prefix)) (setf cur_transp_idxs (nth 1 cur_transp_chosen_prefix))
                  (if (> cur_transp_max_length best_transp_len) (setf best_transp_len cur_transp_max_length best_transp_idxs cur_transp_idxs best_delta delta)))
            (if (> best_transp_len 0)
                (let* ((transp_states best_transp_idxs) (chosen_transp_state (if (randomPrefixOccurrenceMode self) (nth-random transp_states) (apply 'min transp_states))))
                  (if (= *print_info_find* 1) 
                      (format *om-stream* ">>>>Chosen prefix with BEST TRANSP = ~D, TRANSP_GRID = ~a :  ~%----->length = ~D, idxs = ~a ~%~%~%" 
                              best_delta (TransposeGrid grid best_delta) best_transp_len best_transp_idxs))
                  (setf (gethash chosen_transp_state (tabou self)) t) 
                  (incf (CurrentTranspo self) best_delta) chosen_transp_state) 
              nil))
        
        
        ;(bestTranspoMode self) = nil : uses the first transposition returning a prefix with length >= 1
        (loop for delta in (om- intervalTranspo (CurrentTranspo self))
              do 
              (if (= *print_info_find* 1) 
                      (format *om-stream* "[Searching for FIRST RANDOM transpo, with transpo = ~D -> transp-grid = ~a]~%" delta (TransposeGrid grid delta)))
              (let* ((transp_selected_prefixes_info (select-matching-prefixes self (TransposeGrid grid delta)))
                        (transp_max_length (nth 1 transp_selected_prefixes_info)) (transp_selected_prefixes (nth 0 transp_selected_prefixes_info))
                        (transp_chosen_prefix (nth-random transp_selected_prefixes)) 
                        (transp_len (nth 0 transp_chosen_prefix)) (transp_idxs (nth 1 transp_chosen_prefix)))
                   
                   (if (> transp_max_length 0)
                       (let* ((transp_states transp_idxs) (chosen_transp_state (if (randomPrefixOccurrenceMode self) (nth-random transp_states) (apply 'min transp_states))))
                         (if (= *print_info_find* 1) 
                             (format *om-stream* ">>>>Chosen prefix with FIRST RANDOM TRANSP = ~D, TRANSP_GRID = ~a :  ~%----->length = ~D, idxs = ~a ~%" 
                                     delta (TransposeGrid grid delta) transp_len transp_idxs))
                         (setf (gethash chosen_transp_state (tabou self)) t) 
                         (incf (CurrentTranspo self) delta) 
                         (return chosen_transp_state)))
                   )
              finally return nil))
      )))

#|
;==============================================================
;FIND-PREFIX-LABEL-MATCH WITH NEW CONTROLS IN CLASS "IMPROVIZER"
;==============================================================

;Data
;----
(setf beat-list3 '(
((gb) ((60 0 500 80 1)))
((c#) ((60 0 500 80 1)))
((a) ((60 0 500 80 1)))
((a) ((62 0 500 80 1)))
((a) ((64 0 500 80 1)))
((gb) ((60 0 500 80 1)))
((c#) ((62 0 500 80 1)))
((a) ((64 0 500 80 1)))
((d) ((60 0 500 80 1)))
((gb) ((60 0 500 80 1)))
((c#) ((60 0 500 80 1)))
((d#) ((62 0 500 80 1)))
((e#) ((64 0 500 80 1)))
))
(setf oracle (NewImprovizer (make-beat-list beat-list3)))
(setf grid '((gb) (c#) (a) (d) (e) (c)))


; (LengthFactorsFromGrid [Improvizer]) filters the length of the returned prefixes in SELECT-matching-prefixes
;-------------------------------------------------------------------------------------------------------------

;Indexes filtered
(setf (LengthFactorsFromGrid oracle) '(1 2))
(setf resultSelect (select-matching-prefixes oracle grid))
(print-select-matching-prefixes resultSelect)

;No index filtered
(setf (LengthFactorsFromGrid oracle) '(1 1000))
(setf resultSelect (select-matching-prefixes oracle grid))
(print-select-matching-prefixes resultSelect)


; in FIND-prefix-labels-match
; (bestTranspoMode [Improvizer]) = nil : first random transposition giving a prefix with length >=1 
; (bestTranspoMode [Improvizer]) = t : looking for the longest prefix with all the transpositions authorized in (AuthorizedTranspos [self]) (default)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
(setf (bestTranspoMode oracle) t)
(find-prefix-labels-match oracle grid)

(setf (bestTranspoMode oracle) nil)
(find-prefix-labels-match oracle grid)


; (firstWithoutTranspoMode [Improvizer]) = t : first searches with no transposition (default = nil)
;----------------------------------------------------------------------------------------------------
(setf (firstWithoutTranspoMode oracle) t)
(find-prefix-labels-match oracle grid)

;(randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;(randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;------------------------------------------------------------------------------------------------------------------------------------------------------------
(setf (randomPrefixOccurrenceMode oracle) t)
(find-prefix-labels-match oracle grid)

; In find-prefix-label-match, the length is randomly chosen among those returned by select-matching-prefixes
; ==> TO SELECT A PRECISE LENGTH N : (setf (LengthFactorsFromGrid [Improvizer] '(N N)))
;----------------------------------------------------------------------------------------------------------
(setf (LengthFactorsFromGrid oracle) '(4 4))
(setf resultSelect (select-matching-prefixes oracle grid))
(find-prefix-labels-match oracle grid)
|#












;MODE CONTINUITY
(defmethod choose-factor-link ((self improvizer) indexes label)
  (if (null label)
      (nth-random indexes)
    (let* ((succeed (loop for index in indexes 
                          if (eligible-beat? (otext self index) label)      ;;;;;;;MARC 11/5/12
                          collect index))
           (reducedindexes (reduce-eligible-beats self succeed))
           (chosen-index (nth-random reducedindexes)))
      (when chosen-index (setf (gethash chosen-index (tabou self)) t))
      chosen-index)))

          
(defmethod factor-p ((self improvizer) (factor list))
       (loop for etat = 0 then (transition self etat (pop factor))
             while (and factor (not (null etat)))
             finally
             (if (null etat) (return nil)  (return etat))))



;functions transfered to file "Beatlist.lisp"
;-> make-beat-list, make-beat, oracle->beatlist, beats->chseq, beats->5list, events->5list, thread-Beats


#|
;Marc 26/1/213 for preparing oracles in the directory "_Oracles":

(save-improvizer (gethash 8 (oracletable *current-tune*)) "caca.or")
(load-improvizer "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/BluesForAlice_solo.or")
(load-improvizer-from)

(setf path_oracle (format nil "~a/~a" path_dir_live_oracles "Cantalope_Uzeste29janv12.or" ))
(save-improvizer (gethash 8 (oracletable *current-tune*)) path_oracle)

;'choose-new-file-dialog' does not seem to work anymore ---> ask Jean???

|#

#|
;DOES NOT SEEM TO WORK IN LispWork !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defmethod save-improvizer-as ((self improvizer))
  (catch-cancel
     (let* ((name (choose-new-file-dialog :prompt "Save Improvizer As...")))
       (when name
         (save-improvizer self name)))))

(defmethod load-improvizer-from ()
  (catch-cancel
     (let ((name (choose-file-dialog)))
       (when name
         (load-improvizer name)))))
|#


(defmethod save-improvizer ((self improvizer) (name t))
  (WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
  t)


(defmethod load-improvizer ((name t))
  (WITH-OPEN-FILE (in name :direction :input  );:if-does-not-exist) ;:nil)
    (eval (read in)) (eval (read in))))


(defun eval-my-list (list)
  (loop for item in list collect (eval item)))


(defmethod omNG-save ((self array) &optional (values? nil))
   `(make-array ,(length self) :initial-contents (eval-my-list '(,.(map 'list #'(lambda (elt) (omNG-save elt values?)) self)))))








;--------------------------------------------------------------------------------
; Cross alphabet
;---------------

(defmethod mf->crossEvents ((mf-list list))
  (cross->events  (midi->cross  mf-list nil 100 nil nil 32)))



#|
(defmethod mf->crossEvents ((mf-list list))
  (cross->events  
   (if *ofon-mode*
     (midi->cross  mf-list nil 0 nil nil 0)
     (if *Prolifere-mode*
       (midi->cross  mf-list nil 50 100 nil 32)
       (midi->cross  mf-list 50 0 0 50 10)))))

(defmethod mf->crossEvents ((mf-list list))
  (cross->events  
   (if *ofon-mode*
     (midi->cross  mf-list nil 0 nil nil 0)
     (if *Prolifere-mode*
       (midi->cross  mf-list nil 50 100 nil 32)
       (midi->cross  mf-list nil 100 nil nil 32)))))

|#




(defmethod mf->crossEvents ((mf-list list))
  (cross->events  
   (if *ofon-mode*
     (midi->cross  mf-list nil 0 nil nil 0)
     (if *Prolifere-mode*
       (midi->cross  mf-list nil 50 100 nil 32)
       (midi->cross  mf-list nil 50 nil nil 20)))))



;;; dernier 60=tendance a quantifier (donc hacher)

(defmethod clone-object ((self CrossEvent))
  (make-instance 'CrossEvent :duration (duration self) :MidiSet (copy-tree (MidiSet  self))))


;;; deep-clone of beat object
;;; one do not wants to modify beats in the Oracle

(defmethod clone-object ((self beat))
  (let ((cbeat (clone self)))
    (setf (MidiSet cbeat) 
          (copy-tree (MidiSet  self)))
    cbeat))


;;; clone improvized beats and scale their MidiSet (onset+durs) w/regards
;;; to actual (sent by max) beat dur

(defun clone-and-scale-beats (beats dureebeat)
  (loop for beat in beats
        for cbeat = (clone-object beat)
        for beatratio = (/ dureebeat (float (duration cbeat)))
        do (loop for ms  in (MidiSet cbeat)
                 do (setf (MEOnset ms) (round (* (MEOnset ms) beatratio))
                          (MEDur ms) (round (* (MEdur ms) beatratio))))
        collect cbeat))





(defmethod cross->events ((self list) &optional (channel 1))
   (loop for slice in self
         for duration = (second slice)
         collect (make-instance 'CrossEvent 
                   :duration duration 
                   :Midiset
                   (sort (loop for track in (first slice)
                               for chan = channel then (1+ chan)
                               append (loop for (pitch velocity) in track 
                                            collect (list pitch 0 duration velocity chan)))
                         #'< :key #'(lambda (evt) (abs (first evt)))))))
  


(defmethod thread-crossEvents ((self list))
  (setf self (mapcar #'clone-object self))
  (loop for event in  self
        for delay = (duration event)
        with new-waiting-list
        for waiting-list = nil then new-waiting-list
        do (loop for ME in (midiset event)
                 with new-midiset = nil
                 initially do (setf new-waiting-list nil)
                 do (cond ((plusp (MEPitch ME))
                           (push ME new-waiting-list)
                           (push ME new-midiset))
                          ((minusp (MEPitch ME))
                           (let ((waiting (find ME waiting-list 
                                                :test #'(lambda (m1 m2) 
                                                          (and (= (abs (MEPitch m1)) (abs (MEPitch m2))) 
                                                               (= (MEChannel m1) (MEChannel m2)))))))
                             (cond 
                              (waiting
                               (setf (MEDur waiting) (+ (MEDur waiting) (MEDur ME)))
                               (push waiting new-waiting-list))
                              (t (setf (MEPitch ME) (abs (MEPitch ME)))
                                 (push ME new-waiting-list)
                                 (push ME new-midiset))))))
                 finally (setf (Midiset event) new-midiSet)))
  self)

                 
(defmethod CrossEvents->MF ((self list))
  (loop for event in self
        with delay = 0
        for onset = 0 then (+ onset delay)
        do (setf delay (duration event))
        append (loop for (pitch monset dur vel chan) in (MidiSet event)
                     collect (list pitch onset dur vel chan))))


(defmethod CrossEvents->ChordSeq ((self list))
  (if *ofon-mode*
    ; we take no risk of changing the number of cross-evts by threading
    (mf-info->chord-seq (crossevents->mf self))
    (mf-info->chord-seq (crossevents->mf (thread-crossevents self)))
    ))

;;; ------------
;;; UTILITAIRES
;;; ------------



(defun beatlist->chordseq (beatlist beatvalue channel)
  (let ((beats (loop for beat in beatlist
                     for onset = 0 then (+ onset beatvalue)
                     for label = (first beat)
                     for eventlist = (second beat)
                     append (loop for event in eventlist
                                  if (or (eq channel t) (= channel (fifth event)))
                                  collect (list (first event) (+ onset (second event)) (third event) (fourth event) (1+ (fifth event)))))))
        (mf-info->chord-seq  beats)))


(defun filter-beatlist (beatlist channels)
  (loop for beat in beatlist
        for label = (first beat)
        for eventlist = (second beat)
        collect (list label
                      (loop for event in eventlist
                            if (member (fifth event) channels)
                            collect event))))

(defun enhance-channel (beatlist channel)
  (loop for beat in beatlist
        for label = (first beat)
        for eventlist = (second beat)
        collect (list label
                      (loop for event in eventlist
                            for velocity = (fourth event)
                            if (= (fifth event) channel) do (setf velocity (min 127 (* 3 velocity))) end
                            collect (list (first event) (second event) (third event) velocity (fifth event))))))

       
(defmethod mf-info->chord-seq ((self list))
  (let* ((chords (make-quanti-chords self *global-deltachords*))
         (lonset (mapcar 'offset chords))
         (last-note (first (inside (first (last chords))))))
    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset 
      ;:legato 200
)))



;;; bug sort chord in LZ.lisp

(defun continuation (poly legatime arpegtime releastime staccatime toltime)
  (let ((continuation (quantize-voice
                       (unstaccate-voices
                        (synchro-release
                         (unarpeggiate-voices
                          (unlegate-voices
                           (apply #'thread-voices
                                  (merge-voices
                                   (channelize
                                    (prepare-voices (copy-tree poly)))));)
                           legatime)
                          arpegtime)
                         releastime)
                        staccatime)
                       toltime)))
    (mapc #'(lambda (slice) (rplaca  slice (sortchord (first slice))))
             continuation)
    continuation))





#|

(setf beat-list '(((b 7) ((35 0 237 97 1) (68 0 479 69 0) (63 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (92 44 244 100 2) (69 68 67 40 2) (64 79 81 26 2) (58 79 56 21 2) (47 242 237 97 1) (93 288 102 70 2) (92 365 109 48 2) (90 435 49 56 2))) ((f 7) ((29 0 237 97 1) (62 0 479 69 0) (57 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (90 0 56 56 2) (87 56 220 61 2) (41 242 237 97 1) (85 315 126 62 2))) ((bb m7) ((34 0 237 97 1) (65 0 479 69 0) (61 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (46 242 237 97 1))) ((a) ((33 0 237 97 1) (73 0 479 69 0) (68 0 479 69 0) (64 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (63 21 67 21 2) (57 28 60 21 2) (68 32 56 14 2) (45 242 237 97 1))) ((g# m7) ((32 0 479 97 1) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (80 24 265 64 2) (81 303 91 36 2) (80 372 112 25 2) (78 424 60 25 2))) ((g# m7) ((44 0 237 97 1) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (44 0 237 97 1) (78 0 50 25 2) (80 0 15 25 2) (75 39 231 30 2) (32 242 116 97 1) (73 315 108 26 2) (44 363 116 97 1))) ((e m7) ((40 0 237 97 1) (66 0 484 69 0) (62 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (40 242 242 97 1) (75 321 125 47 2) (56 345 104 18 2) (62 351 64 22 2) (67 356 70 13 2))) ((e m7) ((40 0 237 97 1) (55 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (40 242 237 97 1) (70 307 110 28 2))) ((a 7) ((33 0 237 97 1) (66 0 479 69 0) (61 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (61 3 129 21 2) (66 3 105 15 2) (55 13 115 20 2) (40 242 237 97 1) (68 468 16 24 2))) ((a 7) ((45 0 237 97 1) (59 0 479 69 0) (61 0 479 69 0) (45 0 237 97 1) (68 0 169 24 2) (67 121 245 69 0) (71 363 116 69 0) (68 469 15 27 2))) ((g# 7) ((32 0 237 97 1) (65 0 484 69 0) (62 0 484 69 0) (60 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (68 0 310 27 2) (44 242 237 97 1) (70 310 125 24 2))) ((g# 7) ((32 0 237 97 1) (58 0 479 69 0) (60 0 479 69 0) (62 0 479 69 0) (65 0 479 69 0) (32 0 237 97 1) (44 242 237 97 1) (56 316 115 21 2) (51 326 115 18 2) (61 326 95 16 2))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (63 332 150 25 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (29 242 116 97 1) (63 298 175 29 2) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 484 69 0) (64 0 484 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (65 67 49 36 2) (66 162 80 35 2) (46 242 237 97 1) (67 267 45 34 2) (68 357 60 32 2) (69 447 37 60 2))) ((bb 7) ((34 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (64 0 479 69 0) (67 0 479 69 0) (34 0 237 97 1) (69 0 27 60 2) (70 50 103 33 2) (71 163 60 59 2) (46 242 237 97 1) (72 258 80 37 2) (73 338 35 33 2) (74 398 86 46 2))) ((c m7) ((36 0 479 97 1) (62 0 479 69 0) (58 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (74 0 7 46 2) (75 29 105 32 2) (76 144 45 41 2) (77 214 105 27 2) (75 309 100 35 2) (74 419 65 50 2))) ((b) ((35 0 237 97 1) (75 0 479 69 0) (70 0 479 69 0) (66 0 479 69 0) (63 0 479 69 0) (59 0 479 69 0) (74 0 10 50 2) (75 20 95 25 2) (76 135 60 43 2) (77 215 85 51 2) (47 242 237 97 1) (78 300 45 39 2) (79 380 95 43 2))) ((bb m7) ((34 0 237 97 1) (65 0 479 69 0) (61 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (80 14 92 31 2) (81 106 55 45 2) (82 165 101 41 2) (46 242 237 97 1) (81 291 35 14 2) (80 301 85 35 2) (79 381 70 33 2) (80 466 18 23 2))) ((a) ((33 0 237 97 1) (73 0 479 69 0) (68 0 479 69 0) (64 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (80 0 87 23 2) (81 117 63 23 2) (82 192 100 31 2) (45 242 237 97 1) (83 307 45 53 2) (84 387 35 41 2) (85 447 37 20 2))) ((g# m7) ((32 0 479 97 1) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (85 0 18 20 2) (86 43 60 58 2) (87 158 73 22 2) (88 228 35 51 2) (89 288 80 44 2) (87 388 95 40 2))) ((g# m7) ((44 0 237 97 1) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (44 0 237 97 1) (86 0 44 35 2) (87 69 105 27 2) (88 199 35 35 2) (32 242 116 97 1) (89 279 60 32 2) (44 363 116 97 1) (90 385 23 20 2) (91 419 65 51 2))) ((c#) ((37 0 237 97 1) (68 0 479 69 0) (65 0 479 69 0) (63 0 479 69 0) (60 0 479 69 0) (91 0 15 51 2) (92 50 105 26 2) (49 242 237 97 1))) ((g) ((31 0 237 97 1) (71 0 479 69 0) (66 0 479 69 0) (62 0 479 69 0) (59 0 479 69 0) (55 0 479 69 0) (43 242 237 97 1))) ((g 7) ((31 0 237 97 1) (64 0 479 69 0) (59 0 484 69 0) (57 0 484 69 0) (53 0 484 69 0) (68 12 440 18 2) (63 36 418 16 2) (73 57 427 11 2) (38 242 237 97 1))) ((g 7) ((43 0 237 97 1) (57 0 479 69 0) (59 0 479 69 0) (43 0 237 97 1) (73 0 108 11 2) (65 121 245 69 0) (69 363 116 69 0))) ((c 7) ((36 0 237 97 1) (69 0 479 69 0) (62 0 484 69 0) (58 0 484 69 0) (57 0 484 69 0) (52 0 484 69 0) (75 20 464 11 2) (65 29 325 18 2) (70 29 363 16 2) (36 242 242 97 1))) ((c 7) ((36 0 237 97 1) (52 0 479 69 0) (57 0 479 69 0) (58 0 479 69 0) (62 0 479 69 0) (75 0 10 11 2) (64 121 228 69 0) (36 242 237 97 1) (67 363 116 69 0))) ((f 7) ((29 0 237 97 1) (62 0 484 69 0) (59 0 484 69 0) (57 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (68 16 128 18 2) (73 26 129 20 2) (78 26 105 16 2) (41 242 237 97 1))) ((f 7) ((29 0 237 97 1) (55 0 479 69 0) (57 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (29 0 237 97 1) (97 7 230 84 2) (41 242 237 97 1) (96 310 137 98 2))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (41 242 237 97 1) (92 298 105 48 2) (73 473 11 23 2))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (63 0 59 24 2) (68 0 73 23 2) (73 0 108 23 2) (94 3 140 72 2) (68 121 245 69 0) (72 363 116 69 0))) ((b m7) ((35 0 479 97 1) (66 0 484 69 0) (62 0 484 69 0) (61 0 484 69 0) (57 0 484 69 0) (70 0 170 17 2) (90 0 158 48 2) (65 15 165 16 2) (75 19 136 9 2))) ((b m7) ((47 0 237 97 1) (61 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (47 0 237 97 1) (87 0 206 35 2) (35 242 116 97 1) (47 363 116 97 1))) ((e 7) ((40 0 479 97 1) (70 0 479 69 0) (66 0 479 69 0) (62 0 479 69 0) (56 0 479 69 0) (85 2 265 24 2) (82 302 154 25 2) (56 302 115 20 2) (66 316 116 16 2) (61 327 70 19 2))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (46 242 237 97 1) (80 293 140 24 2))) ((eb 7) ((39 0 479 97 1) (69 0 479 69 0) (65 0 479 69 0) (61 0 479 69 0) (55 0 479 69 0) (78 9 164 22 2))) ((g# m7) ((32 0 237 97 1) (63 0 479 69 0) (59 0 479 69 0) (58 0 479 69 0) (54 0 479 69 0) (75 0 210 21 2) (44 242 237 97 1) (78 465 19 18 2))) ((c# 7) ((37 0 237 97 1) (70 0 479 69 0) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (53 0 484 69 0) (78 0 284 18 2) (37 242 242 97 1) (75 284 102 18 2))) ((c# 7) ((37 0 237 97 1) (53 0 479 69 0) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (65 121 228 69 0) (37 242 237 97 1) (80 252 150 25 2) (68 363 116 69 0))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (42 242 242 97 1) (75 243 140 21 2))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (37 242 237 97 1) (78 294 140 20 2))) ((f# m7) ((30 0 479 97 1) (61 0 484 69 0) (57 0 484 69 0) (56 0 484 69 0) (52 0 484 69 0) (63 229 46 22 2) (73 229 111 20 2) (68 240 59 18 2) (75 250 188 19 2))) ((f# m7) ((42 0 237 97 1) (56 0 479 69 0) (57 0 479 69 0) (61 0 479 69 0) (42 0 237 97 1) (80 206 85 35 2) (75 221 70 18 2) (30 242 116 97 1) (70 244 57 15 2) (81 291 139 33 2) (42 363 116 97 1))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (80 2 210 30 2) (78 397 87 22 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (78 0 133 22 2) (68 240 103 15 2) (29 242 116 97 1) (63 248 70 18 2) (75 252 231 24 2) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (73 239 167 26 2) (41 242 237 97 1))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (90 50 434 70 2) (68 121 245 69 0) (72 363 116 69 0))) ((b 7) ((35 0 237 97 1) (68 0 479 69 0) (63 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (90 0 356 70 2) (47 242 237 97 1) (87 321 115 55 2))) ((f 7) ((29 0 237 97 1) (62 0 479 69 0) (57 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (41 242 237 97 1) (85 347 80 32 2))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (46 242 237 97 1) (82 338 80 27 2))) ((eb m7) ((39 0 479 97 1) (65 0 479 69 0) (61 0 479 69 0) (58 0 479 69 0) (54 0 479 69 0) (80 109 235 14 2))) ((g# 7) ((32 0 237 97 1) (65 0 484 69 0) (62 0 484 69 0) (60 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (80 45 185 21 2) (44 242 237 97 1))) ((g# 7) ((32 0 237 97 1) (58 0 479 69 0) (60 0 479 69 0) (62 0 479 69 0) (65 0 479 69 0) (32 0 237 97 1) (78 21 284 16 2) (44 242 237 97 1))) ((c# 7) ((37 0 237 97 1) (70 0 479 69 0) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (53 0 484 69 0) (37 242 242 97 1))) ((c# 7) ((37 0 237 97 1) (53 0 479 69 0) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (73 68 416 22 2) (65 121 228 69 0) (37 242 237 97 1) (68 363 116 69 0))) ((eb m7) ((39 0 237 97 1) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (66 0 49 20 2) (61 0 69 26 2) (73 0 314 22 2) (56 14 45 16 2) (39 242 242 97 1) (75 304 93 33 2))) ((eb m7) ((39 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (63 35 90 20 2) (68 49 81 18 2) (39 242 237 97 1))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (87 16 130 28 2) (42 242 242 97 1) (65 281 130 29 2) (75 281 105 20 2) (70 296 140 19 2))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (37 242 237 97 1) (82 297 45 27 2))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (85 278 80 30 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (63 4 80 22 2) (68 14 60 22 2) (73 29 55 15 2) (85 39 245 34 2) (29 242 116 97 1) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (84 50 120 33 2) (41 242 237 97 1))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (80 36 448 27 2) (68 121 245 69 0) (70 301 115 16 2) (75 301 80 12 2) (65 301 105 18 2) (72 363 116 69 0))) ((c 7) ((36 0 479 97 1) (66 0 479 69 0) (62 0 479 69 0) (58 0 479 69 0) (52 0 479 69 0) (80 0 282 27 2) (82 292 105 45 2))) ((f 7) ((29 0 237 97 1) (62 0 479 69 0) (57 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (41 242 237 97 1) (63 287 66 28 2) (73 298 59 16 2) (68 308 49 17 2))) ((bb m7) ((34 0 237 97 1) (65 0 479 69 0) (61 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (87 9 140 47 2) (46 242 237 97 1))) ((a) ((33 0 237 97 1) (73 0 479 69 0) (68 0 479 69 0) (64 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (65 15 160 20 2) (70 25 165 18 2) (75 25 130 17 2) (45 242 237 97 1) (82 315 140 35 2))) ((g# m7) ((32 0 479 97 1) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (85 286 115 58 2))) ((g# m7) ((44 0 237 97 1) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (44 0 237 97 1) (85 32 185 34 2) (32 242 116 97 1) (73 287 85 13 2) (68 287 60 18 2) (63 297 50 13 2) (44 363 116 97 1) (84 462 22 45 2))) ((g) ((31 0 237 97 1) (71 0 484 69 0) (66 0 484 69 0) (62 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (84 0 118 45 2) (43 242 242 97 1))) ((g) ((43 0 237 97 1) (55 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (71 0 479 69 0) (80 0 260 27 2) (38 242 237 97 1) (78 469 15 27 2))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (78 0 160 27 2) (42 242 242 97 1) (70 310 140 17 2) (75 310 130 14 2) (65 320 105 18 2))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (37 242 237 97 1) (75 276 203 25 2))) ((f# m7) ((30 0 479 97 1) (61 0 484 69 0) (57 0 484 69 0) (56 0 484 69 0) (52 0 484 69 0) (78 262 150 50 2))) ((f# m7) ((42 0 237 97 1) (56 0 479 69 0) (57 0 479 69 0) (61 0 479 69 0) (42 0 237 97 1) (77 0 277 25 2) (30 242 116 97 1) (66 248 140 24 2) (56 253 115 23 2) (73 263 154 29 2) (61 263 105 25 2) (42 363 116 97 1))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (75 239 215 31 2) (68 244 153 18 2) (58 254 178 20 2) (63 258 136 21 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (29 242 116 97 1) (70 250 170 28 2) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (41 242 237 97 1) (73 251 140 61 2))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (72 1 241 21 2) (68 121 245 69 0) (68 257 220 28 2) (72 363 116 69 0))) ((b m7) ((35 0 237 97 1) (66 0 479 69 0) (62 0 484 69 0) (61 0 484 69 0) (57 0 479 69 0) (51 237 186 18 2) (47 242 237 97 1) (70 243 241 26 2) (56 248 150 20 2) (61 258 119 18 2))) ((e 7) ((56 0 479 69 0) (62 0 479 69 0) (34 0 237 97 1) (73 0 479 69 0) (56 0 479 69 0) (70 0 218 26 2) (63 241 173 28 2) (46 242 237 97 1))) ((e 7) ((40 0 479 97 1) (70 0 479 69 0) (66 0 479 69 0) (62 0 479 69 0) (56 0 479 69 0) (63 198 77 27 2) (66 430 54 42 2))) ((eb 7) ((39 0 479 97 1) (69 0 479 69 0) (65 0 479 69 0) (61 0 479 69 0) (55 0 479 69 0) (66 0 151 42 2) (66 421 63 22 2))) ((eb 7) ((66 0 147 22 2) (66 237 95 27 2) (51 237 60 30 2) (61 252 55 20 2) (67 367 117 23 2))) ((g# m7) ((32 0 237 97 1) (63 0 479 69 0) (59 0 479 69 0) (58 0 479 69 0) (54 0 479 69 0) (67 0 103 23 2) (68 218 235 43 2) (44 242 237 97 1))) ((g) ((31 0 237 97 1) (71 0 484 69 0) (66 0 484 69 0) (62 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (68 119 135 34 2) (43 242 242 97 1) (68 409 75 47 2))) ((g) ((43 0 237 97 1) (55 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (71 0 479 69 0) (68 0 40 47 2) (69 50 70 48 2) (38 242 237 97 1) (68 263 221 43 2))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (68 0 484 43 2) (42 242 242 97 1))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (68 0 484 43 2) (37 242 237 97 1) (63 427 57 22 2))) ((f# m7) ((30 0 479 97 1) (61 0 484 69 0) (57 0 484 69 0) (56 0 484 69 0) (52 0 484 69 0) (63 0 63 22 2) (68 0 443 43 2) (66 433 51 28 2))) ((f# m7) ((42 0 237 97 1) (56 0 479 69 0) (57 0 479 69 0) (61 0 479 69 0) (42 0 237 97 1) (66 0 274 28 2) (30 242 116 97 1) (63 274 95 21 2) (42 363 116 97 1))) ((f# 7) ((30 0 237 97 1) (63 0 479 69 0) (58 0 479 69 0) (56 0 479 69 0) (52 0 479 69 0) (42 242 237 97 1) (51 245 80 22 2) (56 255 60 19 2) (61 255 84 16 2))))

)

(setf labels (mapcar 'car beat-list))
(setf beats (make-beat-list beat-list))
(setf charlie (NewImprovizer))
(loop for i from 0 to (1- (length beats)) do (learn-event charlie (nth i beats)))
(setf impro (ImprovizeOnHarmGrid charlie 50 labels))


(SetMaxCont charlie 2)
(play (beats->chseq impro 484 0))

(beats-check-sustain impro 484 5)

(inspect impro)
(inspect charlie)


setf impro (ImprovizeByContinuation charlie 50 (improvize charlie 5 :start-etat 3)))
(setf impro (ImprovizeOnHarmGrid charlie 150 labels))
(inspect impro)
(beats-check-sustain impro 484 5)


(play (beats->chseq impro 484 0))
(save-improvizer-as charlie)
(setf charlie (load-improvizer-from))


(play ( beats->chseq  (ImprovizeByContinuation charlie 50 (improvize charlie 5 :start-etat 3)) 484 0))
(play ( beats->chseq  (ImprovizeOnHarmGrid charlie 100 labels) 484 0))

(flink charlie 2)

(setf impro (ImprovizeByContinuation charlie 50 (improvize charlie 5 :start-etat 3)))
(setf impro (ImprovizeOnHarmGrid charlie 10 labels))
(inspect impro)
(beats-check-sustain impro 484 5)


(play (beats->chseq impro 484 0))
(save-improvizer-as charlie)
(setf charlie (load-improvizer-from))

(mapcar 'length (beats->5list  impro 484 0))

(setf l '(1 2 3 4))
(loop for i in l for j in l collect (list i j))

|#