(in-package :om)

;G. Assayag (additional code by M. Chemillier, J. Nika)
;--------------------------------------------------------------------------------




;slot added by M.C.
(defclass* improvizer (pythie)
   (
    (name :initform "improvizer" :initarg :name :accessor name)
    (context :initform () :initarg :context :accessor context)
    (continuity :initform 0 :initarg :continuity :accessor continuity)
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

    (tabou-mode :initform nil :initarg :tabou-mode :accessor tabou-mode)     ;;;;;;; added by M.C. 11/5/2012: PB after the first impro, 
    (tabou :initform (make-hash-table :test '=) :initarg :tabou :accessor tabou)  ;;;     --> it becomes difficult to find 'matches' 
    (feature  :initform nil :initarg :feature :accessor feature)               ;;;;;;; added by M.C. 15/8/0212, list of 'features' as MIDI codes
    ))

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

;macro must be defined before they are used
;by M.C. 
(defmacro NormLabel (label)   
  `(cons (case (first ,label) (db 'c#) (d# 'eb) (gb 'f#) (ab 'g#) (a# 'bb) (t (first ,label))) (rest ,label)))
(defmacro NormalizeRoot (root) `(case ,root (db 'c#) (d# 'eb) (gb 'f#) (ab 'g#) (a# 'bb) (t ,root)))
(defmacro TransposeRoot (root int) `(nth (mod ,int 12) (member (NormalizeRoot ,root) '(c c# d eb e f f# g g# a bb b c c# d eb e f f# g g# a bb b))))

(defun equalLabel (label1 label2)            ; enharmonies
  (equal (NormLabel label1) (NormLabel label2)))

;(defun TransposeLabel (label int)
;  (if (null label) nil          ; genericity: for using with non 'Beat' events, it must handle the case label = nil
;    (cons (TransposeRoot (first label) int) (rest label))))

(defmacro MidiRoot (root) `(- 12 (length (member (NormalizeRoot ,root) '(c c# d eb e f f# g g# a bb b)))))

(defmacro DeltaRoot (root1 root2)
  `(- (mod (+ 5 (- (length (member (NormalizeRoot ,root1) '(c c# d eb e f f# g g# a bb b)))
                   (length (member (NormalizeRoot ,root2) '(c c# d eb e f f# g g# a bb b))))) 12)
      5))


;MARC 10/2/2012 generic function that works with usual chord labels (c m7), but also with objects of specific classes of labels

(defmethod TransposeLabel ((label list) int)  
  (cond ((null label) nil)        ; genericity: for using with non 'Beat' events, it must handle the case label = nil
        ((numberp (first label)) (om+ label int))         ; for using with lists of notes (melodic signatures for harmonizing)
        (t (cons (TransposeRoot (first label) int) (rest label)))))  ; for using with chord labels when improvizing on a grid


(defmethod TransposeLabel ((label t) int) label)  
      ;for genericity, when label is a not a list (other kind of objects are possible, ex. 'garnerlabels')

(defmethod FormatLabel ((label list)) label)



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



;Jerome, 19/02/13 : set-start-point. Correction of the (bug when NewImprovizer called with no arguments and sequence learnt in a second time : "start-region = (0 maxetat_initial) = (0 0) !)
;by M.C., comparison with learnt object is done by the function 'transition' (see Pythie class in Oracle.lisp)
(defmethod ajouter-objet ((self oracle) (objet t))
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


(defmethod CompareEvents ((Event1 T) (event2 T))
  (equal event1 event2))


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

;transpo added by M.C., generecity on labels added 10/2/2012
(defmethod Improvize ((self improvizer) (length integer) &optional (harmgrid nil))
   ;(format *om-stream*  "Region: ~a Suff. law: ~a Best Suff.: ~a Max Cont.: ~a Hindemith : ~a Estrada ~a~%" 
   ;        (start-region self) (list (bwsuffix self) (fwsuffix self)) (bestSuffixMode self) (max-continuity self)
   ;        (useHindemith self) (useEstrada self))

  (setf (CurrentTranspo self) 0)
  (format *om-stream* "-----------------------~%");(format *om-stream*  "~a~%" harmgrid)
   ;(setf harmgrid (refHarmGrid self))

  (when (null (veclrs self)) (setf (bestSuffixMode self) nil)
    (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))

  (loop for i from 1 to length
        for label =  (TransposeLabel (pop harmgrid) (CurrentTranspo self)) ; search for continuity according to current transpo
        with index = 0
        with previndex = -1
        with index2
        with links
        with suppleances
        with mode = 'continuity
        with tocollect = nil
         
        initially (setf (continuity self) 0)
        do         
        (setf previndex index)
        (when harmgrid
          (format *om-stream* "label=~a" (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))))
          (if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", oracle=~a, " (FormatLabel label)))) 
        (cond
         ((and (zerop index) (setf index2 (find-beat-label-match self label)))
          (format *om-stream* "Starting point : ~a ~%" index2)
          (setf tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                index index2))
         (t (if (check-continuity self) 
                (setf mode 'continuity)  
              (if (available-suppleance self index) 
                  (setf mode 'suppleance)
                (setf mode 'continuity)))

            (when (eq mode 'continuity)
               ;(when (= index (maxetat self)) (setf index 0)
               ;      (format *om-stream* "zero~%" ))
              (setf links (flink self index)
                    index2 (choose-factor-link self links label))
               ;(format *om-stream* " ~%=== Index courant ~a. Flink -> ~a successeurs possibles : ~a~%" index (list-length links) links)

              (if index2 
                  (progn
                    (format *om-stream* "c : ~a ~%" index2)
                    (setf index index2
                          tocollect  (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))  ))                                         
                (if (available-suppleance self index) 
                    (setf mode 'suppleance)
                  (setf mode 'nothing))))
               
               
            (when (eq mode 'suppleance)
              (setf index2 (continuations-by-suppleance self index label)) 
              (if (and index2 (/= index2 previndex))
                  (progn 
                    (format *om-stream* "--->s : ~a ~%" index2)
                    (setf index index2
                          tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                          (continuity self) 0))
                (format *om-stream*  "~a, " (setf mode 'nothing))))


            (when (eq mode 'nothing)
              (setf index2 (find-beat-label-match self label))
              (if index2 
                  (progn (format *om-stream* "new : ~a " index2)
                    (if (or (null harmgrid) (zerop (CurrentTranspo self))) 
                        (format *om-stream* " ~%") (format *om-stream* "transpo=~a ~%" (CurrentTranspo self)))
                    (setf tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                          index index2))
                (progn               
                  (format *om-stream* "~a~%" 'empty)
                  (setf index 0
                        tocollect (null-beat self)
                        ))))))
        when tocollect collect tocollect 
        ))



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
  (and ;(or (not (tabou-mode self)) (gethash index (tabou self)))
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



(defmethod undefined-label? ((label list)) 
  (not (member (first label) (append '(c c# d eb e f f# g g# a bb b) '(db d# gb ab a#)))))

;search up to a transposition added by M.C.
; find-beat-label-match = search for a beat without continuity

(defmethod find-beat-label-match ((self improvizer) label)  
  (let* (;(left (round (* (/ (first (start-region self)) 100) (1- (maxetat self)))))      ;region defined as %
         ;(width (round (* (/ (- (second (start-region self)) (first (start-region self))) 100)  (1- (maxetat self)))))
         ;(start (max 1 (min (+ left (random width)) (1- (maxetat self)))   ;instead of a random integer between the two bounds
         (start (max 1 (min (first (start-region self)) (1- (maxetat self))))))      ;-> take the lower bound of 'start-region' 
    (catch
      'tag-fblm
      (when (= (maxetat self) 1) (throw 'tag-fblm 1))
      (loop for i from start to (1- (maxetat self))      ; first parsing -> search for the exact label, from 'start-region'
            if (eligible-beat? (otext self i) label)  
            do (throw 'tag-fblm i))
      ;(setf  left 1                  ; start at beginning = 1 to include all possibilies FOR THE SECOND PARSING (original GA function)
      ;       width (1- (maxetat self)) start (min (+ (random width) left) (1- (maxetat self))))                                                            
      (loop for i from start to (1- (maxetat self))         ; next parsing -> search for a label up to a 3rd, from 'start-region'
            if (loop for delta in (om- (rotate '(-3 -2 -1 0 1 2 3) (random 7)) (CurrentTranspo self))
                     if (eligible-beat? (otext self i) (TransposeLabel label delta))
                     return (progn (incf (CurrentTranspo self) delta) t))

            do (throw 'tag-fblm i))
      nil)))


#|
(defmethod find-beat-label-match ((self improvizer) label)  
  (if (undefined-label? label)   ;;;; Marc 22/8/2012 for grid with silence excepted specific elements (breaks, etc.)
      (progn (format *om-stream* " undefined ") nil)
    (let* ((start (max 1 (min (first (start-region self)) (1- (maxetat self))))) ;-> take the lower bound of 'start-region' 
           (transpotable (make-hash-table :test '=))
           (deltalist (om- '(-3 -2 -1 0 1 2 3) (CurrentTranspo self))))
      (loop for i from start 
            to (min (second (start-region self)) (1- (maxetat self)))     
            append (loop for delta in deltalist
                         when (and (eligible-beat? (otext self i) (TransposeLabel label delta))  ;;;; M.C. 5/11/12 to get a beat which is the first one of a bar:
                                   (or (= i 1)                             ;=> the previous beat must NOT be eligible according to the harmonic label   
                                       (not (eligible-beat? (otext self (1- i)) (TransposeLabel label delta))))) 
                         do (push delta (gethash i transpotable))
                         and collect i)
            into res
            finally (return (progn (setf res (reduce-eligible-beats self res))
                              (let* ((chosen-index (nth-random res))
                                     (transpochoices (gethash chosen-index transpotable))
                                     (chosen-transpo (nth-random transpochoices))) 

                                (when chosen-index (incf (CurrentTranspo self) chosen-transpo)
                                  (setf (gethash chosen-index (tabou self)) t) 
                                  )
                        
                                chosen-index)))))))



;Old version with two parsings (G.A.):
; -> firstly from the first element of 'start-region' without transposition, then again with transposition
;In the original G.A. version: 
; - firstly from a random point defined by 'start-region', i.e. between (left, left+width), 
; - then from a random point chosen in the whole oracle (1, maxetat) => 'start-region' has priority, but is not exclusive
;VERY FAST!!!!! thanks to the catch throw construction

(defmethod find-beat-label-match ((self improvizer) label)  
  (let* (;(left (round (* (/ (first (start-region self)) 100) (1- (maxetat self)))))      ;region defined as %
         ;(width (round (* (/ (- (second (start-region self)) (first (start-region self))) 100)  (1- (maxetat self)))))
         ;(start (max 1 (min (+ left (random width)) (1- (maxetat self)))   ;instead of a random integer between the two bounds
         (start (max 1 (min (first (start-region self)) (1- (maxetat self))))))      ;-> take the lower bound of 'start-region' 
    (catch
      'tag-fblm
      (when (= (maxetat self) 1) (throw 'tag-fblm 1))
      (loop for i from start to (1- (maxetat self))      ; first parsing -> search for the exact label, from 'start-region'
            if (eligible-beat? (otext self i) label)  
            do (throw 'tag-fblm i))
      ;(setf  left 1                  ; start at beginning = 1 to include all possibilies FOR THE SECOND PARSING (original GA function)
      ;       width (1- (maxetat self)) start (min (+ (random width) left) (1- (maxetat self))))                                                            
      (loop for i from start to (1- (maxetat self))         ; next parsing -> search for a label up to a 3rd, from 'start-region'
            if (loop for delta in (om- (rotate '(-3 -2 -1 0 1 2 3) (random 7)) (CurrentTranspo self))
                     if (eligible-beat? (otext self i) (TransposeLabel label delta))
                     return (progn (incf (CurrentTranspo self) delta) t))

            do (throw 'tag-fblm i))
      nil)))

  
    (catch
          'tag-fblm
        (when (= (maxetat self) 1) (throw 'tag-fblm 1))
        (loop for i from start
              to (min (second (start-region self)) (1- (maxetat self)))     
               ;(1- (maxetat self)) ; first parsing -> search for the exact label, from 'start-region'
              when (and  
                    (eligible-beat? (otext self i) label)
                    (not (remove-if-rejectedfeature i self)))                           ;;;;;;;MARC 11/5/12

              do (throw 'tag-fblm (progn (setf (gethash i (tabou self)) t) 
                                    i))) 
      ;(setf  left 1                  ; start at beginning = 1 to include all possibilies
      ;       width (1- (maxetat self)) start (min (+ (random width) left) (1- (maxetat self))))                                                            
        (loop for i from 1 ;start    --> Marc 19/8/12  
              ;;; start etait utilisé pour que start-region joue un role de delimiteur strict
              ;;; mais on ne peut plus differentier le passage a hauteur reel et le passage a transposition pres
              to (1- (maxetat self))         ; next parsing -> search for a label up to a 3rd, from 'start-region'
              if (loop for delta in (om- (rotate '(-3 -2 -1 0 1 2 3) (random 7)) (CurrentTranspo self))
                       if (and
                           (eligible-beat? (otext self i) (TransposeLabel label delta))
                           (not (remove-if-rejectedfeature i self)))                   ;;;;;;;MARC 11/5/12
                       return (progn (incf (CurrentTranspo self) delta) t))

              do (throw 'tag-fblm (progn
;TABOU
                                  ;(setf (gethash i (tabou self)) t) 
                                    i)))
        nil))))
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


(defmethod save-improvizer-as ((self improvizer))
  (catch-cancel
     (let* ((name (choose-new-file-dialog :prompt "Save Improvizer As...")))
       (when name
         (save-improvizer self name)))))

(defmethod save-improvizer ((self improvizer) (name t))
  (WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
  t)


(defmethod load-improvizer-from ()
  (catch-cancel
     (let ((name (choose-file-dialog)))
       (when name
         (load-improvizer name)))))

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