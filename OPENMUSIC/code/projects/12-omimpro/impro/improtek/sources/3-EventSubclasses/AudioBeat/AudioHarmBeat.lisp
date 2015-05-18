 (in-package :om)


(defclass audiodata (data)
  (
   (IdxInBuffer :initform -1 :initarg :IdxInBuffer :accessor IdxInBuffer)
   (DatesInBuffer :initform '(-1 -1) :initarg :DatesInBuffer :accessor DatesInBuffer)
   (CurrentTransfo :initform 0 :initarg :CurrentTransfo :accessor CurrentTransfo)
   (CurrentTimeStretchCoef :initform 1 :initarg :CurrentTimeStretchCoef :accessor CurrentTimeStretchCoef)
   (InitDuration :initform 500 :initarg :InitDuration :accessor InitDuration)
   (PathAudioFile :initform "unknown path" :initarg :PathAudioFile :accessor PathAudioFile)
   ))

(defun NewAudioData (idx duration &optional dates)
  (let* ((newdata (make-instance 'audiodata
                                   :IdxInBuffer idx
                                   :InitDuration duration
                                   )))
    (when dates (setf (DatesInBuffer newdata) dates))
    newdata))


(defmethod TransposeData ((self audiodata) delta)
  (setf (CurrentTransfo self) (+ (CurrentTransfo self) delta)))

(defmethod TimeStretchData ((self audiodata) coef)
  (setf (CurrentTimeStretchCoef self) (* (CurrentTimeStretchCoef self) coef)))



; Inheritance : Event -> Harmbeat -> AudioHarmbeat (and MidiHarmBeat)
(defclass* audioharmbeat (harmbeat)
  (
   (data :initform (make-instance 'audiodata) :initarg :data :accessor IdxInBuffer :type audiodata)      
   ))

(defun NewAudioHarmbeat (root chordtype idxinbuffer duration &optional dates)
  (let* ((AudioHarmbeat (make-instance 'audioHarmbeat
                                   :label (NewHarmLabel root chordtype)
                                   :duration duration
                                   :data (NewAudioData idxinbuffer duration))))
    (when dates (setf (DatesInBuffer (data AudioHarmbeat)) dates))
    AudioHarmbeat))

(defmethod empty-Event? ((self audioharmbeat))
  (or (null (IdxInBuffer self)) (< (IdxInBuffer self) 0)))





;====================================================================================================================================================================================
;====================================================================================================================================================================================
;MARC 10/2/2012 generic function that works with 'Events', but also with objects of specific classes ('meloEvents', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedEvent ((self Event) int)
;- eligible-Event? ((self Event) (label list))
;- CompareEvents ((Event1 Event) (event2 Event))
;- clone-object ((self Event))
;when using label objects instead of simple lists ('garnerlabel'...):
;- TransposeLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))

;--> continuations-by-suppleance, find-Event-label-match, choose-factor-link uses these functions only
;====================================================================================================================================================================================
;====================================================================================================================================================================================


#|
(defmethod CompareEvents ((event1 Event) (event2 Event)) 
   (or (equal (label event1) (label event2)) 
       (equalLabel (label event1) (label event2))))

(defmethod TransposeClonedEvent ((self Event) int)
   (let ((ClonedEvent (clone-object self))) 
     (setf (label ClonedEvent) (TransposeLabel (label ClonedEvent) int))
     (setf (data ClonedEvent) (TransposeData (data ClonedEvent) int))
     ClonedEvent))

(defmethod empty-Event? ((self Event))
  (null (data self)))

(defmethod eligible-Event? ((self Event) (label list)) 
  (and ;(not (empty-Event? self))                     
       (or (null label) (equalLabel label (label self)))))

(defmethod clone-object ((self Event))
  (let ((cEvent (clone self)))
    (setf (MidiSet cEvent) 
          (copy-tree (MidiSet  self)))
    cEvent))

(defmethod eligible-feature? ((self event) (o improvizer))
  (if (null (feature o)) t 
    (if (integerp (feature self))
        (member (abs (feature self)) (feature o))  ;'features' are MIDI codes, thus 'abs' is needed for prolongation
        nil)))     ;'feature' = nil when the midiharmbeat has no feature, thus it should be rejected if the oracle looks for features
;(defmethod eligible-feature? ((self t) (o improvizer)) t)      ;;;;;;;;;;for genericity


|#









