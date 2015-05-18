 (in-package :om)

;ISSU D'UN DECOUPAGE DE L'ANCIEN "Improvizer.lisp".

;Définition de la classe event, classe mère de tous les types de tranches 
;(Beat midi avec label harmonique, Beat audio avec label harmonique, Beat midi avec label autre, Beat audio avec label autre,
; Melobeat : Beat midi avec label melosignature,...)
;--------------------------------------------------------------------------------

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


(defclass* event ()
  (
   (MidiSet :initform () :initarg :MidiSet :accessor MidiSet)      ; liste 5uples (midi onset dur vel can)
   (duration :initform 500 :initarg :duration :accessor duration)
   (data :initform nil :initarg :data :accessor data)
   ))

;====================================================================================================================================================================================
; A REDEFINIR !!
;===============

(defmethod CompareEvents ((Event1 Event) (event2 Event)) 
  (and ;(not (empty-Event? event1))
   ;(not (empty-Event? event2))
   ;(= (density event1) (density event2))
   (or (equal (harmlabel event1) (harmlabel event2))   ;;(RefHarmlabel event1) (RefHarmlabel event2))
       (equalLabel (harmlabel event1) (harmlabel event2)) )))





; MARC 10/08/11 fixed bug concerning negative MIDI codes
(defmethod TransposeClonedEvent ((self Event) int)
   (let ((ClonedEvent (clone-object self)))           ; cloned Event needed not to modify Events in the Oracle
     (setf (HarmLabel ClonedEvent) (TransposeLabel (HarmLabel ClonedEvent) int))
     (loop for evt in (MidiSet ClonedEvent)  ; only for notes, does not  apply if it remains other kind of event like clicks (onset 248)
           when (= (length evt) 5)          ; but in newer version (MidiSet Event) only contains real notes (not other events like clocks)
           do (let ((note (+ (abs (MEPitch evt)) int)))
              (if (< note 0) (incf note 12))       ; pb MIDI pitch  limits 0-127
              (if (> note 127) (decf note 12))
              (setf (MEPitch evt) (* note (if (>= (MEPitch evt) 0) 1  -1)))))  ;BUG 2011/5/29: transposition error for negative pitch (prolongation)
     ClonedEvent))


 
(defmethod TransposeClonedEvent ((self t) int) self)   ; for genericity, when self is a non 'Event' event (t is for other types)

(defmethod empty-Event? ((self Event))
  (null (midiset self)))

(defmethod null-Event ((self improvizer))    ;Marc 27/2/2012 pb null improvizer
  (when (otext self 1) (make-instance (class-of (otext self 1)))))





;MARC 10/2/2012 generic function that works with 'Events', but also with objects of specific classes ('meloEvents', 'relativechords', ...)

(defmethod eligible-Event? ((self Event) (label list)) 
  (and ;(not (empty-Event? self))                      ;MARC 10/2/2012 why controling that the Event is not empty (= silence)?????
       (or (null label) (equalLabel label (;RefHarmLabel 
                                           harmlabel self)))))

(defmethod eligible-Event? ((self t) (label list)) (null label))   ; for genericity, when self is a non 'Event' event (t is for other types)

;;;;;;;;;;;;Marc 12/5/12 + 15/8/12  -> 'continuations-by-suppleance'
(defmethod reduce-eligible-Events ((self improvizer) list-of-choices)
  (remove nil list-of-choices :test #'(lambda (x y) (not (and (eligible-index? y self) 
                                                              (eligible-feature? (otext self y) self))))))

;;; deep-clone of Event object
;;; one do not wants to modify Events in the Oracle

(defmethod clone-object ((self Event))
  (let ((cEvent (clone self)))
    (setf (MidiSet cEvent) 
          (copy-tree (MidiSet  self)))
    cEvent))





====================================================================================================================================================================================