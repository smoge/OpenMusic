 (in-package :om)

;ISSU D'UN DECOUPAGE DE L'ANCIEN "Improvizer.lisp".

;Commentaires archivés
;--------------------------------------------------------------------------------


#|
(defmethod learn-beat-list ((self improvizer) (beats list) (StartBeatNb integer))
  ;(print StartBeatNb)
  (decf StartBeatNb)
  (let ((beat-list (make-beat-list beats)))
    (when beat-list (setf (StartPhrase (fieventrst beat-list)) T))
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


