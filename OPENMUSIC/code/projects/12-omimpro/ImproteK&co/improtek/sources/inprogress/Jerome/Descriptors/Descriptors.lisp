(in-package :om)

;ATTENTION ! CES FONCTIONS NE PRENNENT PAS EN COMPTE LE FAIT QUE PUISSE ÊTRE SUR CANAUX DIFFERENTS ET DONC PAS COMPARABLES ! A TRAITER EN AMONT !

(setf midiTest     
'((16 0 5 1 14) (12 0 5 1 14) (8 0 1521 3 14) (32 0 1521 96 2) (44 11 1287 60 1) (54 281 990 60 1) (62 297 1031 70 1) (60 302 1073 64 1) (65 302 1042 74 1) 
(12 761 5 1 14) (7 1531 1532 3 14))) 

#|
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
|#


(defun nbOnsets (MidiEvtsList)
  (let ((nb 0))
    (loop for note in MidiEvtsList do
          (if (> (MEVel note) 0) (incf nb)))nb))
;(nbOnsets midiTest)

(defun meanDurByOnset (MidiEvtsList)
  (let ((nb 0) (sumDur 0))
    (loop for note in MidiEvtsList do
          (if (> (MEVel note) 0) 
              (progn (incf nb) (setf sumDur (+ sumDur (MEDur note))))
            ))(round (/ sumDur nb))))
;(meanDurByOnset midiTest)

(defun meanPitch (MidiEvtsList)
  (let ((nb 0) (sumPitch 0))
    (loop for note in MidiEvtsList do
          (setf sumPitch (+ sumPitch (MEPitch note))))
          (round (/ sumPitch (list-length MidiEvtsList)))))
;(meanPitch midiTest)

(defun minMaxPitch (MidiEvtsList)
  (let* ((firstPitch (MEPitch (nth 0 MidiEvtsList)))
         (MinMax (list firstPitch firstPitch)))
    (loop for note in (cdr MidiEvtsList) do
          (if (< (MEPitch note) (nth 0 MinMax))
              (setf (nth 0 MinMax) (MEPitch note)))
          (if (> (MEPitch note) (nth 1 MinMax))
              (setf (nth 1 MinMax) (MEPitch note))))MinMax))
;(MinMaxPitch midiTest)

(defun maxPitchInterval (MidiEvtsList)
(let ((MinMax (MinMaxPitch MidiEvtsList)))
  (- (nth 1 MinMax) (nth 0 MinMax))))
;(MaxPitchInterval midiTest)

