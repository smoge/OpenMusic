 (in-package :om)

;ISSU D'UN DECOUPAGE DE L'ANCIEN "Improvizer.lisp".

;Ensuite à découper en classe beat et midiharmbeat
;--------------------------------------------------------------------------------



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



(defmethod eligible-beat? ((self beat) (label list)) 
  (and ;(not (empty-beat? self))                      ;MARC 10/2/2012 why controling that the beat is not empty (= silence)?????
       (or (null label) (equalLabel label (;RefHarmLabel 
                                           harmlabel self)))))

(defmethod eligible-feature? ((self beat) (o improvizer))
  (if (null (feature o)) t 
    (if (integerp (feature self))
        (member (abs (feature self)) (feature o))  ;'features' are MIDI codes, thus 'abs' is needed for prolongation
        nil)))     ;'feature' = nil when the beat has no feature, thus it should be rejected if the oracle looks for features

(defmethod eligible-feature? ((self t) (o improvizer)) t)      ;;;;;;;;;;for genericity


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
