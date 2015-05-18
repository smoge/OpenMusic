;Ex Labels.lisp : définition d'une classe --> macros deviennent méthodes
;HarmLabels.lisp
;JN 23/02/15 after M.C.
(in-package :om)


;Ex NormalizeRoot
(defmacro NormalizeNote (note) `(case ,note (db 'c#) (d# 'eb) (gb 'f#) (ab 'g#) (a# 'bb) (t ,note)))
;Ex TransposeRoot
(defmacro TransposeNote (root int) `(nth (mod ,int 12) (member (NormalizeNote ,root) '(c c# d eb e f f# g g# a bb b c c# d eb e f f# g g# a bb b))))
;For Midi
(defmacro MidiRoot (root) `(- 12 (length (member (NormalizeNote ,root) '(c c# d eb e f f# g g# a bb b)))))
(defmacro DeltaNote (root1 root2)
  `(- (mod (+ 5 (- (length (member (NormalizeNote ,root1) '(c c# d eb e f f# g g# a bb b)))
                   (length (member (NormalizeNote ,root2) '(c c# d eb e f f# g g# a bb b))))) 12)
      5))


(defclass* harmlabel (label)
           ((root :initform 'c :initarg :root :accessor root) 
            (chordtype :initform 'maj7 :initarg :chordtype :accessor chordtype)))


(defmethod NewHarmLabel ((root symbol) (chordtype t))
  (make-instance 'harmlabel :root root :chordtype chordtype))
;(setf hl1 (NewHarmLabel 'd 'm7) hl2 (NewHarmLabel 'g 7))

(defmethod NewHarmLabelList ((l list))
  (loop for couple in l collect 
        (NewHarmLabel (nth 0 couple) (nth 1 couple))
        ))
;(setf chordprogression (NewHarmLabelList '((d m7) (g 7) (c maj7))))


(defmethod NormalizeLabel ((self harmlabel)) (setf (root self) (NormalizeNote (root self))) self)

(defmethod equalLabel ((h1 harmlabel) (h2 harmlabel))
  (let ((nh1 (NormalizeLabel h1)) (nh2 (NormalizeLabel h2)))
    (and (equal (root nh1) (root nh2)) (equal (chordtype nh1) (chordtype nh2))))) 

(defmethod TransposeLabel ((self harmlabel) delta)
  (cond ((null self) nil)
        (t (progn (setf (root self) (TransposeNote (root self) delta)) self))))
(defmethod TransformLabel ((self harmlabel) delta) (TransposeLabel self delta))

(defmethod undefined-label? ((self harmlabel)) 
  (not (member (root self) (append '(c c# d eb e f f# g g# a bb b) '(db d# gb ab a#)))))

(defmethod FormatLabel  ((self harmlabel)) (list (root self) (chordtype self)))
;(FormatLabel  (NewHarmLabel 'a 'm7))

(defmethod FormatLabelList ((harmlabellist list)) 
  (loop for hl in harmlabellist collect 
        (FormatLabel hl)
        ))
;(FormatLabelList (NewHarmLabelList '((d m7) (g 7) (c maj7))))



(defclass* harmbeat (event)
  (
   (NumBeat :initform 1 :initarg :NumBeat :accessor NumBeat) ; in the measure
   (label :initform (make-instance 'harmlabel) :initarg :label :accessor label :type harmlabel)
   (RelHarmLabel :initform () :initarg :RelHarmLabel :accessor RelHarmLabel :type harmlabel)
   ))





