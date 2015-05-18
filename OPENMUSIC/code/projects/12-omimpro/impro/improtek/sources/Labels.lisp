(in-package :om)



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

;Jerome 20/03/2013
(defmethod TransposeGrid ((harmgrid list) int)
(loop for j from 0 to (1- (list-length harmgrid)) for lab = (nth j harmgrid) collect (TransposeLabel lab int)))



(defmethod CompareEvents ((Event1 T) (event2 T))
  (equal event1 event2))

(defmethod CompareEvents ((l1 list) (l2 list))
  (equallabel l1 l2))

(defmethod undefined-label? ((label list)) 
  (not (member (first label) (append '(c c# d eb e f f# g g# a bb b) '(db d# gb ab a#)))))

