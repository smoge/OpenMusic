(in-package :om)

;======= Oracle.lisp =======

;-----Modification : Definition de la classe Pythie
;La nouvelle table de Hash contient la liste de tous les états appartenant à la même classe. Facilitera donc la recherche "force brute".

;Ancienne version
#|
(defclass* Pythie (oracle)
  (
   ;(hashFactorLink :initform (make-hash-table) :accessor hashFactorLink)
   (comparateur :initform #'comp1 :initarg :comparateur :accessor comparateur)
  )
)
|#

;Nouvelle version
(defclass* Pythie (oracle)
  (
   ;(hashFactorLink :initform (make-hash-table) :accessor hashFactorLink)
   (comparateur :initform #'comp1 :initarg :comparateur :accessor comparateur)
   (hashEquivClasses :initform (make-hash-table :test 'equal) :accessor hashEquivClasses :initarg :hashEquivClasses) ;Jerome 18/12/12
   )
)

;-----Nouveau : méthodes associées
; Index de classe = liste. OK pour Beat, Melobeat,... Trop restrictif en général ?
(defmethod EquivClass ((self Pythie) (classIndex list))
    (gethash classIndex (hashEquivClasses self)))
(defmethod (setf EquivClass) ((etat integer) (self Pythie) (classIndex list))
    (push etat (gethash classIndex (hashEquivClasses self))))





;======= Improvizer.lisp =======

;-----Nouveau : méthodes pour définir l'attribut servant d'étiquelle
(defmethod IndexingField ((event beat)) (NormLabel (harmlabel event)))
(defmethod IndexingField ((event T)) (MidiSet event))

;-----Modification : méthode ajouter-objet
;Rajouter l'attribution du nouvel état à une classe

;Ancienne version
#|
(defmethod ajouter-objet ((self oracle) (objet t))
  (let ((m (maxetat self)) (Pi1))
    (when (>= (1+ m) (length (vectext self)))
      (setf (vectext self) (adjust-array (vectext self) (+ 500 (length (vectext self)))))
      (when (lrsmode self) (setf (veclrs self) (adjust-array (veclrs self) (+ 500 (length (veclrs self)))))))
    
    (creer-etat self (1+ m))
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
|#

;Nouvelle version
(defmethod ajouter-objet ((self oracle) (objet t))
  (let ((m (maxetat self)) (Pi1))
    (when (>= (1+ m) (length (vectext self)))
      (setf (vectext self) (adjust-array (vectext self) (+ 500 (length (vectext self)))))
      (when (lrsmode self) (setf (veclrs self) (adjust-array (veclrs self) (+ 500 (length (veclrs self)))))))
    
    (creer-etat self (1+ m))
    (setf (otext self (1+ m)) objet)
    (setf (EquivClass self (IndexingField objet)) (1+ m)) ;Jerome 18/12/12
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

;----- find-beat-label-match

;Ancienne version
#|
;search up to a transposition added by M.C.
; find-beat-label-match = search for a beat without continuity:
; -> firstly from the first element of 'start-region' without transposition, then again with transposition
; formerly: - firstly from a random point defined by 'start-region', i.e. between (left, left+width), 
;           - then from a random point chosen in the whole oracle (1, maxetat)

(defmethod find-beat-label-match ((self improvizer) label)  
  (let* (;(left (round (* (/ (first (start-region self)) 100) (1- (maxetat self)))))      ;region defined as %
         ;(width (round (* (/ (- (second (start-region self)) (first (start-region self))) 100)  (1- (maxetat self)))))
         ;(start (max 1 (min (+ left (random width)) (1- (maxetat self)))   ;instead of a random integer between the two bounds
         (start (max 1 (min (first (start-region self)) (1- (maxetat self))))))      ;-> take the lower bound of 'start-region' 
    (catch
      'tag-fblm
      (when (= (maxetat self) 1) (throw 'tag-fblm 1))
      (loop for i from start to (1- (maxetat self))      ; first parsing -> search for the exact label, from 'start-region'
            if (and  
                (eligible-beat? (otext self i) label)
                (not (remove-if-tabou-or-region i self)))                           ;;;;;;;MARC 11/5/12
            do (throw 'tag-fblm (progn (setf (gethash i (tabou self)) t) 
                                  i))) 
      ;(setf  left 1                  ; start at beginning = 1 to include all possibilies
      ;       width (1- (maxetat self)) start (min (+ (random width) left) (1- (maxetat self))))                                                            
      (loop for i from start to (maxetat self)         ; next parsing -> search for a label up to a 3rd, from 'start-region'
            if (loop for delta in (om- (rotate '(-3 -2 -1 0 1 2 3) (random 7)) (CurrentTranspo self))
                     if (and
                         (eligible-beat? (otext self i) (TransposeLabel label delta))
                         (not (remove-if-tabou-or-region i self)))                   ;;;;;;;MARC 11/5/12
                     return (progn (incf (CurrentTranspo self) delta) t))

            do (throw 'tag-fblm (progn (setf (gethash i (tabou self)) t) 
                                  i)))
      nil)))
|#

;Nouvelle version
;search up to a transposition added by M.C.
; find-beat-label-match = search for a beat without continuity:
; -> firstly from the first element of 'start-region' without transposition, then again with transposition
; formerly: - firstly from a random point defined by 'start-region', i.e. between (left, left+width), 
;           - then from a random point chosen in the whole oracle (1, maxetat)


(defmethod find-beat-label-match ((self improvizer) label) 
(if (= (maxetat self) 1) 1

  (let ((states (reduce-eligible-beats self (EquivClass self label))))

    (if states
        (let ((min_state (apply 'min states)))
          (setf (gethash min_state (tabou self)) t) min_state) 

      (loop for delta in (om- (rotate '(-3 -2 -1 0 1 2 3) (random 7)) (CurrentTranspo self))
            do (let ((transp_states (reduce-eligible-beats self (EquivClass myoracle3 (TransposeLabel label delta)))))
                 (if transp_states
                     (let ((min_transp_state (apply 'min transp_states)))
                       (setf (gethash min_transp_state (tabou myoracle3)) t) 
                       (incf (CurrentTranspo self) delta) 
                       (return min_transp_state)))
                 )
            finally return nil))
    )
))

;======= MelobeatList.lisp =======
(defmethod IndexingField ((event melobeat)) (harmlabel event)) 
; FAUX !!!! C'EST LA MELOSIGNATURE QUI EST CENSEE ETRE INDEXING ICI !!!!!

#| TEST POUR FIND-BEAT-LABEL-MATCH AVEC MYORACLE3 DANS IMPROTEK TUTORIAL
;PLUS COMPLIQUE QUE "reduce-..." regarder l'histoire du "start" et du lower bound
;Que faire des hash classes,... quand transposition ?
(if (= (maxetat myoracle3) 1) 1

  (let ((states (reduce-eligible-beats myoracle3 (EquivClass myoracle3 label))))

    (if states
        (let ((min_state (apply 'min states)))
          (setf (gethash min_state (tabou myoracle3)) t) min_state) 

      (loop for delta in (om- (rotate '(-3 -2 -1 0 1 2 3) (random 7)) (CurrentTranspo myoracle3))
            do (let ((transp_states (reduce-eligible-beats myoracle3 (EquivClass myoracle3 (TransposeLabel label delta)))))
                 (if transp_states
                     (let ((min_transp_state (apply 'min transp_states)))
                       (setf (gethash min_transp_state (tabou myoracle3)) t) 
                       (incf (CurrentTranspo myoracle3) delta) 
                       (return min_transp_state)))
                 )
            finally return nil))
    )
)
|#




