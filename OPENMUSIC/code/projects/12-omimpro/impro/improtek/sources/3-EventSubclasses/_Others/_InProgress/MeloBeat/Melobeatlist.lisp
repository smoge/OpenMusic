; M. Chemillier, J. Nika, Nov. 2011
;
; Définition de la classe Melobeat pour l'harmonisation


(in-package :om)

; JEROME 15/05/2013
; WHAT TO DO WITH THE CLASSES "beat" AND "melobeat" ???
; The only difference is the MeloSignature !!
;(defclass* melobeat (beat)
;  ((MeloSignature :initform () :initarg :MeloSignature :accessor MeloSignature)))

(defclass* melobeat (event)
  (
   (MeloSignature :initform () :initarg :MeloSignature :accessor MeloSignature) 
   (HarmLabel :initform () :initarg :HarmLabel :accessor HarmLabel)
   (RelHarmLabel :initform () :initarg :RelHarmLabel :accessor RelHarmLabel)
   (NumBeat :initform 1 :initarg :NumBeat :accessor NumBeat) ; in the measure
   (RefHarmLabel :initform 1 :initarg :RefHarmLabel :accessor RefHarmLabel) ; label before substitution
   (StartPhrase :initform () :initarg :StartPhrase :accessor StartPhrase) ; booleen
   (QMidiSet :initform () :initarg :QMidiSet :accessor QMidiSet) ; quantized midiset
   (Qdivision :initform 1 :initarg :Qdivision :accessor Qdivision)
   (Density :initform 1 :initarg :Density :accessor Density)
   ))


(defmethod make-grid-event-for-beat ((melobeat melobeat) beatduration)
  (list (list 12 0 10 100 16)  ; clock dur=10 vel=100
        ;(list (MidiRoot (first (HarmLabel beat))) 50 (- beatduration 100) (case (second (HarmLabel beat)) (majmacro7 100) (m7 101) (7 102)) 16)))
        ;NEW_LABELS
        (list (MidiRoot (first (HarmLabel melobeat))) 50 (- beatduration 100) (case (second (HarmLabel melobeat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 16)))


; ???????
#|
(defmethod makmacroe-grid-event-for-beat ((beat melobeat) beatduration)
  (list (list 12 0 10 100 16)  ; clock dur=10 vel=100
        ;(list (MidiRoot (first (HarmLabel beat))) 50 (- beatduration 100) (case (second (HarmLabel beat)) (maj7 100) (m7 101) (7 102)) 16)))
        ;NEW_LABELS
        (list (MidiRoot (first (HarmLabel beat))) 50 (- beatduration 100) (case (second (HarmLabel beat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 16)))
|#                


;============================================================================================================
;============================================================================================================
;============================================================================================================macro
;============================================================================================================
; Attention !!!! La congruence mod 12 avait été rajoutée dans la fonction de navigation (eligible-beat?) 
;mais pas dans la fonction de comparaison pour construction !!!! (CompareEvents)

;
; for learning
;(defmethod CompareEvents ((Event1 melobeat) (event2 melobeat)) 
;   (or (loop for x in (MeloSignature Event1) when (not (member x (MeloSignature Event2))) return nil finally return t)
;       (loop fmacroor x in (MeloSignature Event2) when (not (member x (MeloSignature Event1))) return nil finally return t)))

;RAJOUTER LA CONGRUENCE MODULO 12 !!!!!!!!!

(defmethod CompareEvents ((Event1 melobeat) (Event2 melobeat)) 
  (let*  ((Mod12_MeloSignature1 (mapcar #'mod (MeloSignature Event1) (make-list (list-length (MeloSignature Event1)) :initial-element 12)))
          (Mod12_MeloSignature2 (mapcar #'mod (MeloSignature Event2) (make-list (list-length (MeloSignature Event2)) :initial-element 12))))
   (or (loop for x in Mod12_MeloSignature1 when (not (member x Mod12_MeloSignature2)) return nil finally return t)
       (loop for x in Mod12_MeloSignature2 when (not (member x Mod12_MeloSignature1)) return nil finally return t))))

;============================================================================================================
;============================================================================================================
;============================================================================================================
;============================================================================================================
; for improvising
;PARCOURS MELO-HARM ORACLE : (oracle melosignature-beatist-a-harmoniser)
;(defmethod eligible-beat? ((self melobeat) (label list)) 
;  (and (not (empty-beat? self)) (or (null label) (loop for x in label when (not (member x (MeloSignature self))) return nil finally return t))))
;------------------------------------------------------------------------------------------------------------

; for improvising
;PARCOURS MELO-HARM ORACLE : (oracle melosignature-beatist-a-harmoniser)
(defmethod eligible-beat? ((self melobeat) (label list))
  (let ((Mod12_MeloSignature (mapcar #'mod (MeloSignature self) (make-list (list-length (MeloSignature self)) :initial-element 12))))
    (and (not (empty-beat? self)) (or (null label) (loop for x in label when (not (member (mod x 12) Mod12_MeloSignature)) return nil finally return t)))))
;============================================================================================================
;============================================================================================================
;(setf l1 (nthcdr (round (/ (list-length l) 2)) l)
;      l2 (nthcdr (round (/ (list-length l 2)) (reverse l))))



;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Nouvelle def classe melobeat -> de CompareEvents pour construction oracle
;                              -> de eligible-beat? pour navigation
; ATTENTION METTRE A JOUR TOUTES LES FONCTIONS QUI UTILISENT LE CHAMP "Melosignature" si ce n'est plus lui qu'on utilise mais un autre (cf empty-beat juste en dessous)
; Ou bien changer le contenu de melosignature pour ne pas avoir à faire tout ça....

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





(defmethod empty-beat? ((self melobeat))
  (null (MeloSignature self)))

;===========================================================================================================================

; VERSION PROBLEMATIQUE
;(defmethod TransposeClonedBeat ((self melobeat) int)
;  (let ((Clonedbeat (clone-object self)))           ; cloned beat needed not to modify beats in the Oracle
;    (format *om-stream* "~%~%------APPEL A TransposeClonedBeat DE HERMETO.LISP------~%")
;    (setf (HarmLabel Clonedbeat) (TransposeLabel (HarmLabel Clonedbeat) int))
;    (format *om-stream* "... Avant transpo :")
;    (print (MeloSignature Clonedbeat))
;    (loop for x in (MeloSignature Clonedbeat) do (incf x int))
;    (format *om-stream* "~%... Apres transpo de ~a :" int)
;    (print (MeloSignature Clonedbeat))
;    Clonedbeat))

; NOUVELLE VERSION
(defmethod TransposeClonedBeat ((self melobeat) int)
  (let ((Clonedbeat (clone-object self)))           ; cloned beat needed not to modify beats in the Oracle
    ;(format *om-stream* "~%~%------APPEL A TransposeClonedBeat DE HERMETO.LISP------~%")
    (setf (HarmLabel Clonedbeat) (TransposeLabel (HarmLabel Clonedbeat) int))
    ;(format *om-stream* "... Avant transpo :")
    (print (MeloSignature Clonedbeat))
    (loop for i from 0 to (1- (list-length (MeloSignature self))) do 
          (setf (nth i (MeloSignature Clonedbeat)) 
                (+ int (nth i (MeloSignature self)))) )
    ;(format *om-stream* "~%... Apres transpo de ~a :" int)
    (print (MeloSignature Clonedbeat))
    Clonedbeat))

;===========================================================================================================================



(defmethod clone-object ((self melobeat))
  (let ((cbeat (clone self))) (setf (MidiSet cbeat) (copy-tree (MidiSet  self)) (MeloSignature cbeat) (copy-list (MeloSignature self)))
    cbeat))



; FORMAT ARGUMENT ???
(defun make-melobeatlist (list)
  (let ((flatlist (loop for x in list append (rest x))))
    (loop for x in flatlist 
          collect (make-instance 'melobeat :HarmLabel (list (first x) (second x)) :nbeats (third x) :melosignature (fourth x)))))



; JEROME 18/05/2013
; avant : ":melosignature (mapcar 'first (MidiSet beat))"
; changé pour morris pratt car pas sur objets mais sur 'indexing fields..)
;after loading a MIDI file into a beatlist (EXAMPLE #12 in ImprotekTutorial.lisp), convert the result into a melobeatlist:
(defun beats->melobeats (beatlist)
  (loop for beat in beatlist
        ; JEROME 13/05/2014 ::: RECOPIER LES MIDISET !!!!!!!!!!!!!!!!!!!!!!!
        collect (make-instance 'melobeat :HarmLabel (HarmLabel beat) :MidiSet (MidiSet beat) :melosignature (sort-list (remove-duplicates (mapcar 'first (MidiSet beat)))) :duration (duration beat))))


(defun listofbeats->listofmelobeats (beatlist)
  (beats->melobeats beatlist))

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ATTENTION : cette fonction prend aussi en argument la beatduration de la beatlist pour pouvoir diviser et ainsi avoir durée relative...

;(defun beats->melobeats (beatlist)
;  (loop for beat in beatlist
;        (setq v (make-array 12 :type-element 'fix  :initial-element 0))
;        
;        (loop for quintuple in (MidiSet beat)
;              (setf (aref v (mod (first quintuple) 12))
;                    (/ (third quintuple) beatduration)
;                    )
;              )
;        
;        collect (make-instance 'melobeat :HarmLabel (HarmLabel beat) :melosignature (mapcar 'first (MidiSet beat)) :nouveauchamp v)
;        )
;  )
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun NewImprovizer-onMelobeats-fromBeatlist (list-of-beats beatdur)
  (learn-melobeats-improvizer list-of-beats beatdur))

(defun NewImprovizer-onMelobeats-fromMidifile (path-midifile)
  (let* ((beatlist (midi-to-beatlist path-midifile))
         (listofbeats (first beatlist))
         (beatdur (second beatlist)))
   (NewImprovizer-onMelobeats-fromBeatlist listofbeats beatdur)))

(defun listofMelobeats->listofMelosignature (listofMelobeats)
 (mapcar 'MeloSignature listofMelobeats))

(defun listofThreadedBeats->listofMelosignature (listofThreadedBeats)
 ;(listofMelobeats->listofMelosignature (listofbeats->listofmelobeats listofBeats))
  (listofMelobeats->listofMelosignature (listofbeats->listofmelobeats listofThreadedBeats)))

(defun listofBeats->listofMelosignature (list-of-beats beatduration)
 (listofThreadedBeats->listofMelosignature (thread-Beats list-of-beats beatduration)))

(defun beatlist->listofMelosignature (beatlist)
  (listofBeats->listofMelosignature (first beatlist) (second beatlist)))



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmethod ImprovizerToList ((self improvizer))
  (loop for i from 1 to (maxetat self) collect (otext self i)))

(defmethod ImprovizerOnBeats->ImprovizerOnMeloBeats ((self improvizer) &optional beatduration)
  (if (equal (type-of (otext self 1)) 'beat)
      (let* ((beatdur (reftempo self)) (list-of-beats (ImprovizerToList self)))
        (when beatduration (setf beatdur beatduration))
        (NewImprovizer-onMelobeats-fromBeatlist list-of-beats beatdur))))

(defun loadImprovizerOnMelobeats-fromSavedImprovizerOnBeats (pathImprovizerOnBeats)
  (ImprovizerOnBeats->ImprovizerOnMeloBeats (load-improvizer pathImprovizerOnBeats)))

(defmethod ImprovizerOnBeatsOrMeloBeats->listofMelosignature ((self improvizer) &optional beatduration)
  (if (equal (type-of (otext self 1)) 'melobeat)
      (listofMelobeats (improvizertolist self))
    (if (equal (type-of (otext self 1)) 'beat)
        (let ((beatdur (reftempo self)))
          (when beatduration (setf beatdur beatduration))
          (listofBeats->listofMelosignature (ImprovizerToList self) beatdur)))))

(defun loadimprovizer-onMelobeats (path)
  (let ((imp (load-improvizer path)))
    (if (equal (type-of (otext imp 1)) 'beat)
        (setf imp (ImprovizerOnBeats->ImprovizerOnMeloBeats imp))
      (if (not (equal (type-of (otext imp 1)) 'melobeat))
          (setf imp nil)))
    imp))


(defun loadBeatlist-fromSavedImprovizerOnBeats (path)
  (let* ((improv (load-improvizer path))
         (list-of-beats (ImprovizerToList improv))
         (beatdur (refTempo improv)))
    (list list-of-beats beatdur)))

(defun loadListofMelosignature-fromSavedImprovizerOnBeats (path)
  (beatlist->listofMelosignature (loadBeatlist-fromSavedImprovizerOnBeats path)))

;;;;;;; ENSUITE ALLER VOIR DANS HARMOARRANG/HARMONISATIONNEW.LISP SI CA TOUCHE A HARMONISATIONARRANG, ICI "JUSTE" CLASSE MELOBEATS ;;;;;;;; 


#|
;Create a MIDI part for chords on channel 14 according to a given beatlist with labels:
(defun restore-labels-channel14 (beatlist beatdur)
  (make-beat-list (loop for beat in beatlist for label = (HarmLabel beat)
                        collect (list label (list (list 12 0 beatdur 2 14) 
                                                  (list (position (NormalizeRoot (first label)) '(c c# d eb e f f# g g# a bb b))
                                                        0 beatdur (1+ (position (second label) '(maj7 m7 7))) 14))))))
|#


;--------------------------------------------------------------------------------
;CREATING A MidiHarmBeatLIST FROM A MIDIFILE

; (mf-info midifile) gives a list of 5uples (pitch onset dur vel chan)

#|
(setf midifromfile (evts-from-midifile)) 
(setf defaultbeatdur (round (om-mean (x->dx (mapcar 'first (first (check-clocks midifromfile)))))))  
                                                        ;'check-clocks' does not make physical modifications on 'midifromfile'
(print defaultbeatdur)
;if needed with beatdur already defined:   (setf midifromfile (timestretch (check-midifile-evts midifromfile) (/ beatdur defaultbeatdur)))

(setf MidiHarmBeatsfromfile (clocked-evts->MidiHarmBeats midifromfile))
(setf MidiHarmBeatlist (make-MidiHarmBeat-list MidiHarmBeatsfromfile) beatdur defaultbeatdur)


(tunename *current-tune*)
(setf or (liveoracle *current-tune*))


|#