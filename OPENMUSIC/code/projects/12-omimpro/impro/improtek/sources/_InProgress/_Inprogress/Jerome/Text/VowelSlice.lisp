(in-package :om)



;----------------------------------------------------------------------
; Corpus = Improvizer; Input = Text
;----------------------------------------------------------------------
#|
(setf Corpus (concatenate-improvizer-list (list
  (load-improvizer (concatenate 'string *pathdossier* "Etranger" ".or"))
  (load-improvizer (concatenate 'string *pathdossier* "Complaintes" ".or"))
  (load-improvizer (concatenate 'string *pathdossier* "AragonEnigme" ".or"))
  (load-improvizer (concatenate 'string *pathdossier* "InciProust" ".or"))
  (load-improvizer (concatenate 'string *pathdossier* "CorbeauRenard" ".or"))
  )))
(ImprovizeOnTextWithVowelSlicesImprovizer
 Corpus
 "Dans la jungle terrible jungle, le lion est mort ce soir"
 )
|#


;----------------------------------------------------------------------
; Corpus = Text; Input = Text
;----------------------------------------------------------------------
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels Complaintes BonneDefunte)
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels Complaintes AragonEnigme)
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels (concatenate 'string Complaintes AragonEnigme) CorbeauRenard)  
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels (concatenate 'string Complaintes AragonEnigme CorbeauRenard) "Tikalikatam ohé ohé ohé") 

;(ImprovizeOnInputTextVowelsOnCorpusTextVowels InciProust "Bonjour, mettez moi une baguette pas trop cuite, vous serez bien aimable")

;(ImprovizeOnInputTextVowelsOnCorpusTextVowels InciProust CorbeauRenard)
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels InciProust AragonEnigme)
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels AragonEnigme CorbeauRenard)
;(ImprovizeOnInputTextVowelsOnCorpusTextVowels Etranger CorbeauRenard)

;(ImprovizeOnInputTextVowelsOnCorpusTextVowels (concatenate 'string Complaintes CorbeauRenard InciProust Etranger AragonEnigme) BonneDefunte)


(defclass* VowelSlice (event)
  (
   (VowelClass :initform nil :initarg :VowelClass :accessor VowelClass)      ; liste 5uples (midi onset dur vel can)
   (TextSlice :initform nil :initarg :TextSlice :accessor TextSlice)
   (SyllabePosInWord :initform 0 :initarg :SyllabePosInWord :accessor SyllabePosInWord)
   (LastSyllabeInWord :initform nil :initarg :LastSyllabeInWord :accessor LastSyllabeInWord)
   ))
(defmethod content ((self VowelSlice))
  (TextSlice self))


(defmethod label ((self VowelSlice))
  (list (VowelClass self) (SyllabePosInWord self)))


;ATTENTION, UTILISE DANS NAVIGATION ET CONSTRUCTION !!!
;----> A REVOIR
(defmethod CompareEvents ((event1 VowelSlice) (event2 VowelSlice)) 
  ;test1
  ;(equal (label event1) (label event2))
  ;test2
  (equal (nth 0 (label event1)) (nth 0 (label event2)))
  ;test3
  ;(and (equal (nth 0 (label event1)) (nth 0 (label event2)))
       ;(>= (nth 1 (label event2)) (nth 1 (label event1))))
  ) 

;NOM A CHANGER !!! ON NE PARLE PAS DE BEATS ICI !!!
(defmethod eligible-beat? ((self VowelSlice) (label t))
  ;(or 
   ;(equal (VowelClass self) "e-muet")
;(and 
   (equal (VowelClass self) (nth 0 label)) 
   ;(and 
   ; (> 0 (- (SyllabePosInWord self) (nth 1 label)))
   ; (<= 1 (- (SyllabePosInWord self) (nth 1 label)))
    ;)
   ;(<= (SyllabePosInWord self) (nth 1 label))
   ;)
   ;)
)

;=======================================================================================================================
; ATTENTION !!! ICI IL PREND LA LISTE POUR UN "HARMLABEL" !!!!!
; DU COUP ICI BRICOLAGE EN "ANNULANT"
; -> NECESSITE DE DEFINIR UNE METHODE QUI DEFINIT POUR CHAQUE CLASSE (Beat, MeloBeat, VowelSlice,...) QUELLE EST 
; LE *NOM* DE LA *METHODE* A UTILISER
;------------------------------------------
;-----> ICI LISTE DES METHODES A "GENERICISER" (via une "classe" ou "méthodes" propres pour label/content (cf plus haut) ?) 
;=======================================================================================================================

(defmethod TransposeLabel ((label list) int) label)
  ;(cond ((null label) nil)        ; genericity: for using with non 'Beat' events, it must handle the case label = nil
   ;     ((numberp (first label)) (om+ label int))         ; for using with lists of notes (melodic signatures for harmonizing)
    ;    (t (cons (TransposeRoot (first label) int) (rest label)))))

(defmethod TransposeGrid ((harmgrid list) int) harmgrid)
;(loop for j from 0 to (1- (list-length harmgrid)) for lab = (nth j harmgrid) collect (TransposeLabel lab int)))

(defmethod HarmGridFromImprovizer ((self Improvizer))
  (if (equal (type-of (otext self 1)) 'beat)
      (loop for i from 1 to (maxetat self) collect (harmlabel (otext self i)))
    (if (equal (type-of (otext self 1)) 'melobeat)
        (loop for i from 1 to (maxetat self) collect (melosignature (otext self i)))
      (if (equal (type-of (otext self 1)) 'VowelSlice)
          (loop for i from 1 to (maxetat self) collect (list (VowelClass (otext self i)) (SyllabePosInWord (otext self i))))
        ))))
;=======================================================================================================================
;=======================================================================================================================
;(defmethod CompareEvents ((Event1 VowelSlice) (event2 VowelSlice)) 
;  (equal (VowelClass event1) (VowelClass event2))) 

;NOM A CHANGER !!! ON NE PARLE PAS DE BEATS ICI !!!
;(defmethod eligible-beat? ((self VowelSlice) (label t))
;  (and (equal (VowelClass self) (nth 0 label)) (>= (nth 1 label) (SyllabePosInWord self))))
;================================================================================================================================================
;================================================================================================================================================
;(defmethod ListOfVowelSlice ((text string))
;  (let ((convertedtext (ConvertToFrenchVowelsClasses text)))
;    (loop for slice in convertedtext
;          collect
;          (make-instance 'VowelSlice :VowelClass (nth 0 slice) :TextSlice (nth 1 slice) :SyllabePosInWord (nth 2 slice)))))
(defmethod ListOfVowelSlice ((text string))
  (let ((convertedtext (ConvertToFrenchVowelsClasses text)))
    (loop for slice in convertedtext
          collect
            (make-instance 'VowelSlice :VowelClass (nth 0 slice) :TextSlice (nth 1 slice) :SyllabePosInWord (nth 2 slice)))
          
          ))

;METHODE QUI RETOURNE LE RESULTAT FORMATE COMME ON VEUT.
;POUR L'INSTANT CAS D'ECHEC TRAITE A CE NIVEAU, MAIS DEVRAIT PLUTÖT ËTRE TRAITE AU SEIN DU PROCESSUS DE NAVIGATION !!!!
;ICI, SIMILI TRICHE : SI NIL ON RECOPIE LA VOYELLE PHONETIQUE DU SCENARIO... MIEUX VAUDRAIT LA SYLLABE !!!
(defmethod ImprovizeOnListOfVoyelClassWithVowelSlicesImprovizer ((self improvizer) (scenario list))
  (let* ((ListOfVowelSlices-result (improvize self (length scenario) scenario))
         (List-result (listofslots-from-listofObjects2 ListOfVowelSlices-result 'TextSlice))
         (result nil))
    ;(om-inspect ListOfVowelSlices-result)
    ;(om-inspect List-result)
    (loop for syllabe in List-result do 
          (let ((i -1))
            (incf i)
            (if syllabe
                (setf result (concatenate 'string result syllabe))
              ;;Hum........
              (if (nth 0 (nth i scenario))
                  (setf result (concatenate 'string result (nth 0 (nth i scenario))))
                (setf result (concatenate 'string result " "))))))
    result)) 

(defmethod ImprovizeOnTextWithVowelSlicesImprovizer ((self improvizer) (text-for-scenario string))
  (let* ((ListOfVowelSlices-scenario (ListOfVowelSlice text-for-scenario)) 
         (scenario-class (listofslots-from-listofObjects2 ListOfVowelSlices-scenario 'VowelClass))
         (scenario-count (listofslots-from-listofObjects2 ListOfVowelSlices-scenario 'SyllabePosInWord))
         (scenario nil))
    (setf scenario (loop for i from 0 to (- (length scenario-class) 1) collect (list (nth i scenario-class) (nth i scenario-count))))
    (ImprovizeOnListOfVoyelClassWithVowelSlicesImprovizer self scenario)))

(defmethod ImprovizeOnInputTextVowelsOnCorpusTextVowels ((corpus string) (text-for-scenario string))
  (let* ((ListOfVowelSlices-corpus (ListOfVowelSlice corpus))
         (Improv (NewImprovizer)))
    ;(om-inspect ListOfVowelSlices-corpus)
    (loop for i from 0 to (1- (length ListOfVowelSlices-corpus)) do (learn-event Improv (nth i ListOfVowelSlices-corpus)))
    (setf (bestTranspoMode Improv) T)
    (setf (firstWithoutTranspoMode Improv) T)
    (setf (AuthorizedTranspos Improv) '(0))
    ;=========== PARAMS DE JEU ===========
    (setf (randomPrefixOccurrenceMode Improv) T)
    (setf (max-continuity Improv) 1000)
    ;=====================================
    (ImprovizeOnTextWithVowelSlicesImprovizer Improv text-for-scenario)))

(defmethod NewImprovizerOnVowelSlicesFromTextCorpus ((corpus string))
  (let ((ListOfVowelSlices-corpus (ListOfVowelSlice corpus))
         (Improv (NewImprovizer)))
    ;(om-inspect ListOfVowelSlices-corpus)
    (loop for i from 0 to (1- (length ListOfVowelSlices-corpus)) do (learn-event Improv (nth i ListOfVowelSlices-corpus)))
    (setf (bestTranspoMode Improv) T)
    (setf (firstWithoutTranspoMode Improv) T)
    (setf (AuthorizedTranspos Improv) '(0))
    ;=========== PARAMS DE JEU ===========
    (setf (randomPrefixOccurrenceMode Improv) T)
    (setf (max-continuity Improv) 1000)
    Improv))

;================================================================================================================================================
;================================================================================================================================================





;------------------------------------------------------------------------------------------------------------------------------------------------
(defun listofslots-from-listofObjects2 (listofobjects symbolslotname)
  (loop for x in (mapcar #'(lambda (obj) (slot-value obj (intern (string-upcase symbolslotname)))) listofobjects) collect x))

(defmethod formatlabel ((label t)) label)
;eligible-object-for-label?

;(defmethod eligible-beat? ((object VowelSlice) (label t))
;  (let ((virtualobject object))
;    (setf (label virtualobject) label)
;    (CompareEvents object virtualobject)))
;TODO : Créer la méthode setf label (ici important car c'est une liste de 2 slots)
;       Créer une méthode eligible-beat-WITH-FILTERS? pour avoir autres paramètres de génération que scénario !!!
;------------------------------------------------------------------------------------------------------------------------------------------------


