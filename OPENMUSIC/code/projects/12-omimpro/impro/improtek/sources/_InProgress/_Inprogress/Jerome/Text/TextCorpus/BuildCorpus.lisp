(in-package :om)

(setf *pathdossier* "/Users/jnika/Google\ Drive/Dev/OM-LIBRARIES/improtek/sources/inprogress/Jerome/Text/TextCorpus/ImprovizersOnText/")


(defun SaveNewImprovizerOnVowSlicesFromText (text namefile)
  (save-improvizer (NewImprovizerOnVowelSlicesFromTextCorpus text) (concatenate 'string *pathdossier* namefile ".or")))


;(setf listeCorpus '("Complaintes" "AragonEnigme" "CorbeauRenard" "InciProust" "Etranger"))
;(loop for c in listeCorpus do (SaveNewImprovizerOnVowSlicesFromText (eval (read-from-string c)) c))