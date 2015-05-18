; J. Nika, Nov. 2O11
;
; Obtenir un arrangement d'une melodie MIDI

(in-package :om)




;JEROME REVIEW 14/05 : DEBILE ! FAIRE UNE FONCTION BRIQUE PRENANT EN PLUS EN ARGUMENT UN ORACLE D'HARMONISATION ET UN ORACLE D'ARRANGEMENT.
; ENSUITE VIRER LES VARIABLES GLOBALES QUI FLOTTENT "*current-meloHarm...* et *current-Voicings...* pour en faire un champ de la classe TUNE AU MEME TITRE QUE LA TABLE DE LIVE ORACLES ???

;HARMONISATION ET ARRANGEMENT EN CASCADE : RETOURNE LA PISTE D'ARRANGEMENT
(defun harmonize-current_oracles (solobeatlist beatdur)
;beatdur du solo beatlist pour pouvoir time stretcher par rapport au reftempo de l'oracle
  (let* ((melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats solobeatlist beatdur))))
         
         ; Parcours MeloHarmOracle sur mélodie pour obtenir labels
         (melobeatres (ImprovizeOnHarmGrid *current-MeloHarmOracle* (length melolist) melolist))
         (labels (loop for x in (mapcar 'HarmLabel melobeatres) collect (if (numberp (first x)) nil x)))
         
         ; AJOUTER SUBSTITUTIONS ???

         ; Parcours VoicingOracle sur labels pour obtenir voicings
         (harmo (ImprovizeOnHarmGrid *current-VoicingOracle* (length labels) labels))
         (harmochseq nil))

    (setf harmochseq (beats->chseq harmo (RefTempo *current-VoicingOracle*) 0))
   ; (setf (Lchan harmochseq) (mapcar #'(lambda (x) (substitute 2 1 x)) (Lchan harmochseq)))
;    (mf-info->chord-seq (timestretch (chord-seq->mf-info harmochseq) (/ beatdur 500)))))
    (list (mf-info->chord-seq (timestretch (chord-seq->mf-info harmochseq) (/ beatdur (RefTempo *current-VoicingOracle*)))) labels)))






;A REMETTRE DANS BEATLIST !!!!
;JEROME REVIEW 14/05 : UTILISE OU NON ???
(defun timestretch_beatlis (beatlist coef)
  (loop for x in 5uples for y = (clone x)
        when (= (length y) 5) do (setf (MEOnset y) (om-round (* coef (MEOnset y))) 
                                       (MEDur y) (if (clock-event-from-midi? y) (MEDur y) (om-round (* coef (MEDur y)))))
        collect y))





;JEROME REVIEW 14/05 : différence avec fonction précédente ?
(defun harmonize-current_oracles_beatlist (solobeatlist beatdur)
  (let* ((melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats solobeatlist))))
         
         ; Parcours MeloHarmOracle sur mélodie pour obtenir labels
         (melobeatres (ImprovizeOnHarmGrid *current-MeloHarmOracle* (length melolist) melolist))
         (labels (loop for x in (mapcar 'HarmLabel melobeatres) collect (if (numberp (first x)) nil x)))
         
         ; AJOUTER SUBSTITUTIONS ???

         ; Parcours VoicingOracle sur labels pour obtenir voicings
         (harmo (ImprovizeOnHarmGrid *current-VoicingOracle* (length labels) labels))
         (harmochseq nil))

    (setf harmochseq (beats->chseq harmo (RefTempo *current-VoicingOracle*) 0))
   ; (setf (Lchan harmochseq) (mapcar #'(lambda (x) (substitute 2 1 x)) (Lchan harmochseq)))
;    (mf-info->chord-seq (timestretch (chord-seq->mf-info harmochseq) (/ beatdur 500)))))
    (list (mf-info->chord-seq (timestretch (chord-seq->mf-info harmochseq) (/ beatdur (RefTempo *current-VoicingOracle*)))) labels)))