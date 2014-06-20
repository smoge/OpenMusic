;TestTR2.lisp
;J.Nika

(in-package :om)

(defmethod! generate-grid-param ((self tune) beatdur)
            
  ;from "generate-grid"
            (let* ((harmgrid (simplegrid self))
                   (gridlength (list-length harmgrid))
                   (beatlist (loop for x in harmgrid collect (make-instance 'beat :HarmLabel x)))
                   (beatduration (beatduration self))
                   (absolute-path-filename (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "grid"))
                                                          :name (format nil "~a-grid-param.txt" (tunename self))))
                   (suffixes "")
                   (midicodes1 "")
                   (midicodes2 "")
                   (suffix nil)
                   (midicode nil)
                   (j 0))
              
  ;from "save-for-antescofo2"
              (with-open-file 
                  (ss absolute-path-filename 
                      :direction :output :if-exists :supersede)
                (setf beatlist (transfer-syncopated-event beatlist beatduration))
                (setf beatlist (add-grid-to-beatlist beatlist beatduration))
                
                (loop with c-beatlist = (mapcar #'clone-object beatlist)
                      for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
                      do (progn 
                           (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second)) 
                           (setf j 0)
                           (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                                 do 
                                 (progn 
                                   (incf j)
                                   (setf midicode (format nil "(~d, \"~a ~a 0.0.~a ~a\")" 
                                                         i 
                                                         (MEPitch event)     
                                                         (MEVel event) 
                                                         (round (* (/ (MEDur event) beatduration) 480))
                                                         (MEChannel event)))
                                   (if (= j 1)
                                       (setf midicodes1 (concatenate 'string midicodes1 midicode))
                                     (if (= j 2)
                                       (setf midicodes2 (concatenate 'string midicodes2 midicode))
                                     ))))
                                 (setf suffix (format nil "(~d,\"\'~a\")" i harmgrid))
                                 (setf suffixes (concatenate 'string suffixes suffix))
                                 (pop harmgrid)
                                 (if harmgrid (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                      ;(format ss "EVENT 1.0~%")
                      ;(format ss "group {~%")
                      (format ss "$suffixes := MAP{")
                      ;QUOTE AU DEBUT ??
                      (format ss "~a}~%~%" suffixes)
                      (format ss "$midicodes1 := MAP{")
                      (format ss "~a}~%~%" midicodes1)
                      (format ss "$midicodes2 := MAP{")
                      (format ss "~a}~%~%" midicodes2)
                      (format ss "$gridlength := ~d~%" gridlength)
                      ;(format ss "}~%")
                      )))
            
#|
(setf tune AllTheThingsYouAre_tune)
(generate-grid-param tune (beatduration tune))
|#

(defmethod string-group-for-anstescofo2 ((self beat) (beatduration integer))
  (let ((c-beat (clone-object self))
        (midicode nil)
        (midicodes ""))
    (setf (MidiSet c-beat) (sort (MidiSet c-beat) #'<= :key #'second)) 
    
    (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet c-beat)) for event in (MidiSet c-beat)
          do 
          (progn 
            (setf midicode (format nil "    ~a   mnote2 ~a ~a \"0.0.~a\" ~a~%" 
                                   (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))
                                   (MEPitch event)     
                                   (MEVel event) 
                                   (round (* (/ (MEDur event) beatduration) 480))
                                   (MEChannel event)))
            (setf midicodes (concatenate 'string midicodes midicode))))
    (format nil "group{~%~a}" midicodes)))






;VERSION "SEQUENTIELLE" : on n'envoie que lorsqu'on a tout le fragment.
; (pour gérer notes tenues, évènements syncopés...)
(defmethod Improvize-send-groupsAnte2-loop-next-factor ((self improvizer) &optional (harmgrid nil) (beatduration integer) (beatIdxInImpro integer) (host t) (port_ante2 integer))
  (let* ((beatlistImpro (Improvize-by-fragments self (list-length harmgrid) harmgrid))
         ;[In non r-t version :] ALWAYS DONE BEFORE CALLING "save-for-antescofo"
         ;In particular in "generate-improbeatlist-from-oraclelist" (Tune.lisp)
         (beatlistImpro (thread-Beats beatlistImpro beatduration))
         ;[In non r-t version :] DONE IN "save-for-antescofo"
         (beatlistImpro (transfer-syncopated-event beatlistImpro beatduration))
         (beatlistImpro (add-grid-to-beatlist beatlistImpro beatduration))
         (beatforAntescofo nil)
         (beatIdx beatIdxInImpro))

    ;BONNE IDEE ?????
    (if (< beatIdxInImpro 2) (reset-navigation-params self))
    
    ; Serveur existe et ouvert ?
    (if (and (boundp '*server*) (not (null *server*)))
         ; Traduction en group antescofo et envoi
        (loop for beat in beatlistImpro do 
              (progn
                (setf beatforAntescofo (string-group-for-anstescofo2 beat beatduration))
                (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
                (incf beatIdx)))
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))






; TRES INSPIRE DE IMPROVIZE (improvizer.lisp) !!!
; MODULARISER ET FAIRE APPEL A DES MEMES FONCTIONS !!!
; -----------------------------------------------------
(defmethod Improvize-by-fragments ((self improvizer) (length integer) &optional (harmgrid nil))
  (let ((impro nil) (current-grid-suffix nil) (next-grid-suffix nil))  
    (format *om-stream* "-----------------------~%")
    (when (null (veclrs self)) (setf (bestSuffixMode self) nil) (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))  
    (when harmgrid (setf next-grid-suffix harmgrid))

    ;===========!!!===========
    ; diff 1/2 avec Improvize !
    ;===========!!!===========    
    (setf (nextPrefixImpactFound self) nil)

    (loop for i from 1 to length
          for label =  (pop harmgrid) 
          do
          (setf current-grid-suffix next-grid-suffix)
          ;Display info : /!\ Current-transpo is managed in "Improvize-one-step". 
          ;Transpositions here (label and current-grid-suffix) are only used to display info.
          (when label
            (format *om-stream* "~%----label=~a, " (FormatLabel label))
            ;(if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", oracle=~a, " (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))))))        
            (if (not (= (CurrentTranspo self) 0)) (format *om-stream* "oracle=~a, " (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))) )))
          (if (= *print_info_navig* 1) (format *om-stream* "i = ~D, current grid suffix = ~a~%" i (TransposeGrid current-grid-suffix (CurrentTranspo self))))

          ;Update for next navigation step
          (setf next-grid-suffix harmgrid)
          ;One navigation step : /!\ (CurrentStateIdx self) is modified
          collect (Improvize-next-state self current-grid-suffix)
          ;===========!!!===========
          ; diff 2/2 avec Improvize !
          ;===========!!!===========
          while (not (nextPrefixImpactFound self))
          )))





















#|
; VERSION AVEC DELAI ET PITCH NEGATIFS 
;(SYNCOPES ET NOTES TENUES NON PRISES EN COMPTE, CAR PLUS TRAVAIL SUR TOUTE UNE BEATLIST AVEC THREAD-BEATS, TRANSFER-SYNCOPATED-EVENTS,... MAIS BEAT PAR BEAT).
;---------------------------------------------------------------------------------


; TODO : TRAITER BEATS VIDES !!!!!
;A RAJOUTER DANS IMPROVIZER.LISP (AU DESSUS DE Improvize-next-state) ?
;---------------------------------------------------------------------------------
;Jerome, 22/07/2013
;Jerome, 26/03/2013 : One step in navigation : modification of current/previous states fields
; RETURNS a group for antescofo2 corresponding to the beat found in the navigation
;Jerome, 25/01/2013 : new navigation with Morris&Pratt ("find-prefix-labels-match")
;---------------------------------------------------------------------------------
(defmethod Improvize-groupAnte2-next-state ((self improvizer)  &optional (harmgrid nil) (beatduration integer))
(let ((next-beat (Improvize-next-state self harmgrid)))
  (string-group-for-anstescofo2 next-beat beatduration)))


;TODO : revoir les arguments... en particuliers les "optionals"
(defmethod Improvize-send-groupsAnte2-loop-next-factor ((self improvizer) &optional (harmgrid nil) (beatduration integer) (beatIdxInImpro integer) (host t) (port_ante2 integer))
(let ((beatforAntescofo nil)
      (beatIdx beatIdxInImpro))
  ;Jerome 23/07/2013
  (setf (nextPrefixImpactFound self) nil)
  (if (and (boundp '*server*) (not (null *server*)))
      (loop while (not (nextPrefixImpactFound self)) do
          (progn
            (setf beatforAntescofo (Improvize-groupAnte2-next-state self harmgrid beatduration))
            (osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) host port_ante2)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
            (incf beatIdx)
            (pop harmgrid))) ; ???????????????????????????????????
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))
|#

