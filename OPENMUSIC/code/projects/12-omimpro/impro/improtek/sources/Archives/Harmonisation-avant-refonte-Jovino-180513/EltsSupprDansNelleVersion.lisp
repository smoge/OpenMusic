;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; HarmonizationTools.lisp
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;(defvar prtRcv 7413)
;(defvar prtSnd 7414)
;(defvar host_server "127.0.0.1") 


;IMPORTE EN BEATLIST UN FICHIER MIDI AVEC MELODIE SUR CH.1, ACCOMP SUR CH. 2, CLOCK (NOIRES DE PITCH 12 = DO octave-4)
;SANS LABELS SUR CH 14 DONC LABELISE TOUT AVEC c
;/!\ Not beatlist : list of beats
(defun midi-to-beats-old ( path_file )

(setf midifromfile (evts-from-midifile path_file)) 
  (progn
    (setf res1 (check-clocks midifromfile) 
          clocksfromfile (first res1) quintuplesfromfile (second res1) 
          defaultbeatdur (round (om-mean (x->dx (mapcar 'first clocksfromfile)))))
    (setf beatsfromfile2 (quintuples->beats clocksfromfile quintuplesfromfile))
    (setf beatsfromfile3 (cut-beat-events clocksfromfile beatsfromfile2))           
    (setf beatsfromfile4 (set-relative-time-beat clocksfromfile beatsfromfile3))
    


    ;-----------------------------------------------------------------------------------
    ;-----------------------------------------------------------------------------------
    ;"""" labellisation """" pour fichier midi sans info de label
    ;(setf beatsfromfile (mapcar #'list beatsfromfile4))
    ;(setf generic_labels (make-list (list-length beatsfromfile) :initial-element '(c)))     
    ;(setf beats_w_labels (mapcar #'cons generic_labels beatsfromfile)))
    ;-----------------------------------------------------------------------------------
    ;labellisation pour fichier avec info de label
    (setf beats_w_labels (label-chord-beat beatsfromfile4)))
    ;-----------------------------------------------------------------------------------
    ;-----------------------------------------------------------------------------------
  
(list beats_w_labels defaultbeatdur)

)

#|
Transferred to "Beatlist.lisp"

;(beats_w_labels defaultbeatdur)
(defun midi-to-beats (path_file)

          (setf midifromfile (evts-from-midifile path_file))
;         (defaultbeatdur (round (/ (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile))))) 2)))   ;;;;;;MARC 2011/6/27 "/ 2" deleted!!!!!
;         (stretchedmidifromfile (timestretch (check-midifile-evts midifromfile) 0.5))
;         (beatlist (make-beat-list (clocked-evts->beats stretchedmidifromfile))))
         (setf defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile)))))))
         (setf beats_w_labels  (clocked-evts->beats midifromfile))
         
(list beats_w_labels defaultbeatdur)

)


(defun midi-to-beatlist (path_file)


(setf beats (midi-to-beats path_file))

;(list (thread-Beats (make-beat-list (first beats)) (second beats) ) (second beats))
(list (make-beat-list (first beats)) (second beats))


)

|#


;A METTRE DANS ANTESCOFO.LISP !!!!!
;A REVOIR, IL FAUT ORACLISER !!!
(defun loop_phrase (path_file)

(setf beatlist (midi-to-beatlist path_file))
  
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (setf absolute_path_filename (make-pathname :directory (append (pathname-directory (tunedir *current-tune*)) (list (tunename *current-tune*) "LoopPhrases"))
                                                  :name (format nil "~a-LoopPhrase_~2,'0d-~d-~a_~2,'0d~2,'0d~2,'0d.txt" 
                                                                (tunename *current-tune*)
                                                                date
                                                                month
                                                                year
                                                                hour
                                                                minute
                                                                second))))

    (ensure-directories-exist absolute_path_filename)
    
    ;(save-for-antescofo beatlist-MeloHarmo beatdur path_harmomelAntescofo)
    (save-for-antescofo (first beatlist) (second beatlist) absolute_path_filename)
)




;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Harmonization.lisp
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;A REMETTRE DANS BEATLIST !!!!
;JEROME REVIEW 14/05 : UTILISE OU NON ???
(defun timestretch_beatlis (beatlist coef)
  (loop for x in 5uples for y = (clone x)
        when (= (length y) 5) do (setf (MEOnset y) (om-round (* coef (MEOnset y))) 
                                       (MEDur y) (if (clock-event-from-midi? y) (MEDur y) (om-round (* coef (MEDur y)))))
        collect y))

