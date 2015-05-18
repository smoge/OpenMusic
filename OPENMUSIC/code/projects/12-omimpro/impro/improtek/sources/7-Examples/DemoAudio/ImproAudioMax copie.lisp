(in-package :om)

(defmethod equalLabel ((d1 OrderedAudioDescr) (d2 OrderedAudioDescr))
  (and 
   (equal (round ( / (IdxClusterDesc1 d1) 3)) (round (/ (IdxClusterDesc1 d2) 3)))
   (equal (round ( / (IdxClusterDesc2 d1) 6)) (round (/ (IdxClusterDesc2 d2) 6)))
   )
)

#|
NewYorkCounterpoint_1_rms10-pitch10_tune
electriccounterpoint_fast_rms10-pitch10_tune
Gurtu_ED10-SC4_tune

SW_SC20-R20_tune
Ligeti_SC20-R20_tune

|#


(setf scenario SW_SC20-R20_tune
      memory Ligeti_SC20-R20_tune)
(setf *current-tune* (clone scenario))
(setf *Current-Memory* (clone memory))








(setf TABOOO t)
(if TABOOO (setf tab (make-hash-table :test '=)))
(loop for VOICE from 1 to 1 do
(progn

;(setf memory_tune NewYorkCounterpoint_1_rms10-pitch4_tune)
;(setf memory_tune electriccounterpoint_fast_rms10-pitch4_tune)
(setf memory_tune (Clone-Object *Current-Memory*))

(format *om-stream* "~%=================Generating Voice ~a...=================~%" VOICE)

#|
Mobile_Partie1_tune
Mobile_Partie2_tune
Mobile_Partie3_tune
Mobile_Tot_tune
Mobile_Tot_MoreClasses_tune
(Clone-Object *Current-Memory*)
|# 


(setf *modeTR* nil)
      

(setf scenario (NewOrderedAudioDescrLabelList (expand_grid (grid *current-tune*))))
(setf beatduration (beatduration *current-tune*))
(setf Improvizer (NewSymbolicRealtimeImprovizer_AudioDescrBeats memory_tune beatduration 1))
;(om-inspect Improvizer)

(if TABOOO (setf (tabou-mode Improvizer) t))
(if TABOOO (setf (tabou Improvizer) tab ))


(setf (AuthorizedTranspos Improvizer) '( 0))
;(setf (AuthorizedTranspos Improvizer) '(1 2 3 4 5 6 7 8 0 1 -2 -3 -4 -5 -6 -7 -8))
(setf (max-continuity Improvizer) 1000)

(setf (bestTranspoMode Improvizer) nil)
(setf (FirstWithoutTranspoMode Improvizer) t)
(setf (randomPrefixOccurrenceMode Improvizer) t)
(setf (LengthFactorsFromScen Improvizer) '(1 1000))


(setf current-scenario-suffix scenario
      idx-beginning-next-phase 0
      impro-fragment nil)

(loop while (and current-scenario-suffix (< idx-beginning-next-phase (list-length scenario))) do 


      ;-----------Evaluate
      (format *om-stream* "------~%Idx ~a / ~a~%" idx-beginning-next-phase (list-length scenario))
      (if *modeTR* (setf (start-region Improvizer) (list 1 (+ idx-beginning-next-phase 0)))
        (setf (start-region Improvizer) (list 1 (maxetat Improvizer))))
      (setf impro-fragment 
            (Improvize_OnePhase 
             Improvizer (list-length current-scenario-suffix) current-scenario-suffix idx-beginning-next-phase))
      ;(om-inspect impro-fragment)
      (if impro-fragment
          (progn
            ;(format *om-stream* "Generated sequence of ~a elements~%" (list-length impro-fragment))
            (osc-send-sequence-fragment impro-fragment idx-beginning-next-phase "127.0.0.1" 7657 "/modify" VOICE)
            (setf idx-beginning-next-phase (+ idx-beginning-next-phase (list-length impro-fragment))
                  current-scenario-suffix (nthcdr idx-beginning-next-phase scenario))
            ;(om-inspect current-scenario-suffix)
            )
        (format *om-stream* "No impro fragment generated~%"))
      ;(format *om-stream* "----------NEXT : Idx ~a / ~a WITH SUFF ~a~%" idx-beginning-next-phase (list-length scenario) (FormatLabellist current-scenario-suffix))
      ;-----------Evaluate
      ;(sleep 0.1)

)

(format *om-stream* "~%=================... voice ~a... generated !=================~%" VOICE)
(if TABOOO (setf tab (tabou Improvizer)))

)
      

      
      )
      
      