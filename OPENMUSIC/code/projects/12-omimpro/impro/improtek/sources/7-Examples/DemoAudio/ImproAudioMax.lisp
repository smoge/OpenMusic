#|
NewYorkCounterpoint_1_rms10-pitch10_tune
electriccounterpoint_fast_rms10-pitch10_tune
Gurtu_ED10-SC4_tune
SW_SC20-R20_tune
Ligeti_SC20-R20_tune
|#
(in-package :om)



#|
ElectricCounterpoint_Fast_RMS20-B20_tune
NewYorkCounterpoint_1_RMS20-B20_tune

SW_B20-R20_tune
Ligeti_B20-R20_tune

SW_B20-ZC20_tune
Ligeti_B20-ZC20_tune

Gurtu_ED20-B20_tune

Gurtu_SC20-R20_tune

Ligeti_B20-R20_tune

Ligeti_B20-ZC20_tune

Poulenc_P20-TC20_tune
|#


(defmethod equalLabel ((d1 OrderedAudioDescr) (d2 OrderedAudioDescr))
  (let (
        (div1 1) 
        (div2 1)
        )
  (and 
   (equal (round ( / (IdxClusterDesc1 d1) div1)) (round (/ (IdxClusterDesc1 d2) div1)))
   (equal (round ( / (IdxClusterDesc2 d1) div2)) (round (/ (IdxClusterDesc2 d2) div2)))
   ))
)
(setf scenario Poulenc_P20-TC20_tune
      memory Poulenc_P20-TC20_tune
      )
(setf *current-tune* (clone scenario))
(setf *Current-Memory* (clone memory))
(setf memory_tune (Clone-Object *Current-Memory*))
(setf scen (NewOrderedAudioDescrLabelList (expand_grid (grid *current-tune*))))
(setf beatdur (beatduration *current-tune*))
(setf Imp (NewSymbolicRealtimeImprovizer_AudioDescrBeats memory_tune beatduration 1))








(defmethod equalLabel ((d1 OrderedAudioDescr) (d2 OrderedAudioDescr))
  (let (
        (div1 10) 
        (div2 4)
        )
  (and 
   (equal (round ( / (IdxClusterDesc1 d1) div1)) (round (/ (IdxClusterDesc1 d2) div1)))
   (equal (round ( / (IdxClusterDesc2 d1) div2)) (round (/ (IdxClusterDesc2 d2) div2)))
   ))
)

(setf TABOOO t)
(if TABOOO (setf tab (make-hash-table :test '=)))
(loop for VOICE from 1 to 1 do
(progn


(setf scenario (NewOrderedAudioDescrLabelList (expand_grid (grid *current-tune*))))
(setf beatduration (clone beatdur))
(setf Improvizer (clone Imp))

;(setf memory_tune (Clone-Object *Current-Memory*))

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
      


;(om-inspect Improvizer)

(if TABOOO (setf (tabou-mode Improvizer) t))
(if TABOOO (setf (tabou Improvizer) tab ))


;(setf (AuthorizedTranspos Improvizer) '(0))
(setf (AuthorizedTranspos Improvizer) '(1 2 0 -1 -2))
(setf (max-continuity Improvizer) 100)

(setf (bestTranspoMode Improvizer) t)
(setf (FirstWithoutTranspoMode Improvizer) nil)
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
      
      