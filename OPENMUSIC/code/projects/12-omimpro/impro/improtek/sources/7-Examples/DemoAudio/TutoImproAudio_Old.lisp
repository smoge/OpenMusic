(in-package :om)


;(setf memory_tune NewYorkCounterpoint_1_tune)
(setf memory_tune ElectricCounterpoint_Fast_tune)
      
(setf scenario_tune ElectricCounterpoint_Fast_tune)
(setf scenario (NewOrderedAudioDescrLabelList (expand_grid (grid scenario_tune))))

(setf beatduration (beatduration scenario_tune))
(setf Improvizer (NewSymbolicRealtimeImprovizer_AudioDescrBeats memory_tune beatduration 1))
;(om-inspect Improvizer)

(setf (max-continuity Improvizer) 10)
(setf (bestTranspoMode Improvizer) t)
(setf (FirstWithoutTranspoMode Improvizer) nil)
(setf (randomPrefixOccurrenceMode Improvizer) t)
(setf (AuthorizedTranspos Improvizer) '(-1 0 1))
(setf (LengthFactorsFromScen Improvizer) '(0 1000))


(setf current-scenario-suffix scenario
      idx-beginning-next-phase 0
      impro-fragment nil)


(loop while (<= idx-beginning-next-phase (list-length scenario)) do



;Evaluate
  (setf impro-fragment 
        (Improvize_OnePhase 
         Improvizer (list-length current-scenario-suffix) current-scenario-suffix idx-beginning-next-phase))
;(om-inspect impro-fragment)
  (if impro-fragment
      (osc-send-sequence-fragment impro-fragment idx-beginning-next-phase "127.0.0.1" 7657 "/modify" 1))
  (setf idx-beginning-next-phase (+ idx-beginning-next-phase (list-length impro-fragment))
        current-scenario-suffix (nthcdr idx-beginning-next-phase current-scenario-suffix))
  

)



#| 
(defun ImproTruK (numVoice scenario_tune memory_tune maxcont randomOccurrence lengthfactor Transpos mult &optional beg end)
  (let* ((beatdur (beatduration scenario_tune))
         (listofemptyevents '())
         (i 0)
         (shift (if beg beg 0))
         (scenar (if (and beg end) (nthcar (- end beg) (nthcdr beg (expand_grid (grid scenario_tune)))) (expand_grid (grid scenario_tune))))
         (labels '()))
    
    (setf Improv 
          (NewSymbolicImprovizer numVoice scenario_tune memory_tune maxcont randomOccurrence lengthfactor Transpos mult))
   
   
    (setf (max-continuity Improv) maxcont)
     ;++++++++++++++++++++++++++++++++++++++++
     ;;;;; (bestTranspoMode oracle) : t 
     (setf (bestTranspoMode Improv) t)
     ;;;;; (FirstWithoutTranspoMode oracle) : nil 
     (setf (FirstWithoutTranspoMode Improv) nil)
     ;;;;; (randomPrefixOccurrenceMode oracle) : t 
     (setf (randomPrefixOccurrenceMode Improv) randomOccurrence)
     (setf (AuthorizedTranspos Improv) Transpos)
     ;++++++++++++++++++++++++++++++++++++++++
     (set-LengthFactorsFromGrid Improv lengthfactor)

     (setf impro (ImprovizeOnProfile Improv (length scenar) numVoice scenar))
     
     impro
     ))
 |#