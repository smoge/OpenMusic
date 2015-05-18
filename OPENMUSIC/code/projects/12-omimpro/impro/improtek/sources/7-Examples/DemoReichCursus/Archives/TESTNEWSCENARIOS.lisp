 (in-package :om)




 (defun ImproTruK (numVoice scen inmem maxcont randomOccurrence lengthfactor sizemem &optional sizescen)
   (let* ((beatdur (beatduration scen))
          (listofbeats '())
          (i 0)
          (scenar (expand_grid (grid scen)))
          (inmemtotal nil))
     (when sizescen (setf scenar (nthcar sizescen scenar)))

     (loop for i from 1 to sizemem do
           (setf inmemtotal (append inmemtotal (expand_grid (grid inmem)))))
     
     (loop for label in inmemtotal do 
           (progn
             (setf listofbeats (append listofbeats (list (make-beat (list label (list (list 0 i i i i)))))))
             (setf i (+ i 1))
             ))

     ;(om-inspect listofbeats)
     
     (setf Improv (NewImprovizer listofbeats beatdur))

     ;(om-inspect Improv)
     
     (setf (max-continuity Improv) maxcont)
     ;++++++++++++++++++++++++++++++++++++++++
     ;;;;; (bestTranspoMode oracle) : t 
     (setf (bestTranspoMode Improv) nil)
     ;;;;; (FirstWithoutTranspoMode oracle) : nil 
     (setf (FirstWithoutTranspoMode Improv) t)
     ;;;;; (randomPrefixOccurrenceMode oracle) : t 
     (setf (randomPrefixOccurrenceMode Improv) randomOccurrence)
     ;++++++++++++++++++++++++++++++++++++++++
     (set-LengthFactorsFromGrid Improv lengthfactor)
     
     (setf impro (ImprovizeOnHarmGrid Improv (length scenar) scenar))

     (setf s "MAP {")
     (setf i 0)
     (loop for b in impro do
           (setf s (concatenate 'string s (format nil "(~a, TAB [~a, ~a]), " i 
                                                  (if (nth 1 (car (Midiset b))) (nth 1 (car (Midiset b))) -1)
                                                  (if (nth 0 (car (Midiset b))) (nth 0 (car (Midiset b))) 0 ) )))
           (setf i (+ i 1))
           )
     (setf s (concatenate 'string s "(-12,-12) }"))

     (format *om-stream* s)

     (osc-send (list (format nil "/replace ~a" numVoice) s) "127.0.0.1" 7657)
     
     ))
                                      

(setf size 1)
(setf sizescen 100)

(ImproTruK 1 ElectricCounterpoint_Fast_tune ElectricCounterpoint_Fast_2_tune 1000 nil '(1 1000) size sizescen)
(ImproTruK 2 ElectricCounterpoint_Fast_tune ElectricCounterpoint_Fast_2_tune 1000 t '(1 1000) size sizescen)
(ImproTruK 3 ElectricCounterpoint_Fast_tune ElectricCounterpoint_Fast_2_tune 1000 t '(1 1000) size sizescen)


;(ImproTruK 1 ElectricCounterpoint_Fast_2_tune ElectricCounterpoint_Fast_2_tune 2 t '(1 100) size sizescen)
;(ImproTruK 2 ElectricCounterpoint_Fast_2_tune ElectricCounterpoint_Fast_2_tune 4 nil '(1 100) size sizescen)
;(ImproTruK 3 ElectricCounterpoint_Fast_2_tune ElectricCounterpoint_Fast_2_tune 4 t '(1 100) size sizescen)


