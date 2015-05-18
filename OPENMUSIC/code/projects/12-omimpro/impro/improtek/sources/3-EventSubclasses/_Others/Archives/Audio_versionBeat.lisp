 (in-package :om)




 (defun ImproTruK (numVoice scen inmem maxcont randomOccurrence lengthfactor Transpos sizemem beg end)
   (let* ((beatdur (beatduration scen))
          (listofbeats '())
          (i 0)
          (scenar (expand_grid (grid scen)))
          ;(scenar (nthcar (- end beg) (nthcdr beg (expand_grid (grid scen)))))
          (inmemtotal nil))
     
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
     (setf (bestTranspoMode Improv) t)
     ;;;;; (FirstWithoutTranspoMode oracle) : nil 
     (setf (FirstWithoutTranspoMode Improv) t)
     ;;;;; (randomPrefixOccurrenceMode oracle) : t 
     (setf (randomPrefixOccurrenceMode Improv) randomOccurrence)
     (setf (AuthorizedTranspos Improv) Transpos)
     ;++++++++++++++++++++++++++++++++++++++++
     (set-LengthFactorsFromGrid Improv lengthfactor)
     
     (setf impro (ImprovizeOnHarmGrid Improv (length scenar) scenar))

     (setf s "MAP {")
     (setf i 0)
     (loop for b in impro do
           (setf s (concatenate 'string s (format nil "(~a, TAB [~a, ~a]), " i (nth 1 (car (Midiset b))) (nth 0 (car (Midiset b)))) ))
           (setf i (+ i 1))
           )
     (setf s (concatenate 'string s "(-12,-12) }"))

     (format *om-stream* s)

     (osc-send (list (format nil "~a" numVoice) s) "127.0.0.1" 7657)
     
     ))
                                      

(setf size 1)
(setf Transpos '(0))
(setf beg 400)
(setf end 600)

#|
(ImproTruK 1 Spain_tune Spain_tune 8 t '(1 1000) size)
(ImproTruK 2 Spain_tune Caravan_tune 8 t '(1 100) size)
(ImproTruK 3 Spain_tune Spain_tune 4 t '(1 1000) size)
|#

(ImproTruK 1 ElectricCounterpoint_Fast_2_tune ElectricCounterpoint_Fast_2_tune 8 t '(1 100) Transpos size beg end)
(ImproTruK 2 ElectricCounterpoint_Fast_tune ElectricCounterpoint_Fast_2_tune 8 t '(1 100) Transpos size beg end)
(ImproTruK 3 ElectricCounterpoint_Fast_2_tune ElectricCounterpoint_Fast_2_tune 4 t '(1 100) Transpos size beg end)


