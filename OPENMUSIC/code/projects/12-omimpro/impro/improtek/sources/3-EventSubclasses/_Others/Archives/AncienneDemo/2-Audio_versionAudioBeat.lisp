 (in-package :om)


;---------------------------------------------------------------------------------------------------------------------------

(defmethod FormatOutputSequenceOf ((sequence list) (whencontent audioharmbeat) &optional beatduration) sequence)
(defmethod FormatOutputSequenceOf ((sequence list) (whencontent audiodescrbeat) &optional beatduration) sequence)

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent Audiobeat) beatIdxInImpro hostsend portsend adresssend numVoice)
  (let ((s (old-write-impro-as-map-for-antescofo sequence beatIdxInImpro)))
    (osc-send (list adresssend numvoice s) hostsend portsend)
    ))

(defmethod old-write-impro-as-map-for-antescofo ((impro list) (startidxinimpro integer) )
  (let (( s "MAP {") (i 0))
    (loop for el in impro do
          (progn
            (format *om-stream* "Writing ~a~%" el)
            (setf s 
                  (concatenate 'string s 
                               (format nil "(~a, TAB [~a, ~a])" (+ i startidxinimpro) (IdxInBuffer el) (TranspoRMSratio el)))) 
            (setf i (+ i 1))
            (if (nth i impro)
                (setf s (concatenate 'string s (format nil ", ") )) 
              (setf s (concatenate 'string s (format nil " }") )) 
            ))
            )s))
;---------------------------------------------------------------------------------------------------------------------------

 ;////!!!!\\\\ PRESUPPOSE AVANT REEL APPRENTISSAGE :LES BUFFERS COMMENCENT BIEN EN POSITION 1 DU SCEN...
(defun NewSkeletonImprovizer (numVoice tune_scen tune_inmem maxcont randomOccurrence lengthfactor Transpos sizemem &optional beg end)
  (let* ((beatdur (beatduration tune_scen))
         (listofSkeletonAudiobeats '())
         (i 0)
         (shift (if beg beg 0))
         (scenar (if (and beg end) (nthcar (- end beg) (nthcdr beg (expand_grid (grid tune_scen)))) (expand_grid (grid tune_scen))))
         (labels_to_build_listofSkeletonAudiobeats '()))
    
    (loop for l from 1 to sizemem do
          (setf labels_to_build_listofSkeletonAudiobeats (append labels_to_build_listofSkeletonAudiobeats (expand_grid (grid tune_inmem)))))
    
    (loop for label in labels_to_build_listofSkeletonAudiobeats do 
          (progn
            (setf listofSkeletonAudiobeats 
                  (append listofSkeletonAudiobeats 
                          (list 
                           (NewAudiobeat 
                            label i '(-1 -1) 
                            0 (beatduration tune_inmem) "Unknown buffer path"
                            (class1_value label tune_inmem)
                            (gethash (symbol-name (nth 0 label)) tab_corresp_label_idx_class)
                            1.0 ))))
            (setf i (+ i 1))))
    (NewImprovizer listofSkeletonAudiobeats beatdur)))

#|
(setf imp (NewSkeletonImprovizer 1 ElectricCounterpoint_Fast_tune NewYorkCounterpoint_1_tune 1000 nil '(1 1000) Transpos size))
|#

(defun ImproTruK (numVoice tune_scen tune_inmem maxcont randomOccurrence lengthfactor Transpos sizemem &optional beg end)
  (let* ((beatdur (beatduration tune_scen))
         (listofSkeletonAudiobeats '())
         (i 0)
         (shift (if beg beg 0))
         (scenar (if (and beg end) (nthcar (- end beg) (nthcdr beg (expand_grid (grid tune_scen)))) (expand_grid (grid tune_scen))))
         (labels_to_build_listofSkeletonAudiobeats '()))
    
    (setf Improv 
          (NewSkeletonImprovizer numVoice tune_scen tune_inmem maxcont randomOccurrence lengthfactor Transpos sizemem))
#|    
    (defun sendimpro (impro numVoice shift)
       (loop for j from 0 to (- (list-length impro) 1) do
             
             (osc-send-sequence-fragment (list (nth j impro)) (+ j shift) "127.0.0.1" 7657 "/modify" numVoice)
             (sleep 0.2)
             (format *om-stream* "Pos impro ~a : Idx ~a ; Label ~a ; RMStransf ~a ~%" j 
                     (IdxInBuffer (nth j impro))
                     (Label (nth j impro))
                     (CurrentTransfo (nth j impro))
                     )
             ))
 |#   
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
