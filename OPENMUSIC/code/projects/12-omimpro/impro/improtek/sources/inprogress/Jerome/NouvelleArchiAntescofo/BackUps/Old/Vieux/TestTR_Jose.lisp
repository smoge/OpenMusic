;TestTR.lisp
;J.Nika

(in-package :om)


(defmethod! generate-grid-TR ((self tune) beatdur)
  
  ;from "generate-grid"
  (let* ((harmgrid (simplegrid self))
         (beatlist (loop for x in harmgrid collect (make-instance 'beat :HarmLabel x)))
         (beatduration (beatduration self))
         (absolute-path-filename (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "grid"))
                                                :name (format nil "~a-grid-for-TR.txt" (tunename self)))))
        
  ;from "save-for-antescofo2"
  (with-open-file 
      (ss absolute-path-filename 
          :direction :output :if-exists :supersede)

    (format ss "oscsend osc_OM 3002 \"/antescofo/improvize_next_step\"~%oscrecv osc_OM 3002 \"/om\" $actions $pos~%oscrecv hey 3007 \"/grille\" $grille~%oscrecv hey 3007 \"/beat\" $beat")
    
         
    (setf beatlist (transfer-syncopated-event beatlist beatduration))
    (setf beatlist (add-grid-to-beatlist beatlist beatduration))
    (loop with c-beatlist = (mapcar #'clone-object beatlist)
          
          for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
          do (progn 
               (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second)) 
               (format ss "NOTE  60 1.0 beat~a~%" i) 
               (format ss "	GFWD for-killall {~%")
               ;;currentGridSuffix (avant ou apres mnote2 ????)
               ;(format ss "    ~a   currentGridSuffix ~a ~%" 
                       ;;(float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration)
                       ;;0.1
                       ;0.0 
                       ;harmgrid)
               ;mnote2
               (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                     do (format ss "    ~a   mnote2 ~a ~a 0.0.~a ~a ~%"      
                                (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))
                                (MEPitch event)     
                                (MEVel event) 
                                (round (* (/ (MEDur event) beatduration) 480))
                                (MEChannel event)))
               ;currentGridSuffix (avant ou apres mnote2 ????)
               (format ss "    ~a   currentGridSuffix ~a ~%" 
                       (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration)
                       ;0.1
                       ;0.0 
                       harmgrid)
               (format ss "    }~%")
               (pop harmgrid))))))

#|
(setf tune Dicidenbas_tune)
(generate-grid-TR tune (beatduration tune))
|#