(in-package :om)


; TODO : LABEL / CURRENT_GRID_SUFFIX, RIDICULE DE PRENDRE LA TÊTE D'UNE LISTE POUR LUI RAJOUTER ENSUITE !!
(defmethod Improvize_TR ((self improvizer) (length integer) (harmgrid list))

  (setf (CurrentTranspo self) 0)
  (format *om-stream* "-----------------------~%")

  (loop for i from 1 to length
        for label =  (TransposeLabel (pop harmgrid) (CurrentTranspo self)) ; search for continuity according to current transpo

        with index = 0
        with previndex = -1
        with index2
        with links
        with suppleances
        with mode = 'continuity
        with tocollect = nil 
        initially (setf (continuity self) 0)
        
        do        

        (setf previndex index)
        ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
        (setf current_grid_suffix (append (list label) (TransposeGrid harmgrid (CurrentTranspo self))))
        (setf result_step (Improvize_TR_one-step self current_grid_suffix index index2))
        (setf index (nth ??? result_step) index2 (nth ??? result_step))

        when ???index2??? collect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))

        ;when tocollect collect tocollect

        ))


; TODO: ETAT COURANT DANS L'ORACLE A TRANSFORMER EN CHAMP DANS LA CLASSE IMPROVIZER !!!
(defmethod Improvize_TR_one-step ((self improvizer) (grid_suffix list) (index integer) (index2 integer))
  
  (let ((label (car grid_suffix)))
    
    ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
    (if (= print_info 1) (format *om-stream* "current grid suffix = ~a~%" current_grid_suffix))
    
    (cond
     ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
     ;((and (zerop index) (setf index2 (find-beat-label-match self label)))
     ((and (zerop index) (setf index2 (find-prefix-labels-match self grid_suffix)))
      
      (format *om-stream* "Starting point : ~a ~%" index2)
      
    ;++++++++++++++++TOCOLLECT
      (setf tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
            index index2))
     (t (if (check-continuity self) 
            (setf mode 'continuity)  
          (if (available-suppleance self index) 
              (setf mode 'suppleance)
            (setf mode 'continuity)))
        
        
        (when (eq mode 'continuity)
               ;(when (= index (maxetat self)) (setf index 0)
               ;      (format *om-stream* "zero~%" ))
          (setf links (flink self index)
                index2 (choose-factor-link self links label))
               ;(format *om-stream* " ~%=== Index courant ~a. Flink -> ~a successeurs possibles : ~a~%" index (list-length links) links)
          
          (if index2 
              (progn
                (format *om-stream* "c : ~a ~%" index2)
                (setf index index2
    ;++++++++++++++++TOCOLLECT
                      tocollect  (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))  ))                                         
            (if (available-suppleance self index) 
                (setf mode 'suppleance)
              (setf mode 'nothing))))
        
        
        (when (eq mode 'suppleance)
          (setf index2 (continuations-by-suppleance self index label)) 
          (if (and index2 (/= index2 previndex))
              (progn 
                (format *om-stream* "--->s : ~a ~%" index2)
                (setf index index2
    ;++++++++++++++++TOCOLLECT
                      tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                      (continuity self) 0))
            (format *om-stream*  "~a, " (setf mode 'nothing))))
        
        
        (when (eq mode 'nothing)
              ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
              ;(setf index2 (find-beat-label-match self label))
          (setf index2 (find-prefix-labels-match self grid_suffix))
          
          (if index2 
              (progn (format *om-stream* "new : ~a " index2)
                (if (or (null harmgrid) (zerop (CurrentTranspo self))) 
                    (format *om-stream* " ~%") (format *om-stream* "transpo=~a ~%" (CurrentTranspo self)))
                (setf tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                      index index2))
            (progn               
              (format *om-stream* "~a~%" 'empty)
              (setf index 0
                    tocollect (null-beat self)
                    ))))))
    ))

