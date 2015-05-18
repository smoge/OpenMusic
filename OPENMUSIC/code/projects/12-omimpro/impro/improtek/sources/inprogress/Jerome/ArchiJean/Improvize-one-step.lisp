(defmethod Improvize-one-step ((self improvizer)  &optional (harmgrid nil))
  
  (let* ((index (CurrentStateIdx self)) (previndex (PrevStateIdx self)) (nextindex nil)
         (harmgrid-current-transp (TransposeGrid harmgrid (CurrentTranspo self)))
         (label-current-transp (car harmgrid-current-transp))
         (links nil))
    
    (cond
     
     ;Starting point
     ((and (zerop index) (setf nextindex (find-prefix-labels-match self harmgrid-current-transp)))
      (format *om-stream* "Starting point : ~a ~%" nextindex))
     
     ;Navigation
     (t
      ;Update navigation mode
      (if (check-continuity self) 
          (setf (NavigationMode self) 'continuity)  
        (if (available-suppleance self index) 
            (setf (NavigationMode self) 'suppleance)
          (setf (NavigationMode self) 'continuity)))
      
      ; MODE CONTINUITY
      (when (eq (NavigationMode self) 'continuity)
        (setf links (flink self index)
              nextindex (choose-factor-link self links label-current-transp))
        (if nextindex
            (format *om-stream* "c : ~a ~%" nextindex))                                         
        (if (available-suppleance self index) 
            (setf (NavigationMode self) 'suppleance)
          (setf (NavigationMode self) 'nothing)))
      ;MODE SUPPLEANCE
      (when (eq (NavigationMode self) 'suppleance)
        (setf nextindex (continuations-by-suppleance self index label-current-transp)) 
        (if (and nextindex (/= nextindex previndex))
            (progn 
              (format *om-stream* "--->s : ~a ~%" nextindex)
              (setf (continuity self) 0))
          (format *om-stream*  "~a, " (setf (NavigationMode self) 'nothing))))
      ;MODE NOTHING
      (when (eq (NavigationMode self) 'nothing)
        (setf nextindex (find-prefix-labels-match self harmgrid-current-transp))
        (if nextindex 
            (progn 
              (format *om-stream* "new : ~a " nextindex)
              (if (or (null harmgrid) (zerop (CurrentTranspo self))) 
                  (format *om-stream* " ~%") (format *om-stream* "transpo=~a ~%" (CurrentTranspo self)))
              (setf (continuity self) 0))
          (progn               
            (format *om-stream* "~a~%" 'empty)
            (setf index 0))))
      
      ))
    ;Update for next navigation step
    (setf (PrevStateIdx self) index)
    (setf (CurrentStateIdx self) nextindex)))
