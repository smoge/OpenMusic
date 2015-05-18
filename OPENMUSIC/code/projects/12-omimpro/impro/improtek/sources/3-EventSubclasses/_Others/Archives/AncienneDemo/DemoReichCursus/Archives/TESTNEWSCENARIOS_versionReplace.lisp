 (in-package :om)

(defun tableConvertCLusterIdxs2Labels (listA listB)
  (let ((tabA (make-hash-table :test 'equal))
        (iA 0)
        (tabB (make-hash-table :test 'equal))
        (iB 0))
    (loop for lab in listA do
          (progn
            (push lab (gethash iA tabA))
            (incf iA)
            ))
    (loop for lab in listB do
          (progn
            (push lab (gethash iB tabB))
            (incf iB)
            ))
    (list tabA tabB)))


(defun ClusterIdxsLists2Scenario (clustidxsdimA clustidxsdimB tabLabA tabLabB)
  (let ((scen '()))
    (if (not (= (list-length clustidxsdimA) (list-length clustidxsdimB)))
        (format *om-stream* "Error : cluster idxs lists must have the same length~%")
      (progn 
        (loop for i from 0 to (- (list-length clustidxsdimA) 1) do
              (setf scen (append scen
                                 (list (gethash (nth i clustidxsdimA) tabA) (gethash (nth i clustidxsdimB) tabB) )))
              
        ))) scen))

(setf tabLabels (tableConvertCLusterIdxs2Labels '("a" "bb" "a") '("m7" "maj7"))
      tabLabA (nth 0 tabLabels)
      tabLabB (nth 1 tabLabels))

(setf scen (ClusterIdxsLists2Scenario '(0 1 2 0 1 2 1 0) '(0 1 0 0 1 1 1 0) tabLabA tabLabB))
