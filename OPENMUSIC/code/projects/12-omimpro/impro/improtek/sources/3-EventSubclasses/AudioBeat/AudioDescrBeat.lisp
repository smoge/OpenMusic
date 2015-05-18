 (in-package :om)

;Transposition / transformation only on descriptor 1.
(defclass* OrderedAudioDescr (label)
           ((IdxClusterDesc1 :initform 0 :initarg :IdxClusterDesc1 :accessor IdxClusterDesc1) 
            (IdxClusterDesc2 :initform 0 :initarg :IdxClusterDesc2 :accessor IdxClusterDesc2) 
            (ClusterMeanValuesDesc1 :initform nil :initarg :ClusterMeanValuesDesc1  :accessor ClusterMeanValuesDesc1 )
            (ClusterMeanValuesDesc2 :initform nil :initarg :ClusterMeanValuesDesc2  :accessor ClusterMeanValuesDesc2)))


(defmethod NewOrderedAudioDescrLabel (IdxClusterDesc1 IdxClusterDesc2 &optional ClusterMeanValuesDesc1 ClusterMeanValuesDesc2)
  (let ((ClusterMeanVals1 nil) (ClusterMeanVals2 nil))
    (when ClusterMeanValuesDesc1 (setf ClusterMeanVals1 ClusterMeanValuesDesc1))
    (when ClusterMeanValuesDesc2 (setf ClusterMeanVals2 ClusterMeanValuesDesc2))
    (make-instance 'OrderedAudioDescr :IdxClusterDesc1 IdxClusterDesc1 :IdxClusterDesc2 IdxClusterDesc2 :ClusterMeanValuesDesc1 ClusterMeanValuesDesc1 :ClusterMeanValuesDesc2 ClusterMeanValuesDesc2)))
;(FormatLabel (NewOrderedAudioDescrLabel 1 2))

(defmethod NewOrderedAudioDescrLabelList ((l list))
  (loop for el in l
        collect (NewOrderedAudioDescrLabel (nth 0 el) (nth 1 el) (nth 2 el) (nth 3 el))))


(defmethod equalLabel ((d1 OrderedAudioDescr) (d2 OrderedAudioDescr))
  (and 
   (equal (IdxClusterDesc1 d1) (IdxClusterDesc1 d2))
   (equal (IdxClusterDesc2 d1) (IdxClusterDesc2 d2))
   )
)

(defmethod TransposeLabel ((self OrderedAudioDescr) delta)
  (cond ((null self) nil)
        (t 
         (let ((newIndex (+ (IdxClusterDesc1 self) delta)))
           (setf newIndex (max newIndex 0))
           (if (ClusterMeanValuesDesc1 self) 
               (setf newIndex (min newIndex (- (list-length (ClusterMeanValuesDesc1 self)) 1))))
           (setf (IdxClusterDesc1 self) newIndex)
           self))
        ))
(defmethod TransformLabel ((self OrderedAudioDescr) delta) (TransposeLabel self delta))

(defmethod undefined-label? ((self OrderedAudioDescr)) 
  (or (< (IdxClusterDesc1 self) 0)
      (< (IdxClusterDesc2 self) 0)
      (and (ClusterMeanValuesDesc1 self) 
           (> (IdxClusterDesc1 self) (- (list-length (ClusterMeanValuesDesc1 self)) 1)))
      (and (ClusterMeanValuesDesc2 self) 
           (> (IdxClusterDesc2 self) (- (list-length (ClusterMeanValuesDesc2 self)) 1)))))
   

(defmethod FormatLabel  ((self OrderedAudioDescr)) (list (IdxClusterDesc1 self) (IdxClusterDesc2 self)))


(defclass* AudioDescrBeat (event)
  (
   (label :initform (make-instance 'OrderedAudioDescr) :initarg :label :accessor label :type OrderedAudioDescr)
   (data :initform (make-instance 'audiodata) :initarg :data :accessor data :type audiodata)
   (NumBeat :initform 1 :initarg :NumBeat :accessor NumBeat) ; in the measure
   ))


(defun NewAudioDescrBeat (IdxClusterDesc1 IdxClusterDesc2 IdxInBuffer duration &optional ClusterMeanValuesDesc1 ClusterMeanValuesDesc2 dates)
  (let* ((AudioDescrBeat (make-instance 'AudioDescrBeat
                                   :label (NewOrderedAudioDescrLabel
                                           IdxClusterDesc1 IdxClusterDesc2 ClusterMeanValuesDesc1 ClusterMeanValuesDesc2)
                                   :duration duration
                                   :data (NewAudioData idxinbuffer duration))))
    (when dates (setf (DatesInBuffer (data AudioDescrBeat)) dates))
    AudioDescrBeat))


(defmethod TransposeClonedEvent ((self AudioDescrBeat) delta)
   (let* ((ClonedEvent (clone-object self))
          (OrigIdx (IdxClusterDesc1 (label ClonedEvent)))
          (NewIdx 0)
          (ValuesDesc1 (ClusterMeanValuesDesc1 (label ClonedEvent))))
     (setf (label ClonedEvent) (TransposeLabel (label ClonedEvent) delta))
     (setf NewIdx (IdxClusterDesc1 (label ClonedEvent)))
     (if ValuesDesc1
         (setf (CurrentTransfo (data ClonedEvent)) 
               (/ 
                (nth NewIdx ValuesDesc1)  
                (nth OrigIdx ValuesDesc1))
               ); Transformation value = "coef"
       (setf (CurrentTransfo (data ClonedEvent)) 
             (* 1.0 (- NewIdx OrigIdx))
             ); Transformation value = "idx"
       )
     ClonedEvent))

