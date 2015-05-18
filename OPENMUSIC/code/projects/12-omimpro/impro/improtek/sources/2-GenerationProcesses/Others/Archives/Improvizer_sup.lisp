 (in-package :om)

;ISSU D'UN DECOUPAGE DE L'ANCIEN "Improvizer.lisp".

;Elements à ne pas mettre dans une version light de la lib
;--------------------------------------------------------------------------------

(defvar *current-improvizer*)


(defclass* CrossEvent (event)
  (
   ))


(defmethod set-Estrada-Hind ((self improvizer) (value list))
  (setf (useEstrada self) (first value)
        (useHindemith self) (second value)))

(defmethod beat-density ((self beat))
  (length (remove-duplicates (mapcar #'second (qMidiSet self)))))



;--------------------------------------------------------------------------------
; Cross alphabet
;---------------

(defmethod mf->crossEvents ((mf-list list))
  (cross->events  (midi->cross  mf-list nil 100 nil nil 32)))



#|
(defmethod mf->crossEvents ((mf-list list))
  (cross->events  
   (if *ofon-mode*
     (midi->cross  mf-list nil 0 nil nil 0)
     (if *Prolifere-mode*
       (midi->cross  mf-list nil 50 100 nil 32)
       (midi->cross  mf-list 50 0 0 50 10)))))

(defmethod mf->crossEvents ((mf-list list))
  (cross->events  
   (if *ofon-mode*
     (midi->cross  mf-list nil 0 nil nil 0)
     (if *Prolifere-mode*
       (midi->cross  mf-list nil 50 100 nil 32)
       (midi->cross  mf-list nil 100 nil nil 32)))))

|#

#|
;not used in beat mode
(defmethod CompareEvents ((Event1 CrossEvent) (Event2 CrossEvent))
  (or (and (= (length (MidiSet event1)) (length (MidiSet event2)))
           (loop for ME1 in (MidiSet event1)
                 for ME2 in (MidiSet event2)
                 always (= (mod (abs (MEPitch ME1)) 12) (mod (abs (MEPitch ME2)) 12))))
      (and 
       (> (length (MidiSet event1)) 2)
       (> (length (MidiSet event2)) 2)
       (or (useEstrada *current-improvizer*) (useHindemith *current-improvizer*))
       (let ((ch1 (mapcar #'(lambda (e) (abs (first e)))  (MidiSet event1)))
             (ch2 (mapcar #'(lambda (e) (abs (first e)))  (MidiSet event2))))
         (or
          (and (useEstrada *current-improvizer*)
               (<= (dist-estrada (om* ch1 100) (om* ch2 100)) 1))
          (and (useHindemith *current-improvizer*)
               (eq (hindem  ch1) (hindem  ch2))))))))
|#



(defmethod mf->crossEvents ((mf-list list))
  (cross->events  
   (if *ofon-mode*
     (midi->cross  mf-list nil 0 nil nil 0)
     (if *Prolifere-mode*
       (midi->cross  mf-list nil 50 100 nil 32)
       (midi->cross  mf-list nil 50 nil nil 20)))))



;;; dernier 60=tendance a quantifier (donc hacher)

(defmethod clone-object ((self CrossEvent))
  (make-instance 'CrossEvent :duration (duration self) :MidiSet (copy-tree (MidiSet  self))))




;;; clone improvized beats and scale their MidiSet (onset+durs) w/regards
;;; to actual (sent by max) beat dur

(defun clone-and-scale-beats (beats dureebeat)
  (loop for beat in beats
        for cbeat = (clone-object beat)
        for beatratio = (/ dureebeat (float (duration cbeat)))
        do (loop for ms  in (MidiSet cbeat)
                 do (setf (MEOnset ms) (round (* (MEOnset ms) beatratio))
                          (MEDur ms) (round (* (MEdur ms) beatratio))))
        collect cbeat))





(defmethod cross->events ((self list) &optional (channel 1))
   (loop for slice in self
         for duration = (second slice)
         collect (make-instance 'CrossEvent 
                   :duration duration 
                   :Midiset
                   (sort (loop for track in (first slice)
                               for chan = channel then (1+ chan)
                               append (loop for (pitch velocity) in track 
                                            collect (list pitch 0 duration velocity chan)))
                         #'< :key #'(lambda (evt) (abs (first evt)))))))
  


(defmethod thread-crossEvents ((self list))
  (setf self (mapcar #'clone-object self))
  (loop for event in  self
        for delay = (duration event)
        with new-waiting-list
        for waiting-list = nil then new-waiting-list
        do (loop for ME in (midiset event)
                 with new-midiset = nil
                 initially do (setf new-waiting-list nil)
                 do (cond ((plusp (MEPitch ME))
                           (push ME new-waiting-list)
                           (push ME new-midiset))
                          ((minusp (MEPitch ME))
                           (let ((waiting (find ME waiting-list 
                                                :test #'(lambda (m1 m2) 
                                                          (and (= (abs (MEPitch m1)) (abs (MEPitch m2))) 
                                                               (= (MEChannel m1) (MEChannel m2)))))))
                             (cond 
                              (waiting
                               (setf (MEDur waiting) (+ (MEDur waiting) (MEDur ME)))
                               (push waiting new-waiting-list))
                              (t (setf (MEPitch ME) (abs (MEPitch ME)))
                                 (push ME new-waiting-list)
                                 (push ME new-midiset))))))
                 finally (setf (Midiset event) new-midiSet)))
  self)

                 
(defmethod CrossEvents->MF ((self list))
  (loop for event in self
        with delay = 0
        for onset = 0 then (+ onset delay)
        do (setf delay (duration event))
        append (loop for (pitch monset dur vel chan) in (MidiSet event)
                     collect (list pitch onset dur vel chan))))


(defmethod CrossEvents->ChordSeq ((self list))
  (if *ofon-mode*
    ; we take no risk of changing the number of cross-evts by threading
    (mf-info->chord-seq (crossevents->mf self))
    (mf-info->chord-seq (crossevents->mf (thread-crossevents self)))
    ))


;;; ------------





(defmethod cross->events ((self list) &optional (channel 1))
   (loop for slice in self
         for duration = (second slice)
         collect (make-instance 'CrossEvent 
                   :duration duration 
                   :Midiset
                   (sort (loop for track in (first slice)
                               for chan = channel then (1+ chan)
                               append (loop for (pitch velocity) in track 
                                            collect (list pitch 0 duration velocity chan)))
                         #'< :key #'(lambda (evt) (abs (first evt)))))))
  


(defmethod thread-crossEvents ((self list))
  (setf self (mapcar #'clone-object self))
  (loop for event in  self
        for delay = (duration event)
        with new-waiting-list
        for waiting-list = nil then new-waiting-list
        do (loop for ME in (midiset event)
                 with new-midiset = nil
                 initially do (setf new-waiting-list nil)
                 do (cond ((plusp (MEPitch ME))
                           (push ME new-waiting-list)
                           (push ME new-midiset))
                          ((minusp (MEPitch ME))
                           (let ((waiting (find ME waiting-list 
                                                :test #'(lambda (m1 m2) 
                                                          (and (= (abs (MEPitch m1)) (abs (MEPitch m2))) 
                                                               (= (MEChannel m1) (MEChannel m2)))))))
                             (cond 
                              (waiting
                               (setf (MEDur waiting) (+ (MEDur waiting) (MEDur ME)))
                               (push waiting new-waiting-list))
                              (t (setf (MEPitch ME) (abs (MEPitch ME)))
                                 (push ME new-waiting-list)
                                 (push ME new-midiset))))))
                 finally (setf (Midiset event) new-midiSet)))
  self)

                 
(defmethod CrossEvents->MF ((self list))
  (loop for event in self
        with delay = 0
        for onset = 0 then (+ onset delay)
        do (setf delay (duration event))
        append (loop for (pitch monset dur vel chan) in (MidiSet event)
                     collect (list pitch onset dur vel chan))))


(defmethod CrossEvents->ChordSeq ((self list))
  (if *ofon-mode*
    ; we take no risk of changing the number of cross-evts by threading
    (mf-info->chord-seq (crossevents->mf self))
    (mf-info->chord-seq (crossevents->mf (thread-crossevents self)))
    ))





;;; bug sort chord in LZ.lisp

(defun continuation (poly legatime arpegtime releastime staccatime toltime)
  (let ((continuation (quantize-voice
                       (unstaccate-voices
                        (synchro-release
                         (unarpeggiate-voices
                          (unlegate-voices
                           (apply #'thread-voices
                                  (merge-voices
                                   (channelize
                                    (prepare-voices (copy-tree poly)))));)
                           legatime)
                          arpegtime)
                         releastime)
                        staccatime)
                       toltime)))
    (mapc #'(lambda (slice) (rplaca  slice (sortchord (first slice))))
             continuation)
    continuation))




