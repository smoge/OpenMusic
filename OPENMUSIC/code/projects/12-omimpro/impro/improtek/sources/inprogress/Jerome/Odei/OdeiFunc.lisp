(in-package :om)


; Adapté de midi-to-beatlist (beatlist.lisp)
(defun midievtsList-to-beatlist2 (l newbeatdur)
  (let ((beats (midievtsList-to-beats2 l newbeatdur)))
    ; JEROME 03/09/12 : DELETE OU NON LES FIRST BEATS VIDES ????
    ;(when beats (list (delete_first_empty_beats (make-beat-list (first beats) (second beats))) 
    (when beats (list (make-beat-list (first beats) (second beats))
    ;(when beats (list (make-beat-list (first beats))
                      (second beats)))))   ;Marc 5/3/2012 'make-beat-list' WITH beatdur

; Adapté de midi-to-beats (beatlist.lisp)
(defun midievtsList-to-beats2 (l newbeatdur)
  (let ((midifromevtsList (evts-from-midievtsList l)))
    (cond ((null midifromevtsList) (format *om-stream* "Empty MIDI events list~%"))  
                                                                     
          ((and (not (member 16 midifromevtsList :key 'fifth)) 
                (not (member 14 midifromevtsList :key 'fifth)))     ;22/4/2012   TEST CHANNEL 16 (and 14 for older MIDI files)
           (format *om-stream* "No chord data on channel 16~%"))
          ; 05/08/2013 !!! 
          ; POUR CORRIGER BUG DE LA DIVISION PAR 0 DANS OM-MEAN
          (t (let ((time-intervals (x->dx (mapcar 'first (car (check-clocks midifromevtsList))))))
               (if time-intervals 
                   (let* ((defaultbeatdur (round (om-mean time-intervals)))
                         (midifromevtsListSTRETCHED (timestretch (check-midifile-evts midifromevtsList) (float (/ newbeatdur defaultbeatdur))))
                          ;(midifromevtsListSTRETCHED (timestretch midifromevtsList (float (/ newbeatdur defaultbeatdur))))
                         (beats_w_labels  (clocked-evts->beats midifromevtsListSTRETCHED)))
                     (list beats_w_labels newbeatdur))
                 (format *om-stream* "Impossible to compute \"x->dx\" for time-intervals~%")))))))


(defun align-track-on-tempo-grid (path_dir name_track name_metrotrack tempo-theor)
  (let* ((path_track (format nil "~a~a.mid" path_dir name_track))
         (path_metro (format nil "~a~a.mid" path_dir name_metrotrack))
         (evts-from-track (evts-from-midifile path_track))
         (evts-from-metro (evts-from-midifile path_metro))
         (beatdur (om-round (/ 60000 tempo-theor)))
         #|
          The result of mf-info is a list of tracks. Each track is a list of notes. 
          Each note is a list of parameters in the form :
          (midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)
         |#
         ;Metro evts -> pitch = 12, onset ok, dur =10, vel=100, chan = 16-bit-string
         (formatted-evts-from-metro (loop for e in evts-from-metro
                                          collect (list 12 (nth 1 e) 10 100 16)))
         ;Virtual label evts (c Maj7) -> pitch = 0, onset = temp+50, dur = dur - 100, vel = 100, chan = 16
         (virtual-evts-for-label (loop for e in evts-from-metro
                                       collect (list 0 (+ 50 (nth 1 e)) (- (nth 2 e) 100) 100 16)))
         (evts-mix (append formatted-evts-from-metro virtual-evts-for-label evts-from-track))
         (beatlist-mix (midievtsList-to-beatlist2 evts-mix beatdur))
         (listofbeats-mix (nth 0 beatlist-mix))
         (beatdur-mix (nth 1 beatlist-mix))
         (chseq-mix (beats->chseq (thread-beats listofbeats-mix beatdur-mix) beatdur-mix 0)))
    (save-as-midi-with-tempo chseq-mix beatdur-mix (format nil "~a~aMODIF.mid" path_dir name_track))))

(defun align-tracklist-on-tempo-grid (path_dir list-name_track name_metrotrack tempo-theor)
  (loop for nametrack in list-name_track do
        (align-track-on-tempo-grid path_dir nametrack name_metrotrack tempo-theor)))


#|
(setf path_dir "/Users/jnika/Desktop/odei_16_02_preparation/TEST/"
      name_metrotrack "metro"
      list-name_track (list "track1" "track2" name_metrotrack)
      tempo 126)

(align-tracklist-on-tempo-grid path_dir list-name_track name_metrotrack tempo)
|#
      



