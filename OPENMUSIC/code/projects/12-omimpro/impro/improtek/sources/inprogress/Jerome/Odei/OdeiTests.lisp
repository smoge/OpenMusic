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





(setf path_dir "/Users/jnika/Desktop/odei_16_02_preparation/TEST/";tempo=126
;(setf path_dir "/Users/jnika/Desktop/odei_16_02_preparation/TEST2/";tempo=104
      path_metro (format nil "~a~a" path_dir "metro.mid")
      path_track1 (format nil "~a~a" path_dir "track1.mid")
      path_track2 (format nil "~a~a" path_dir "track2.mid")
      tempo 126)

;(evts-from-midifile)

(setf evts-from-metro (evts-from-midifile path_metro)
      evts-from-track (evts-from-midifile path_metro)
      beatdur (om-round (/ 60000 tempo)))
#|
(om-inspect evts-from-metro)
|#

#|
The result of mf-info is a list of tracks. Each track is a list of notes. 
Each note is a list of parameters in the form :
(midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)
|#
;Metro evts -> pitch = 12, onset ok, dur =10, vel=100, chan = 16
(setf formatted-evts-from-metro
      (loop for e in evts-from-metro
            collect (list 12 (nth 1 e) 10 100 16)))
;Virtual label evts (c Maj7) -> pitch = 0, onset = temp+50, dur = dur - 100, vel = 100, chan = 16
(setf virtual-evts-for-label 
      (loop for e in evts-from-metro
            collect (list 0 (+ 50 (nth 1 e)) (- (nth 2 e) 100) 100 16)))
;Track -> chan 1
;(loop for e in evts-from-track do
;      (setf (nth 4 e) 1))
#|
(om-inspect evts-from-metro)
|#


; Mix
(setf evts-mix (append formatted-evts-from-metro virtual-evts-for-label evts-from-track))

(setf beatlist-mix (midievtsList-to-beatlist2 evts-mix beatdur)
      listofbeats-mix (nth 0 beatlist-mix)
      beatdur-mix (nth 1 beatlist-mix)
      chseq-mix (beats->chseq (thread-beats listofbeats-mix beatdur-mix) beatdur-mix 0))
      ;chseq-mix (beats->chseq listofbeats-mix beatdur-mix 0))
;(om-inspect beatlist-mix)

(Stop-Player *general-player*)
(play chseq-mix)

(save-as-midi-with-tempo chseq-mix beatdur-mix (format nil "~a~a" path_dir "ModifiedTEMPOTrack.mid"))


