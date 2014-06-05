;Beatlist.lisp
;by Marc Chemillier (2004, revised version 2012)
 
(in-package :om)




(defmacro nthcar (n l) `(loop for x in ,l for i from ,n downto 1 collect x))

;'timestretch' transferred into "Improvizer.lisp"

;Open a MIDI file and gives a pair: (beats_w_labels defaultbeatdur)   ;from Jerome "CreationCorpus.lisp"
(defun midi-to-beats (&optional path_file)
  (let ((midifromfile (evts-from-midifile path_file)))
    (cond ((null midifromfile) (format *om-stream* "Empty MIDI file~%"))  ;Marc 16/2/2012 nil when "midibuff.mid" is empty 
                                                                     
          ((and (not (member 16 midifromfile :key 'fifth)) 
                (not (member 14 midifromfile :key 'fifth)))     ;22/4/2012   TEST CHANNEL 16 (and 14 for older MIDI files)
           (format *om-stream* "No chord data on channel 16~%"))
          (t (let ((defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile)))))))
                   (beats_w_labels  (clocked-evts->beats midifromfile)))
               (list beats_w_labels defaultbeatdur))))))

(defun midi-to-beatlist (&optional path_file)
  (let ((beats (midi-to-beats path_file)))
    (when beats (list (delete_first_empty_beats (make-beat-list (first beats) (second beats))) 
                      (second beats)))))   ;Marc 5/3/2012 'make-beat-list' WITH beatdur

;(make-beat-list beatsfromfile beatdur)  -> give a list of objects 'beats', see below
;(NewImprovizer beatlist) -> give the oracle associated with a list of objects 'beats', see "Improvizer.lisp"

;Jerome 01/05/12
;Jérôme 02/08/13 : "(and (car beatlist ...!
; (si on a vidé toute la liste...)
;Retourne une beatlist sans les premiers beats vides
(defun delete_first_empty_beats (beatlist)
  (loop while (and 
               (car beatlist) 
               (not (MidiSet (car beatlist)))) 
        do (setf beatlist (cdr beatlist))) 
  beatlist
)

(defun add-beat-list (beatlist refbeatdur beatlist1 beatdur1)
  (let ((adjustedbeatlist1 (loop for beat in beatlist1 for newbeat = (clone beat)
                                 do (setf (MidiSet newbeat) (timestretch (MidiSet newbeat) (/ refbeatdur beatdur1))
                                          (duration newbeat) refbeatdur)
                                 collect newbeat)))
    (append beatlist adjustedbeatlist1)))

;MAJ 26/10/13 : WARNING: this function has been deleted in previous version:
(defun add-list-of-beat-list (list-of-beatlist)    ; Marc 23/11/2012
  (loop for x in (cdr list-of-beatlist) with res = (car list-of-beatlist)
        do (setf (car res) (add-beat-list (car res) (cadr res) (car x) (cadr x)))
        finally return res))

; JEROME REVIEW 15/05 : A METTRE DANS IMPROVIZER.LISP ?
(defun add-improvizer (refimprovizer improvizer)
  (when (/= (RefTempo improvizer) (RefTempo refimprovizer)) (SetReftempo improvizer (RefTempo refimprovizer)))
  (loop for i from 1 to (maxetat improvizer) do (learn-event refimprovizer (otext improvizer i)))
  refimprovizer)


;Jerome: ----> CreationCorpus.lisp:  (defun concatenate-improvizers (improvizers_list)

; dans Antescofo.lisp ---> uniquement les appels des differents types de generation:
; 'generate-grid', 'generate-offline-impros', 'load-realtime-data-and-generate-impros', 
; 'generate-accomp-current-voicing-oracle', 'generate-offline-harmos'



#|
;Open a MIDI file with chords on channel 16, and then give a couple: (list of pairs label+mididata, beatdur)
(midi-to-beats)

(mf-info (load-midi-file (om-choose-file-dialog)))

;Example of such a list of pairs label+mididata, with its corresponding 'beatdur':

(setf Zisteinit_beatdur 536
      Zisteinit_beatsfromfile
      '(      ;"Ziste zeste", BPM = 112, beat duration = 60000/BPM = 536 ms, Bernard's solo recorded in Uzeste, April 4th 2003
((bb m7) nil) 
((bb m7) nil) 
((bb m7) nil) 
((bb m7) ((70 190 82 111 11) (76 306 141 100 11))) 
((f 7) ((74 35 153 111 11) (68 212 52 55 11) (73 343 147 72 11))) 
((f 7) nil) 
((f 7) nil) 
((f 7) nil) 
))

;BEATLIST:
(setf Zistebeatlist (make-beat-list Zisteinit_beatsfromfile Zisteinit_beatdur))

(setf Drumsbeatlist   ;beatdur=536, BPM=112
  (let ((2beatpoumchiBPM112 
         '((nolabel nolabel 2) ((36 0 229 80 10) (42 268 229 80 10) (36 536 229 80 10) (40 536 229 80 10) (42 804 229 80 10)))))
    (make-beat-list (list 2beatpoumchiBPM112 2beatpoumchiBPM112 2beatpoumchiBPM112 2beatpoumchiBPM112) Zisteinit_beatdur)))

(play (merger (beats->chseq Zistebeatlist Zisteinit_beatdur 0) (beats->chseq Drumsbeatlist Zisteinit_beatdur 0)))
(Stop-Player *general-player*)

;ORACLE:
(setf oracleziste (NewImprovizer Zistebeatlist Zisteinit_beatdur))

(setf labels0 '((bb m7) (bb m7) (bb m7) (bb m7) (f 7) (f 7) (f 7) (f 7)) labels (append labels0 labels0 labels0 labels0))

(progn (pgmout 4 11) (ctrlchg 7 127 11) (setf (max-continuity oracleziste) 1)
(play (setf impro (merger (beats->chseq (ImprovizeOnHarmGrid oracleziste (length labels) labels) Zisteinit_beatdur 0) 
                          (beats->chseq (loop for i from 1 to (floor (/ (length labels) 8)) append Drumsbeatlist) 
                                        Zisteinit_beatdur 0)))))

(my-save-as-midi impro Zisteinit_beatdur)
(om-inspect Zistebeatlist)

|#



;--------------------------------------------------------------------------------
;Chord labels and clocks are coded in MIDI files as notes on channel 16:
;   - clocks = note 12 (duration 10 ms, vel 100)
;   - root chord = note between 0 and 11
;   - quality chord = velocity 100=maj7, 101=m7, 102=7, 103=m7b5, dim=104
; NEW_LABELS : 103 = m7b5, 104 =dim

(defun make-label-from-midi (event) 
  (list (nth (mod (MEPitch event) 12) '(c c# d eb e f f# g g# a bb b))
        ;(nth (mod (MEVel event) 100) '(maj7 m7 7))))
        ; NEW_LABELS
        (nth (cond ((= (MEChannel event) 16) (mod (MEVel event) 100))
                   ((= (MEChannel event) 14) (1- (MEVel event))))
             '(maj7 m7 7 m7b5 dim))))



(defun make-clock-from-midi (event) (list (MEOnset event) 248))
(defun normalize-tempoevent-from-midi (event) (list 12 (MEOnset event) 10 100 16))     ;clock dur=10 vel=100

(defun grid-event-from-midi? (event) (or (= (MEChannel event) 16) (= (MEChannel event) 14)))
; JEROME REVIEW 15/05/2013 : CANAL 12 ???
(defun clock-event-from-midi? (event) (and (or (= (MEChannel event) 16) (= (MEChannel event) 14)) (= (MEPitch event) 12)))
(defun label-event-from-midi? (event) (and (or (= (MEChannel event) 16) (= (MEChannel event) 14)) (<= (MEPitch event) 11)))

;MIDI CANAL 16 FOR THE GRID
(defmethod make-grid-event-for-beat ((beat beat) beatduration)
  (list (list 12 0 10 100 16)  ; clock dur=10 vel=100
        ;(list (MidiRoot (first (HarmLabel beat))) 50 (- beatduration 100) (case (second (HarmLabel beat)) (maj7 100) (m7 101) (7 102)) 16)))
        ;NEW_LABELS
        (list (MidiRoot (first (HarmLabel beat))) 50 (- beatduration 100) (case (second (HarmLabel beat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 16)))



;============================================
;;;;;;; DEPLACE DANS MELOBEAT.LISP !!!!!!!!!
;============================================
#|
(defmethod make-grid-event-for-beat ((melobeat melobeat) beatduration)
  (list (list 12 0 10 100 16)  ; clock dur=10 vel=100
        ;(list (MidiRoot (first (HarmLabel beat))) 50 (- beatduration 100) (case (second (HarmLabel beat)) (maj7 100) (m7 101) (7 102)) 16)))
        ;NEW_LABELS
        (list (MidiRoot (first (HarmLabel melobeat))) 50 (- beatduration 100) (case (second (HarmLabel melobeat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 16)))
|#
                
;Conversion function for older format (channel 14 instead of 16):
;   - quality chord = velocity 1=maj7 2=m7 3=7

#|
;Create a MIDI part for chords on channel 14 according to a given beatlist with labels:
(defun restore-labels-channel14 (beatlist beatdur)
  (make-beat-list (loop for beat in beatlist for label = (HarmLabel beat)
                        collect (list label (list (list 12 0 beatdur 2 14) 
                                                  (list (position (NormalizeRoot (first label)) '(c c# d eb e f f# g g# a bb b))
                                                        0 beatdur (1+ (position (second label) '(maj7 m7 7))) 14))))))
|#


;--------------------------------------------------------------------------------
;CREATING A BEATLIST FROM A MIDIFILE

; (mf-info midifile) gives a list of 5uples (pitch onset dur vel chan)

#|
(setf midifromfile (evts-from-midifile)) 
(setf defaultbeatdur (round (om-mean (x->dx (mapcar 'first (first (check-clocks midifromfile)))))))  
                                                        ;'check-clocks' does not make physical modifications on 'midifromfile'
(print defaultbeatdur)
;if needed with beatdur already defined:   (setf midifromfile (timestretch (check-midifile-evts midifromfile) (/ beatdur defaultbeatdur)))

(setf beatsfromfile (clocked-evts->beats midifromfile))
(setf beatlist (make-beat-list beatsfromfile) beatdur defaultbeatdur)


(tunename *current-tune*)
(setf or (liveoracle *current-tune*))


|#

(defun clocked-evts->beats (evts)      
  (let (tmp beats)
    (when (listp evts)
      (setf tmp (check-clocks evts) clocks (first tmp) 5uples (second tmp))
      (setf beats (quintuples->beats clocks 5uples))
      (setf beats (cut-beat-events clocks beats))
      (setf beats (set-relative-time-beat clocks beats))
      (setf beats (label-chord-beat beats)))))
 
(defun evts-from-midifile (&optional absolute-path-filename)      
  (catch-cancel
    (let ((name (or absolute-path-filename (om-choose-file-dialog))))
      (when name
        ;(first (mf-info (load-midi-file name)))  ; mf-info gives a list of tracks, each track is a list of notes as 5uplets  
                                                 ;WARNING: in live data, the last tempo clock (midi code=12) should be repeated at least twice
                                                 ; because there may be some NoteOn before which do not correspond to NoteOff
        (sort (apply 'append (mf-info (load-midi-file name))) '< :key 'second)))))


(defun check-midifile-evts (evts)
   (setf evts (loop with res = nil            ; remove multiple occurrences of evts
                    for x in evts
                    when (not (member x res :test 'equal)) do (setf res (cons x res))
                    finally return (reverse res)))
   (setf evts (loop with res = nil            ; remove too close clocks, or too long clocks
                    for x in evts
                    if (clock-event-from-midi? x)  ; tempo clock 
                    do (when (not (member x res :test #'(lambda (x y) (and (clock-event-from-midi? y)  ; midi=12 tempo clock 
                                                                           (< (abs (- (second x) 
                                                                                      (second y))) 100))))) ; too close onsets
                         (setf res (cons (normalize-tempoevent-from-midi x) res)))   ; clock duration which must be 10 ms, vel=1
                    else do (setf res (cons x res))
                    finally return (reverse res)))
   evts)

(defun check-clocks (evts)
   (loop for event in evts
         with cur-clock = nil
         with collectedclocks = nil
         with collectedevents = nil
         do (if (clock-event-from-midi? event)
                (when (or (null cur-clock)   
                          (> (MEOnset event) (+ (MEOnset cur-clock) 100)))   ; remove clocks when too close from each other !!!!!!!
                  (push (make-clock-from-midi event) collectedclocks) (setf cur-clock event))
                (push event collectedevents))
         finally return (list (reverse collectedclocks) (reverse collectedevents))))

(defun quintuples->beats (clocks quintuples)
  (loop for startbeat in clocks
        for endbeat in (rest clocks)   ; only takes events before a endclock, may lose some at the end of the sequence
        append (list (loop while quintuples
                           while (< (second (first quintuples)) 
                                    (- (first endbeat) 100)) ; unquantified note falling on next beat (will get a negative relative time)
                           collect (pop quintuples)))))

(defun cut-beat-events (clocks beat-list)       ;warning: physical modifications
  (loop for startbeat in clocks
        for endbeat in (rest clocks)
        for cur-beat in beat-list
        for next-beat in (rest beat-list)
        for next-parent on (rest beat-list)
        for new-events = (loop for event in cur-beat
                               for end-event  = (+ (MEOnset event) (MEDur event))
                               if (and (< (- (first endbeat) (MEOnset event)) 
                                          (* 0.25 (- (first endbeat) (first startbeat))))
                                       (> end-event (* 1.25 (first endbeat))))  
                                  ; syncopation belonging to the next beat
                               collect event       ; the event is put into the next beat (and will get a negative relative time)
                               if (> end-event (first endbeat))
                               do (setf (MEDur event)  
                                        (- (first endbeat) (MEOnset event)))   ; cut event
                               and collect (list (if (label-event-from-midi? event) 
                                                     (MEPitch event) ;pitch as chord label
                                                     (- (abs (MEPitch event))))  ; minus => becomes a prolongation in the next beat
                                                                                 ; be careful that - applied twice becomes + ...
                                                 (first endbeat) (- end-event (first endbeat)) 
                                                 (MEvel event) (MeChannel event)))  
        if new-events do (setf (car next-parent) (append new-events (car next-parent)))  ;add remaining part to next beat
      )
  beat-list)
        ;finally return beat-list))

(defun set-relative-time-beat (clocks beat-list)          ; physical modification of beat-list
  (loop for startbeat in clocks
        for cur-beat in beat-list
        do (loop for event in cur-beat
                 do (setf (MEOnset event) (- (MEOnset event) (first startbeat)))))    ; possibly negative onset
  beat-list)

(defun label-chord-beat (beat-list)
   (loop for cur-beat in beat-list
         ;for lpitch = nil
         ;for lvel = nil
         for label = nil                
         for newevents = (loop for event in cur-beat
                               if (label-event-from-midi? event)    
                               do (when (> (MEDur event) 20) (push (make-label-from-midi event) label))        
                                                             ;(push (MEPitch event) lpitch) (push (MEVel event) lvel))
                               else collect event)
                          ;for label = (pitch-vel->label (reverse lpitch) (reverse lvel))
                          ;if label collect (list  label newevents)))
          if label collect (list (last-elem label) newevents)))     ; if more than one label in a beat, take only the first


; JEROME REVIEW 15/05/2013 : A REVOIR ! NOTE LES HARMO TROUVEE PAR MODULE D'HARMONISATION (?)
; /!\ Writes on channel 15 the labels in "Harmlabel => physical modification of beat-list
;(midi onset dur vel can)
(defun annote_chords_generated_harmo  (beat-list)
  
  (loop for cur-beat in beat-list
       when (HarmLabel cur-beat)

       do

       ;
       (format *om-stream* "HarmLabel du beat : ~a~%" (HarmLabel cur-beat)) 
       ;
       (setf (MidiSet cur-beat)
              (append 
               (list (list (MidiRoot (first (HarmLabel cur-beat))) 50 (- (duration cur-beat) 100) (case (second (HarmLabel cur-beat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 15))
               (MidiSet cur-beat)
               )  
              )
        )
  beat-list
)
  

;----------------------------------------
;formerly in file "Improvizer.lisp" 10/2/21012


;by M.C. (12/2/2012 bug fixed "mapcar '(lambda" instead of "mapcar #'(lambda" finally replaced by "loop")
(defun make-beat-list (list-of-beats &optional beatdur)
  (let ((beats (mapcar 'make-beat list-of-beats)))
    (if beatdur 
        (loop for beat in beats 
              do (setf (duration beat) 
                       (if (and (listp (harmlabel beat)) (third (harmlabel beat)))   ;for older versions with "multiple beats": 
                           (* beatdur (third (harmlabel beat)))            ;-> 3rd element of harmlable MUST be a number of beats
                         beatdur))
              collect beat)
      beats)))

(defun make-beat (beat-list)
  (make-instance 'beat :HarmLabel (first beat-list) :MidiSet (second beat-list)))

(defmethod oracle->beatlist ((self oracle))
  (loop for i from 1 to (maxetat self)
        collect (otext self i)))

#|
;Tests 29/4/2012 ---> MISSING NoteOff  ????????????????

(loop for i from 0 for x in (thread-Beats (oracle->beatlist (oracle1 *current-tune*)) (RefTempo (oracle1 *current-tune*)))
      do (format *om-stream* "beat=~a chord=~a midiset=~a~%" i (HarmLabel x) (MidiSet x)))

(loop for i from 0 for x in (first (midi-to-beats))
      do (format *om-stream* "beat=~a data=~a ~%" i x))        

(loop for i from 0 with res = (midi-to-beats) for x in (thread-Beats (make-beat-list (first res) (second res)) (second res))
      do (format *om-stream* "beat=~a chord=~a midiset=~a~%" i (HarmLabel x) (MidiSet x)))

(load-midi-file (om-choose-file-dialog))


|#

;by M.C.
;WARNING: for older versions with "multiple beats" -> if label has a 3rd element, it MUST be the duration in number of beats
(defun beats->chseq (beatlist refbeatvalue deltachords)
   (let (chords beatvalue
         lonset 
         ;last-note 
         (beats (loop for beat in beatlist
                      for eventlist = (MidiSet beat)
                      for label = (HarmLabel beat)
                      for onset = 0 then (+ onset beatvalue)
                      do (setf beatvalue              ; beatvalue of previous beat for shifting onset 
                               (if (and (listp label) (third label)) (* refbeatvalue (third label)) 
                                   refbeatvalue))    ;if label does not contain informations on the number of beats,
                                                     ;the chords has the same duration as refbeatvalue
                      ;;;;;;do (format *om-stream* "~a ~a~%" (HarmLabel beat) beatvalue)
                      append (loop for event in eventlist
                                   collect (list (first event) (+ onset (second event)) (third event) (fourth event) 
                                                 ;(1+ 
                                                  (fifth event)      ;)
                                                 )))))
     (setf chords (make-quanti-chords beats deltachords)
         lonset (mapcar 'offset chords))
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset )))


(defun beats->5list (beatlist beatvalue deltachords)
   (let* ((chseq (beats->chseq beatlist beatvalue deltachords)))
     (list (om/ (Lmidic  chseq) 100)
           (butlast (LOnset chseq))
           (Ldur chseq)
           (Lvel chseq)
           (om- (LChan chseq) 1))))
        
(defun events->5list (eventlist  deltachords)
   (let* ((chseq (CrossEvents->ChordSeq eventlist)))
     (list (om/ (Lmidic  chseq) 100)
           (butlast (LOnset chseq))
           (Ldur chseq)
           (Lvel chseq)
           (om- (LChan chseq) 1))))
        

;Function prolongating notes that have a negative midi code (after a similar function 'thread-crossEvents' for non Beat events)
;bug fixed 08/07/11
;--------------------------
(defmethod thread-Beats ((beatlist list) &optional beatdur)   ; prolongation of notes marked with negative midi codes
   (let ((clonedbeatlist (mapcar #'clone-object beatlist)))
     (loop for event in clonedbeatlist
           for delay = (if beatdur beatdur (duration event))
           with new-waiting-list
           for waiting-list = nil then new-waiting-list
           initially do (setf new-waiting-list nil)    ;WARNING: 'initially' clause is executed in each step of the loop (for each 'event')
                                         ;thus it must be placed in this main loop, not in the next secondary loop!!!!!!!!!!!!!!!!!!!!!!!
           do  (loop for ME in (midiset event)
                     with new-midiset = nil

                     do (cond ((plusp (MEPitch ME))
                               (when (>= (+ (MEOnset ME) (MEDur ME)) (- delay 30))       
                                 (push ME new-waiting-list)   ; add condition: the event duration must reach the end of beat
                                       ;BUG 2011/7/4 ... but in human tempo, the "reaching condition" MUST be up to an approximation value
                                              ;(there is no "exact" beatduration in human tempo)
                                 )
                              (push ME new-midiset))
                             ((minusp (MEPitch ME))      ; negative midi code = prolongation
                              (let ((waiting (find ME waiting-list 
                                                   :test #'(lambda (m1 m2) 
                                                             (and (= (abs (MEPitch m1)) (abs (MEPitch m2))) 
                                                                  (= (MEChannel m1) (MEChannel m2)))))))
                                (cond 
                                 (waiting
                                  (setf (MEDur waiting) (+ (MEDur waiting) (MEDur ME)))
                                  (push waiting new-waiting-list))
                                 (t ;forget prolongation note when there is no beginning note (setf (MEPitch ME) (abs (MEPitch ME)))
                                  ;(push ME new-waiting-list)
                                  ;(push ME new-midiset)
                                  )))))
                    finally (setf (Midiset event) new-midiSet)))
     clonedbeatlist))



;-------------------------------------------
;MIDIFILE UTILITIES

#|
;SAVING MIDIFILES BY ADJUSTING BARLINES TO THE CORRECT TEMPO:
;1) Calculate the mean beat duration given by clocks on channel 16 as loaded by OM (= defaultbeatdur), and then stretch durations 
;   with the coef. = 1000/defaultbeatdur so that durations are adapted to OM default tempo BPM=60 and adjusted to the barlines 
;2) Then indicate the real tempo of the original sequence by a metaevent at the beginning of the file for playing notes at the correct speed,
;   (= defaultbeatdur)

;REMARK: Max creates MIDI files thanks to 'seq' with correct durations of the notes, but without tempo metaevent,
;so that an implicit wrong tempo is fixed with value BPM=120 (beat duration = 500 ms).
;When OM loads these files, it does not find any tempo, so another wrong tempo is fixed (BPM=60, beat duration = 1000 ms),
;thus BPM=120 being replaced by BPM=60, durations of the notes are multiplied by 2 (= played half tempo). 
;Consequently, before applying 'save-as-midi-with-tempo', one must divide durations by 2 using 'timestretch' 

;SIMPLE TEST: Write a simple MIDI file with the correct tempo:
(setf chsq700 
      (make-instance 'chord-seq :lmidic '(1200 1200 1200 1200 1200) :lonset '(0 700 1400 2100 2800) :ldur '(10 10 10 10 10) :lchan '(16 16 16 16 16)))

(save-as-midi-with-tempo chsq700 700)
;---> Intuem: barlines are adjusted + tempo is correct BPM=85.71 (=60000/700)

(dump-straight (om-choose-file-dialog))
;---> ((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) 4D 54 72 6B 0 0 0 13 0 FF 58 4 4 2 0 0 0 FF 51 3 A AE 60    etc.
;---> gives the correct tempo meta event = (0 FF 51 3 A AE 60) last three bytes A AE 60 give beat duration in microseconds
(write #x0AAE60 :base 10)     ;---> 700000 = 700 ms BPM=85.71


|#

;REMARK  (22/4/2012): 'save-as-midi-with-tempo' is not intended to change the tempo !!!!!!!!!
;It works well only if 'beatdur' is the correct tempo according to the data on channel 16

(defmethod! save-as-midi-with-tempo ((object t) (beatdur integer) &optional filename &key (approx 2) (format nil))   ;defmethod! = automatic loading of the library
  (when *midiplayer*
      (let ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog  :directory (def-save-directory) 
                                                                                     :prompt (om-str "Save as...") 
                                                                                     :types (list (om-str "MIDI Files") "*.mid;*.midi")))))
      (when name 
        (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
        (my-MidiSaveAny object beatdur approx)         ;write events at OM fixed tempo BPM=60
        (my-save-seq *playing-midi-seq* name beatdur)  ;add a tempo change (but it does not change barlines)
        (namestring name)
        ))))

;for compatibility with older version 'my-save-as-midi' = 'save-as-midi-with-tempo'
;'save-as-midi' already exists in OpenMusic, but it saves the file with a fixed tempo BPM=60

(defun my-save-as-midi (chseq beatdur) (save-as-midi-with-tempo chseq beatdur))   


(defmethod my-MidiSaveAny ((object t) beatdur approx)               ;write notes on different tracks, thanks to Jean Bresson, Oct. 12th 2010
  (when *midiplayer*
    (setf *MidiShare-start-time* 0)
    (setf *playing-midi-seq* (om-midi-new-seq))
    (let ((5uplelist ;(chord-seq->mf-info object))                                 ; -> save with OM fixed tempo BPM=60 for adjusting barlines
                     (timestretch (chord-seq->mf-info object) (/ 1000 beatdur)))   ; + -> THEN update to 'beatdur'   ;MARC 7/2/2012
                     ;(timestretch (chord-seq->mf-info object) 2))   ;MARC 22/4/2012   ??????????????? for barlines to be adjusted to midi data:
                                                                     ;---> it seems that seq. must be saved by OM twice slower (half tempo)

          (hashchan (make-hash-table))) 
      (loop for x in 5uplelist 
            do (push x (gethash (MEChannel x) hashchan)))  ; groups events with the same channel into different sequences devoted to one channel
      (loop for i from 1 to 16 with voice = 1              ; for each channel, write the corresponding sequence into a separate track of the MIDI file
            when (gethash i hashchan) 
            do (progn (PrepareToPlay t      ;this function add events to *playing-midi-seq* ---> (om-midi-seq-add-evt *playing-midi-seq* newevent)
                                     (mf-info->chord-seq (reverse (gethash i hashchan))) 0 :approx approx :voice voice) 
                 (incf voice))))))                                                                                       

(defun my-save-seq (seq name beatdur)                         ;add the correct tempo at the beginning of the MIDI file
  (let ((tempo-evnt (om-midi-new-evt (om-midi-get-num-from-type "Tempo")
                                     :date 0 :vals (* beatdur 1000))))    ;"Tempo" -> = beatdur expressed in microseconds
    (om-midi-seq-concat-evt seq tempo-evnt nil)      ;nil ---> tempo-evnt placed at the beginning
    (om-midi-save-seq-in-file seq (om-path2cmdpath name) :fileformat 1)    ; fileformat default value = 1
    ))

#|
;===========================================================================================
;This will load a MIDI file and resave it with the correct tempo  corresponding to the beats on channel 16

(update-midifile-tempo-from-max)        ;-> from Max = WITHOUT A TEMPO META EVENT, but an implicit tempo equal to BPM=120,
                                        ;              thus when loaded in OM with default tempo BPM=60, durations are doubled       

(update-midifile-tempo-from-intuem)     ;-> from Intuem = with a tempo meta event, OM takes the correct durations        

;Notice that in Intuem, the correct tempo is written at the BEGINNING of the MIDI file
;Thus time should not be deleted at the beginning, otherwise the tempo will be deleted too
;(and Intuem default tempo BPM=120 will be inserted instead)
;===========================================================================================

;EXAMPLE IN MAX: Make a MIDI file in Max with simple data: 5 notes with pitch=12, dur=10, velo=100 played by a 'metro' with deltatime=700
(dump-straight (om-choose-file-dialog))
;---> ((4D 54 68 64 0 0 0 6 0 0 0 1 0 60) 4D 54 72 6B 0 0 0 30 0 9F C 64 1 9F C 0 81 5 9F C 64 2 9F C 0   etc.
;---> THERE IS NO META EVENT (0 FF 51 ...) !!!!!!!!!!!!!!!!!!!!

;Re-save it with Inutem (after deleting unused tracks):
;---> ((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) 4D 54 72 6B 0 0 0 19 0 FF 51 3 7 A1 20  etc.
;---> tempo meta event = (0 FF 51 3 7 A1 20) last three bytes 7 A1 20 give beat duration in microseconds
(write #x07A120 :base 10)     ;---> 500000 = 500 ms BPM=120

|#

(defmethod! update-midifile-tempo-from-max ()
  (let* ((midifromfile (evts-from-midifile)))
    (if (null midifromfile) (format *om-stream* "Empty MIDI file~%")  ;Marc 16/2/2012 nil when "midibuff.mid" is empty 
                                                                     ;+ TEST CANAL 16 ???????????????????
                                        ;22/4/2012   FROM MAX: no tempo meta event, but an implicit tempo BPM=120
                                        ;when slowed down by OM to BPM=60, durations must be divided by 2
      (let* ((midifromfile-halfdurations (timestretch midifromfile 0.5))
             (defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile-halfdurations))))))))
        (print defaultbeatdur)
        (format *om-stream* "~%Beat duration ~a ms, BPM = ~a~%" defaultbeatdur (round (/ 60000 defaultbeatdur))) 
        (let ((name (om-choose-new-file-dialog  :directory (def-save-directory) :prompt (om-str "Save as...") 
                                                :types (list (om-str "MIDI Files") "*.mid;*.midi"))))
          (when name 
            (format *om-stream* "Delete unused tracks and update tempo ~%Save MIDI file as \"~a.mid\"~%~%" (pathname-name name))
            (unless (stringp (pathname-type name))
              (setf name (make-pathname :device (pathname-device name) :directory (pathname-directory name)
                                        :name (pathname-name name) :type "midi")))
            (save-as-midi-with-tempo (mf-info->chord-seq midifromfile-halfdurations) defaultbeatdur name)))))))

(defmethod! update-midifile-tempo-from-intuem ()
  (let* ((midifromfile (evts-from-midifile)))
    (if (null midifromfile) (format *om-stream* "Empty MIDI file~%")  ;Marc 16/2/2012 nil when "midibuff.mid" is empty 
                                                                     ;+ TEST CANAL 16 ???????????????????
      (let ((defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile))))))))
        (format *om-stream* "~%Beat duration ~a ms, BPM = ~a~%" defaultbeatdur (round (/ 60000 defaultbeatdur))) 
        (let ((name (om-choose-new-file-dialog  :directory (def-save-directory) :prompt (om-str "Save as...") 
                                                :types (list (om-str "MIDI Files") "*.mid;*.midi"))))
          (when name 
            (format *om-stream* "Delete unused tracks and update tempo ~%Save MIDI file as \"~a.mid\"~%~%" (pathname-name name))
            (unless (stringp (pathname-type name))
              (setf name (make-pathname :device (pathname-device name) :directory (pathname-directory name)
                                        :name (pathname-name name) :type "midi")))
            (save-as-midi-with-tempo (mf-info->chord-seq midifromfile) defaultbeatdur name)))))))






;///////////// A FAIRE: FILTRAGE DES EVENEMENTS D'UN MIDIFILE /////////////

#|
-> le beat7 de l'impro "Jaime-new11.9.2011-9-21.5.txt" semble correspondre à l'état 243 de l'oracle

(start-region or)
(midiset (aref (vectext or) 242))
(mapcar 'midiset (list (aref (vectext or) 240) (aref (vectext or) 241) (aref (vectext or) 242) (aref (vectext or) 243)))
(loop for i from 1 to 250
      do (format *om-stream* "etat=~a midiset=~a~%" i (midiset (aref (vectext or) i))))
etat=179 midiset=((54 -42 58 111 2) (55 47 167 105 2) (56 224 63 113 2) (57 334 260 113 2))
etat=180 midiset=((-57 0 584 113 2) (53 -52 36 85 2) (54 36 141 113 2) (53 234 74 106 2) (50 338 94 103 2))
etat=181 midiset=((-57 0 588 113 2) (54 -74 110 123 2) (53 41 141 121 2) (50 343 245 127 2))

-> L'erreur est dans l'oracle lui-meme. A partir de l'etat 179, le prolongement -57 est present dans tous les etats jusqu'a la fin

(setf midifromfile (evts-from-midifile (MidiFixedBuff *current-tune*)))

(setf beatlist (make-beat-list (clocked-evts->beats midifromfile)))

(loop for i from 175 to 185
      do (format *om-stream* "etat=~a midiset=~a~%" i (midiset (nth i beatlist))))
etat=178 midiset=((54 -42 58 111 2) (55 47 167 105 2) (56 224 63 113 2) (57 334 260 113 2))
etat=179 midiset=((-57 0 584 113 2) (53 -52 36 85 2) (54 36 141 113 2) (53 234 74 106 2) (50 338 94 103 2))
etat=180 midiset=((-57 0 588 113 2) (54 -74 110 123 2) (53 41 141 121 2) (50 343 245 127 2))
etat=181 midiset=((-57 0 594 113 2) (-50 0 594 127 2))
etat=182 midiset=((-57 0 588 113 2) (-50 0 234 127 2))
etat=183 midiset=((-57 0 594 113 2))
etat=184 midiset=((-57 0 594 113 2))

(length midifromfile)

(loop for i from 1 to 1393
      do (format *om-stream* "etat=~a midiset=~a~%" i (nth i midifromfile)))  ; puis on cherche un repere: velocite 113 de (57 334 260 113 2)
etat=965 midiset=(12 106088 47 100 16)
etat=966 midiset=(55 47 167 105 2)
etat=967 midiset=(2 47 500 102 16)
etat=968 midiset=(56 224 63 113 2)
etat=969 midiset=(57 334 260 113 2)     -> on le trouve la, duree=260
etat=970 midiset=(53 -52 36 85 2)
etat=971 midiset=(12 106682 42 100 16)
etat=972 midiset=(54 36 141 113 2)
etat=973 midiset=(2 42 494 102 16)
etat=974 midiset=(53 234 74 106 2)
etat=975 midiset=(50 338 94 103 2)
etat=976 midiset=(54 -74 110 123 2)
etat=977 midiset=(12 107266 46 100 16)
etat=978 midiset=(53 41 141 121 2)
etat=979 midiset=(2 46 495 102 16)
etat=980 midiset=(50 343 245 127 2)
etat=981 midiset=(12 107854 42 100 16)

-> on detaille les etapes du calcul de beatlist:

(setf midifromfile (evts-from-midifile (MidiFixedBuff *current-tune*)))

-> on voit bien les pbs de "modifs physiques", car apres avoir recharge 'midifromfile', l'affichage devient:
etat=965 midiset=(12 106088 47 100 16)
etat=966 midiset=(55 106135 167 105 2)
etat=967 midiset=(2 106135 500 102 16)
etat=968 midiset=(56 106312 63 113 2)
etat=969 midiset=(57 106422 111296 113 2)  -> ici le 57 a une duree ANORMALE = 111296 ms

-> donc le BUG est present des l'ouverture du fichier dans OM avec 'evts-from-midifile'


;Jean Bresson, 10/11/2011 (+ mars 2006)
;========================
;Fonction 'get-midievents' definies dans "midifile.lisp" pour les MIDIfiles:
;   - prend en entrée un MIDIfile (obtenu en ouvrant un fichier avec 'load-midi-file' et 'om-choose-file-dialog'
;   - retourne la liste des evenements MIDI sous forme de crochets
;[MIDIEVENT date-ms type-evt / track X / port X / chan X / VALUE=X]
;Exemple: pour un note on/off:
;[MIDIEVENT 3016 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(66 113)]     -> couple de valeurs (pitch velo)


(loop for x in (get-midievents (load-midi-file (om-choose-file-dialog))) 
      do (format *om-stream* "~a~%" x))

(setq tmp (get-midievents (load-midi-file (om-choose-file-dialog))))
([MIDIEVENT 0 Tempo / track 0 / port 0 / chan 1 / VALUE=(60)] [MIDIEVENT 0 TimeSign / track 0 / port 0 / chan 1 / VALUE=(4 2 0 0)] [MIDIEVENT 0 Tempo / track 0 / port 0 / chan 1 / VALUE=(60)] [MIDIEVENT 0 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(60 100)] [MIDIEVENT 611 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(66 100)] [MIDIEVENT 998 KeyOff / track 1 / port 0 / chan 1 / VALUE=(60 64)] [MIDIEVENT 1250 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(66 100)] [MIDIEVENT 1609 KeyOff / track 1 / port 0 / chan 1 / VALUE=(66 64)] [MIDIEVENT 1694 KeyOn  / track 1 / port 0 / chan 1 / VALUE=(61 100)] [MIDIEVENT 2248 KeyOff / track 1 / port 0 / chan 1 / VALUE=(66 64)] [MIDIEVENT 2692 KeyOff / track 1 / port 0 / chan 1 / VALUE=(61 64)] [MIDIEVENT 2692 EndTrack / track 1 / port 0 / chan 1 / VALUE=nil])


(ev-date (nth 10 tmp))

;Dans le fichier problematique, on a autour de l'onset 106000 ms une note anormalement longue:

[MIDIEVENT 106135 KeyOff / track 2 / port 0 / chan 16 / VALUE=(12 0)]
[MIDIEVENT 106135 KeyOn  / track 2 / port 0 / chan 16 / VALUE=(2 102)]
[MIDIEVENT 106302 KeyOff / track 1 / port 0 / chan 2 / VALUE=(55 0)]
[MIDIEVENT 106312 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(56 113)]
[MIDIEVENT 106375 KeyOff / track 1 / port 0 / chan 2 / VALUE=(56 0)]
[MIDIEVENT 106422 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(57 113)]
[MIDIEVENT 106422 KeyOff / track 1 / port 0 / chan 2 / VALUE=(57 1)]     ===> PROBLEME: KeyOn et KeyOff SIMULTANES a la date = 106422 ms
[MIDIEVENT 106630 KeyOn  / track 1 / port 0 / chan 2 / VALUE=(53 85)]
[MIDIEVENT 106635 KeyOff / track 2 / port 0 / chan 16 / VALUE=(2 0)]
[MIDIEVENT 106666 KeyOff / track 1 / port 0 / chan 2 / VALUE=(53 0)]

;POUR PASSER D'UNE LISTE DE 'MidiEvents' A UNE LISTE DE 5UPLES (mail de Jean 14/11/2011):

(setf tmp-EventMidi-seq (objfromobjs tmp (make-instance 'EventMidi-seq)))

(get-midi-notes tmp-EventMidi-seq)
(((60 0 998 100 1) (66 611 998 100 1) (66 1250 998 100 1) (61 1694 998 100 1)))

-> avec 'objfromobjs' et 'get-midi-notes':
on passe d'une liste de 'MidiEvents' à une liste de 5uples
Donc il faut:
- filtrer la liste de 'MidiEvents' récupérée à partir du fichier MIDI
- après filtrage, la transformer en liste de 5uples pour créer une beatlist

|#





;==================
;dump of a midifile

#|
(dump-midifile (om-choose-file-dialog))

;The meta event to indicate a tempo change is: FF 51 03 tt tt tt 	
;The 3 data bytes of tt tt tt are the tempo in MICROseconds (not MILLIseconds) per quarter note. 
;If tt tt tt = 07 A1 20, then each quarter note should be 07 A1 20 = 500 000 microseconds long (= 500 ms).
;For example, a tempo of 120 BPM = 07 A1 20 microseconds per quarter note.

(write #xFF :stream *om-stream* :base 10)
(write '(#x00 #xFF #x51 #x03 #x07 #xA1 #x20) :stream *om-stream* :base 10)    -> (0 255 81 3 7 161 32)
(write #x07A120 :stream *om-stream* :base 10)                                 -> 500000 = 500 ms -> BPM=60000/500=120
(write '(#x00 #xFF #x51 #x03 #x09 #x27 #xC0) :stream *om-stream* :base 10)    -> (0 255 81 3 9 39 192)
(write #x0927C0 :stream *om-stream* :base 10)                                 -> 600000 = 600 ms -> BPM=60000/600=100

(dump-straight "/Users/marc/Documents/OpenMusic/MyWS/out-files/tempo100.mid")    ; File "tempo100.mid" created by sequencer INTUEM
((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) 
4D 54 72 6B 0 0 0 19 
0 FF 51 3 9 27 C0   ; FF 51 (tempo meta event) + nb bytes= (3) + 3 bytes 9 27 C0 (beatduration in microseconds=600000, BPM=100)
0 FF 58 4 4 2 8 8 
0 FF 59 2 0 0 
0 FF 2F 0 
4D 54 72 6B 0 0 0 4D 
0 FF 3 8 54 72 61 63 6B 3A 20 31 
0 FF 4 1D 51 75 69 63 6B 74 69 6D 65 20 4D 75 73 69 63 61 6C 20 49 6E 73 74 72 75 6D 65 6E 74 73 0 B0 0 0 0 B0 20 0 0 C0 0 0 B0 7 64 0 B0 A 
40 0 90 3C 40  ; note Do3 = 3C noteon (90)
9D A 80 3C 40  : note Do3 = 3C noteoff (80)
0 FF 2F 0 nil)

;There seems to be a bug in the multitrack recording of MIDI files by 'detonate' in Max,
;since the Track #O devoted to meta events is not taken into account in the number of tracks of the header.
;Here is a dump of a MIDI file created by 'detonate' in Max:
(dump-straight "/Users/marc/Documents/OpenMusic/MyWS/out-files/a.mid")  
((4D 54 68 64 0 0 0 6 0 1 0 1 0 60) ; format 1 (0 1), ERROR: nb track=1 (0 1) whereas the file contains two tracks
4D 54 72 6B 0 0 0 B ; = Track 1
0 FF 51 3 7 A1 20   ; FF 51 (tempo meta event) + nb bytes= (3) + 3 bytes 7 A1 20 (beatduration in microseconds=500000, BPM=120)
0 FF 2F 0 
4D 54 72 6B 0 0 0 14 ; = Track 2
0 99 43 78 0 99 38 78 13 99 38 0 0 99 43 0 
0 FF 2F 0 nil)

;It suffices to load the file into INTUEM and resave it, then the number of tracks will be updated to its correct value including Track #0:
(dump-straight "/Users/marc/Documents/OpenMusic/MyWS/out-files/aINTUEM.mid")  
((4D 54 68 64 0 0 0 6 0 1 0 2 3 E8) ; format 1 (0 1), nb track=2 (0 2)
4D 54 72 6B 0 0 0 19 
0 FF 51 3 7 A1 20 
0 FF 58 4 4 2 8 8 
0 FF 59 2 0 0 0 FF 2F 0 
4D 54 72 6B 0 0 0 4A 
0 FF 3 8 54 72 61 63 6B 3A 20 31 
0 FF 4 1D 51 75 69 63 6B 74 69 6D 65 20 4D 75 73 69 63 61 6C 20 49 6E 73 74 72 75 6D 65 6E 74 73 0 B9 7 40 0 B9 A 40 
0 99 43 78 0 99 38 78 81 45 89 38 0 0 89 43 0 
0 FF 2F 0 nil)
|#

(defun dump-midifile (file)
  (setq midifileaccepted " Midifile not accepted")
  (with-open-file (stream file :direction :input)
    (let* ((res1 (read-header-chunk stream)) (bool (car res1)))
      (when bool (let ((res2 (read-track-chunk stream)))
                   (write (cons (cadr res1) (cadr res2)) :stream *om-stream* :base 16) (format *om-stream* midifileaccepted))))))

(defun dump-straight (file)
  (with-open-file (stream file :direction :input)
    (let* ((res1 (read-header-chunk stream)) (bool (car res1)))
      (when bool (let ((res2 (read-inside stream)))
                   (write (cons (cadr res1) (cadr res2)) :stream *om-stream* :base 16) (format *om-stream* "end of file"))))))

(setq midifileaccepted " Midifile accepted")

(defun read-inside (stream)
  (let* ((byte (my-read-byte stream ())) (res (list byte)))
    (when (not byte) (setq midifileaccepted " Midifile accepted"))   ;;; FINAL STATE
    (loop until (not byte) do (progn (setq byte (my-read-byte stream ())) (push byte res)))
    (list byte (reverse res))))

(defun read-header-chunk (stream) (read-n-bytes 14 stream))

(defun read-track-chunk (stream)
  (let* ((res1 (read-init-track-chunk stream)) (bool (car res1)) (res-notes ()))
    (loop until (not bool) do (let ((res2* (read-event-track-chunk stream))) (setq bool (car res2*)) (when bool (push (cadr res2*) res-notes))))
    (list bool (cons (cadr res1) (reverse res-notes)))))

(defun read-init-track-chunk (stream) (read-n-bytes 8 stream))

(defun read-event-track-chunk (stream)
  (let* ((res1 (read-deltatime stream)) (bool (car res1)))
    (when bool (let ((res2 (read-event stream))) (setq bool (car res2)) (list bool (append (cadr res1) (cadr res2)))))))
  
(defun read-deltatime (stream)
  (let* ((byte (my-read-byte stream ())) (res (list byte)))
    (when (not byte) (setq midifileaccepted " Midifile accepted"))   ;;; FINAL STATE
    (loop until (or (not byte) (< byte #x80)) do (progn (setq byte (my-read-byte stream ())) (push byte res)))
    (list byte (reverse res))))

(defun read-event (stream)
  (let ((statut (my-read-byte stream ()))) (cond ((= statut #xFF) (read-meta-event stream)) 
                                                 ((or (= statut #x90) (= statut #x80)) (read-note statut stream)))))

(defun read-note (statut stream) (let ((res (read-n-bytes 2 stream))) (list (car res) (cons statut (cadr res)))))

(defun read-meta-event (stream)
  (let ((type (my-read-byte stream ())))
    (case type (#x58 (let ((res (read-n-bytes 5 stream))) (list (car res) (append (list #xFF #x58) (cadr res)))))  ;;; signature
               (#x51 (let ((res (read-n-bytes 4 stream))) (list (car res) (append (list #xFF #x51) (cadr res)))))  ;;; tempo
               (#x2F (let ((res (read-n-bytes 1 stream))) (list (car res) (append (list #xFF #x2F) (cadr res)))))))) ;;; end-of-track

;;; Read n first bytes in the stream:

(defun read-n-bytes (n stream)
  (let* ((byte (my-read-byte stream ())) (res (list byte)))
    (do ((i n (- i 1))) ((or (= i 1) (not byte)) (list byte (reverse res))) (setq byte (my-read-byte stream ())) (push byte res))))

;;; Ascii: (char-code #\a) = 97, (code-char 97) = #\a

(defun my-read-byte (stream bool) (let ((x (read-char stream bool))) (if (not x) x (char-code x))))















