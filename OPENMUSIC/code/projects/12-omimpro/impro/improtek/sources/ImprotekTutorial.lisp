(in-package :om)
 
;ImprotekTutorial.lisp
;--------------------------------------------------------------------
;TUTORIAL FOR THE LISP CLASSES 'ORACLE' AND 'IMPROVIZER' WITH 'BEATS'
;Marc Chemillier (after Gerard Assayag)
;May 12th 2009, transfered to OM.6 on March 10th 2010, modified March 1rst 2012
;--------------------------------------------------------------------
;Examples for a quick understanding of the Lisp code of the 'Improvizer' function in mode 'Beats'
;In the Open Music environment, first evaluate the whole file (Ctrl-Y)
;Then select each expression and evaluate it (Return)
;Look at the result in the Listener window and listen to the sound examples
;Maj-Alt-Dot opens the definition of a function when its name is selected

;For loading the whole library, load 'Improtek.lisp'
#| 
(let ((file-lib-improtek (make-pathname :directory (reverse (cdr (reverse (pathname-directory *load-pathname*)))) :name "Improtek")))
  (if (om-standalone-p) (load file-lib-improtek) (compile&load file-lib-improtek)))
|#
;"ImprotekData.lisp" contains musical data for testing the functions

;------------
;EXAMPLE # 01
;------------
;Basic oracle on letters a, b, c

#|
(setf o1 (oracle-online '(a b a c)))        ; = (make-instance 'oracle :otext '(a b a c) :lrsMode t)

(om-inspect o1)
(transition o1 2 'a)
(suppleance o1 2)
(veclrs o1)
(loop for i from 0 to 10 collect (suppleance o1 i))

(maxetat o1)

;The following expressions display the structure of the oracle such as transitions, suffix links:
(loop for x being the hash-key of (hashtransition o1) using (hash-value q) do (format *om-stream* "~a ---> ~a~%" x q))
(loop for x being the hash-key  of (hashsuppl o1) using (hash-value q) do (format *om-stream* "~a ===> ~a~%" x q))
(loop for x being the hash-key  of (hashsuppl-> o1) using (hash-value q) do (format *om-stream* "~a ===> ~a~%" x q))

(loop for etat from 1 to (maxetat o1) do  (format *om-stream* "~a ===> ~a~%" etat (suppleance o1 etat))))



;Revaluate the previous expressions after evaluating the Pythie o1, in order to display its structure:
(setf o1 (make-instance 'Pythie :otext '(a b a c) :comparateur 'equal :lrsMode t))

|#

;------------
;EXAMPLE # 02
;------------
;Musical oracles with events of the class 'Beat' including labels and midi data, using CompareEvent defined for beats

#|
(setf beat-list2 '( 
((c) ((60 0 500 80 1))) 
((d) ((62 0 500 80 1))) 
((e) ((64 0 500 80 1))) 
((f) ((65 0 500 80 1))) 
((e) ((64 0 500 80 1))) 
((d) ((62 0 500 80 1))) 
((c) ((60 0 500 80 1))) 
))

(setf beats2 (make-beat-list beat-list2)) 
(setf myoracle2 (NewImprovizer))                    
(loop for i from 0 to (1- (length beats2)) do (learn-event myoracle2 (nth i beats2)))
(om-inspect myoracle2)
(loop for x being the hash-key of (hashtransition myoracle2) using (hash-value q) 
      do (format *om-stream* "~a ---> ~a~%" x (reverse q)))
(loop for x being the hash-key of (hashsuppl myoracle2) using (hash-value q) 
      do (format *om-stream* "~a ===> ~a~%" x q))

(loop for i from 1 to (maxetat myoracle2) collect (harmlabel (otext myoracle2 i)))

(setf labels2 '((e) (c) (f) (e) (d) (c)))
(setf newbeats2 (ImprovizeOnHarmGrid myoracle2 (length labels2) labels2))
(mapcar 'HarmLabel newbeats2)

(play (beats->chseq newbeats2 500 0))   ;;; val 500 = beat duration in ms

(my-save-as-midi (beats->chseq newbeats2 500 0) 500) 

(setf labels2 '((c) (c) (c) (d) (e) (d) (c)))
(setf newbeats2 (ImprovizeOnHarmGrid myoracle2 (length labels2) labels2))
(mapcar 'HarmLabel newbeats2)
(play (beats->chseq newbeats2 500 0))



|#

;------------
;EXAMPLE # 03
;------------
;Different midi data sets for the same label

#|
(setf beat-list3 '(
((c) ((60 0 500 80 1)))
((d) ((62 0 500 80 1)))
((e) ((64 0 500 80 1)))
((c) ((72 0 500 80 1))) ; same labels but with different midi data (notes one 8ve higher)
((d) ((74 0 500 80 1)))
((e) ((76 0 500 80 1)))
))

(setf myoracle3 (NewImprovizer (make-beat-list beat-list3)))
(om-inspect myoracle3)

(setf labels3 '((c) (d) (e) (d) (e) (c) (d) (e) (d) (e) (c) ))

(setf newbeats3 (ImprovizeOnHarmGrid myoracle3 (length labels3) labels3))
(mapcar 'HarmLabel newbeats3)
(play (beats->chseq newbeats3 500 0))   ;;; val 500 = beat duration in ms


(play (beats->chseq (ImprovizeOnHarmGrid myoracle3 (length labels3) labels3) 500 0))

|#

;------------
;EXAMPLE # 04
;------------
;Pattern matching of the labels is done up to a transposition of at most a minor third,
;thus labels can match beats in the oracle even if they do not appear in the original sequence thanks to transposition

#|
(setf beat-list4 '(
((c) ((60 0 500 80 1)))
((c#) ((61 0 500 80 1)))
((f) ((65 0 500 80 1)))
((f#) ((66 0 500 80 1)))
))

(setf myoracle4 (NewImprovizer (make-beat-list beat-list4) 500))

(MidiSet (otext myoracle4 2))
(MidiSet (TransposeClonedBeat (otext myoracle4 2) -35))


(setf labels4 '((c) (c#) (c) (g) (g#) (c#) (c) (c) (g#) (g) ))    ; should transpose g and g# on f or f#
(setf impro4 (ImprovizeOnHarmGrid myoracle4 (length labels4) labels4))
(mapcar 'HarmLabel impro4)
(mapcar 'MidiSet impro4)
(play (beats->chseq impro4 500 0))

(setf beat-list4 '(
((c) ((60 0 500 80 1)))
((c#) ((61 0 500 80 1)))
((d) ((62 0 500 80 1)))
((eb) ((63 0 500 80 1)))
))

(setf labels4 '((c) (c#) (d) (eb) (e) (f) (f#) (a) (bb) (b) (c)))

;Reavalute the construction of myoracle4 above with the new values for beat-list4 and labels4, 
;then play an improvisation generated by this new oracle:
(play (beats->chseq (ImprovizeOnHarmGrid myoracle4 (length labels4) labels4) 500 0))

|#

;------------
;EXAMPLE # 05
;------------
;A more complicated example using transposition with real "chord" labels Bm7 and E7 and full midi data
;Taken from a house tune entitled "Rain" (DJ Kerri Chandler, 1999)

(setf Fromrain_beatdur 484
      Fromrain_beatsfromfile '(         ;beat duration = 484 ms = 60000/BPM (BPM = 124)               
((b m7) ((11 0 484 2 14) (35 0 242 97 2) (66 0 484 69 1) (62 0 484 69 1) (61 0 484 69 1) (57 0 484 69 1) (47 242 242 97 2))) 
((e 7)  ((4 0 484 3 14) (34 0 242 97 2) (73 0 484 69 1) (62 0 484 69 1) (61 0 484 69 1) (56 0 484 69 1) (46 242 242 97 2)))
))

(setf oracle5 (NewImprovizer (make-beat-list Fromrain_beatsfromfile) Fromrain_beatdur))

(setf labels5                 ;too many chords so that transposition is required
'((b m7) (e 7) (bb m7) (eb 7) (a m7) (d 7) (e 7) (e 7) (f# 7) (f# 7) (c# m7) (c# m7) (g# m7) (g# m7) (b m7) (a m7)))

#|
(progn (setf impro5 nil)
(pgmout 4 1) (pgmout 33 2)
(ctrlchg 7 127 1) (ctrlchg 7 127 2)
(let* ((n (length labels5)) 
       (drumpart (make-list (round (/ n 2)) :initial-element 2beatpoumchi)))
  (setf impro5 (merger (beats->chseq (ImprovizeOnHarmGrid oracle5 n labels5) Fromrain_beatdur 0)
                       (beats->chseq (make-beat-list drumpart) Fromrain_beatdur 0)))
  (play impro5))
)


(my-save-as-midi impro5 Fromrain_beatdur)

|#

(setf 2beatpoumchi 
 '((nolabel nolabel 2) ((36 0 207 121 10) (42 242 207 121 10) (36 484 207 121 10) (40 484 207 121 10) (42 726 207 121 10))))



;------------
;EXAMPLE # 06
;------------
;Continuation of EXAMPLE # 05 with an oracle on real midi data, BPM = 124 (beat duration = 60000/BPM = 484 ms)
;Taken from a harmonic loop by DJ Kerri Chandler in his house tune "Rain", 1999
;(itself borrowed from the chord changes of "Round Midnight" bars 4, 5, 6, 7)
;The number of beats in each chord is indicated by a number as the third element
;The full beat list is in ImprotekData.lisp
;Included are variations of the drum pattern, and an audio sample of the original piano loop 'rain-piano.aiff'

;AUDIO EXAMPLE WITH A BETTER SOUND (not QuickTime): ehess.modelisationsavoirs.fr/atiam/improtek
;- original piano sample alternates with algorithmic substitutions (December 15th 2001)

(setf oracle6 (NewImprovizer (make-beat-list Rain_beatsfromfile) Rain_beatdur))

(setf labels6 '((b m7 1) (e 7 1) (bb m7 1) (eb 7 1) (g# m7 2) (c# 7 2) (f# maj7 2) (g# 7 2) (b 7 2) (bb 7 2)))

#|
(progn (ctrlchg 7 100 10)
(pgmout 4 1)      ; channel 1 = "4 - Electric Piano 1", for a complete list evaluate the variable *midi-programs*
(pgmout 33 2)     ; channel 2 = "33 - Electric Fingered Bass"
(let* ((nbgrids (/ (loop for x in labels6 sum (third x)) 16))
       (drumpart (make-drumpart nbgrids)))
  (play (merger (beats->chseq (ImprovizeOnHarmGrid oracle6 (length labels6) labels6) Rain_beatdur 0)
                (beats->chseq (make-beat-list drumpart) Rain_beatdur 0))))
)

(mapcar 'Harmlabel (ImprovizeOnHarmGrid oracle6 (length labels6) labels6))

;Revaluate the previous expression after evaluating the following label list:

(setf labels6 
'((c 7 1) (f 7 1) (bb m7 1) (eb 7 1) (g# maj7 1) (g# m7 1) (g maj7 1) (g maj7 1) (f# maj7 1) (eb m7 1) (c 7 2) (f m7 2) (bb 7 2)
(f 7 2) (bb 7 2) (eb 7 1) (g# 7 1) (c# 7 2) (f# maj7 2) (g# maj7 1) (c 7 1) (f maj7 2) (bb 7 2))
)

(setf rhodes (get-sound))         ; choose the file name: 'rain-piano.aiff'

;WARNING: You need to load the soundfile 'rain-piano.aiff' before playing this example
;The original piano sample is followed by variations on different chord changes:
(progn (setf impro nil) (ctrlchg 7 100 10)
(pgmout 4 1)      ; channel 1 = "4 - Electric Piano 1", for a complete list evaluate the variable *midi-programs*
(pgmout 33 2)     ; channel 2 = "33 - Electric Fingered Bass"
(let* ((labels (append (list '(no label 16)) labels6))
       (nbgrids (/ (loop for x in labels sum (third x)) 16))
       (drumpart (make-drumpart nbgrids)))
  (setf impro (merger (beats->chseq (ImprovizeOnHarmGrid oracle6 (length labels) labels) Rain_beatdur 0)
                      (beats->chseq (make-beat-list drumpart) Rain_beatdur 0)))

(Stop-Player *general-player*)
  (play (list rhodes impro))     ;;;;; does not work well since audio and MIDI are not synchronized
))

(Stop-Player *general-player*)    ; if needed... -> stops both midi and audio

(my-save-as-midi impro Rain_beatdur)  ; visualization of the voicings in a piano-roll

|#

(defun make-drumpart (nbgrids)
  (loop for i from 1 to nbgrids with basic4beat = (list 2beatpoumchi 2beatpoumchi)
        append (append (if (= (random 2) 0) (list 4beatvar1) basic4beat) basic4beat
                       (if (= (random 2) 0) (list 8beatvar2) (append basic4beat basic4beat)))))
(setf 4beatvar1 
 '((nolabel nolabel 4) ((36 0 242 121 10) (42 242 241 121 10) (40 484 242 121 10) (36 484 242 121 10) (42 726 242 121 10) (36 968 242 121 10) (42 1210 242 121 10) (40 1331 121 121 10) (40 1452 242 121 10) (36 1452 242 121 10) (42 1694 242 121 10) (40 1815 121 121 10))))
(setf 8beatvar2 
 '((nolabel nolabel 8) ((36 0 242 121 10) (42 242 241 121 10) (40 484 242 121 10) (36 484 242 121 10) (42 726 242 121 10) (36 968 242 121 10) (40 1129 242 121 10) (42 1210 242 121 10) (40 1452 242 121 10) (36 1452 242 121 10) (42 1694 242 121 10) (40 1775 242 121 10) (36 1936 242 121 10) (40 2097 242 121 10) (42 2178 242 121 10) (36 2420 242 121 10) (40 2581 242 121 10) (42 2662 242 121 10) (36 2904 242 121 10) (40 3065 242 121 10) (42 3146 242 121 10) (40 3388 242 121 10) (36 3388 242 121 10) (42 3630 242 121 10))))

;------------
;EXAMPLE # 07
;------------
;BPM = 145 dotted quarter note (beat duration = 414 ms)
;A boogie-woogie style example played on the organ, the full beat list is in ImprotekData.lisp
;Taken from a piano solo by K.Jarrett in "Sun Bear Concerts", "Sapporo" Part II at 5'30, Nov 18th 1976
;Music notation in M. Chemillier, "Grammaires, automates et musique", in J.P. Briot & F. Pachet 2004
;Voicings available for all possible chord labels by means of transposition
;Labels contain a 4th element indicating the position in the bar (1st or 3rd beat)

;AUDIO EXAMPLE WITH A BETTER SOUND (not QuickTime): ehess.modelisationsavoirs.fr/atiam/improtek
;- solo added manually by Marc Chemillier, April 5th 2000

(setf oracle7 (NewImprovizer (make-beat-list Sapporo_beatsfromfile) Sapporo_beatdur))

(setf labels7 '((e 7 8 1) (e 7 8 1) (a 7 8 1) (e 7 8 1) (b 7 8 1) (e 7 8 1)))

#|
;Plays the basic blues chord changes:
(progn (setf impro nil)
(pgmout 17 1)      ; channel 1 = "17 - Percussive Organ" 
(pgmout 52 2)      ; channel 2 = "52 - Choir Aahs", channel 1 duplicated for a more sustained sound
(pgmout 78 3)      ; channel 3 = "78 - Whistle", channel 1 duplicated for the bass part
(ctrlchg 7 110 1) (ctrlchg 7 92 2) (ctrlchg 7 127 3)
(let* ((impro1 (beats->chseq (ImprovizeOnHarmGrid oracle7 (length labels7) labels7) Sapporo_beatdur 0))
       (impro2 (clone impro1)) (impro3 (clone impro1)))
  (setf (Lchan impro2) (change-element 2 1 (Lchan impro2)) (Lchan impro3) (change-element 3 1 (Lchan impro3))
        (Lmidic impro3) (mapcar #'(lambda (l) (remove-if #'(lambda (x) (> x 6000)) l)) (Lmidic impro3))    ; bass part
        impro (merger (merger impro1 impro2) impro3))
  (play impro)
))

(mapcar 'HarmLabel (ImprovizeOnHarmGrid oracle7 (length labels7) labels7)

(Stop-Player *general-player*)

;Revaluate the previous progn expression after evaluating the following label list:
(setf labels7 '(
(e 7 8 1) (e 7 4 1) (b 7 2 1) (bb 7 2 3) (a 7 8 1) (e 7 4 1) (e 7 2 1) (c# 7 2 3) (f# 7 4 1) (b 7 4 1) (e 7 8 1)
(e 7 8 1) (c 7 4 1) (b 7 2 1) (bb 7 2 3) (a 7 8 1) (e 7 4 1) (e 7 2 1) (g 7 2 3) (f# 7 4 1) (f 7 4 1) (e 7 8 1)
(a 7 4 1) (g# 7 2 1) (g 7 2 3) (c 7 4 1) (b 7 2 1) (bb 7 2 3) (a 7 8 1) (e 7 4 1) (e 7 2 1) (g 7 2 3) (f# 7 4 1) (f 7 4 1) (e 7 8 1)
(eb 7 4 1) (d 7 2 1) (c# 7 2 3) (c 7 4 1) (b 7 2 1) (bb 7 2 3) (a 7 8 1) (e 7 4 1) (e 7 2 1) (g 7 2 3) (f# 7 4 1) (f 7 4 1) (e 7 8 1)
))

(Stop-Player *general-player*)

(my-save-as-midi impro Sapporo_beatdur)

|#

(defun change-element (new old list) (mapcar #'(lambda (x) (substitute new old x)) list))

;------------
;EXAMPLE # 08
;------------
;Bernard Lubat's tune "D'ici d'en bas" BPM = 188 (beat duration = 60000/BPM = 319 ms), 128 beats (32 bars)
;An example involving a real live solo performed by Bernard Lubat at Ircam, May 8th 2004
;When dealing with live midi data it is necessary to take into account the prolongation of notes accross the beats:
;This is done by 'thread-Beats' using negative MIDI codes (in older version, it was done by 'beats-check-sustain' 
;which connected notes from the end of one beat to the beginning of the next as soon as they are on the same pitch)
;Bernard's solo full beat list is in ImprotekData.lisp
;Also included are an audio sample of percussion '188.64dicirhytmic.aiff', and a midi bass line

;AUDIO EXAMPLE WITH A BETTER SOUND (not QuickTime): ehess.modelisationsavoirs.fr/atiam/improtek
;- three choruses flute solo played by Bernard Lubat (May 8th 2004), five choruses trumpet phrases generated by the computer

(setf beats8 (make-beat-list Dicidenbas_beatsfromfile))

#|
(setf percu (get-sound))         ; choose the file name: '188.64dicirhytmic.aiff'

;WARNING: You need to load the soundfile '188.64dicirhytmic.aiff' before playing this example
;Plays part of Bernard Lubat's original solo:
(progn
(pgmout 75 11)      ; channel 11 = "75 - Pan Flute"
(Stop-Player *general-player*)
(play (list percu                      ;;;;; does not work well since audio and MIDI are not synchronized
            (beats->chseq (thread-Beats (subseq (nthcdr (random (- (length beats8) 65)) beats8) 0 64)) 
                          Dicidenbas_beatdur 30)))
)

(Stop-Player *general-player*)

; visualization of the live solo in a piano-roll
(my-save-as-midi (beats->chseq (thread-Beats beats8) Dicidenbas_beatdur 0) Dicidenbas_beatdur)  
|#

(setf oracle8 (NewImprovizer beats8 Dicidenbas_beatdur))

(setf labels8aa
'((f m7) (f m7) (f m7) (f m7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7)
  (f m7) (f m7) (f m7) (f m7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7)
  (f m7) (f m7) (f m7) (f m7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7)
  (f m7) (f m7) (f m7) (f m7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c 7) (c 7) (c 7) (c 7)))
(setf labels8ab
'((f m7) (f m7) (f m7) (f m7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7) (c m7)
  (f m7) (f m7) (f m7) (f m7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c 7) (c 7) (c 7) (c 7)
  (f m7) (f m7) (f m7) (f m7) (bb 7) (bb 7) (bb 7) (bb 7) (eb) (eb) (eb) (eb) (g#) (g#) (g#) (g#)
  (d 7) (d 7) (d 7) (d 7) (g 7) (g 7) (g 7) (g 7) (c m7) (c m7) (c m7) (c m7) (c 7) (c 7) (c 7) (c 7)))

#|
(loop for i from 1 to (maxetat oracle8) collect (harmlabel (otext oracle8 i)))

;WARNING: You need to load the soundfile '188.64dicirhytmic.aiff' before playing this example
(progn
(pgmout 75 11)      ; channel 11 = "75 - Pan Flute"
(pgmout 78 3)       ; channel 3 = "78 - Whistle", a kind of electronic bass...
(let* ((newbeats (ImprovizeOnHarmGrid oracle8 (length labels8aa) labels8aa))
       (impro (beats->chseq (thread-Beats newbeats) Dicidenbas_beatdur 0))
       (mix (merger impro (beats->chseq (make-beat-list (list bassline-aa)) Dicidenbas_beatdur 0))))
  (Stop-Player *general-player*)
  (play (list percu mix))                ;;;;; does not work well since audio and MIDI are not synchronized
))
;WARNING: You need to load the soundfile '188.64dicirhytmic.aiff' before playing this example
(progn
(pgmout 75 11)      ; channel 11 = "75 - Pan Flute"
(pgmout 78 3)       ; channel 3 = "78 - Whistle", a kind of electronic bass...
(let* ((newbeats (ImprovizeOnHarmGrid oracle8 (length labels8ab) labels8ab))
       (impro (beats->chseq (thread-Beats newbeats) Dicidenbas_beatdur 0))
       (mix (merger impro (beats->chseq (make-beat-list (list bassline-ab)) Dicidenbas_beatdur 0))))
  (Stop-Player *general-player*)
  (play (list percu mix))                ;;;;; does not work well since audio and MIDI are not synchronized
))   

|#

(setf bassline-aa 
 '((nolabel nolabel 64) 
   ((0 0 2 1 3) (29 447 204 117 3) (36 616 360 93 3) (41 942 97 118 3) (31 1751 184 120 3) (38 1910 305 101 3) (43 2192 98 110 3) (36 2984 234 111 3) (31 3162 346 108 3) (24 3484 120 127 3) (36 4273 231 117 3) (31 4428 336 110 3) (24 4760 96 127 3) (29 5576 204 117 3) (36 5744 306 105 3) (41 6051 90 111 3) (31 6836 178 117 3) (38 6977 321 100 3) (43 7298 104 108 3) (36 8114 202 120 3) (31 8262 354 100 3) (24 8598 99 127 3) (36 9396 194 117 3) (31 9534 332 111 3) (24 9846 122 127 3) (29 10664 204 117 3) (36 10814 332 101 3) (41 11125 96 114 3) (31 11928 188 113 3) (38 12070 322 95 3) (43 12400 96 110 3) (36 13205 198 117 3) (31 13362 342 108 3) (24 13682 122 127 3) (36 14452 212 114 3) (31 14610 329 113 3) (24 14930 114 127 3) (29 15755 210 106 3) (36 15901 288 97 3) (41 16219 92 118 3) (31 17026 179 113 3) (38 17172 309 106 3) (43 17484 90 108 3) (36 18302 197 114 3) (31 18455 322 110 3) (24 18758 94 127 3) (24 19569 178 127 3) (26 19712 382 100 3) (28 20054 322 118 3))
))
(setf bassline-ab 
 '((nolabel nolabel 64) 
   ((0 0 2 1 3) (29 447 204 117 3) (36 616 332 101 3) (41 927 96 114 3) (31 1730 188 113 3) (38 1872 323 95 3) (43 2202 96 110 3) (36 3008 198 117 3) (31 3164 343 108 3) (24 3484 122 127 3) (36 4254 212 114 3) (31 4412 329 113 3) (24 4732 114 127 3) (29 5558 209 106 3) (36 5703 288 97 3) (41 6021 92 118 3) (31 6828 180 113 3) (38 6974 310 106 3) (43 7286 90 108 3) (36 8104 197 114 3) (31 8258 322 110 3) (24 8560 94 127 3) (24 9371 178 127 3) (26 9514 382 100 3) (28 9856 322 118 3) (29 10176 373 97 3) (29 10620 198 111 3) (32 10786 386 120 3) (32 11236 184 120 3) (34 11405 436 98 3) (34 11906 160 124 3) (38 12069 400 113 3) (38 12556 171 106 3) (39 12722 396 117 3) (39 13205 160 120 3) (43 13334 377 100 3) (43 13810 210 103 3) (44 13966 398 123 3) (44 14432 174 108 3) (48 14588 400 108 3) (48 15055 178 118 3) (50 15252 398 120 3) (50 15722 192 114 3) (42 15877 402 113 3) (42 16358 192 114 3) (43 16508 388 91 3) (43 16944 214 101 3) (47 17132 392 103 3) (47 17586 186 113 3) (48 17779 354 95 3) (48 18230 226 103 3) (43 18416 370 89 3) (43 18879 167 118 3) (36 19042 252 110 3) (36 19342 356 111 3) (38 19660 340 101 3) (40 19962 321 110 3))
))



;------------
;EXAMPLE # 09 
;------------
;"Ziste zeste", BPM = 112, beat duration = 60000/BPM = 536 ms, Bernard's solo recorded in Uzeste, April 4th 2003
;Funky rhythm with slap bass

;AUDIO EXAMPLE WITH A BETTER SOUND (not QuickTime): ehess.modelisationsavoirs.fr/atiam/improtek
;- original solo as played by Bernard Lubat on April 4th 2003
;- older example with substitutions on a Bb7 vamp and a solo generated by a former algorithm (whithout oracle), October 30th 2002

(setf beats9 (make-beat-list Zistesolo_beatsfromfile))

(setf oracle9 (NewImprovizer beats9 Zistesolo_beatdur))

(setf (max-continuity oracle9) 5)

(setf labels9
'((bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7)
  (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (eb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7) (bb 7)
  (f 7) (f 7) (f 7) (f 7) (eb 7) (eb 7) (eb 7) (eb 7) (bb 7) (bb 7) (bb 7) (bb 7) (f 7) (f 7) (f 7) (f 7)))

#|
;Plays the beginning of Bernard Lubat's solo:
(progn  (setf impro nil)                  
(pgmout 36 4)       ; "36 - Slap Bass 1"
(pgmout 4 11)       ; channel 11 = "4" (solo by Bernard Lubat)
(ctrlchg 7 90 4) (ctrlchg 7 127 11) (ctrlchg 7 80 10)
(setf impro (merger (beats->chseq (subseq beats9 0 48) Zistesolo_beatdur 0) 
                    (beats->chseq (make-beat-list (list Zistefunk)) Zistesolo_beatdur 0)))
(play impro)
)

(Stop-Player *general-player*)
(my-save-as-midi impro Zistesolo_beatdur)
(om-inspect beats9)

;Plays solo on the blues chord changes:
(progn  (setf impro nil)               
(pgmout 36 4)     ; "36 - Slap Bass 1", fixed bass pattern
(pgmout 4 11)     ; channel 11 = "4" (solo by Bernard Lubat)
(ctrlchg 7 80 4) (ctrlchg 7 127 11) (ctrlchg 7 80 10) 
(setf impro (merger (beats->chseq (ImprovizeOnHarmGrid oracle9 (length labels9) labels9) Zistesolo_beatdur 0)
                    (beats->chseq (make-beat-list (list Zistefunk)) Zistesolo_beatdur 0)))
(play impro)
)


|#


;------------
;EXAMPLE # 10
;------------
;Complex chord changes derived from Mingus "Goodbye Porkpie Hat", BPM = 77, beat duration = 60000/BPM = 779 ms
;Bernard's solo recorded on October 1st 2004 with various chord changes on five choruses
;Slow blues rock (cf. "Rooftops" by Jerome Sabbagh, "Pogo", 2006)

;AUDIO EXAMPLE WITH A BETTER SOUND (not QuickTime): ehess.modelisationsavoirs.fr/atiam/improtek
;- flute solo played by Bernard Lubat (October 1st 2004), drums by Lubat and guitare by Fabrice Vieira added in rerecording, 
;mixed by "JPax" Louis at Uzeste (December 7th 2007)

(setf beats10 (make-beat-list Goodbye_beatsfromfile))

(setf oracle10 (NewImprovizer beats10 Goodbye_beatdur))

(setf (max-continuity oracle10) 100)

(setf labels10        ;chord changes of Bernard's first chorus
'((g# 7) (g# 7) (g 7) (g 7) (f# 7) (f 7) (e 7) (e 7) (e 7) (eb 7) (bb 7) (bb 7) (c 7) (f# 7) (f# 7) (f 7)
  (b maj7) (bb m7) (a 7) (a 7) (g# maj7) (d 7) (c# 7) (eb 7) (g# 7) (g# 7) (g# 7) (g# 7) (g 7) (g 7) (f# 7) (f 7)
  (b 7) (eb 7) (d 7) (d 7) (c# 7) (g 7) (f# 7) (f# 7) (d 7) (d 7) (g maj7) (c# 7) (c 7) (c 7) (b 7) (b 7)))

#|
(inspect oracle10)

;Plays on the chord changes of Bernard Lubat's first chorus:
(progn (setf impro nil)
(pgmout 4 1)       ; channel 1 = piano voicings "4 - Electric Piano 1"
(pgmout 78 2)      ; channel 2 = bass "78 - Whistle"      
(pgmout 75 11)     ; channel 11 = flute "75" (solo by Bernard Lubat)
(ctrlchg 7 110 1) (ctrlchg 7 127 2) (ctrlchg 7 127 11) (ctrlchg 7 100 10)
(let* ((drumpart (loop for i from 1 to (floor (length labels10) 8) collect Roof_drums)))     ; roof duration = 8 beats (2 bars)
  (setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid oracle10 (length labels10) labels10)) Goodbye_beatdur 0)
                      (beats->chseq (make-beat-list drumpart) Goodbye_beatdur 0)))
  (print labels10) 
  (play impro))
)

(Stop-Player *general-player*)

(my-save-as-midi impro Goodbye_beatdur)
;(str2ctrlNum "Effects3Depth")   ; # 93 = chorus effect (does not work in Open Music)

;Revaluate the previous progn expression after evaluating the following label lists:
;- the first one corresponds to Bernard's second chorus, 
;- the second one is not is the oracle, and gives much more discontinuity

(setf  labels10        ;chord changes of Bernard's second chorus
'((eb 7) (eb 7) (d 7) (g# 7) (g 7) (f# 7) (f 7) (f 7) (e 7) (eb 7) (g 7) (g 7) (f# 7) (f# 7) (f# 7) (f 7) 
  (b maj7) (a 7) (c 7) (c 7) (b 7) (bb 7) (a 7) (a 7) (g# maj7) (g# maj7) (d 7) (d 7) (d 7) (c# 7) (b 7) (b 7) 
  (d 7) (d 7) (d 7) (c# 7) (f 7) (f 7) (e 7) (g 7) (f# 7) (d 7) (eb 7) (d 7) (c# 7) (c 7) (b 7) (b 7)))

(setf  labels10        ;chord changes which are not in the oracle
'((g# 7) (g# 7) (g 7) (g 7) (c 7) (f 7) (e 7) (e 7) (e 7) (eb 7) (bb 7) (bb 7) (c# m7) (f# 7) (f# 7) (f 7) 
  (b maj7) (bb m7) (a 7) (a 7) (g# maj7) (d 7) (c# 7) (a maj7) (g# maj7) (g# maj7) (g# 7) (g# 7) (g 7) (g 7) (f# 7) (f 7) 
  (b 7) (eb 7) (d 7) (d 7) (d m7) (g 7) (f# 7) (f# 7) (d 7) (d 7) (g maj7) (c# 7) (c 7) (c 7) (b 7) (b 7)))
|#


;------------
;EXAMPLE # 11 
;------------
;Bill Evans original solo on "Israel", February 2nd 1961 (midifile transcription from www.billevans.nl)
;Walking bass in medium up tempo, BPM = 182, beat duration = 60000/BPM = 328 ms

(setf beats11 (make-beat-list Billevans_beatsfromfile))

(setf oracle11 (NewImprovizer beats11 Billevans_beatdur))    

(setf labels11
'((d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d m7) (d 7) (d 7) (d 7) (d 7) 
  (g m7) (g m7) (g m7) (g m7) (c 7) (c 7) (c 7) (c 7) (f maj7) (f maj7) (f maj7) (f maj7) (bb maj7) (bb maj7) (bb maj7) (bb maj7)
  (e m7) (e m7) (e m7) (e m7) (a 7) (a 7) (a 7) (a 7) (d m7) (d m7) (d m7) (d m7) (a 7) (a 7) (a 7) (a 7)))


#|
;Plays Bill Evans original five choruses on "Israel":
(progn  (setf impro nil)                 
(pgmout 4 1) (pgmout 4 2)    ; channel 1 = Bill Evans' right hand, channel 2 = Bill Evans' left hand
(pgmout 32 4)                ; "32 - Acoustic Bass"
(ctrlchg 7 127 1) (ctrlchg 7 127 2) (ctrlchg 7 127 3) (ctrlchg 7 0 10)
(setf impro (merger (beats->chseq (thread-Beats beats11) Billevans_beatdur 0)
                    (beats->chseq (make-beat-list (list Israelwalking Israelwalking Israelwalking Israelwalking Israelwalking)) 
                                  Billevans_beatdur 0))  )
(play impro)
)

(Stop-Player *general-player*)

;Plays stepwise transformations of Bill Evans solo with less and less continuity along four choruses:
(progn  (setf impro nil)                    
(pgmout 4 1) (pgmout 4 2)    ; channel 1 = Bill Evans' right hand, channel 2 = Bill Evans' left hand
(pgmout 32 4)                ; "32 - Acoustic Bass"
(ctrlchg 7 127 1) (ctrlchg 7 127 2) (ctrlchg 7 127 4) (ctrlchg 7 0 10)
(let* ((beats11a (progn (setf (max-continuity oracle11) 100) (ImprovizeOnHarmGrid oracle11 (length labels11) labels11)))
       (beats11b (progn (setf (max-continuity oracle11) 10) (ImprovizeOnHarmGrid oracle11 (length labels11) labels11)))
       (beats11c (progn (setf (max-continuity oracle11) 5) (ImprovizeOnHarmGrid oracle11 (length labels11) labels11)))
       (beats11d (progn (setf (max-continuity oracle11) 1) (ImprovizeOnHarmGrid oracle11 (length labels11) labels11))))
  (setf impro (merger (beats->chseq (thread-Beats (append beats11a beats11b beats11c beats11d)) Billevans_beatdur 0)
                      (beats->chseq (make-beat-list (list Israelwalking Israelwalking Israelwalking Israelwalking))
                                    Billevans_beatdur 0)))
  (play impro))
)

(mapcar 'HarmLabel (ImprovizeOnHarmGrid oracle11 (length labels11) labels11))

(my-save-as-midi impro Billevans_beatdur)

|#


;------------
;EXAMPLE # 12
;------------
;Segmentation of live midi data into labelled beats
;Tempo clocks and chord changes must be coded as notes in the midifile, and stored on channel 14
;(clocks = midi code 12, chords = midi codes from 0 to 11 with velocity 1=maj7, 2=m7, 3=7)
;Five successive treatments are applied: 
;1 - check-midifile-evts = separate midi data into 5uples and clocks
;2 - quintuples->beats = group 5uples when they fall between two clocks
;3 - cut-beat-events = cut note duration and add negative midi code in the next beat when duration exceed beat duration 
;4 - set-relative-time-beat = each beat begins with onset 0 (or negative onset when syncopation)
;5 - label-chord-beat = transform midi data from channel 14 into chord labels
;The result is a list of pairs (chordlabel midiset) which can be processed by make-beat-list in order to compute the oracle

;Example taken from B. Lubat's impro on "Goodbye Porkpie Hat" (Uzeste, October 1st 2004), BPM = 77, beat duration = 60000/BPM = 779 ms


#|
;The following expression opens a midifile, and gives a list of evts as 5uples or clocks:

(setf midifromfile (evts-from-midifile))             ;for "Goodbye Pork Pie Hat", choose the file name: 'ch1+basse.mid' 

;Further treatments require that it contains clocks and chords on channel 14
;The successive treatments are achieved by the following progn expression

(progn
(setf res1 (check-clocks midifromfile) 
      clocksfromfile (first res1) quintuplesfromfile (second res1) 
      defaultbeatdur (round (om-mean (x->dx (mapcar 'first clocksfromfile)))))
(setf beatsfromfile2 (quintuples->beats clocksfromfile quintuplesfromfile))
(setf beatsfromfile3 (cut-beat-events clocksfromfile beatsfromfile2))           
(setf beatsfromfile4 (set-relative-time-beat clocksfromfile beatsfromfile3))
(setf beatsfromfile (label-chord-beat beatsfromfile4))
)

;WARNING: these treatments make physical modifications.
;Thus reevaluate 'midifromfile' (by opening the midifile) before reevaluating any of them.

;If 'defaultbeatdur' does not match the expected beat duration, it might be necessary to stretch time 
;(OpenMusic loads midifiles by modifying durations according to a default tempo, this could perhaps be prevented 
;by including an explicit tempo in the midifile before loading it into OpenMusic)
 
(setf beatdur 779)
(setf midifromfile (timestretch (check-midifile-evts midifromfile) (/ beatdur defaultbeatdur)))
(setf beatsfromfile (clocked-evts->beats midifromfile))                      ;'clocked-evts->beats' = all the successive treatments

;You can play the given midi data in order to check the result by storing them into a beatlist object
;(this beatlist is also used to compute the corresponding oracle):

(setf beatlist (make-beat-list beatsfromfile))

(pgmout 75 1)     ; channel 11 = flute  "75" (solo by Bernard Lubat)
(play (beats->chseq (thread-Beats beatlist) beatdur 0))

(play (beats->chseq (thread-Beats beatlist) defaultbeatdur 0))

(setf beatdur 400)

(Stop-Player *general-player*)


|#

;When opening the file 'ch1+basse.mid' by evaluating (setf midifromfile (evts-from-midifile)), 
;the result should have its first evts like the ones shown in the list below:
(setf midifromfile      
'((16 0 5 1 14) (12 0 5 1 14) (8 0 1521 3 14) (32 0 1521 96 2) (44 11 1287 60 1) (54 281 990 60 1) (62 297 1031 70 1) (60 302 1073 64 1) (65 302 1042 74 1) 
(12 761 5 1 14) (7 1531 1532 3 14) 
(12 1531 5 1 14) (31 1531 1532 96 2) (43 1594 1375 52 1) (65 1854 1052 65 1) (53 1854 1245 54 1) (61 1860 844 65 1) (59 1860 1182 70 1) (67 2084 115 98 11) 
(12 2281 5 1 14) (65 2281 157 98 11) (67 2490 141 106 11) (70 2693 188 106 11) (6 3073 766 3 14) (12 3073 5 1 14) (30 3073 766 96 2) (42 3089 797 40 1) (60 3490 792 68 1) (58 3490 344 48 1) (52 3495 411 48 1) (5 3849 755 3 14) 
(12 3849 5 1 14) (29 3849 755 96 2) (41 3891 521 58 1) (51 4266 386 50 1) (59 4266 334 80 1) (57 4287 636 52 1) (42 4474 177 24 1) (4 4620 1547 3 14) 
(12 4620 5 1 14) (40 4620 1547 96 2) (28 4620 1547 96 2) (40 4662 1276 64 1) (50 4891 1344 54 1) (58 4901 1276 76 1) (56 4912 1250 76 1) 
(12 5396 5 1 14) (4 6172 761 3 14) 
(12 6172 5 1 14) (40 6172 761 96 2) (28 6172 761 96 2) (16 6188 11 1 14) (40 6214 693 60 1) (58 6459 391 54 1) (50 6459 521 54 1) (56 6469 391 58 1) (39 6943 808 54 1) (3 6953 766 3 14) 
(12 6953 5 1 14) (63 6953 360 88 11) (39 6953 766 96 2) (27 6953 766 96 2) (55 7198 474 42 1) (69 7203 542 36 1) (65 7214 479 52 1) (61 7224 474 54 1) (62 7729 823 108 11) (10 7729 1547 3 14) 
))


;With the same file 'ch1+basse.mid', the result of the overall successive treatments should look like this for the first evts of the list:
(setf beatsfromfile         
'(((g# 7) ((44 10 760 60 1) (32 0 770 96 2) (65 302 468 74 1) (60 302 468 64 1) (62 297 473 70 1) (54 281 489 60 1))) 
((g# 7) ((-44 0 526 60 1) (-32 0 751 96 2) (-65 0 574 74 1) (-60 0 605 64 1) (-62 0 558 70 1) (-54 0 501 60 1))) 
((g 7) ((31 -10 786 96 2) (43 53 723 52 1) (59 319 457 70 1) (61 319 457 65 1) (53 313 463 54 1) (65 313 463 65 1) (67 543 114 98 11))) 
((g 7) ((-31 0 746 96 2) (-43 0 652 52 1) (-59 0 725 70 1) (-61 0 387 65 1) (-53 0 772 54 1) (-65 0 589 65 1) (65 -36 156 98 11) (67 173 140 106 11) (70 375 188 106 11))) 
((f# 7) ((53 0 10 54 1) (42 0 776 40 1) (30 -16 766 96 2) (52 407 369 48 1) (58 401 344 48 1) (60 401 375 68 1))) 
((f 7) ((-42 0 21 40 1) (-52 0 42 48 1) (-60 0 417 68 1) (41 26 521 58 1) (29 -16 755 96 2) (57 422 349 52 1) (59 401 334 80 1) (51 401 370 50 1) (42 609 162 24 1))) 
((e 7) ((-57 0 287 52 1) (-51 0 16 50 1) (-42 0 15 24 1) (40 26 750 64 1) (28 -16 792 96 2) (40 -16 792 96 2) (56 275 501 76 1) (58 264 512 76 1) (50 254 522 54 1))) 
((e 7) ((-40 0 526 64 1) (-28 0 754 96 2) (-40 0 754 96 2) (-56 0 749 76 1) (-58 0 764 76 1) (-50 0 776 54 1))) 
((e 7) ((50 0 46 54 1) (40 26 692 60 1) (28 -16 760 96 2) (40 -16 760 96 2) (56 280 390 58 1) (50 270 520 54 1) (58 270 390 54 1))))
)



#|
;Adding a drum pattern for the file 'ch1+basse.mid', "Goodbye Pork Pie Hat" (EXAMPLE # 10 above), BPM = 77, beatdur 60000/77 = 779 ms

(progn                                                  
(pgmout 4 1)       ; channel 1 = piano voicings "4 - Electric Piano 1", complete list of sounds in *midi-programs*
(pgmout 78 2)      ; channel 2 = bass "78 - Whistle"
(pgmout 75 11)     ; channel 11 = flute  "75" (solo by Bernard Lubat)
(ctrlchg 7 110 1) (ctrlchg 7 127 2) (ctrlchg 7 127 11) (ctrlchg 7 127 10)
(let* ((drumpart (loop for i from 1 to (round (/ (length beats12) 8)) collect roof)))  ; roof duration = 8 beats (2 bars)
   (play (merger (beats->chseq beats12 779 0)                  ;beginning of Bernard Lubat's solo with corresponding voicings
                 (beats->chseq (make-beat-list drumpart) 779 0))))  
)

(Stop-Player *general-player*)

(my-save-as-midi (beats->chseq (thread-Beats beats12) 779 0) 779)  ; visualization of the live solo in a piano-roll

;For this player configuration, you can test other midifiles 'ch2+basse.mid' to 'ch5+basse.mid'
(setf beats12
      (make-beat-list (clocked-evts->beats (timestretch (check-midifile-evts (evts-from-midifile)) 0.500006))))             



|#




