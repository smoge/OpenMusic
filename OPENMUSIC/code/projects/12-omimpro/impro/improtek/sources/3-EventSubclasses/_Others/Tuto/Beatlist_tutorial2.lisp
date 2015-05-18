;ISSU DU DECOUPAGE DE BEATLIST.LISP
;COMMENTAIRES ET TUTORIALS
;-----------------------------------------

(in-package :om)


#|
;Open a MIDI file with chords on channel 16, and then give a couple: (list of pairs label+mididata, beatdur)
(midi-to-MidiHarmBeats)

(mf-info (load-midi-file (om-choose-file-dialog)))

;Example of such a list of pairs label+mididata, with its corresponding 'beatdur':

(setf Zisteinit_beatdur 536
      Zisteinit_MidiHarmBeatsfromfile
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

;MidiHarmBeatLIST:
(setf ZisteMidiHarmBeatlist (make-MidiHarmBeat-list Zisteinit_MidiHarmBeatsfromfile Zisteinit_beatdur))

(setf DrumsMidiHarmBeatlist   ;beatdur=536, BPM=112
  (let ((2beatpoumchiBPM112 
         '((nolabel nolabel 2) ((36 0 229 80 10) (42 268 229 80 10) (36 536 229 80 10) (40 536 229 80 10) (42 804 229 80 10)))))
    (make-MidiHarmBeat-list (list 2beatpoumchiBPM112 2beatpoumchiBPM112 2beatpoumchiBPM112 2beatpoumchiBPM112) Zisteinit_beatdur)))

(play (merger (MidiHarmBeats->chseq ZisteMidiHarmBeatlist Zisteinit_beatdur 0) (MidiHarmBeats->chseq DrumsMidiHarmBeatlist Zisteinit_beatdur 0)))
(Stop-Player *general-player*)

;ORACLE:
(setf oracleziste (NewImprovizer ZisteMidiHarmBeatlist Zisteinit_beatdur))

(setf labels0 '((bb m7) (bb m7) (bb m7) (bb m7) (f 7) (f 7) (f 7) (f 7)) labels (append labels0 labels0 labels0 labels0))

(progn (pgmout 4 11) (ctrlchg 7 127 11) (setf (max-continuity oracleziste) 1)
(play (setf impro (merger (MidiHarmBeats->chseq (ImprovizeOnHarmGrid oracleziste (length labels) labels) Zisteinit_beatdur 0) 
                          (MidiHarmBeats->chseq (loop for i from 1 to (floor (/ (length labels) 8)) append DrumsMidiHarmBeatlist) 
                                        Zisteinit_beatdur 0)))))

(my-save-as-midi impro Zisteinit_beatdur)
(om-inspect ZisteMidiHarmBeatlist)

|#

