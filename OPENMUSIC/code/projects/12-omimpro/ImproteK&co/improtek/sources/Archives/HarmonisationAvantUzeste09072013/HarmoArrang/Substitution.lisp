;Substitution.lisp
;by Marc Chemillier
;From  M.J. Steedman, "A Generative Grammar for Jazz Chord Sequences", Music Perception, vol. 2, no1, 1984, 52-77. 
(in-package :om)


(setf multbeatgrid '((b m7 1) (e 7 1) (bb m7 1) (eb 7 1) (ab m7 2) (c# 7 2) (f# maj7 2) (ab 7 2) (b 7 2) (bb 7 2)))


#|
;Musical examples:
;(for basic examples with simple letters, see below)

;Play different voicings on the same chord progression 'multbeatgrid':

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; basic voicings        
(play (setf harmo (beats->chseq (PlayVoicings basicvoicings multbeatgrid 500) 500 0))))

(progn (pgmout 4 1) (ctrlchg 7 120 1)                    ; voicings from Hermeto Pascoal            
(play (setf harmo (beats->chseq (PlayVoicings hermetovoicings multbeatgrid 500) 500 0))))

(save-as-midi-with-tempo harmo 500)


;Same thing with substitutions applied to 'multbeatgrid':
;(warning: for more than 4 rewriting rules, it fails...)

(progn (pgmout 4 1) (ctrlchg 7 120 1)               
(setf harmo (beats->chseq (PlayVoicings hermetovoicings (GridLabel (Rewrite bluesgrammar (InitMultiplebeatGrid multbeatgrid) 4)) 
                                        500) 500 0))    ; voicings from Hermeto Pascoal
(play harmo))

(Stop-Player *general-player*)


;DIDIDENBAS



;Open a MIDI file with chords on channel 16, and then give a couple: (list of pairs label+mididata, beatdur)
(let ((tmp (midi-to-beats)))
(setf multbeatgrid Dicidenbas_grid)
(setf DiciChiff_beatdur (second tmp)
      DiciChiff_beatsfromfile (first tmp)
      DiciChifflist (make-beat-list DiciChiff_beatsfromfile DiciChiff_beatdur)
      oracleDiciChiff (NewImprovizer DiciChifflist DiciChiff_beatdur)
(setf labelDiciChiff (GridLabelBeat (Rewrite bluesgrammar (InitMultiplebeatGrid multbeatgrid) 5))
      ;labelDiciChiff (GridLabelBeat (InitMultiplebeatGrid multbeatgrid))
)
))

(progn (pgmout 4 1) (ctrlchg 7 0 15) (setf (max-continuity oracleDiciChiff) 100)
(setf ;labelDiciChiff (GridLabelBeat (Rewrite bluesgrammar (InitMultiplebeatGrid multbeatgrid) 3))
      labelDiciChiff (GridLabelBeat (InitMultiplebeatGrid multbeatgrid))
)
(setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid oracleDiciChiff (length labelDiciChiff) labelDiciChiff) 
                                                DiciChiff_beatdur)
                                  DiciChiff_beatdur 0)
                    (beats->chseq (make-clocks (length labelDiciChiff) DiciChiff_beatdur 2) 
                                  DiciChiff_beatdur 0)))
(play impro))



;(my-save-as-midi impro DiciChiff_beatdur)
;(om-inspect Zistebeatlist)



|#


(defclass* rule ()
  ((leftpart :initform nil :initarg :leftpart :accessor leftpart)         ; = object list (symbols, chords, etc.)
   (rightpart :initform nil :initarg :rightpart :accessor rightpart)))

(defclass* grammar ()
  ((rulelist :initform nil :initarg :rulelist :accessor rulelist)))


(defclass* substitution (rule)
  (
   (rulename :initform 'myrule :initarg :rulename :accessor rulename)
   (dividing :initform nil :initarg :dividing :accessor dividing)
   (marking :initform t :initarg :marking :accessor marking)))

(defclass* steedmangrammar (grammar)
  ((threshold :initform 1 :initarg :threshold :accessor threshold)))   ;control the deepness of rule application


(defclass* jazzchord ()
  (
   (root :initform 'c :initarg :root :accessor root)
   (quality :initform 'maj7 :initarg :quality :accessor quality)
   (nbeats :initform 4 :initarg :nbeats :accessor nbeats)
   (marked :initform nil :initarg :marked :accessor marked)
))


;Steedman's grammar
(setf rule-I 
   (make-instance 'substitution :leftpart '((x maj7)) :rightpart '((x maj7) (x maj7)) :dividing t :marking nil :rulename 'I))
(setf rule-I7 
   (make-instance 'substitution :leftpart '((x 7)) :rightpart '((x maj7) (x 7)) :dividing t :marking nil :rulename 'I7))
(setf rule-Im7                                                ;;;'((x maj7) (x m7)) pourquoi "majorer" l'accord ?????
   (make-instance 'substitution :leftpart '((x m7)) :rightpart '((x m7) (x m7)) :dividing t :marking nil :rulename 'Im7))

(setf rule-V7-I7 (make-instance 'substitution :leftpart '(w (x 7)) :rightpart '(((D x) 7) (x 7)) :rulename 'V7-I7))
(setf rule-V7-Im7 (make-instance 'substitution :leftpart '(w (x m7)) :rightpart '(((D x) 7) (x m7)) :rulename 'V7-Im7))
(setf rule-IIm7-V7 (make-instance 'substitution :leftpart '(w (x 7)) :rightpart '(((D x) m7) (x 7)) :rulename 'IIm7-V7))
(setf rule-IIb7-I7 (make-instance 'substitution :leftpart '(((D x) 7) (x 7)) :rightpart '(((Stb x) 7) (x 7)) :rulename 'IIb7-I7))
(setf rule-IIb-I (make-instance 'substitution :leftpart '(((D x) 7) (x maj7)) :rightpart '(((Stb x)) (x maj7)) :rulename 'IIb-I))
(setf rule-IIb-Im7 (make-instance 'substitution :leftpart '(((D x) 7) (x m7)) :rightpart '(((Stb x)) (x m7)) :rulename 'IIb-Im7))

(setf bluesgrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7 rule-IIb-I rule-IIb-Im7)))



(defmethod Rewrite ((self grammar) (word list) (n integer))
   (loop for i from n downto 1
         do (let* ((PossibleRules (loop for r in (rulelist self)
                                        when (loop for l = word then (cdr l) until (null l)
                                                   when (= (mismatch (leftpart r) l) (length (leftpart r)))
                                                   return t)
                                        collect r))
                   (ChosenRule (nth (random (length PossibleRules)) PossibleRules))
                   (res (ApplyRule ChosenRule word)))
              (setf word (nth (random (length res)) res)))
         finally return word))           

;gives ALL the sequences obtained by applying a rule in all possible positions
(defmethod ApplyRule ((r rule) (word list) &optional (threshold 1))   ;return nil if no match, 'threshold' specific to musical grammars
   (loop for l = word then (cdr l) until (null l)
         for i from 0
         when (let ((nomatch (mismatch (leftpart r) l :test 'equal)))
                (or (null nomatch) (= nomatch (length (leftpart r)))))
         collect (append (nthcar i word) 
                         (rightpart r) 
                         (nthcdr (+ i (length (leftpart r))) word))))



;Applies one not dividing rule, and eventually one dividing rule
(defmethod Rewrite ((self steedmangrammar) (grid list) (n integer))
  (format *om-stream* "------------------------------------------------------------~%") (PrintLabelList (GridLabel grid))
  (format *om-stream* "------------------------------------------------------------~%")
   (loop for i from n downto 1
         do (progn ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(when (> (random 3) 0) (setf grid (rewrite-once self grid t)))   ;;; eventually one dividing rule
                   (setf grid (rewrite-once self grid nil)))                        ;;; + one not dividing rule
         finally return (progn (format *om-stream* "------------------------------------------------------------~%")
                          grid))) 

;Applies a dividing rule if true, and a not dividing one if false
;;;;;;;;;;;;SEPARER EN DEUX foNCTIONS: rewrite-by-dividing et rewrite-by-subtituting

(defmethod rewrite-once ((self steedmangrammar) (grid list) (dividingbool t))
   (let* ((PossibleRules (loop for r in (funcall (if dividingbool 'remove-if-not 'remove-if) 'dividing (rulelist self))
                               when (loop for l = grid then (cdr l) until (null l)
                                          when (eligible-rule? r l (threshold self))
                                          return t)
                               collect r))
          (ChosenRule (nth (random (length PossibleRules)) PossibleRules)))
     (when ChosenRule 
       (let* ((res (ApplyRule ChosenRule grid (threshold self)))
              (newgrid (nth (random (length res)) res)))
;         (pushr (list ChosenRule res) (lastderivation self))
         (when (not dividingbool) (PrintLabelList (GridLabel newgrid)))
         newgrid))))


;gives ALL the sequence obtained by applying a rule in all possible positions
;'threshold' used to control the deepness of rule application (optional since it is not used for general grammars)
(defmethod ApplyRule ((r substitution) (jazzchordlist list) &optional (threshold 1))
   (loop for l = jazzchordlist then (cdr l) until (null l)
         for i from 0
         when (eligible-rule? r l threshold)
         collect (append (nthcar i jazzchordlist) 
                         (AdaptRule r (subseq jazzchordlist i (+ i (length (leftpart r)))))
                         (nthcdr (+ i (length (leftpart r))) jazzchordlist))))

(defmethod eligible-rule? ((r substitution) prefixegrid threshold)
   (and (>= (length prefixegrid) (length (leftpart r)))     ;;;;;;???????? A VERIFIER
        (<= (nbeats (first prefixegrid)) 2);;;;;;;;;;;;;;;;;;;;;;;;;1/7/12: uniquement accords de durée <= 2 beats
        (let* ((root (FindRoot (leftpart r) prefixegrid))
               (actualleftpart (AdaptRoot (leftpart r) root))
               (nomatchleft (mismatch actualleftpart prefixegrid :test 'CompareJazzChord))
               (actualrightpart (AdaptRoot (rightpart r) root))
               (nomatchright (mismatch actualrightpart prefixegrid :test 'CompareJazzChord)))
          (and (or (null nomatchleft) (= nomatchleft (length (leftpart r))))
               (or (not (dividing r)) (loop for chord in (nthcar (length actualleftpart) prefixegrid)
                                            when (<= (nbeats chord) threshold) return nil 
                                            finally return t))
               (and nomatchright (< nomatchright (length (rightpart r))))))))

(defmethod CompareJazzChord (label (chord jazzchord)) 
  (if (equal label 'w) (not (marked chord))
      (and (eq (first label) (root chord)) (eq (second label) (quality chord)))))

(defun FindRoot (rulepart grid)
  (loop for label in rulepart for chord in grid 
        when (and (listp label) (eq (first label) 'x)) return (root chord)))

(defun AdaptRoot (rulepart root) 
   (loop for label in rulepart
         if (atom label) collect label
         else collect (TransposeLabel (cons root (cdr label))
                                      (if (atom (first label)) 0
                                          (case (caar label) (D 7) (Stb 1))))))

(defun AdaptRootLabels (rulepart root) 
   (loop for label in rulepart
         if (atom label) collect label
         else collect (TransposeLabel (cons root (cdr label))
                                      (if (atom (first label)) 0
                                          (case (caar label) (D 7) (Stb 1))))))

(defmethod AdaptRule ((r substitution) oldgridportion)
   (let* ((newleftpart (AdaptRoot (rightpart r) (FindRoot (leftpart r) oldgridportion)))
          (newgridportion 
           (loop for ch1 in oldgridportion for x in newleftpart
                 collect (make-instance 'jazzchord 
                           :root (first x) :quality (if (null (cdr x)) 'maj7 (second x)) 
                           :nbeats (nbeats ch1) :marked (marking r))))
          (rest (nthcdr (length oldgridportion) newleftpart))
          (dur (/ (nbeats (car (last oldgridportion))) (1+ (length rest)))))
     (setf (nbeats (car (last newgridportion))) dur)
     (append newgridportion
             (loop for x in rest 
                   collect (make-instance 'jazzchord 
                             :root (first x) :quality (if (null (cdr x)) 'maj7 (second x)) 
                             :nbeats dur :marked (marking r))))))
#|
(defmethod AdaptRule ((r substitution) oldgridportion)
   (let* ((newrightpart (AdaptRootLabels (rightpart r) (FindRoot (leftpart r) oldgridportion)))
          (newgridportion (loop for x in newrightpart
                                collect (make-instance 'jazzchord :root (first x) :quality (if (null (cdr x)) 'maj7 (second x))))))
     (loop for x in newgridportion for y in oldgridportion
           do (setf (nbeats x) (nbeats y)) 
           when (not (eq (root x) (root y))) do (setf (marked x) t))
     newgridportion))
|#

;----------------------------------------------------------------
;PLAYING VOICINGS


(defmethod! PlayVoicings ((voicinglist list) (multbeatgrid list) beatdur)
  (let* ((timedvoicings (loop for x in voicinglist for n = (random (length (second x))) for y = (nth n (second x)) ;if more than one voicing
                              do (print (list (first x) n))
                              collect (list (first x) (loop for z in y collect (list z 0 (- beatdur 50) 80 5))))) 
         (beats (mapcar 'make-beat timedvoicings))
         (voicingoracle (NewImprovizer beats beatdur))
         (labels (mapcar #'(lambda (x) (list (first x) (second x))) multbeatgrid))
         (durlist (mapcar #'third multbeatgrid))
         (harmo (ImprovizeOnHarmGrid voicingoracle (length labels) labels)))
    (loop for x in harmo for z in durlist 
          do (setf (HarmLabel x) (append (HarmLabel x) (list z))
                   (MidiSet x) (timestretch (MidiSet x) z))
          collect x)))


(setf basicvoicings '(
((b m7)     ((47 57 61 62 66)))     
((e 7)      ((40 56 61 62 66)))
((a maj7)   ((45 56 59 61 64))) 

((e m7)     ((40 55 59 62 66))) 
((a 7)      ((45 55 59 61 66)))
((d maj7)   ((38 54 57 61 64)))
))

(setf hermetovoicings '(
((b m7)     ((47 57 61 62 64)))            ; (- 4 7 9)
((e 7)      ((40 56 60 62 67)))            ; (7 9+ 13-)
((a maj7)   ((45 56 59 61 66)))            ; (6 7+ 9)

((e m7)     ((40 55 57 62 66)))      ; (- 4 7 9)
((a 7)      ((45 55 59 61 63 66)))    ;11+
((d maj7)   ((38 54 57 61 64)))
))


;----------------------------------------------------------------
;FORMATING DATA

(defun InitMultiplebeatGrid (multiplebeat-labellist)
  (loop for x in multiplebeat-labellist
        collect (make-instance 'jazzchord :root (first x) :quality (second x) :nbeats (third x))))



(defun GridLabel (grid) (loop for x in grid collect (list (root x) (quality x) (nbeats x))))
(defun Bluesify (labels) 
   (loop for x in (copy-list labels) do (when (not (eq (second x) 'm7)) (setf (second x) '7)) collect x))

(defun GridLabelBeat (grid) 
   (loop for x in grid 
         append (make-list (nbeats x) :initial-element (list (root x) (quality x)))))

(defun PrintLabel (y) (format *om-stream* "~@(~a~)~a(~a) " (first y) (if (eq (second y) 'maj7) "" (second y)) (third y)))
(defun PrintLabelList (list) (loop for x in list do (PrintLabel x)) (format *om-stream* "~%"))



;----------------------------------------------------------------
;COMPILING ALL SEQUENCES GENERATED BY THE GRAMMAR
;WARNING: can only work for words with a few letters


(defmethod CompileLanguage ((self grammar) word) (ScanTree self (list word) (rulelist self)))

(defmethod ScanTree ((self grammar) wordlist rulelisttmp)
  (if (null rulelisttmp) wordlist
      (let ((new-wordlist (copy-list wordlist)))
        (loop for w in wordlist do (loop for x in (ApplyRule (first rulelisttmp) w)
                                         when (not (member x new-wordlist :test 'equal)) do (pushr x new-wordlist)))
        (if (= (length new-wordlist) (length wordlist)) 
          (ScanTree self wordlist (cdr rulelisttmp))
          (ScanTree self new-wordlist (rulelist self))))))

(defmethod ScanTree ((self steedmangrammar) jazzchordlist rulelisttmp)
  (if (null rulelisttmp) jazzchordlist
      (let ((new-jazzchordlist (loop for w in jazzchordlist collect (clone w))))
        (loop for w in jazzchordlist do (loop for x in (ApplyRule (first rulelisttmp) w (threshold self))
                                              when (not (member x new-jazzchordlist :test 'eqjazzchordlist)) do (pushr x new-jazzchordlist)))
        (if (= (length new-jazzchordlist) (length jazzchordlist)) 
          (ScanTree self jazzchordlist (cdr rulelisttmp))
          (ScanTree self new-jazzchordlist (rulelist self))))))

(defun eqjazzchordlist (jazzchordlist1 jazzchordlist2)
  (loop for x in jazzchordlist1 for y in jazzchordlist2
        when (not (and (eq (root x) (root y)) (eq (quality x) (quality y)) (eq (nbeats x) (nbeats y)))) return nil finally return t))



;-------------
;BASIC EXAMPLE
;-------------

#|

(setf r (make-instance 'rule :leftpart '(a b c) :rightpart '(x y z)))
(ApplyRule r '(a a a a a a a b c d d a b c d d))
(setf r (make-instance 'rule :leftpart '(b l o n d e) :rightpart '(b r u n e)))
(ApplyRule r '(u n e b e l l e b l o n d e))
(ApplyRule r '(u n e b l o n d e a u x y e u x e n a m e n d e s))

(setf r (make-instance 'rule :leftpart '(a b c d) :rightpart '(x y z)))

(setf r1 (make-instance 'rule :leftpart '(a) :rightpart '(x)))
(setf r2 (make-instance 'rule :leftpart '(a b) :rightpart '(y)))
(ApplyRule r1 '(a a b))

(setf g (make-instance 'grammar :rulelist (list r1 r2)))
(loop for x in (CompileLanguage g '(a b)) do (format *om-stream* "~a~%" x))
(loop for x in (CompileLanguage g '(a a b)) do (format *om-stream* "~a~%" x))
(loop for x in (CompileLanguage g '(a b a b)) do (format *om-stream* "~a~%" x))


(GridLabel raingrid)
(PrintLabelList (GridLabel raingrid))

(AdaptRoot '((x m7) ((D x) 7) (x) ((Stb x) maj7)) 'c)
(CompareJazzChord '(c 7) (make-instance 'jazzchord :root 'c :quality '7))
(mismatch '((d 7) (g 7)) (InitGridWithBars '((d 7) (g 7) / (c 7))) :test 'CompareJazzChord)
(eligible-rule? rule-V7-I7 (InitGridWithBars '((c m7) (g 7) / (c 7))) 1)
(eligible-rule? rule-V7-I7 (InitGridWithBars '((d 7) (g 7) / (c 7))) 1)  ; nil if the rule does not change anything
(FindRoot '(((D x) 7) (x 7)) (InitGridWithBars '((c m7) (g 7))))
(GridLabel (AdaptRule rule-V7-I7 (InitGridWithBars '((c m7) (g 7)))))

(mapcar 'GridLabel (ApplyRule rule-V7-I7 (InitGridWithBars '((c m7) (g 7) / (c 7))) 1))

(mapcar 'GridLabel (ApplyRule rule-V7-I7 raingrid 1))

(GridLabel (rewrite-once raingrammar raingrid t))    ;dividing
(GridLabel (rewrite-once raingrammar raingrid nil))  ;not dividing

(mapcar 'GridLabel (ApplyRule rule-V7-I7 (InitGridWithBars '((c m7) (g 7) / (c 7))) 1))

(mapcar 'GridLabel (ApplyRule rule-V7-I7 raingrid 1))

(GridLabel (rewrite raingrammar raingrid 3))

(GridLabel (rewrite raingrammar raingrid 4))


|#



;------------------------------------------
;MORE EXAMPLES FROM 'ImprotekTutorial.lisp'
;------------------------------------------

(defun InitGridWithBars (labellist)
  (loop for x in labellist with beat = nil with beatlist = nil
        do (if (not (eq x '/)) (push (make-instance 'jazzchord :root (first x) 
                                                    :quality (if (null (cdr x)) 'maj7 (second x)))
                                     beat)
               (progn (push (reverse beat) beatlist) (setf beat nil)))
        finally return (loop for chordsinbeat in (reverse (push (reverse beat) beatlist))
                             append (loop for x in chordsinbeat for n = (length chordsinbeat)
                                          do (setf (nbeats x) (/ 4 n)) 
                                          collect x))))


(setf *rain-changes*  '((b m7) (e 7) (bb m7) (eb 7) / (g# m7) (c# 7) / (f#) (g# 7) / (b 7) (bb 7)))

#|
;EXAMPLE # 06 "Rain" 
;=====================

(progn (ctrlchg 7 100 10)
(pgmout 4 1)      ; channel 1 = "4 - Electric Piano 1", for a complete list evaluate the variable *midi-programs*
(pgmout 33 2)     ; channel 2 = "33 - Electric Fingered Bass"
(let* ((newlabels6 (GridLabel (rewrite bluesgrammar (InitGridWithBars *rain-changes*) 4)))
       (nbgrids (/ (loop for x in newlabels6 sum (third x)) 16))
       (drumpart (make-drumpart nbgrids)))
  (play (merger (beats->chseq (ImprovizeOnHarmGrid oracle6 (length newlabels6) newlabels6) 484 0)
                (beats->chseq (make-beat-list drumpart) 484 0))))
)

;'oracle6' and 'make-drumpart' see ImprotekTutorial.lisp

|#



(setf *boogie-changes*               ; a blues in E
'((e) / (e) / (e) / (e 7) / (a) / (a) / (e) / (e) / (b) / (b 7) / (e) / (e)))
(setf boogiegrammar (make-instance 'steedmangrammar :threshold 2
                      :rulelist (list rule-I rule-I7 
                                      rule-V7-I7 rule-V7-Im7 rule-IIm7-V7
                                      rule-IIb7-I7 rule-IIb-I)))

(defun GridLabelBoogie (grid)  ;4th elt in labels = pos in the bar (1st or 3rd beat) + all qualities = 7
   (let ((BeatList
          (loop for x in grid with boolstrongbeat = t
                when (>= (nbeats x) 4) collect (list (root x) 7 (nbeats x) 1)
                when (= (nbeats x) 2) 
                collect (if boolstrongbeat
                          (progn (setf boolstrongbeat nil) (list (root x) 7 (nbeats x) 1))
                          (progn (setf boolstrongbeat t) (list (root x) 7 (nbeats x) 
                                                               3))))))   ;position=3rd beat
     (GroupChordPairs (GroupChordPairs BeatList 8) 8)))

(defun GroupChordPairs (labellist nbeatmax)   ;WARNING: does not check the position of the barlines
  (loop for l = labellist then l until (null l)
        collect (if (or (null (cdr l)) 
                        (not (eq (first (car l)) (first (cadr l))))
                        (not (eq (second (car l)) (second (cadr l))))
                        (/= (third (car l)) (third (cadr l)))
                        (/= (fourth (car l)) 1)
                        (= (third (car l)) nbeatmax))
                  (pop l)
                  (let* ((x1 (pop l)) (x2 (pop l))) (setf (third x1) (+ (third x1) (third x2))) x1))))


#|
;The grid used has a fourth element for each chord indicating the position in the bar (1 or 3)
;The following function groups chords having the same label:
(GroupChordPairs '((e 7 4 1) (e 7 4 1) (c 7 4 1) (b 7 4 1) (e 7 4 1) (a 7 4 1) (a 7 4 1) (e 7 4 1) (e 7 4 1) (b 7 4 1) (f 7 4 1) (e 7 4 1) (e 7 4 1))
8)

;'Ad hoc' function which fits the requirements of the associated voicing in the oracle for the "Boogie" example:
(GridLabelBoogie (rewrite boogiegrammar (InitGridWithBars *boogie-changes*) 5))

;EXAMPLE # 07 "Boogie" 
;=====================

(progn (setf impro nil)
(pgmout 17 1)      ; channel 1 = "17 - Percussive Organ" 
(pgmout 52 2)      ; channel 2 = "52 - Choir Aahs", channel 1 duplicated for a more sustained sound
(pgmout 78 3)      ; channel 3 = "78 - Whistle", channel 1 duplicated for the bass part
(ctrlchg 7 110 1) (ctrlchg 7 92 2) (ctrlchg 7 127 3)
(let* ((newlabels7 (GridLabelBoogie (rewrite boogiegrammar (InitGridWithBars *boogie-changes*) 5)))
       (impro1 (beats->chseq (ImprovizeOnHarmGrid oracle7 (length newlabels7) newlabels7) 414 0))
       (impro2 (clone impro1)) (impro3 (clone impro1)))
  (setf (Lchan impro2) (change-element 2 1 (Lchan impro2)) (Lchan impro3) (change-element 3 1 (Lchan impro3))
        (Lmidic impro3) (mapcar #'(lambda (l) (remove-if #'(lambda (x) (> x 6000)) l)) (Lmidic impro3))    ; bass part
        impro (merger (merger impro1 impro2) impro3))
  (play impro)
))


|#

(setf *ziste-changes* '(
(bb) / (bb) / (bb) / (bb 7) / (eb) / (eb) / (bb) / (bb) / (f 7) / (eb) / (bb) / (f 7)))
;(setf zistegrammar (make-instance 'steedmangrammar        ;"Ziste" whithout maj7
;          :rulelist (list rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 rule-IIb7-I7)))
(setf zistegrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7 rule-IIb-I rule-IIb-Im7)))

(setf beats9comp (make-beat-list zistecomp))

(setf oracle9comp 
      (let ((o (NewImprovizer)))      
        (loop for i from 0 to (1- (length beats9comp)) do (learn-event o (nth i beats9comp))) o))

(setf (max-continuity oracle9comp) 1)

#|

(Bluesify (GridLabelBeat (InitGridWithBars *ziste-changes*))))

;EXAMPLE # 09 "Ziste" 
;====================

;Plays solo on the blues chord changes with voicings on substituted chord changes
(progn  (setf impro nil)               
(pgmout 4 1) (pgmout 4 3)     ; channel 4 = sustained voicings, channel 3 = rhythmic voicings
(pgmout 36 4)     ; "36 - Slap Bass 1", fixed bass pattern
(pgmout 36 2)     ; "36 - Slap Bass 1", bass related to chords
(pgmout 4 11)     ; channel 11 = "4" (solo by Bernard Lubat)
(ctrlchg 7 100 1) (ctrlchg 7 80 3) (ctrlchg 7 50 4) (ctrlchg 7 127 11) (ctrlchg 7 90 10) (ctrlchg 7 50 2) 
(let* ((labels9comp (Bluesify (GridLabel (rewrite zistegrammar (InitGridWithBars *ziste-changes*) 5)))))
  (setf impro (merger (merger (beats->chseq (ImprovizeOnHarmGrid oracle9 (length labels9) labels9) 536 0)
                              (beats->chseq (ImprovizeOnHarmGrid oracle9comp (length labels9comp) labels9comp) 536 0))
                      (beats->chseq (make-beat-list (list zistefunk)) 536 0)))
  (format *om-stream* "------------------------------------------------------------~%") (PrintLabelList labels9comp)
  (format *om-stream* "------------------------------------------------------------~%")
  (play impro))
)
|#



(setf *goodbye-changes*      '(          ;"Goodbye Pork Pie Hat" by Mingus
        (f 7) (c# 7) / (f#) (b 7) / (eb 7) (c# 7) / (eb 7) (f 7) /
        (bb m7) (g# 7) / (g m7) (c 7) / (d 7) (g 7) / (c# 7) (f#) /
        (bb 7) (c# 7) / (c 7) (eb 7) / (f 7) (c# 7) / (f#) (b 7)))
(setf *goodbye-changes1* '(              ;substitutions applied to "Goodbye Pork Pie Hat" (see grid below)
(g# 7) (g 7) / (f# 7) (f 7) (e 7) (e 7) / (e 7) (eb 7) (bb 7) (bb 7) / (c 7) (f# 7) (f# 7) (f 7) /
(b maj7) (bb m7) (a 7) (a 7) / (g# maj7) (d 7) (c# 7) (eb 7) / (g# 7) / (g 7) (g 7) (f# 7) (f 7) /
(b 7) (eb 7) (d 7) (d 7) / (c# 7) (g 7) (f# 7) (f# 7) / (d 7) (d 7) (g maj7) (c# 7) / (c 7) (b 7)))
#|
_____________________________________________________________________________________
|G#7       G7        |F#7  F7   E7        |E7   Eb7  Bb7       |C    F#7  F#7  F7   |
|____________________|____________________|____________________|____________________|
|B    Bbm7 A7        |G#   D7   C#7  Eb7  |G#7       G#7       |G7        F#7  F7   |
|____________________|____________________|____________________|____________________|
|B7   Eb7  D7        |C#7  G7   F#7       |D7        G    C#7  |C7        B7        |
_____________________________________________________________________________________
|#
(setf goodbyegrammar (make-instance 'steedmangrammar 
          :rulelist (list rule-I rule-I7 rule-Im7 
                          rule-V7-I7 rule-V7-Im7 rule-IIm7-V7 
                          rule-IIb7-I7 rule-IIb-I rule-IIb-Im7)))


#|

(GridLabel (InitGridWithBars *goodbye-changes1*))

;In this grid, every label has the same 1 beat duration, and contains two elements: root + quality of the chord
(GridLabelBeat (InitGridWithBars *goodbye-changes1*))
(GridLabelBeat (rewrite goodbyegrammar (InitGridWithBars *goodbye-changes*) 10))

;EXAMPLE # 10 "Goodbye" 
;======================

(progn (setf impro nil)
(pgmout 4 1)       ; channel 1 = piano voicings "4 - Electric Piano 1"
(pgmout 78 2)      ; channel 2 = bass "78 - Whistle"      
(pgmout 75 11)     ; channel 11 = flute "75" (solo by Bernard Lubat)
(ctrlchg 7 110 1) (ctrlchg 7 127 2) (ctrlchg 7 127 11)
(let* ((newlabels10 (GridLabelBeat (rewrite goodbyegrammar (InitGridWithBars *goodbye-changes1*) 3)))
       (drumpart (loop for i from 1 to (floor (length newlabels10) 8) collect roof)))             ; roof duration = 8 beats (2 bars)
  (setf impro (merger (beats->chseq (thread-Beats (ImprovizeOnHarmGrid oracle10 (length newlabels10) newlabels10)) 779 0)
                      (beats->chseq (make-beat-list drumpart) 779 0)))
  (play impro))
)

(Stop-Player *general-player*)

|#


;From EXAMPLE # 11:
(setf *israel-changes* '(
(d m7) / (d m7) / (d m7) / (d 7) / (g m7) / (c 7) / (f maj7) / (bb maj7) / (e m7) / (a 7) / (d m7) / (a 7)))







#|
;VERSION DE STEEDMAN AVEC PROCEDURE INDEPENDANTE 'divideduration'
;================================================================

;- application d'une regle qui choisit au lieu d'etre exhaustif

(defmethod rewrite ((self steedmangrammar) (grid list) (n integer))
   (loop for i from n downto 1
         do (let* ((PossibleRules (loop for r in (rulelist self)
                                        when (loop for l = grid then (cdr l) until (null l)
                                                   when (eligible-rule? r l (threshold self))
                                                   return t)
                                        collect r))
                   (ChosenRule (nth (random (length PossibleRules)) PossibleRules))
                   (ResultingGrids (ApplyRule ChosenRule grid)))
              (pushr (list ChosenRule grid) (lastderivation self))
              (when ChosenRule (setf grid (ApplyRule ChosenRule grid))))
         finally return grid))

(defmethod AdaptRule ((r substitution) oldgridportion)
   (let* ((newrightpart (AdaptRootLabels (rightpart r) (FindRoot (leftpart r) oldgridportion)))
          (newgridportion (loop for x in newrightpart
                                collect (make-instance 'jazzchord :root (first x) :quality (if (null (cdr x)) 'maj7 (second x))))))
     (if (= (length (leftpart r)) 1)
       (loop for x in newgridportion do (setf (nbeats x) (/ (nbeats (first oldgridportion)) (length newgridportion))))
       (loop for x in newgridportion for y in oldgridportion
             do (setf (nbeats x) (nbeats y))
             if (eq (root x) (root y)) do (setf (dividable x) nil) else do (setf (marked x) t)))
     newgridportion))


;- application d'une regle qui donne liste exhaustive

(defmethod rewrite ((self steedmangrammar) (grid list) (n integer))
   (setf (lastderivation self) nil)
   (loop for i from n downto 1
         do (let* ((PossibleRules (loop for r in (rulelist self) for res = (ApplyRule r grid)
                                        when res collect (list r res)))
                   (ChosenRule (nth (random (length PossibleRules)) PossibleRules)))
              (when ChosenRule (pushr (list (first ChosenRule) grid) (lastderivation self))
                    (setf grid (nth (random (length (second ChosenRule))) (second ChosenRule)))))
         finally return grid))

(defmethod divideduration ((self steedmangrammar) jazzchordlist)
   (loop for x in jazzchordlist
         append (let ((divlist (dividenumber (nbeats x) (threshold self))))
                  (loop for l = (nth (random (length divlist)) divlist) then (cdr l) until (null l)
                        collect (make-instance 'jazzchord :root (root x) :nbeats (first l)
                                               :quality (if (and (cdr l) (eq (quality x) '7)) 'maj7 (quality x)))))))

(defun dividenumber (n threshold)
   (if (<= n threshold) (list (list n))
       (cons (list n) (let ((res-for-n/2 (dividenumber (/ n 2) threshold))) 
                        (loop for x in res-for-n/2 append (loop for y in res-for-n/2 collect (append x y)))))))

;essai foireux avec pourcentage:

(defmethod divideduration ((self steedmangrammar) jazzchordlist &optional percent)
   (let ((CadentialSeq (loop for ltmp = jazzchordlist then (cdr ltmp) until (null ltmp)
                             with seq = nil and res = nil
                             do (pushr (first ltmp) seq)
                             when (eq (quality (first ltmp)) '7) do (progn (pushr seq res) (setf seq nil))
                             finally return (progn (when seq (pushr seq res)) res))))
     (loop for seq in CadentialSeq for n = (- (length seq) (round (/ (* (length seq) percent) 100)))
           append (append (nthcar n seq)
                          (loop for x in (nthcdr n seq) 
                                append (let ((divlist (dividenumber (nbeats x) (threshold self))))
                                         (loop for l = (nth (random (length divlist)) divlist) then (cdr l) until (null l)
                                               collect (make-instance 'jazzchord :root (root x) :nbeats (first l)
                                                                      :quality (if (and (cdr l) (eq (quality x) '7)) 'maj7 
                                                                                   (quality x))))))))))

(setf grid2 (InitGridWithBars '((g 7) / (c 7))))
(PrintLabelList (GridLabel (rewrite s2 (divideduration s2 grid2 100) 20)))

|#








