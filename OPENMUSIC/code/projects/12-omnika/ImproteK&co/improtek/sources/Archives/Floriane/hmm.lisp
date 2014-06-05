
(in-package :om)


;HIDDEN MARKOV MODELS
;-------------------------

(defclass* hmm ()
  (
   (initial-distribution :initform (make-hash-table :test 'equal) :accessor ini-dis :initarg :ini-dis) ;initial state probability distribution chord1 (label i) ----> proba
   (observations :initform (make-hash-table :test 'equal) :accessor observations :initarg :observations) ;observation symbol probability (theorical label, notes ) = (obs i) ---> (label j proba) list 
   (transitions :initform (make-hash-table :test 'equal) :accessor transitions :initarg :transitions) ;chord transition (label i label j) ----> proba
   (substitutions :initform (make-hash-table :test 'equal) :accessor subs :initarg :subs) ; (theorical label) --> (label proba)

   (notes-happ :initform (make-hash-table :test 'equal) :accessor notes-happ :initarg :notes-happ) ;learnt instances of notes that happened (theorical label) ---> (notes proba) list
   
   (dictionnaire-voicings :initform (make-hash-table :test 'equal) :accessor dico :initarg :dico) ;instances of the chords as in channel 15 (label) -> (beat) list
   (dictionnaire-solos :initform (make-hash-table :test 'equal) :accessor dico-solo :initarg :dico-solo) ;real instances of the chords as played by musician (real label) ->
   
   (emissions :initform (make-hash-table :test 'equal) :accessor emissions :initarg :emissions) ;label i --> (obs proba) list
   ))





;HMM BUILDING AND LEARNING PHASE
;-------------------------------


;Extracting meaningful data : We have a midi file, with grid labels on channel 16 (= theorical chord labels), voicings played on channel 1 (= notes), voicing labels on channel 15 (post-labeling by musical analysis -> real chord labels)
;We first build a beatlist with midi-to-beatlist
;We collect the info into a list of triplets (notes, harmtheo, harmreel)

(defun collect-Midi (m-list)
  (let ((result ()))
    (dolist (m m-list)
      (push (list (MidiSet m) (Harmlabel m)) result))
    (nreverse result)
    )
  )



(defun convertbeat (m-list) 
"converts (beat harmlabel-grid) to notes present and real harm labels if on canal 15
                             (12 elts list : 0 if absent, 1 if present) -> binary code between 0 and 4096"          
  (let ((notes (list 0 0 0 0 0 0 0 0 0 0 0 0))
        (harmreel (list 0 0 0 0 0 0 0 0 0 0 0 0))
        (harmtheo (second m-list))
        (midi 0)
        )
    (dolist (m (first m-list))
      (setf midi (mod (abs (first m)) 12))
      (if (= (elt m 4) 15) 
          (if (= (elt harmreel midi) 0)
               (setf (elt harmreel midi) 1))
          (if (= (elt notes midi) 0)
               (setf (elt notes midi) 1))
          )
       )
    (setf notes (chordcode notes)) ;converting lists into number between 0 and 4096
    (setf harmreel (chordcode harmreel))  
    (list notes harmtheo harmreel)
))


(defun chordcode (note-list)
"transforms a 12-uplet in (0,1) to the binary code -> number between 0 and 4096"
  (let ((result 0))
    (loop for i from 0 to 11
          do (if (> (elt note-list i) 0)
                 (setf result (+ result (expt 2 i)))) ;binary code
    )
    result
  )
)



;Subsidiary functions to update the model


(defun collect-15-and-other (beat) ;extract information with rythm to add it in the dictionnary
  (let ((result15)
        (resultother)
        )
    (dolist (m (MidiSet beat))
        (if (equal (elt m 4) 15)
            (push m result15)
          (push m resultother)
          )
        )
    (setf result15 (reverse result15))
    (setf resultother (reverse resultother))
    (list result15 resultother)
    )
  )



(defun lookup-and-add (liste x)
"lookup-and-add liste x where liste is an ordered (event nb_of_occurences) list adds the event x to liste"
  (cond 
   ((null liste) (cons (list x 1) nil))
   ((null (rest liste)) (if (equal (first (car liste)) x)
          (cons (list x (+ 1 (second (car liste)))) (rest liste))
        (cons (car liste) (cons (list x 1) nil))))
   (t (if (equal (first (car liste)) x)
          (cons (list x (+ 1 (second (car liste)))) (rest liste))
        (if (equal (first (second liste)) x)
            (if (< (second (first liste)) (+ 1 (second (second liste))))
                (cons (list x (+ 1 (second (second liste)))) 
                      (cons (first liste) (nthcdr 2 liste)))
              (cons (first liste)
                    (cons (list x (+ 1 (second (second liste)))) (nthcdr 2 liste))))
          
          (cons (car liste) (lookup-and-add (rest liste) x))

        )
      )
   )
   )
  )



(defun somme (list) ;list of (event nb_of_occurences)
  (let ((s 0))
    (dolist (x list)
       (setf s (+ s (second x))))
s
))

(defun make-proba-list (list)
"transforms a list of events into a probability distribution"
  (let ((new-list)
        (s (somme list)))
    (dolist (x list)
      (push (list (first x) (* (second x) (/ 1.0 s))) new-list))
    (reverse new-list)
))



(defun make-proba-hmm (model long)
"normalization of the different elements of an hmm, with the length of the learnt sequence"  
  (loop for key being the hash-keys of (transitions model)
        do 
        (setf (gethash key (transitions model)) (* (/ 1.0 (gethash (first key) (ini-dis model))) (gethash key (transitions model))))
        )

  (print "transitions normalized")
  (loop for key being the hash-keys of (observations model)
        do 
        (setf (gethash key (observations model)) (make-proba-list (gethash key (observations model))))
            )
  (print "observations normalized")
  (loop for key being the hash-keys of (emissions model)
        do 
        (setf (gethash key (emissions model)) (make-proba-list (gethash key (emissions model))))
        )
  (print "emissions normalized")
  (loop for key being the hash-keys of (subs model)
        do 
        (setf (gethash key (subs model)) (make-proba-list (gethash key (subs model))))
        )      
  (print "substitutions normalized")
  (loop for key being the hash-keys of (ini-dis model)
        do 
        (setf (gethash key (ini-dis model)) (* (/ 1.0 long) (gethash key (ini-dis model))))
        )
   (print "ini-dis normalized")
   (loop for key being the hash-keys of (notes-happ model)
        do 
        (setf (gethash key (notes-happ model)) (make-proba-list (gethash key (notes-happ model))))
    )
   (print "notes-happ normalized")
   (loop for key being the hash-keys of (dico model)
        do 
        (setf (gethash key (dico model)) (make-proba-list (gethash key (dico model))))
    )
   (print "dico normalized")
   (loop for key being the hash-keys of (dico-solo model)
        do 
        (setf (gethash key (dico-solo model)) (make-proba-list (gethash key (dico-solo model))))
    )
   (print "dico-solo normalized")
   
model
)


;Learning and building the model 


(defun midi-to-learn (path)
"builds a hmm learning from specified file (with chord labels)"
  (let* ((data (midi-to-beatlist path))
        (midi (first data))
        (harmreel ())
        (model (make-instance 'hmm))
        (precedent)
        (leng (length midi))
        )
    
    (setf midi (collect-midi midi))
     
    (loop for beat from 0 to (- leng 1)
          
          do (format *om-stream* "beat processing ~a~%" beat)
          
          (setf (elt midi beat) (convertbeat (elt midi beat)))
          (format *om-stream* "on lit l'element ~a~%" (elt midi beat))
;midi contains now the (notes, theoric harmlabel, real harmlabel) of each beat
          
;transitions update
          (if (not precedent)
              (setf precedent (third (elt midi beat)))
            (progn
             
              (if (gethash (list precedent (third (elt midi beat))) (transitions model))        
                 (progn
                  
                  (incf (gethash (list precedent (third (elt midi beat))) (transitions model))))
               (progn
                (setf (gethash (list precedent (third (elt midi beat))) (transitions model)) 1)))
            
             (format *om-stream* "transition treated : ~a ---> ~a~%" precedent (third (elt midi beat)))
              (setf precedent (third (elt midi beat)))
             )
            )
            
;ini-dis update
          (if (gethash (third (elt midi beat)) (ini-dis model))
              (incf (gethash (third (elt midi beat)) (ini-dis model)))
              (setf (gethash (third (elt midi beat)) (ini-dis model)) 1))
          (format *om-stream* "ini-dis treated : ~a ---> ~a~%" (third (elt midi beat))  (gethash (third (elt midi beat)) (ini-dis model)))
;observations update
         (let ((pos-states (gethash (list (first (elt midi beat)) (second (elt midi beat))) (observations model))))
           
           (format *om-stream* "obs treated : ~a ---> ~a~%" (list (first (elt midi beat)) (second (elt midi beat))) (lookup-and-add pos-states (third (elt midi beat))))
           (setf (gethash (list (first (elt midi beat)) (second (elt midi beat))) (observations model)) (lookup-and-add pos-states (third (elt midi beat))))
         
         )
;notes-happened update
         (let ((pos-notes (gethash (second (elt midi beat)) (notes-happ model)))
               (new-pos-notes ()))
           (setf new-pos-notes (lookup-and-add pos-notes (first (elt midi beat))))
           (setf (gethash (second (elt midi beat)) (notes-happ model)) new-pos-notes)

           (format *om-stream* "note instance treated ~a ---> ~a~%" (second (elt midi beat)) new-pos-notes)
           )
;substitutions update
         (let ((pos-chords (gethash (second (elt midi beat)) (subs model)))
               (new-pos-chords ()))
           (setf new-pos-chords (lookup-and-add pos-chords (third (elt midi beat))))
           (setf (gethash (second (elt midi beat)) (subs model)) new-pos-chords)
           (format *om-stream* "substitution treated ~a ---> ~a~%" (second (elt midi beat)) (unchordcode (third (elt midi beat))))
           )

;emission update
         (let ((pos-obs (gethash (third (elt midi beat)) (emissions model)))
               (new-pos-obs ()))
           (setf new-pos-obs (lookup-and-add pos-obs (list (first (elt midi beat)) (second (elt midi beat)))))
           
           (setf (gethash (third (elt midi beat)) (emissions model)) new-pos-obs)
           (format *om-stream* "emission treated : ~a (code ~a) ---> ~a~%" (unchordcode (third (elt midi beat))) (third (elt midi beat)) new-pos-obs)
     
         )
         
;dictionnaire-voicings & dico-solo update
         (let ((instance (elt (first data) beat))
               (pos-ins (gethash (third (elt midi beat)) (dico model)))
               (pos-ins-solo (gethash (third (elt midi beat)) (dico-solo model)))
               (instance-15)
               (instance-solo)
               )
           (setf instance (collect-15-and-other instance))
           (setf instance-15 (first instance))
           (setf instance-solo (second instance))
           (setf (gethash (third (elt midi beat)) (dico model)) (lookup-and-add pos-ins (list instance-15 (second data))))
           (setf (gethash (third (elt midi beat)) (dico-solo model)) (lookup-and-add pos-ins-solo (list instance-solo (second data))))
           (format *om-stream* "dictionnary updated ~%~%")
           )



         )
           
;normalization
    (print (transitions model))
    (print (ini-dis model))
    (print (observations model))
    (print (notes-happ model))
    (print (subs model))
    (print (emissions model))
    (print (dico model))
    (print (dico-solo model))
    (print leng)
    (setf model (make-proba-hmm model leng))
    )
  )

  






; TESTS 
;--------------------------

;D'abord rentrer dans data la beatlist extraite du fichier
;lancer test-ssmotperm sur data et une liste d'indices de son choix



;(setf midi (midi-to-beatlist "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.16.2012-12-3-37-2oracles-acc+solo.DATA1accords+CHIF-1.mid"))
;(setf midi (first midi))

;(setf mididi (collect-midi (first midi)))



;(setf data (midi-to-beatlist "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.16.2012-12-3-37-2oracles-acc+solo.DATA1accords+CHIF-1.mid"))






;(setf reconnus (viterbi hmmodel (first (midi-to-beatlist "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas19nov2011/Dicidenbas.3.16.2012-12-3-37-2oracles-acc+solo.DATA1accords+CHIF-2.mid" )) t))

;(setf seq1 (first (midi-to-beatlist "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.15.2012-18-32-5-solo->acc.DATA4accords-1.mid")))
;(length seq1)


;The objective is to recognize chords from a midi sequence with a hmm

;First we decide to which known observation each beat ressembles the most : we examine the melodic content and compare them both (more or less like melobeats)

(defun unchordcode (code)
"transforms a binary code of a chord into the set of notes from 0 to 11"
  (let ((notes)
        (temp code))
    (loop for i from 11 downto 0 
          do
          (if (> (- temp (expt 2 i)) 0)
              (progn
                (push i notes)
                (setf temp (- temp (expt 2 i))))
            )
          
          )
    notes
    ))

(defun distance (notes1 notes2)
"defines a notion of distance between two ordered sets of notes : notes added and notes omitted"
(cond 
 ((null notes1) 
  ;(format t "~a were added in second set ~%" notes2) 
  (length notes2))
 ((null notes2) (length notes1))
 ((< (car notes1) (car notes2)) 
  ;(format t "~a present in first and absent in second ~%" (car notes1)) 
  (+ 1 (distance (rest notes1) notes2)))
 ((> (car notes1) (car notes2)) 
  ;(format t "~a absent in first and present in second ~%" (car notes2)) 
  (+ 1 (distance notes1 (rest notes2))))
 (t (distance (rest notes1) (rest notes2)))
 )
)

;We will build an array for the Viterbi algorithm, so we convert each known observation into an index, and each state to another index 

(defun correspondance (trans i-d obs-or-subs)
 "builds a correspondance : hash-table (number -> real state), hash-table (number -> real obs), hash-table (real state -> number), hash-table (real obs ->  number)"
  (let ((N-states (hash-table-count i-d))
       (N-obs (hash-table-count obs-or-subs))
       
       (corres-states (make-hash-table :test 'equal))
       (corres-obs (make-hash-table :test 'equal))
       (invers-states (make-hash-table :test 'equal))
       (invers-obs (make-hash-table :test 'equal))
       (index-sta 0)
       (index-obs 0)
       )

    (loop for key being the hash-keys of i-d
          do (setf (gethash index-sta corres-states) key)
          (setf (gethash key invers-states) index-sta)
          (incf index-sta)
          )
    (loop for key being the hash-keys of obs-or-subs
          do (setf (gethash index-obs corres-obs) key)
          (setf (gethash key invers-obs) index-obs)
          (incf index-obs)
          )
    (if (or (> index-sta N-states) (> index-obs N-obs))
        (print "error suspected")
      (print "correspondance established")
      )
    (loop for key being the hash-keys of invers-obs
          do (format *om-stream* "~a ---> ~a~%" key (gethash key invers-obs))
          )
    (loop for key being the hash-keys of corres-obs
          do (format *om-stream* "~a ---> ~a~%" key (gethash key corres-obs))
          )
 (loop for key being the hash-keys of corres-states
          do (format *om-stream* "~a ---> ~a~%" key (gethash key corres-states))
          )
 (loop for key being the hash-keys of invers-states
          do (format *om-stream* "~a ---> ~a~%" key (gethash key invers-states))
          )


    (list corres-states corres-obs invers-states invers-obs)
    )
)
        
;We convert the whole sequence into a list of indexes-observations
;the distance function is chosen so that proba of occurence and distance are combined.

;It can be an incomplete sequence --> used for generation

(defun convert-seq (sequence correspondance model complete-boolean) ;sequence being a beatlist, complete-boolean if sequence is complete : for recognition. complete-boolean is nil for generation.
  (let ((new-seq (collect-midi sequence))
        (invers-obs (elt correspondance 3))
        )
    (if complete-boolean
    
          (loop for beat from 0 to (- (length sequence) 1)
                do (setf (elt new-seq beat) (convertbeat (elt new-seq beat))) ;new-seq becomes a list of triplets (notes harmtheo harmreel)
                (format *om-stream* "~a ---> ~a~%" (list (first (elt new-seq beat)) (second (elt new-seq beat))) (gethash (list (first (elt new-seq beat)) (second (elt new-seq beat))) invers-obs))
                
                (if (gethash (list (first (elt new-seq beat)) (second (elt new-seq beat))) invers-obs)
                    (setf (elt new-seq beat) (gethash (list (first (elt new-seq beat)) (second (elt new-seq beat))) invers-obs))
                  (progn 
                    (format *om-stream* "no observations found for ~a~%" (list (unchordcode (first (elt new-seq beat))) (second (elt new-seq beat))))
;trouver une solution
                    (let* ((note (unchordcode (first (elt new-seq beat))))
                           (chord (second (elt new-seq beat)))
                           (pos-notes (gethash chord (notes-happ model)))
                           (nearest-note (car pos-notes))
                           (dis (* (second nearest-note) (/ 1.0 (expt (distance note (unchordcode (first nearest-note))) 4))))
                           (pos-notes-readable)
                           )
                      (dolist (m pos-notes)
                        (push (list (unchordcode (first m)) (second m)) pos-notes-readable)
                        )
                      (reverse pos-notes-readable)
                      
                      (format *om-stream* "possible matches are ~a~%" pos-notes-readable)
                      (loop for i from 0 to (- (length pos-notes) 1)
                            do (if (< dis (* (second (elt pos-notes i)) (/ 1.0 (expt (distance note (unchordcode (first (elt pos-notes i)))) 4))))
                                   (progn
                                     
                                     (setf nearest-note (elt pos-notes i))
                                     (setf dis (* (second nearest-note) (/ 1.0 (expt (distance note (unchordcode (first nearest-note))) 4))))
                                     
                                     )
                                 
                                 )
                            (format *om-stream* "for the ~a th possibility, there is a distance of ~a and with proba weight : ~a~%" (unchordcode (first (elt pos-notes i))) (distance note (unchordcode (first (elt pos-notes i)))) (* (second (elt pos-notes i)) (/ 1.0 (expt (distance note (unchordcode (first (elt pos-notes i)))) 4))))
                            
                            )
                      (setf (elt new-seq beat) (gethash (list (first nearest-note) chord) invers-obs))
                      (format *om-stream* "correspondace to ~a~%~%" (list (unchordcode (first nearest-note)) chord))
                      )
                    )
                  
                  )
                )


      (loop for beat from 0 to (- (length sequence) 1)
            do (setf (elt new-seq beat) (convertbeat (elt new-seq beat))) ;new-seq becomes a list of triplets (notes harmtheo harmreel)
            (setf (elt new-seq beat) (gethash (second (elt new-seq beat)) invers-obs))
            )
      )
    new-seq
    )
  )


;Subsidiary functions for quicker access to hash-tables tra, i-d and obs

(defun c-i-d (corr i-d i)
  (if (gethash (gethash i (first corr)) i-d)
      (gethash (gethash i (first corr)) i-d)
    0.0)
  )

(defun c-tra (corr tra i j)
  (if (gethash (list (gethash i (first corr)) (gethash j (first corr))) tra)
      (gethash (list (gethash i (first corr)) (gethash j (first corr))) tra)
    0.0
    )
  )
    


(defun c-obs (corr obs i n)
  (let ((sta-pos (gethash (gethash n (second corr)) obs))
        (ini (- 0 1))
        (state (gethash i (first corr)))
        )
  (loop for j from 0 to (- (length sta-pos) 1) while (= ini (- 0 1))
       do (if (equal (first (elt sta-pos j)) state)
            (setf ini j))
        )
  (if (= ini (- 0 1))
      0.0
      (second (elt sta-pos ini))
      )
    
  )
  )



;Bishop's Pattern Recognition and Machine Learning stated that likelihood had to be computed with logarithms :

(defun logarithme (number) ;prolongation of log to 0 (= -5)
  (if (= number 0.0)
      (- 0.0 15)
    (log number)
    )
)



;Viterbi algorithm : both to recognize and for generation
;For recognition : use real observations and maximize the likelihood
;For generation : use incomplete observations (= the substitution hash-table) to generate a sequence of real chord label



(defun viterbi (model sequence complete-obs &optional (start 0) (end 10000)) ;if we only know the theorical labels, incomplete-obs is true
"for chord recognition of a (beat-list) sequence, use (model sequence nil), for generation use (model sequence t start-state arrival-state)"
(let ((obs-or-subs))
  (if complete-obs (setf obs-or-subs (observations model))
    (setf obs-or-subs (subs model)))
  (let* ((N-states (hash-table-count (ini-dis model)))
         (N-obs (hash-table-count obs-or-subs))
      
         (maxi)
         (maxi_ant)
         (corres (correspondance (transitions model) (ini-dis model) obs-or-subs))
         (obs-seq (convert-seq sequence corres model complete-obs))
         (sta-seq)
         (harmtheo)
         
         (l (length obs-seq))
         
         (vit (make-array (list N-states l) :initial-element (- 0.0 5))) 
; vit(n,t) = max proba of a sequence of length t+1 finishing on n 
         (ant (make-array (list N-states l) :initial-element 0)) 
; ant(n,t) argmax between the states preceding n
      
         (i-d (ini-dis model))
         (obs obs-or-subs)
         (tra (transitions model))
         (new-end (min end l))
      )
    

;proba calculations
    (if (> start 0)
        (progn 
          (setf (aref vit (gethash start (third corres)) 0) 0.0)
          (print "start")
          (print (gethash start (third corres))))
      
      (loop for i from 0 to (- N-States 1)   
            do 
            (setf (aref vit i 0) (logarithme (* (c-i-d corres i-d i) (c-obs corres obs i (elt obs-seq 0)))));vit(xi,0) = i-d(xi)*obs(y0 given xi) ;update : logarithme
      
            )
      )

    
  (loop for n from (+ 1 start) to (- new-end 1)
        do (let ((y (elt obs-seq n))) 
             (print y)

             (loop for i from 0 to (- N-States 1)
                   do (setf maxi (- 0.0 9000))
                   (print "setf maxi")
                   (print maxi)
                   (loop for p from 0 to (- N-states 1) 
                         do 
                         (if (< maxi  (+ (logarithme (c-obs corres obs i y)) (+ (logarithme (c-tra corres tra p i)) (aref vit p (- n 1)))))
                             (progn 
                               (print (+ (logarithme (c-obs corres obs i y)) (+ (logarithme (c-tra corres tra p i)) (aref vit p (- n 1)))))
                               (print (logarithme (c-obs corres obs i y)))
                               (print (+ (logarithme (c-tra corres tra p i)) (aref vit p (- n 1))))
                               (setf maxi (+ (logarithme (c-obs corres obs i y)) (+ (logarithme (c-tra corres tra p i)) (aref vit p (- n 1)))))
                               (setf (aref vit i n) maxi )
                               (setf (aref ant i n) p)
                                  ;(format t " vit ~a ~a = ~a et ant ~a ~a = ~a" i n (aref vit i n) i n (aref ant i n))
                                  ;(format t "on avait des multiplications par ~a et ~a et ~a" (c-obs corres obs i y) (c-tra corres tra p i) (aref vit p (- n 1)))
                               )
                           )
                         )
              ;(format t " vit ~a ~a = ~a et ant ~a ~a = ~a~%" i n (aref vit i n) i n (aref ant i n))
              )
              
        )
       
        )
              
       
 
;sequence calculation
  (setf maxi (- 0.0 9000))
;compute arrival point and probabiblity

  (if (< end 10000)
      (progn 
        (setf maxi (aref vit (elt obs-seq end) (- new-end 1)))
        (setf maxi_ant (elt obs-seq new-end))
        (setf harmtheo (elt obs-seq end))
        (setf maxi_ant (list maxi_ant harmtheo)))


    (loop for i from 0 to (- N-States 1)
          do (if (< maxi (aref vit i (- l 1)))
                 (progn 
                   (setf maxi (aref vit i (- l 1)))
                   (setf maxi_ant i)
                   (setf harmtheo (elt obs-seq i)))
                   (setf maxi_ant (list maxi_ant harmtheo))
               )
          
          )
    )
  ;(format t "arrival point computed : ~a~%" maxi_ant)


 


;finding the path
  (print "on va chercher le chemin")
  (setf sta-seq (list maxi_ant))
  (setf maxi_ant (list (aref ant (- N-states 1) (- new-end 1)) (elt obs-seq (- new-end 1)) ))
  (loop for n from (- new-end 2) downto 0
        do (push maxi_ant sta-seq)
           (setf maxi_ant (list (aref ant (first maxi_ant) n) (elt obs-seq n) ))
        
            
  )
  (print sta-seq)
(format *om-stream* "path found !~%~%")
  ;(setf sta-seq (reverse sta-seq))
  (loop for s from 0 to (- (length sta-seq) 1)
        do (setf (elt sta-seq s) (list (gethash (first (elt sta-seq s)) (elt corres 0)) (gethash (second (elt sta-seq s)) (elt corres 1))))
        ;(format t "chord recognized for the beat ~a : ~a (with code ~a)~%" s (unchordcode (elt sta-seq s)) (elt sta-seq s) )
        )

(list sta-seq maxi)

)))


;testing recognition with permutation and "sous-mots" of the learnt sequence

(defun sous-mot-permut (sequence indexes)
"takes a sequence and a list of indexes (with order) and returns the indexes of that sequence made as a list"
(cond
 ((null indexes) nil)
 (t (cons (elt sequence (car indexes)) (sous-mot-permut sequence (rest indexes))))
 )
)


(defun test-ssmotperm (model beatlist indexes) ; directly the beatlist to avoid midi file opening errors
  (let* ((seq-test (sous-mot-permut (first beatlist) indexes))
        (results)
        (compare (collect-midi seq-test))
        (global-distance 0)
        (dis 0)
        )
        
    
    (setf results (first (viterbi model seq-test t)))
   
   
    (loop for s from 0 to (- (length results) 1)
        do (format *om-stream* "~& chord recognized for the beat ~a : ~a (with code ~a)~%" s (unchordcode (elt results s)) (elt results s) )
        (format *om-stream* "the expected chord was ~a (with code ~a) ~%" (unchordcode (third (convertbeat (elt compare s)))) (third (convertbeat (elt compare s))))
        (setf dis (distance (unchordcode (elt results s)) (unchordcode (third (convertbeat (elt compare s))))))
        (format *om-stream* "distance between recognized and expected : ~a~%~%" dis)
        (setf global-distance (+ global-distance dis))
        )
    global-distance
    )
  )




;(setf hmmodel (midi-to-learn "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.16.2012-12-3-37-2oracles-acc+solo.DATA1accords+CHIF-1.mid"))

;(setf blabla (apply-subs hmmodel (first midi) 0 7))



;(test-ssmotperm data (list 0 1 2 3 4 5 6 7 8))

(defun rand-list (liste &optional (no-random nil)) ;if no-random is true, we take the first one
"liste is a list of (event, event probability) with the sum of probabilities equal to 1. rand-list returns a randomly chosen event from liste"
  (let ((somme-inf 0)
        (chosen (/ (random 10000) 10000.0))
        (event 0)
        )
    (if no-random
        (setf event 0)
        (loop for i from 0 to (- (length liste) 1)
              do (setf somme-inf (+ somme-inf (second (elt liste i))))
              (if (> chosen somme-inf)
                  (setf event (+ i 1)))
              
              ))
    (first (elt liste event))
      )
  )





(defun observation-subs (model subs-seq harmlabels)
  (let ((obs-subs-seq (append subs-seq '()))
        (beatdur 0)
        )
        
    
  (loop for i from 0 to (- (length subs-seq) 1)
        do 
        (setf (elt obs-subs-seq i) (rand-list (gethash (elt subs-seq i) (dico model))))
        (setf beatdur (second (elt obs-subs-seq i)))
        ; (rand-list (gethash (elt subs-seq i) (dico-solo model)))
        (setf (elt obs-subs-seq i)  (list (elt harmlabels i) (first (elt obs-subs-seq i))))

        )

  (setf obs-subs-seq (make-beat-list obs-subs-seq beatdur))
 
  (list obs-subs-seq beatdur)

  )
  ) 



(defun apply-subs (model sequence start-index end-index)
"applies substitutions to the sequence from start-index to end-index (excluded as they are constraints points for the substitutions)"
(let* ((modified (viterbi model sequence nil start-index end-index))
      (proba (second modified))
      (modified-beats (mapcar #'first (first modified)))
      (harmlabels (mapcar #'second (first modified)))
      (modif)
      (new-seq)
      )
  (print modified)
  (print modified-beats)
  (setf modified-beats (observation-subs model modified-beats harmlabels))
  (setf new-seq (append (subseq sequence 0 start-index) (first modified-beats) (subseq sequence (+ 1 end-index))))
  (setf new-seq (list new-seq (second modified-beats)))
  new-seq
  )
)



(defun learn-and-substitute (learn-path midibuff-path start-index end-index)
  (let ((model (midi-to-learn learn-path))
        (beatlist (first (midi-to-beatlist midibuff-path)))
        (output))
    (setf output (apply-subs model beatlist start-index end-index))

    )
  )

;(learn-and-substitute "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.16.2012-12-3-37-2oracles-acc+solo.DATA1accords+CHIF-1.mid"
;                      "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.15.2012-18-32-5-solo->acc.DATA4accords-1.mid"
 ;                     0
  ;                    10)





