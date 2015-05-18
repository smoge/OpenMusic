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
    (list corres-states corres-obs invers-states invers-obs)
    )
)
        
;We convert the whole sequence into a list of indexes-observations
;the distance function is chosen so that proba of occurence and distance are combined.

(defun convert-seq (sequence correspondance model) ;sequence being a beatlist
  (let ((new-seq (collect-midi sequence))
        (invers-obs (elt correspondance 3))
        )
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
      (- 0.0 5)
    (log number)
    )
)


;Viterbi algorithm : both to recognize and for generation
;For recognition : use real observations and maximize the likelihood
;For generation : use incomplete observations (= the substitution hash-table) to generate a sequence of real chord labels

(defun viterbi (model sequence incomplete-obs &optional start end) ;if we only know the theorical labels, incomplete-obs is true
"for chord recognition of a (beat-list) sequence, use (model sequence nil), for generation use (model sequence t start-state arrival-state)"
(let ((obs-or-subs))
  (if incomplete-obs (setf obs-or-subs (subs model))
    (setf obs-or-subs (observations model)))
  (let* ((N-states (hash-table-count (ini-dis model)))
         (N-obs (hash-table-count obs-or-subs))
      
         (maxi)
         (maxi_ant)
         (corres (correspondance (transitions model) (ini-dis model) obs-or-subs))
         (obs-seq (convert-seq sequence corres model))
         (sta-seq)
         
         (l (length obs-seq))
         
         (vit (make-array (list N-states l) :initial-element 0)) 
; vit(n,t) = max proba of a sequence of length t+1 finishing on n 
         (ant (make-array (list N-states l) :initial-element 0)) 
; ant(n,t) argmax between the states preceding n
      
         (i-d (ini-dis model))
         (obs obs-or-subs)
         (tra (transitions model))
      )
    
    (print "let bim")
;proba calculations
    (if start
        (setf (aref vit (gethash start (third corres)) 0) 1.0)
      
      (loop for i from 0 to (- N-States 1)   
            do 
            (setf (aref vit i 0) (logarithme (* (c-i-d corres i-d i) (c-obs corres obs i (elt obs-seq 0)))));vit(xi,0) = i-d(xi)*obs(y0 given xi) ;update : logarithme
      
            )
      )
    (print "ini bim")
    
  (loop for n from 1 to (- l 1)
        do (let ((y (elt obs-seq n))) 

        ;(format t "loop sur obs no ~a~%" n) 
        (loop for i from 0 to (- N-States 1)
              do (setf maxi (- 0.0 9000))
              (loop for p from 0 to (- N-states 1) 
                    do 
                         (if (< maxi  (+ (logarithme (c-obs corres obs i y)) (+ (logarithme (c-tra corres tra p i)) (aref vit p (- n 1)))))
                                (progn 
                                  
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
  (if end
      (progn 
        (setf maxi (aref vit (gethash end (third corres)) (- l 1)))
        (set maxi_ant (gethash end (third corres))))

    (loop for i from 0 to (- N-States 1)
          do (if (< maxi (aref vit i (- l 1)))
                 (progn 
                   (setf maxi (aref vit i (- l 1)))
                   (setf maxi_ant i))
               )
          
          )
    )
  ;(format t "arrival point computed : ~a~%" maxi_ant)


;finding the path
  (setf sta-seq (cons maxi_ant nil))
  (setf maxi_ant (aref ant (- N-states 1) (- l 1)))
  (loop for n from (- l 2) downto 0
        do (push maxi_ant sta-seq)
           (setf maxi_ant (aref ant maxi_ant n))
        
            
  )
  (print sta-seq)
(format *om-stream* "path found !~%~%")
  ;(setf sta-seq (reverse sta-seq))
  (loop for s from 0 to (- (length sta-seq) 1)
        do (setf (elt sta-seq s) (gethash (elt sta-seq s) (elt corres 0)))
        ;(format t "chord recognized for the beat ~a : ~a (with code ~a)~%" s (unchordcode (elt sta-seq s)) (elt sta-seq s) )
        )

(list sta-seq maxi)

)))

(setf hmmodel (midi-to-learn "/Users/florianedardard/Documents/DERUSH\ UZESTE/Dicidenbas.3.16.2012-12-3-37-2oracles-acc+solo.DATA1accords+CHIF-1.mid"))

(viterbi hmmodel seq1 t 0 3)


;testing recognition with permutation and "sous-mots" of the learnt sequence



(defun sous-mot-permut (sequence indexes)
"takes a sequence and a list of indexes (with order) and returns the indexes of that sequence made as a list"
(cond
 ((null indexes) nil)
 (t (cons (elt sequence (car indexes)) (sous-mot-permut sequence (rest indexes))))
 )
)


(defun test-ssmotperm (beatlist indexes) ; directement la beatlist pour des erreurs de lecture de fichier midi 
  (let* ((seq-test (sous-mot-permut (first beatlist) indexes))
        (results)
        (compare (collect-midi seq-test))
        (global-distance 0)
        (dis 0)
        )
        
    
    (setf results (first (viterbi hmmodel seq-test nil)))
   
   
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
