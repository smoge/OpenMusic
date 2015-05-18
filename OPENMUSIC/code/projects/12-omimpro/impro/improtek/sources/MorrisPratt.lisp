(in-package :om)
;MorrisPratt.lisp
;------------------------------------------------------------------------------------------------------------
;IMPLEMENTATION AND TUTORIAL OF ALGORITHMS INSPIRED BY MORRIS&PRATT (used in Improvizer.lisp for the navigation through an oracle) 
;Jérôme Nika
;Feb 20th 2012
;------------------------------------------------------------------------------------------------------------


;********************************************************************************
;Jerome 26/10/13
;Now in "LoadImprotek"
;(defparameter *print_info_MP* 0) ;Print every step in "Morris&Pratt"
;(defparameter *print_info_find* 0) ;Print every step in "find-prefix-label-match"
;(defparameter *print_info_navig* 0) ;Print every step in "Improvize
;********************************************************************************


;====================================================================== ORIGINAL MORRIS & PRATT ALGORITHM (searching for the whole word) =====================================================================
#|
;====================================
; EXAMPLES ORIGINAL MORRIS&PRATT
;====================================
 
(MorrisPratt '(A Z E R T Y U A Z E R P O) '(A Z E R))

(MorrisPratt '(A Z E R T Y U A Z E R P O) '(H E L L O))

(om-inspect (MorrisPratt_failure '(A B C A B D A)))

(om-inspect (MorrisPratt_failure '(G C A G A G A G)))

(om-inspect (MorrisPratt_failure '(A B A B A B C)))

(om-inspect (MorrisPratt_failure '(A B A B A B C A B A)))




(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(A B C A B D A))))

(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(G C A G A G A G))))

(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(A B A B A B C))))

(om-inspect (nth 1 (MorrisPratt_failure&internalprefixes '(A B A B C A B C A B A B A))))

(gethash 5 (nth 1 (MorrisPratt_failure&internalprefixes '(A B A B A B C A B A))))


|#



;Original Morris&Pratt "failure fonction"
;----------------------------------------
(defmethod MorrisPratt_failure ((l list))
  (setf fail_table (make-hash-table :test #'CompareEvents))
  (let* ((p 0) (i -1))
    (setf (gethash 0 fail_table) -1)
    (loop while (< p (list-length l)) do
          (loop while (and (> i -1) (not (CompareEvents (nth p l) (nth i l)))) do
                 (setf i (gethash i fail_table)))
           (setf (gethash (incf p) fail_table) (incf i)))
    fail_table))


;nth 0 : Morris&Pratt "failure fonction" 
;nth 1 : table lengths_ending_prefixes : table[idx] = list lengths of prefixes ending at idx
;----------------------------------------
(defmethod MorrisPratt_failure&internalprefixes ((l list))
  (setf fail_table (make-hash-table :test #'CompareEvents))
  (setf lengths_ending_prefixes (make-hash-table :test #'CompareEvents))
  (let* ((p 0) (i -1))
    (setf (gethash 0 fail_table) -1)
    (setf (gethash 0 lengths_ending_prefixes) '(1))
    (loop while (< p (list-length l)) do
          ;Failure i
          (loop while (and (> i -1) (not (CompareEvents (nth p l) (nth i l)))) do
                 (setf i (gethash i fail_table)))
          (incf p)
          (incf i)
          (setf (gethash p fail_table) i)
          ;lengths_ending_prefixes i-1
          (if (> p 1)
              (setf (gethash (- p 1) lengths_ending_prefixes) (append (list p) (gethash (- (gethash p fail_table) 1) lengths_ending_prefixes))))

          )
    (list fail_table lengths_ending_prefixes)))



#|
;KNUTH Morris&Pratt "failure fonction"
;----------------------------------------
 (defmethod MorrisPratt_failure ((l list))
  (setf fail_table (make-hash-table :test #'CompareEvents))
  (let* ((p 0) (i -1))
    (setf (gethash 0 fail_table) -1)
    (loop while (< p (list-length l)) do
          (loop while (and (> i -1) (not (CompareEvents (nth p l) (nth i l)))) do
                 (setf i (gethash i fail_table)))
           (incf i)
           (incf p)
           (if (CompareEvents (nth p l) (nth i l))
               (setf (gethash p fail_table) (gethash i fail_table))
             (setf (gethash p fail_table) i)))
    fail_table))
|#






;Original Morris&Pratt algorithm
;--------------------------------
;/!\ Arguments : 1)text/corpus 2)word/motive to search
;Returns the list of idxs where the word is found
;--------------------------------
(defmethod MorrisPratt ((text list) (word list))
  (setf idxs '())
  
  (if (= *print_info_MP* 1) 
      (format *om-stream* "~%++++++++++++++++++++++++++++++++++++MORRIS & PRATT++++++++++++++++++++++++++++++++++++~%++++++++++++++++++++++++++++++++++++Looking for ~a in ~a++++++++++++++++++++++++++++++++++++" word text))
  (let* ((fail (MorrisPratt_failure word)) (i 0) (j 0) (prev_i -1))

    (loop while (< j (list-length text)) do
          (if (= *print_info_MP* 1) (format *om-stream* "~%++  ----> text: j=~D, word: i=~D, (j-i)=~D [[echec(i) = ~D]] <----~%++ Labels = text : ~a / word : ~a ===> equal = ~a~%" 
                  j i (- j i) (gethash i fail) (nth j text) (nth i word) (CompareEvents (nth i word) (nth j text))))

          (if (and (> i -1) (not (CompareEvents (nth i word) (nth j text))))
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  =========Different labels (and i>-1) == ~%"))
                
                
                (loop while (and (> i -1) (not (CompareEvents (nth i word) (nth j text)))) do
                      (if (= *print_info_MP* 1) (format *om-stream* "++  === Still I>-1 and different labels : (text j=~D) != (word i=~D) ie ~a != ~a~%" j i (nth j text) (nth i word)))
                      (setf i (gethash i fail))
                      (if (= *print_info_MP* 1) (format *om-stream* "++  => Failure function : i ->~D ~%" i)))))
          
          (incf i) (incf j) (if (= *print_info_MP* 1) (format *om-stream* "++  j++->~D ,i++->~D~%++ ~%++ ~%" j i))
         
          (if (> i (- (list-length word) 1)) 
              (progn 
                (if (= *print_info_MP* 1) (format *om-stream* "++  >>>>>>> Word found ! Begins at index = ~D<<<<<<<~%++ ~%" (- j i)))
                (push (- j i) idxs)
                (setf i (gethash i fail))
                (if (= *print_info_MP* 1) (format *om-stream* "++  Failure function : i ->~D~%" i))))
          )
    (if (= *print_info_MP* 1) (format *om-stream* "~%++++++++++++++++++++++++++++++++++++END MORRIS & PRATTS++++++++++++++++++++++++++++++++++++~%"))
    idxs))



;========================================================================= MORRIS & PRATT USED IN NAVIGATION : searching for prefixes ========================================================================
#|
;=======================================
;EXAMPLES MORRIS&PRATT FOR PREFIXES
;=======================================

(print-MorrisPratt_prefixes (MorrisPratt_prefixes '(B A B B A B C A B C A B) '(A B C A B A)))

(print-MorrisPratt_prefixes (MorrisPratt_prefixes '(D A A B A B A B A B D A B A B A B) '(A B A B A B C)))


 
;Example with lists of characters
;---------------------------------
  (setf word '(a b c d))
  (setf text '(a b c d a b a a b c))
  (setf resultMP (MorrisPratt_prefixes text word))
  (print-MorrisPratt_prefixes resultMP)

;Example with lists of characters (particular case : some factors in the word are prefixes)
;--------------------------------------------------------------------------------------------
  (setf word '(a b c a b d))
  (setf text '(a b c a b a a b c))
  (setf resultMP (MorrisPratt_prefixes text word))
  (print-MorrisPratt_prefixes resultMP)

;Example with lists of beats
;------------------------------------------
  (setf word (make-beat-list Dicidenbassolo-juil2011_beatsfromfile))
  (setf text (make-beat-list Jaimesolo-juil2011_beatsfromfile))
  (setf resultMP (MorrisPratt_prefixes text word))
  (print-MorrisPratt_prefixes resultMP)


;Example with grids  : /!\ Use "expand_grid" !!! (ex. (f m7 3) -> ( f m7) (f m7) (f m7))
;-----------------------------------------------------------------------------------------
  (setf word (expand_grid (grid Dicidenbas_tune)))
  (setf text (expand_grid (grid CecileMaFille_tune)))
  (setf resultMP (MorrisPratt_prefixes text word))
  (print-MorrisPratt_prefixes resultMP)

 
;Example Improvizer / grid (1)
;-------------------------------
  (setf beat-list3 '(
                     ((g#) ())
                     ((g) ())
                     ((f) ())
                     ((g) ())
                     ((ab) ())
                     ((g) ())
                     ((a) ())
                     ((d) ())
                     ((ab) ())
                     ((g) ())
                     ((a) ())
                     ((g) ())
                     ))
  (setf oracle (NewImprovizer (make-beat-list beat-list3)))
  (setf grid '((ab) (g) (a) (d) (a) (c)))
  (setf resultMP (MorrisPratt_prefixes oracle grid))
  (print-MorrisPratt_prefixes resultMP)


;Example Improvizer / grid (2)
;-------------------------------
 
  (setf tune Dicidenbas_tune)
  (setf beatduration (beatduration tune))
  (setf Improvizer (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
  (setf grid (grid tune))

  (setf resultMP (MorrisPratt_prefixes Improvizer (expand_grid grid)))
  (print-MorrisPratt_prefixes resultMP)

|#

#|
(print-MorrisPratt_prefixes (MorrisPratt_prefixes '(D A A B A B A B A B D A B A B A B) '(A B A B A B C)))

(print-MorrisPratt_prefixes (MorrisPratt_prefixes '(B A B B A B C A B C A B) '(A B C A B A)))
|#

;Looking for prefixes (inspired by Morris&Pratt)
;-----------------------------------------------
;/!\ Arguments : 1)text/corpus 2)word/motive to search
;Returns a 2 elements list :
; (nth 0 ) -> HashTable : prefix_length -> (idxs of the different occurrences)
; (nth 1 ) -> length of the longest prefix
;-----------------------------------------------
(defmethod MorrisPratt_prefixes ((text list) (word list))
  (setf prefix_idx (make-hash-table :test #'equal))
  
  (if (= *print_info_MP* 1) 
      (format *om-stream* "~%++++++++++++++++++++++++++++++++++++MORRIS & PRATT PREFIXES++++++++++++++++++++++++++++++++++++~%++++++++++++++++++++++++++++++++++++Looking for prefixes of ~a in ~a++++++++++++++++++++++++++++++++++++" word text))
  (let* ((resultsMPfailureInternalPrefixes (MorrisPratt_failure&internalprefixes word))
         (fail (nth 0 resultsMPfailureInternalPrefixes))
         (lengthInternalPrefixes (nth 1 resultsMPfailureInternalPrefixes))
         (i 0) 
         (j 0)
         (tmp-length-longest 0))

    (loop while (< j (list-length text)) do
          (if (= *print_info_MP* 1) (format *om-stream* "~%++  ----> text: j=~D, word: i=~D, (j-i)=~D [[echec(i) = ~D]] <----~%++ Labels = text : ~a / word : ~a ===> equal = ~a~%" 
                  j i (- j i) (gethash i fail) (nth j text) (nth i word) (CompareEvents (nth i word) (nth j text))))
          
          (if (and (> i -1) (not (CompareEvents (nth i word) (nth j text))))
    
              ;The labels are different : end of a prefix if the previous were equal
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  =========DIFFERENT LABELS (and i>-1) ==> ~%++ >>>Found prefix, begins at idx=(j-i)= ~D : length = i = ~D<<<~%" (- j i) i))
                (if (> i 0)
                    (progn
                      (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                      (if (> i tmp-length-longest) (setf tmp-length-longest i))
                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      (if (= *print_info_MP* 1) (format *om-stream* "~%+++  ====Looking for internal prefixes...~%"))
                      (let ((jFinPref (- j 1)) (iFinPref (- i 1))) ;; j,i ont été incrémentés -> préfixe arrive un cran avant !!! ET POUR I ??? UTILISER i OU iFinPref ???
                        (loop for i0 from 0 to iFinPref do
                              (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D -> looking for prefixes arriving in i-i0=~D (word)-> j-i0=~D (text) ...+++~%" i0 (- iFinPref i0) (- jFinPref i0)))
                              (loop for l in (gethash (- iFinPref i0) lengthInternalPrefixes) do
                                    (progn
                                      (if (= *print_info_MP* 1) (format *om-stream* "+++ One of length l=~D ! +++~%" l))
                                      (if (not (member (+ (- (- jFinPref l) i0) 1) (gethash l prefix_idx))) (push (+ (- (- jFinPref l) i0) 1) (gethash l prefix_idx))) 
                                      (if (= *print_info_MP* 1)(format *om-stream* "+++  ((>>Found prefix moving backward in the failure function : i0 =~D ---> begins at idx j-i0-l+1= ~D, length l= ~D<<))~%"i0 (+ (- (- jFinPref l) i0) 1) l))
                                      ))))
                      (if (= *print_info_MP* 1) (format *om-stream* "+++  ====  END Looking for internal prefixes...~%"))
                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      ))
               
                (loop while (and (> i -1) (not (CompareEvents (nth i word) (nth j text)))) do
                      (if (= *print_info_MP* 1) (format *om-stream* "++  === Still i>-1 and different labels : (text j=~D) != (word i=~D) ie ~a != ~a~%" j i (nth j text) (nth i word)))
                      (setf i (gethash i fail))
                      (if (= *print_info_MP* 1) (format *om-stream* "++  => Failure function : i ->~D ~%" i))
                      
                      #|
                      (if (> i 0) 
                          (progn 
                            (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                            (if (> i tmp-length-longest) (setf tmp-length-longest i))
                            (if (= *print_info_MP* 1) (format *om-stream* "++  ((>>Found prefix (moving backward in the failure function), begins at idx =(j-i)= ~D : length = i = ~D<<))~%~%" (- j i) i))))
                      |#

                      )
                (if (> i -1) 
                    (if (= *print_info_MP* 1) 
                        (format *om-stream* "++ => New label (word) = ~a ((label text = ~a))~%" (nth i word) (nth j text))))
                
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                #|
                (if (> i 0)
                    (let ((i0 i))
                      (if (= *print_info_MP* 1) (format *om-stream* "+++  ====Keep moving backward to find prefixes...~%"))
                      (setf i0 (gethash i0 fail)) 
                      (loop while (> i0 0) do
                            (progn
                              (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D > 0...~%" i0))
                              (loop for k from i0 downto 1 do (push (- j i0) (gethash k prefix_idx)))
                              (if (= *print_info_MP* 1) (format *om-stream* "+++  ((>>Found prefix (moving MORE backward in the failure function ONCE THE NEXT COMPARISON IDX FOUND) , begins at idx =(j-i0)= ~D : length = i0 = ~D<<))~%" (- j i0) i0))
                              (setf i0 (gethash i0 fail))
                              (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 <- echec(i0)= ~D...~%" i0))))
                      (if (= *print_info_MP* 1) (format *om-stream* "+++  ==== echec(i0=~D) = ~D  <= 0 ->END Keep moving backward to find prefixes...~%" i0 (gethash i0 fail)))))
                |#
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                )
            
            ;The labels are equal...
            (if (= *print_info_MP* 1) (format *om-stream* "++  =========EQUAL LABELS (or i<=-1)~%")))        

          (incf i) (incf j) (if (= *print_info_MP* 1) (format *om-stream* "++  j++->~D ,i++->~D~%++ ~%++ ~%" j i))

          ;Whole word found (original Morris & Pratt)
          (if (> i (- (list-length word) 1))   
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  >>>>>>> Word found ! Begins at index = ~D<<<<<<<~%++  ~%" (- j i)))
                (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                (if (> i tmp-length-longest) (setf tmp-length-longest i))
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (if (= *print_info_MP* 1) (format *om-stream* "~%+++  ====Looking for internal prefixes...~%"))
                (let ((jFinPref (- j 1)) (iFinPref (- i 1))) ;; j,i ont été incrémentés -> préfixe arrive un cran avant !!! ET POUR I ??? UTILISER i OU iFinPref ???
                  (loop for i0 from 0 to iFinPref do
                        (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D -> looking for prefixes arriving in i-i0=~D (word)-> j-i0=~D (text) ...+++~%" i0 (- iFinPref i0) (- jFinPref i0)))
                        (loop for l in (gethash (- iFinPref i0) lengthInternalPrefixes) do
                              (progn
                                (if (= *print_info_MP* 1) (format *om-stream* "+++ One of length l=~D ! +++~%" l))
                                (if (not (member (+ (- (- jFinPref l) i0) 1) (gethash l prefix_idx))) (push (+ (- (- jFinPref l) i0) 1) (gethash l prefix_idx))) 
                                (if (= *print_info_MP* 1)(format *om-stream* "+++  ((>>Found prefix moving backward in the failure function (AFTER WHOLE WORD FOUND) : i0 =~D ---> begins at idx j-i0-l+1= ~D, length l= ~D<<))~%"i0 (+ (- (- jFinPref l) i0) 1) l))
                                ))))
                #|
                (let ((L iFinPref))
                  (loop for p from (+ (- jFinPref L) 1) to jFinPref do
                        (progn
                          (if (= *print_info_MP* 1) (format *om-stream* "~%+++ p = ~D... +++~%" p))
                          (let ((i0 (- p (+ (- jFinPref L) 1))))
                            (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D / echec(i0) = ~D...+++~%" i0 (gethash i0 fail)))
                            (loop while (> (gethash i0 fail) 0) do
                                  (progn
                                    (loop for k from (gethash i0 fail) downto 1 do (if (not (member (+ (- p (gethash i0 fail)) 1) (gethash k prefix_idx))) (push (+ (- p (gethash i0 fail)) 1) (gethash k prefix_idx))))
                                    (if (= *print_info_MP* 1) (format *om-stream* "+++  ((>>Found prefix moving backward in the failure function (AFTER WHOLE WORD FOUND)~%p= ~D / i0 =~D / echec(i0)= ~D ---> begins at idx p - echec(i0) +1= ~D : length = echec(i0) = ~D<<))~%" p i0 (gethash i0 fail) (+ (- p (gethash i0 fail)) 1) (gethash i0 fail)))
                                    (setf i0 (gethash i0 fail))
                                    (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 <- echec(i0)= ~D...~%" i0))))
                            (if (= *print_info_MP* 1) (format *om-stream* "+++ echec(i0=~D) = ~D  <= 0 -> stop going backward in failure function~%" i0 (gethash i0 fail)))))))
                |#
                (if (= *print_info_MP* 1) (format *om-stream* "+++  ====  END Looking for internal prefixes...~%"))
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (setf i (gethash i fail))
                (if (= *print_info_MP* 1) (format *om-stream* "++  Failure function : i ->~D~%" i))
                ))
          
          ;If the end of the text is reached while recognizing a prefix
          (if (and (>= j (list-length text)) (> i 0)) 
              (progn
                  (if (= *print_info_MP* 1) (format *om-stream* "++  =========End of text reached while recognizing a prefix ~%++ >>>Found prefix (end of text), begins at idx = ~D : length ~D<<<~%++ ~%" (- j i) i))
                  (if (> i tmp-length-longest) (setf tmp-length-longest i))
                  (loop for k from i downto 1 do (if (not (member (- j i) (gethash k prefix_idx))) (push (- j i) (gethash k prefix_idx))))
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  (if (= *print_info_MP* 1) (format *om-stream* "~%+++  ====Looking for internal prefixes...~%"))
                  (let ((jFinPref (- j 1)) (iFinPref (- i 1))) ;; j,i ont été incrémentés -> préfixe arrive un cran avant !!! ET POUR I ??? UTILISER i OU iFinPref ???
                    (loop for i0 from 0 to iFinPref do
                          (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D -> looking for prefixes arriving in i-i0=~D (word)-> j-i0=~D (text) ...+++~%" i0 (- iFinPref i0) (- jFinPref i0)))
                          (loop for l in (gethash (- iFinPref i0) lengthInternalPrefixes) do
                                (progn
                                  (if (= *print_info_MP* 1) (format *om-stream* "+++ One of length l=~D ! +++~%" l))
                                  (if (not (member (+ (- (- jFinPref l) i0) 1) (gethash l prefix_idx))) (push (+ (- (- jFinPref l) i0) 1) (gethash l prefix_idx))) 
                                  (if (= *print_info_MP* 1)(format *om-stream* "+++  ((>>Found prefix moving backward in the failure function (AFTER END OF THE TEXT) : i0 =~D ---> begins at idx j-i0-l+1= ~D : length l= ~D<<))~%"i0 (+ (- (- jFinPref l) i0) 1) l))
                                  ))))
                  #|
                  (let ((L i)) 
                    (loop for p from (+ (- jFinPref L) 1) to jFinPref do
                          (progn
                            (if (= *print_info_MP* 1) (format *om-stream* "~%+++ p = ~D...+++~%" p))
                            (let ((i0 (- p (+ (- jFinPref L) 1))))
                              (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D / echec(i0) = ~D...+++~%" i0 (gethash i0 fail)))
                              (loop while (> (gethash i0 fail) 0) do
                                    (progn
                                      (loop for k from (gethash i0 fail) downto 1 do (if (not (member (+ (- p (gethash i0 fail)) 1) (gethash k prefix_idx))) (push (+ (- p (gethash i0 fail)) 1) (gethash k prefix_idx))))
                                      (if (= *print_info_MP* 1) (format *om-stream* "+++  ((>>Found prefix moving backward in the failure function (AFTER END OF THE TEXT)~%p= ~D / i0 =~D / echec(i0)= ~D ---> begins at idx p - echec(i0) +1= ~D : length = echec(i0) = ~D<<))~%" p i0 (gethash i0 fail) (+ (- p (gethash i0 fail)) 1) (gethash i0 fail)))
                                      (setf i0 (gethash i0 fail))
                                      (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 <- echec(i0)= ~D...~%" i0))))
                              (if (= *print_info_MP* 1) (format *om-stream* "+++ echec(i0=~D) = ~D  <= 0  -> stop going backward in failure function~%" i0 (gethash i0 fail)))))))
                  |#
                  (if (= *print_info_MP* 1) (format *om-stream* "+++  ====  END Looking for internal prefixes...~%"))
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23/10/13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;))
                  )))
    (if (= *print_info_MP* 1) (format *om-stream* "~%++++++++++++++++++++++++++++++++++++END MORRIS & PRATT PREFIXES++++++++++++++++++++++++++++++++++++~%"))
    (list prefix_idx tmp-length-longest)))



#|
;;;ORIGINAL
;-----------
;Looking for prefixes (inspired by Morris&Pratt)
;-----------------------------------------------
;/!\ Arguments : 1)text/corpus 2)word/motive to search
;Returns a 2 elements list :
; (nth 0 ) -> HashTable : prefix_length -> (idxs of the different occurrences)
; (nth 1 ) -> length of the longest prefix
;-----------------------------------------------
(defmethod MorrisPratt_prefixes ((text list) (word list))
  (setf prefix_idx (make-hash-table :test #'equal))
  
  (if (= *print_info_MP* 1) 
      (format *om-stream* "~%++++++++++++++++++++++++++++++++++++MORRIS & PRATT PREFIXES++++++++++++++++++++++++++++++++++++~%++++++++++++++++++++++++++++++++++++Looking for prefixes of ~a in ~a++++++++++++++++++++++++++++++++++++" word text))
  (let* ((fail (MorrisPratt_failure word)) (i 0) (j 0) (prev_i -1) (prefix_beginning nil) (tmp-length-longest 0))

    (loop while (< j (list-length text)) do
          (if (= *print_info_MP* 1) (format *om-stream* "~%++  ----> text: j=~D, word: i=~D, (j-i)=~D [[echec(i) = ~D]] <----~%++ Labels = text : ~a / word : ~a ===> equal = ~a~%" 
                  j i (- j i) (gethash i fail) (nth j text) (nth i word) (CompareEvents (nth i word) (nth j text))))
          
          (if (and (> i -1) (not (CompareEvents (nth i word) (nth j text))))
    
              ;The labels are different : end of a prefix if the previous were equal
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  =========DIFFERENT LABELS (and i>-1) ==> ~%++ >>>Found prefix, begins at idx=(j-i)= ~D : length = i = ~D<<<~%" (- j i) i))
                (loop for k from i downto 1 do (push (- j i) (gethash k prefix_idx)))
                (if (> i tmp-length-longest) (setf tmp-length-longest i))
                (loop while (and (> i -1) (not (CompareEvents (nth i word) (nth j text)))) do
                      (if (= *print_info_MP* 1) (format *om-stream* "++  === Still i>-1 and different labels : (text j=~D) != (word i=~D) ie ~a != ~a~%" j i (nth j text) (nth i word)))
                      (setf i (gethash i fail))
                      (if (= *print_info_MP* 1) (format *om-stream* "++  => Failure function : i ->~D ~%" i))
                      
                      (if (> i 0) 
                          (progn 
                            (loop for k from i downto 1 do (push (- j i) (gethash k prefix_idx)))
                            (if (> i tmp-length-longest) (setf tmp-length-longest i))
                            (if (= *print_info_MP* 1) (format *om-stream* "++  ((>>Found prefix (moving backward in the failure function), begins at idx =(j-i)= ~D : length = i = ~D<<))~%~%" (- j i) i)))
                        (if (> i -1) (if (= *print_info_MP* 1) (format *om-stream* "++ => New label (word) = ~a ((label text = ~a))~%" (nth i word) (nth j text)))))))
            
            ;The labels are equal...
            (progn
              (if (= *print_info_MP* 1) (format *om-stream* "++  =========EQUAL LABELS (or i<=-1)~%"))
              
              ;... if the previous were equal too, a prefix (itself factor of the word ?) is being recognized
              (if (and (> i 0) (= i (+ prev_i 1)))
                  ;... if the failure function is decreasing : the factor beginning at "prefix_beginning" (see below) of the prefix being recognized was a "internal" prefix !
                  (if (and prefix_beginning (< (gethash i fail) (gethash (- i 1) fail))) 
                      (progn 
                        (loop for k from (gethash (- i 1) fail) downto 1 do (push prefix_beginning (gethash k prefix_idx)))
                        (if (> (gethash (- i 1) fail) tmp-length-longest) (setf tmp-length-longest (gethash (- i 1) fail))) 
                        (if (= *print_info_MP* 1) (format *om-stream* "++  ((>>Found prefix being itself a factor of the word, begins at idx = potentiel_pref_begin = ~D : length ~D<<))~%++ ~%" prefix_beginning (gethash (- i 1) fail))))
                    ;... if the failure function is increasing, a prefix (itself factor of the word ?) is being recognized
                    (if (= *print_info_MP* 1) (format *om-stream* "++  ... failure(i) increasing...~%"))))

              ;... if the previous were different, we may have found the beginning of an "internal" prefix (prefix_beginning)
              (if (= (gethash i fail) 1)
                  (progn
                    (setf prefix_beginning (- j 1))
                    (if (= *print_info_MP* 1) (format *om-stream* "++   failure(i) = 1 : potential prefix beginning at index (j-1) = ~D~%" prefix_beginning))))
        
              (setf prev_i i)))        

          (incf i) (incf j) (if (= *print_info_MP* 1) (format *om-stream* "++  j++->~D ,i++->~D~%++ ~%++ ~%" j i))

          ;Whole word found (original Morris & Pratt)
          (if (> i (- (list-length word) 1)) 
              (progn 
                (if (= *print_info_MP* 1) (format *om-stream* "++  >>>>>>> Word found ! Begins at index = ~D<<<<<<<~%++  ~%" (- j i)))
                (loop for k from i downto 1 do (push (- j i) (gethash k prefix_idx)))
                (if (> i tmp-length-longest) (setf tmp-length-longest i))
                (setf i (gethash i fail))
                (if (= *print_info_MP* 1) (format *om-stream* "++  Failure function : i ->~D~%" i))))
          
          ;If the end of the text is reached while recognizing a prefix
          (if (and (>= j (list-length text)) (> i 0)) 
              (progn
                (if (= *print_info_MP* 1) (format *om-stream* "++  =========End of text reached while recognizing a prefix ~%++ >>>Found prefix (end of text), begins at idx = ~D : length ~D<<<~%++ ~%" (- j i) i))
                (if (> i tmp-length-longest) (setf tmp-length-longest i))
                (loop for k from i downto 1 do (push (- j i) (gethash k prefix_idx)))))
          )
    (if (= *print_info_MP* 1) (format *om-stream* "~%++++++++++++++++++++++++++++++++++++END MORRIS & PRATT PREFIXES++++++++++++++++++++++++++++++++++++~%"))
    (list prefix_idx tmp-length-longest)))
|#



;Display MorrisPratt_prefixes results
;------------------------------------
(defun print-MorrisPratt_prefixes (resultMP)
  (let* ((hash (nth 0 resultMP)) (length-longest (nth 1 resultMP)))
    (format *om-stream* "Longest prefix length = ~D~%Found prefixes :~%" length-longest)
    (loop for len being the hash-key of hash using (hash-value idxs)
          do (format *om-stream* "Length =~D -> ~D occurence [idxs = ~a]~%" len (list-length idxs) idxs))))




























