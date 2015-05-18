(in-package :om)

;Navig_MorrisPratt.lisp
;------------------------------------------------------------------------------------------------------------
; NAVIGATION THROUGH AN IMPROVIZER USING ALGORITHMS INSPIRED BY MORRIS&PRATT : IMPLEMENTATION AND TUTORIAL
;Jérôme Nika
;Feb 20th 2012
;------------------------------------------------------------------------------------------------------------


;********************************************************************************
(defparameter *print_info_MP* 0) ;Print every step in "Morris&Pratt"
(defparameter *print_info_find* 0) ;Print every step in "find-prefix-label-match"
(defparameter *print_info_navig* 0) ;Print every step in "Improvize
;********************************************************************************


;=================================================================================================== TOOLS ===================================================================================================
; Modif. -> GridData.Lisp
; (d 7 4) => (d 7)(d 7)(d 7)(d 7)
(defun expand_grid (grid)
  (setf expanded_grid 
        (loop for i from 0 to (1- (list-length grid)) 
              append 
(if (> (list-length (nth i grid)) 2)
                    (loop for j from 0 to (1- (nth 2 (nth i grid))) 
                          collect
                          (list (nth 0 (nth i grid)) (nth 1 (nth i grid)))
                          ) (list (nth i grid))))))

;((New -> To add in Improvizer.lisp))
(defmethod TransposeGrid ((harmgrid list) int)
(loop for j from 0 to (1- (list-length harmgrid)) for lab = (nth j harmgrid) collect (TransposeLabel lab int)))

(defmethod HarmGridFromImprovizer ((self Improvizer))
(loop for i from 1 to (maxetat self) collect (harmlabel (otext self i))))


;====================================================================== ORIGINAL MORRIS & PRATT ALGORITHM (searching for the whole word) =====================================================================
#|
;====================================
; EXAMPLES ORIGINAL MORRIS&PRATT
;====================================
 
(MorrisPratt '(A Z E R T Y U A Z E R P O) '(A Z E R))

(MorrisPratt '(A Z E R T Y U A Z E R P O) '(H E L L O))
|#


;Original Morris&Pratt "failure fonction"
;----------------------------------------
(defmethod MorrisPratt_failure ((l list))
  (setf fail_table (make-hash-table :test #'CompareEvents))
  (let ((p 0) (i -1))
    (setf (gethash 0 fail_table) -1)
    (loop while (< p (list-length l)) do
          (loop while (and (> i -1) (not (CompareEvents (nth p l) (nth i l)))) do
                 (setf i (gethash i fail_table)))
           (setf (gethash (incf p) fail_table) (incf i)))
    fail_table))


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


;Original Morris&Pratt algorithm : overloaded method
;---------------------------------------------------
;/!\ Arguments : 1)Improvizer 2)grid (list)
;Returns the list of idxs where the whole grid is found
;---------------------------------------------------
(defmethod MorrisPratt ((memory Improvizer) (grid list))
  (MorrisPratt (append (list '(nil)) (HarmGridFromImprovizer memory)) grid)) ;/!\ append nil because the first state (idx=0) of an oracle is empty !







;========================================================================= MORRIS & PRATT USED IN NAVIGATION : searching for prefixes ========================================================================
#|
;=======================================
;EXAMPLES MORRIS&PRATT FOR PREFIXES
;=======================================
 
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



;Overloaded method finding prefixes for the navigation
;-----------------------------------------------------
;/!\ Arguments : 1)Improvizer 2)grid (list)
;-----------------------------------------------------
(defmethod MorrisPratt_prefixes ((memory Improvizer) (grid list))
  (MorrisPratt_prefixes (append (list '(nil)) (HarmGridFromImprovizer memory)) grid)) ; /!\ append nil because the first state (idx=0) of an oracle is empty !


;Display MorrisPratt_prefixes results
;------------------------------------
(defun print-MorrisPratt_prefixes (resultMP)
  (let* ((hash (nth 0 resultMP)) (length-longest (nth 1 resultMP)))
    (format *om-stream* "Longest prefix length = ~D~%Found prefixes :~%" length-longest)
    (loop for len being the hash-key of hash using (hash-value idxs)
          do (format *om-stream* "Length =~D -> ~D occurence [idxs = ~a]~%" len (list-length idxs) idxs))))


;========================================================================= MODIFICATION Improvizer and CompareEvents (Improvizer.lisp) =======================================================================
;Jerome 08/02/13 : "LengthFactorFromGrid" : filters the length of the returned prefixes in SELECT-matching-prefixes (see below)
;Jerome 20/02/13 : "bestTranspoMode" : (bestTranspoMode [Improvizer]) = nil : first random transposition giving a prefix with length >=1 (in FIND-prefix-label-match (see below))
;                                      (bestTranspoMode [Improvizer]) = t : looking for the longest prefix with all the transpositions authorized in (AuthorizedTranspos [self]) (default)
;Jerome 20/02/13 : "randomPrefixOccurrenceMode" : (randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;                                                 (randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;Jerome 20/02/13 : "firstWithoutTranspoMode" : in FIND-prefix-label-match, (firstWithoutTranspoMode [Improvizer]) = t : first searches with no transposition (default = nil)
(defclass* improvizer (pythie)
   (
    (name :initform "improvizer" :initarg :name :accessor name)
    (context :initform () :initarg :context :accessor context)
    (continuity :initform 0 :initarg :continuity :accessor continuity)
    (max-continuity :initform 8 :initarg :max-continuity :accessor max-continuity)
    (start-region :initform '(0 100) :initarg :start-region :accessor start-region)
    (fwsuffix :initform T :initarg :fwsuffix :accessor fwsuffix)
    (bwsuffix :initform T :initarg :bwsuffix :accessor bwsuffix)
    (bestSuffixMode :initform nil :initarg :bestSuffixMode :accessor bestSuffixMode)    ;;;;;;2/5/2012 nil
    (useEstrada :initform nil :initarg :useEstrada :accessor useEstrada)
    (useHindemith :initform nil :initarg :useHindemith :accessor useHindemith)
    (RefHarmGrid :initform nil :initarg :RefHarmGrid :accessor RefHarmGrid)
    (HarmGridLength :initform 48 :initarg :HarmGridLength :accessor HarmGridLength)
    (Beats/Measure :initform 4 :initarg :Beats/Measure :accessor Beats/Measure)
    (RefTempo :initform 536 :initarg :RefTempo :accessor RefTempo) ; beat duration in ms
    (CurrentTranspo :initform 0 :initarg :CurrentTranspo :accessor CurrentTranspo)    ; value beetwen -3 and +3
    
    ;+++++++++++++
    ;New controls, Jerome 20/02/13
    (LengthFactorsFromGrid :initform '(1 100) :initarg :start-region :accessor LengthFactorsFromGrid)
    (bestTranspoMode :initform T :initarg :bestTranspoMode :accessor bestTranspoMode)
    (firstWithoutTranspoMode :initform nil :initarg :firstWithoutTranspoMode :accessor firstWithoutTranspoMode)
    (AuthorizedTranspos :initform '(-3 -2 -1 1 2 3) :initarg :AuthorizedTranspos :accessor AuthorizedTranspos)
    (randomPrefixOccurrenceMode :initform T :initarg :randomPrefixOccurrenceMode :accessor randomPrefixOccurrenceMode)
    ;+++++++++++++

    (tabou-mode :initform nil :initarg :tabou-mode :accessor tabou-mode)     ;;;;;;; added by M.C. 11/5/2012: PB after the first impro, 
    (tabou :initform (make-hash-table :test '=) :initarg :tabou :accessor tabou)  ;;;     --> it becomes difficult to find 'matches' 
    (feature  :initform nil :initarg :feature :accessor feature)               ;;;;;;; added by M.C. 15/8/0212, list of 'features' as MIDI codes
    ))

(defmethod set-LengthFactorsFromGrid ((self improvizer) (interval list))
  (setf (LengthFactorsFromGrid self) interval))

(defmethod CompareEvents ((l1 list) (l2 list))
  (equallabel l1 l2))


;================================================================================== FIND-PREFIX-LABEL-MATCH (Improvizer.lisp) ================================================================================
#|
;==============================================================
;FIND-PREFIX-LABEL-MATCH WITH NEW CONTROLS IN CLASS "IMPROVIZER"
;==============================================================

;Data
;----
(setf beat-list3 '(
((gb) ((60 0 500 80 1)))
((c#) ((60 0 500 80 1)))
((a) ((60 0 500 80 1)))
((a) ((62 0 500 80 1)))
((a) ((64 0 500 80 1)))
((gb) ((60 0 500 80 1)))
((c#) ((62 0 500 80 1)))
((a) ((64 0 500 80 1)))
((d) ((60 0 500 80 1)))
((gb) ((60 0 500 80 1)))
((c#) ((60 0 500 80 1)))
((d#) ((62 0 500 80 1)))
((e#) ((64 0 500 80 1)))
))
(setf oracle (NewImprovizer (make-beat-list beat-list3)))
(setf grid '((gb) (c#) (a) (d) (e) (c)))


; (LengthFactorsFromGrid [Improvizer]) filters the length of the returned prefixes in SELECT-matching-prefixes
;-------------------------------------------------------------------------------------------------------------

;Indexes filtered
(setf (LengthFactorsFromGrid oracle) '(1 2))
(setf resultSelect (select-matching-prefixes oracle grid))
(print-select-matching-prefixes resultSelect)

;No index filtered
(setf (LengthFactorsFromGrid oracle) '(1 1000))
(setf resultSelect (select-matching-prefixes oracle grid))
(print-select-matching-prefixes resultSelect)


; in FIND-prefix-labels-match
; (bestTranspoMode [Improvizer]) = nil : first random transposition giving a prefix with length >=1 
; (bestTranspoMode [Improvizer]) = t : looking for the longest prefix with all the transpositions authorized in (AuthorizedTranspos [self]) (default)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
(setf (bestTranspoMode oracle) t)
(find-prefix-labels-match oracle grid)

(setf (bestTranspoMode oracle) nil)
(find-prefix-labels-match oracle grid)


; (firstWithoutTranspoMode [Improvizer]) = t : first searches with no transposition (default = nil)
;----------------------------------------------------------------------------------------------------
(setf (firstWithoutTranspoMode oracle) t)
(find-prefix-labels-match oracle grid)

;(randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;(randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;------------------------------------------------------------------------------------------------------------------------------------------------------------
(setf (randomPrefixOccurrenceMode oracle) t)
(find-prefix-labels-match oracle grid)

; In find-prefix-label-match, the length is randomly chosen among those returned by select-matching-prefixes
; ==> TO SELECT A PRECISE LENGTH N : (setf (LengthFactorsFromGrid [Improvizer] '(N N)))
;----------------------------------------------------------------------------------------------------------
(setf (LengthFactorsFromGrid oracle) '(4 4))
(setf resultSelect (select-matching-prefixes oracle grid))
(find-prefix-labels-match oracle grid)
|#


;Collect the prefixes of a grid (list) found in an improvizer
;------------------------------------------------------------------------------------------------------------------------------------
;Prefixes filtered according to :
;   - the length (in (LengthFactorsFromGrid [Improvizer]))
;   - "tabous"
;Returns a 2 elements list :
; (nth 0 ) -> List of (prefix_length list_of_idxs_beginning)
; (nth 1 ) -> length of the longest prefix (used in find-prefix-label-match to compare the results of thee different transpositions)
;------------------------------------------------------------------------------------------------------------------------------------
(defmethod select-matching-prefixes ((self improvizer) (grid list))
  (let* ((MP (MorrisPratt_prefixes self grid)) (found-grid-prefixes (nth 0 MP)) (length_longest (nth 1 MP)) (length_longest (nth 1 MP)) (selected-prefixes nil))
    
    (list 
     (setf selected-prefixes 
           (loop for len being the hash-key of found-grid-prefixes using (hash-value idxs)
                 if (and (reduce-eligible-beats self idxs) 
                         (and (>= len (nth 0 (LengthFactorsFromGrid self))) 
                              (<= len (nth 1 (LengthFactorsFromGrid self))))) 
                 collect (list len (reduce-eligible-beats self idxs))
                 if (= *print_info_find* 1) do (format *om-stream* "---->Found prefix(es) : Length = ~D, Idxs =~a ------> FILTERED : idxs =~a~%" len idxs (reduce-eligible-beats self idxs) )
                 ))
     (if selected-prefixes length_longest 0))))

;Display select-matching-prefixes results
;-----------------------------------------
(defun print-select-matching-prefixes (list)
(format *om-stream* "~%Length longest prefix before filtering : ~D~%" (nth 1 list))
(format *om-stream* "Selected prefixes after filtering (LengthFactorsFromGrid and tabous):~%")
(loop for l in (nth 0 list) do
      (format *om-stream* "Length = ~D -> ~D occurence [idxs = ~a]~%" (nth 0 l) (list-length (nth 1 l)) (nth 1 l))))



  

;Get an index in the Improvizer where a prefix of the grid begins, according to the navigation modes in the Improvizer class
;--------------------------------------------------------------------------------------------------------------------------- ------------------------------
; Replaces "find-beat-label-match"
; -- Transpositions are managed by (bestTranspoMode [Improvizer]) and (firstWithoutTranspoMode [Improvizer])
; -- For all the transpositions, the prefixes are selected and filtered in select-matching-prefixes (above).
; -- The length is randomly chosen among those returned by select-matching-prefixes
;   ==> TO SELECT A PRECISE LENGTH N : (setf (LengthFactorsFromGrid [Improvizer] '(N N)))
; -- (randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;    (randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;--------------------------------------------------------------------------------------------------------------------------- ------------------------------
(defmethod find-prefix-labels-match ((self improvizer) (grid list)) 
  (if (= *print_info_find* 1) (format *om-stream* "~%~%=========================================FIND-PREFIX-LABELS-MATCH  of  ~a, length in ~a=========================================~%" grid (LengthFactorsFromGrid self))) 
  (if (= (maxetat self) 1) 1  

    (let* ((Transpos (AuthorizedTranspos self))
           (intervalTranspo 
            (if (FirstWithoutTranspoMode self) ;(firstWithoutTranspoMode self) = t : first search without transposition
                (append '(0) (rotate Transpos (random (list-length Transpos)))) 
              (rotate (append '(0) Transpos) (random (1+ (list-length Transpos))))))) 
      
          
      ;(bestTranspoMode self) = t : all the transpositions in (AuthorizedTranspos self) are compared to chose the transposition giving the longest prefix
      (if (bestTranspoMode self)
          (let* ((best_transp_len 0) (best_transp_idxs nil) (best_delta 0)
                 (cur_transp_selected_prefixes_info nil) (cur_transp_max_length nil) (cur_transp_selected_prefixes nil) (cur_transp_chosen_prefix nil) (cur_transp_idxs nil) )
            (loop for delta in (om- intervalTranspo (CurrentTranspo self)) 
                  do 
                  (if (= *print_info_find* 1) 
                      (format *om-stream* "[Searching for BEST transpo, with transpo = ~D -> transp-grid = ~a]~%" delta (TransposeGrid grid delta)))
                  (setf cur_transp_selected_prefixes_info (select-matching-prefixes self (TransposeGrid grid delta)))
                  (setf cur_transp_max_length (nth 1 cur_transp_selected_prefixes_info)) (setf cur_transp_selected_prefixes (nth 0 cur_transp_selected_prefixes_info))
                  (setf cur_transp_chosen_prefix (nth-random cur_transp_selected_prefixes)) 
                  (setf cur_transp_len (nth 0 cur_transp_chosen_prefix)) (setf cur_transp_idxs (nth 1 cur_transp_chosen_prefix))
                  (if (> cur_transp_max_length best_transp_len) (setf best_transp_len cur_transp_max_length best_transp_idxs cur_transp_idxs best_delta delta)))
            (if (> best_transp_len 0)
                (let* ((transp_states best_transp_idxs) (chosen_transp_state (if (randomPrefixOccurrenceMode self) (nth-random transp_states) (apply 'min transp_states))))
                  (if (= *print_info_find* 1) 
                      (format *om-stream* ">>>>Chosen prefix with BEST TRANSP = ~D, TRANSP_GRID = ~a :  ~%----->length = ~D, idxs = ~a ~%~%~%" 
                              best_delta (TransposeGrid grid best_delta) best_transp_len best_transp_idxs))
                  (setf (gethash chosen_transp_state (tabou self)) t) 
                  (incf (CurrentTranspo self) best_delta) chosen_transp_state) 
              nil))
        
        
        ;(bestTranspoMode self) = nil : uses the first transposition returning a prefix with length >= 1
        (loop for delta in (om- intervalTranspo (CurrentTranspo self))
              do 
              (if (= *print_info_find* 1) 
                      (format *om-stream* "[Searching for FIRST RANDOM transpo, with transpo = ~D -> transp-grid = ~a]~%" delta (TransposeGrid grid delta)))
              (let* ((transp_selected_prefixes_info (select-matching-prefixes self (TransposeGrid grid delta)))
                        (transp_max_length (nth 1 transp_selected_prefixes_info)) (transp_selected_prefixes (nth 0 transp_selected_prefixes_info))
                        (transp_chosen_prefix (nth-random transp_selected_prefixes)) 
                        (transp_len (nth 0 transp_chosen_prefix)) (transp_idxs (nth 1 transp_chosen_prefix)))
                   
                   (if (> transp_max_length 0)
                       (let* ((transp_states transp_idxs) (chosen_transp_state (if (randomPrefixOccurrenceMode self) (nth-random transp_states) (apply 'min transp_states))))
                         (if (= *print_info_find* 1) 
                             (format *om-stream* ">>>>Chosen prefix with FIRST RANDOM TRANSP = ~D, TRANSP_GRID = ~a :  ~%----->length = ~D, idxs = ~a ~%" 
                                     delta (TransposeGrid grid delta) transp_len transp_idxs))
                         (setf (gethash chosen_transp_state (tabou self)) t) 
                         (incf (CurrentTranspo self) delta) 
                         (return chosen_transp_state)))
                   )
              finally return nil))
      )))



;================================================================================== MODIFICATION IMPROVIZE (Improvizer.lisp) ================================================================================ 
#|
;==============================================================
;IMPROVIZING WITH MORRIS & PRATT
;==============================================================

;Example : D'ici d'en bas
;------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 1000)


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(10 40))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(play impro)



;Example : "J'aime pour la vie counterpoint"
;-------------------------------------------
(setf tune Jaime_tune)
(setf beatduration Jaimesolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp2 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp3 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration))

(setf (max-continuity oracle_solo) 1
      (max-continuity oracle_accomp) 10
      (max-continuity oracle_accomp2) 50
      (max-continuity oracle_accomp3) 100)

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(setf (LengthFactorsFromGrid oracle_solo) '(1 4)
      (LengthFactorsFromGrid oracle_accomp) '(10 20)
      (LengthFactorsFromGrid oracle_accomp2) '(10 40)
      (LengthFactorsFromGrid oracle_accomp3) '(10 50))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid)
      impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid) grid)
      impro_accomp2 (ImprovizeOnHarmGrid oracle_accomp2 (length grid) grid)
      impro_accomp3 (ImprovizeOnHarmGrid oracle_accomp3 (length grid) grid))

(setf impro (merger 
             (beats->chseq impro_solo beatduration 0) 
             (merger 
              (beats->chseq impro_accomp beatduration 0) 
              (merger 
               (beats->chseq impro_accomp2 beatduration 0) 
               (beats->chseq impro_accomp3 beatduration 0)))))

(pgmout 33 2)
(Stop-Player *general-player*)
(play impro)




;Example : "J'aime pour la vie" + "Cantelope Island"
;-------------------------------------------
(setf tune3 Jaime_tune)
(setf grid3 (expand_grid (grid tune3)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) (beatduration tune3)))
(setf oracle_accomp (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantalope_Uzeste29janv13.or"))

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(4 10))
(set-LengthFactorsFromGrid oracle_accomp '(1 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid3) grid3))
(setf impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid3) grid3))

(setf impro (merger (beats->chseq impro_solo (beatduration tune3) 0) (beats->chseq impro_accomp (beatduration tune3) 0)))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)



;Example : Cantelope
;-------------------
(setf tune CantelopeIsland_tune)
(setf beatduration (beatduration tune))
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))
(setf (max-continuity oracle_solo) 1000)
(setf (start-region oracle_solo) '(33 100))


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromGrid oracle_solo '(10 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




===========Save===========
(my-save-as-midi impro beatduration) 
|#





;Jerome "find-prefix-labels-match", 25/1/2013
;transpo added by M.C., generecity on labels added 10/2/2012
(defmethod Improvize ((self improvizer) (length integer) &optional (harmgrid nil))
   ;(format *om-stream*  "Region: ~a Suff. law: ~a Best Suff.: ~a Max Cont.: ~a Hindemith : ~a Estrada ~a~%" 
   ;        (start-region self) (list (bwsuffix self) (fwsuffix self)) (bestSuffixMode self) (max-continuity self)
   ;        (useHindemith self) (useEstrada self))

  (setf (CurrentTranspo self) 0)
  (format *om-stream* "-----------------------~%");(format *om-stream*  "~a~%" harmgrid)
   ;(setf harmgrid (refHarmGrid self))

  (when (null (veclrs self)) (setf (bestSuffixMode self) nil)
    (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))

  ;Jerome : 20/02/13
  (when harmgrid (setf current_grid_suffix harmgrid))

  (loop for i from 1 to length
        for label =  (TransposeLabel (pop harmgrid) (CurrentTranspo self)) ; search for continuity according to current transpo

        with index = 0
        with previndex = -1
        with index2
        with links
        with suppleances
        with mode = 'continuity
        with tocollect = nil 
         
        initially (setf (continuity self) 0)
        do        

        (setf previndex index)
        ;(format *om-stream* "~%HARMGRID = ~a, " harmgrid)

        ;Jerome : 20/02/13
        ;(when harmgrid
         (when label
        ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
          ;Jerome : 20/02/13
          ;(setf current_grid_suffix (append (list label) (TransposeGrid harmgrid (CurrentTranspo self))))

          (format *om-stream* "~%----label=~a" (FormatLabel (TransposeLabel label (- (CurrentTranspo self)))))

          (if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", oracle=~a, " (FormatLabel label)))) 
        
       
        ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
        (if (= *print_info_navig* 1) (format *om-stream* "i = ~D, current grid suffix = ~a~%" i current_grid_suffix))

        (cond
         ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
         ;((and (zerop index) (setf index2 (find-beat-label-match self label)))
         ((and (zerop index) (setf index2 (find-prefix-labels-match self current_grid_suffix)))

          (format *om-stream* "Starting point : ~a ~%" index2)
          (setf tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                index index2))
         
         
         (t
          ;Jerome 20/02/13
          ;(if (= *print_info_navig* 1) (format *om-stream* "Before checking mode : ~a  (Current cont. = ~D / Max cont. = ~D) ~%" mode (continuity self) (max-continuity self)))
          (if (check-continuity self) 
                (setf mode 'continuity)  
              (if (available-suppleance self index) 
                  (setf mode 'suppleance)
                (setf mode 'continuity)))
          ;Jerome 20/02/13
          ;(if (= *print_info_navig* 1) (format *om-stream* "After checking mode : ~a  (Current cont. = ~D / Max cont. = ~D) ~%" mode (continuity self) (max-continuity self)))
            

            (when (eq mode 'continuity)
               ;(when (= index (maxetat self)) (setf index 0)
               ;      (format *om-stream* "zero~%" ))
              (setf links (flink self index)
                    index2 (choose-factor-link self links label))
               ;(format *om-stream* " ~%=== Index courant ~a. Flink -> ~a successeurs possibles : ~a~%" index (list-length links) links)

              (if index2 
                  (progn
                    (format *om-stream* "c : ~a ~%" index2)
                    (setf index index2
                          tocollect  (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))  ))                                         
                (if (available-suppleance self index) 
                    (setf mode 'suppleance)
                  (setf mode 'nothing))))
               
               
            (when (eq mode 'suppleance)
              (setf index2 (continuations-by-suppleance self index label)) 
              (if (and index2 (/= index2 previndex))
                  (progn 
                    (format *om-stream* "--->s : ~a ~%" index2)
                    (setf index index2
                          tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                          (continuity self) 0))
                (format *om-stream*  "~a, " (setf mode 'nothing))))


            (when (eq mode 'nothing)
              ;-------- Jerome 24/01/13, ->"find-prefix-labels-match"
              ;(setf index2 (find-beat-label-match self label))
              (setf index2 (find-prefix-labels-match self current_grid_suffix))

              (if index2 
                  (progn (format *om-stream* "new : ~a " index2)
                    (if (or (null harmgrid) (zerop (CurrentTranspo self))) 
                        (format *om-stream* " ~%") (format *om-stream* "transpo=~a ~%" (CurrentTranspo self)))
                    (setf tocollect (TransposeClonedBeat (otext self index2) (- (CurrentTranspo self)))
                          index index2
                          ; Jerome 20/02/13 : CORRECTION BUG !!!
                          (continuity self) 0))
                (progn               
                  (format *om-stream* "~a~%" 'empty)
                  (setf index 0
                        tocollect (null-beat self)
                        ))))))
        ;Jerome : 20/02/13
        (setf current_grid_suffix (TransposeGrid harmgrid (CurrentTranspo self)))
        when tocollect collect tocollect 
        ))










;================================================================================== PISTES POUR CALCUL SUR LES INTERVALLES ================================================================================== 
(defmethod GridRootsIntervals_rel ((harmgrid list))
  (append 
   (list (append '(0) (cdr (nth 0 harmgrid))))
   (reverse (loop for j from (1- (list-length harmgrid)) downto 1 
         for label_root = (car (nth j harmgrid)) 
         for prev_label_root = (car (nth (- j 1) harmgrid)) collect 
         (append (list (DeltaRoot prev_label_root label_root)) (cdr (nth j harmgrid)))))))

(defmethod GridRootsIntervals_rel ((self Improvizer))
  (GridRootsIntervals_rel (HarmGridFromImprovizer self)))

(defmethod GridRootsIntervals_abs ((harmgrid list))
  (append 
   (list (append '(0) (cdr (nth 0 harmgrid))))
   (reverse (loop for j from (1- (list-length harmgrid)) downto 1 
         for label_root = (car (nth j harmgrid)) collect 
         (append (list (DeltaRoot (nth 0 (nth 0 harmgrid)) label_root)) (cdr (nth j harmgrid)))))))

(defmethod GridRootsIntervals_abs ((self Improvizer))
  (GridRootsIntervals_abs (HarmGridFromImprovizer self)))



#|
(setf grille '((d m7) (d m7) (d m7) (d m7) (g m7) (g m7) (g m7) (g m7) (c maj7) (c m7) (c m7) (c maj7)))
(GridRootsIntervals_abs grille)
(GridRootsIntervals_rel grille)

(om-inspect (MorrisPratt_failure grille))
(om-inspect (MorrisPratt_failure (GridRootsIntervals_abs grille)))
(om-inspect (MorrisPratt_failure (GridRootsIntervals_rel grille)))


(setf Improvizer (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantalope_Uzeste29janv13.or"))
(GridRootsIntervals Improvizer)
(om-inspect Improvizer)
|#