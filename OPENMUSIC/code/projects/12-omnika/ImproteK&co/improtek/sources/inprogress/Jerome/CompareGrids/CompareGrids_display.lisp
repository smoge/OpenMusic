(in-package :om)

;*****************************************
; ATTENTION
; Dans CompareGrid.lisp, les tests associés aux différentes HashTable sont corrects, mais la fonction utilisée pour l'écriture dans un fichier ne les conserve pas.
; SURTOUT LES CLES NE SONT PAS DE TYPES STRING MAIS DE TYPE ARRAY !!! PAS ASSEZ D'EVALUATION A LA LECTURE ???
; ===========> A PARTIR D'UNE CHAINE DE REQUETE, POUR CONVERTIR AU BON TYPE POUR LES CLES :
;                    (setf request "Jaime")
;                    (setf request (concatenate 'vector (concatenate 'list request)))
; ================> EQUALP COMPREND !!!

;/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\
;/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\
; Le test par défaut "eql" est donc utilisé, et celui-ci ne renvoie pas true pour 2 strings identiques.
; Les tables impliquées ont pour clé des strings (string=) et des entiers (CompareEvents). Mais tous les 2 peuvent être comparés avec equal.
; => EN ATTENDANT D'AVOIR MODIFIE CETTE FONCTION d'ECRITURE : AVANT D'UTILISER CE QUI SUIT, OUVRIR LES FICHIERS AVEC UN EDITEUR DE TEXTE et faire 
; remplacer (make-hash-table) par (make-hash-table :test #'equalp)
;/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\
;/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\-/!\

;d'autre part ???
; remplacer (sethash hashtable key val) par (setf (gethash key hashtable) val)) ???
;*****************************************

;(setf path_table "/Users/jnika/Desktop/TablesCompareGrids/TableCompareGrids")
(setf path_table "/Users/jnika/Desktop/TablesCompareGrids020214/TableCompareGrids020214")
(defvar *table_prefixes_of_suffixes_in_corpus* (WITH-OPEN-FILE (in path_table :direction :input)
    (eval (read in)) (eval (read in))))
;(setf path_table_length "/Users/jnika/Desktop/TablesCompareGrids/TableLengthLongest")
(setf path_table_length "/Users/jnika/Desktop/TablesCompareGrids020214/TableLengthLongest020214")
(defvar *table_length_longest_prefixes_in_corpus* (WITH-OPEN-FILE (in path_table_length :direction :input)
    (eval (read in)) (eval (read in))))

#|
(om-inspect *table_prefixes_of_suffixes_in_corpus*)
(om-inspect *table_length_longest_prefixes_in_corpus*)

(loop for k being the hash-key of *table_prefixes_of_suffixes_in_corpus* do (format *om-stream* "~S -> ~a ~%" k (gethash k *table_prefixes_of_suffixes_in_corpus*) ))
(setf request "Jaime")
(setf request (concatenate 'vector (concatenate 'list request)))
(equalp request "Jaime")
(gethash request *table_prefixes_of_suffixes_in_corpus*)
|#

;======================================================================= Retrieve results in hash tables =======================================================================
;Simplement résultats sur longueur maximale des préfixes trouvés
;----------------------------------------------------------------
(defun table_compare-tune_max-len (tune)
  ;(gethash (concatenate 'list tune) *table_length_longest_prefixes_in_corpus*))
  (gethash tune *table_length_longest_prefixes_in_corpus*))

(defun list_max-len-prefix-in-2-of-suffixes-of-1 (tune1 tune2)
  (let ((table_tune (table_compare-tune_max-len tune1)))
    ;(gethash (concatenate 'list inTune) table_tune)))
    (if table_tune (gethash tune2 table_tune) nil)))



;Tous les résultats pour une grille
;--------------------------------------------------------------------
;Retourne une table dont les clés sont les titres des autres grilles
;-------------------------------------------------------------------
(defun table_compare-tune (tune)
  ;(gethash (concatenate 'list tune) *table_prefixes_of_suffixes_in_corpus*))
  (gethash tune *table_prefixes_of_suffixes_in_corpus*))


;Résultat de la recherche d'une grille dans une autre
;------------------------------------------------------------------------------------
;Retourne liste de tables, décrite pour un suffixe dans explication fonction suivante
;------------------------------------------------------------------------------------
(defun list_prefixes-in-2-of-suffixes-of-1 (tune1 tune2)
  (let ((table_tune (table_compare-tune tune1)))
    ;(gethash (concatenate 'list inTune) table_tune)))
    (if table_tune (gethash tune2 table_tune) nil)))


;Tous les prefixes d'un suffixe d'une grille dans une autre
;-----------------------------------------------------------
; Retourne une table :
; key : length
; value : list ( (transpo (idx*)) (transpo (idxs*))...) /!\ LISTE DE LISTES /!\
;-----------------------------------------------------------
(defun table_prefixes-in-2-of-a-suffix-of-1 (tune1 num_suffix tune2)
  (let ((list_for_all_suffixes (list_prefixes-in-2-of-suffixes-of-1 tune1 tune2)))
    (if (<= num_suffix (1- (list-length list_for_all_suffixes))) (nth num_suffix list_for_all_suffixes) nil)))


#|
(table_compare-tune "Dicidenbas")
(list_prefixes-in-2-of-suffixes-of-1 "Dicidenbas" "AllTheThingsYouAre")
(table_prefixes-in-2-of-a-suffix-of-1 "Dicidenbas" 21 "AllTheThingsYouAre")
|#



; Draw the graph of the max length prefixes of inTune in tune (with length > min_length)
;----------------------------------------------------------------------------------------
; max_mode = t : only max length prefixes
;----------------------------------------------------------------------------------------
(defun draw-graph-tune-in-tune (title_tune title_inTune min_length max_length max_mode nbOccuMin path)
  (let* ((tune (gethash title_tune *available-grids*))
         (grid_tune (expand_grid (grid tune)))
         (tune1-in-tune2 (list_prefixes-in-2-of-suffixes-of-1 title_tune title_inTune)))
    ;Graph : header
    (WITH-OPEN-FILE (out path :direction :output;;;;
                         :if-does-not-exist :create :if-exists :supersede);;;;
      (format out "digraph G { ~%");;;;
      (format out "rankdir=LR ~%");;;;
      
      ;Graph : list of states
      (loop for i from 0 to (- (list-length grid_tune) 1) do;;;;
            (progn;;;;
              (if (= (mod i (nbbeatspermeasure tune)) 0) (format out "subgraph cluster~D {~%node [style=filled,color=white,shape=box,fontsize=24];~%style=filled;~%color=lightgrey;~%fontsize=32;~%" (/ i (nbbeatspermeasure tune))));;;;
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13 : label = juste label harmo
              (format out "\"~D:~a\" [label=\"~a ~a\"]; ~%" (+ i 1) (nth i grid_tune) (nth 0 (nth i grid_tune)) (nth 1 (nth i grid_tune)));;;;
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13 : plus de label pour afficher num de mesure
              ;(if (= (mod i (nbbeatspermeasure tune)) (- (nbbeatspermeasure tune) 1)) (format out "label = \"Mes. ~D\"~%}~%" (/ (+ i 1) (nbbeatspermeasure tune))))));;;;
              (if (= (mod i (nbbeatspermeasure tune)) (- (nbbeatspermeasure tune) 1)) (format out "~%}~%" ))));;;;
      
      ;Graph : links between states of the grid
      (loop for i from 0 to (- (list-length grid_tune) 2) do;;;;
            (progn;;;;
              (format out "\"~D:~a\" -> \"~D:~a\" [color=\"black\", constraint=false];~%" (+ i 1) (nth i grid_tune) (+ i 2) (nth (+ i 1) grid_tune));;;;
              ));;;;
      
      ;For all the prefixes links
      (setf num_suffix 0)
      (loop for prefixes_of_this_suffix in tune1-in-tune2 do

             ;==============Do what you have to do==============

            (setf max_length_for_this_suffix 0)
            (setf other_prefixes_to_display '())
            (loop for len being the hash-key of prefixes_of_this_suffix do
                  (if (> len max_length_for_this_suffix) (setf max_length_for_this_suffix len))
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 11/10/13 -> INTRODUCTION DE L'ARGUMENT MAX_LENGTH
                  ;(if (> len min_length) (setf other_prefixes_to_display (append other_prefixes_to_display (list len))))
                  (if (and (<= len max_length) (>= len min_length)) (setf other_prefixes_to_display (append other_prefixes_to_display (list len)))))
           
            ;Display max length prefix REFAIRE AVEC LET, cf other prefixes !!!
            ;-------------------------
            (if (>= max_length_for_this_suffix min_length) 
                (progn
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 11/10/13 -> INTRODUCTION DE L'ARGUMENT MAX_LENGTH
                  (if (> max_length_for_this_suffix max_length) (setf max_length_for_this_suffix max_length)) 
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 11/10/13
                  (setf infos_longests (gethash max_length_for_this_suffix prefixes_of_this_suffix)) ;ATTENTION : possible que plusieurs transpositions différentes donnent le prefixe de taille maximale ! Liste de listes !
                  
                  ;Prepare string to display the transpoS and nbS occurrences associated to the max length prefix
                  ;;;;;(setf string_infos_longests (format nil "~D (~D)" (list-length (nth 1 (car infos_longests))) (nth 0 (car infos_longests)) ));;;;
                  (setf string_infos_longests (format nil "~D" (nth 0 (car infos_longests)) ));;;;
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13
                  (setf nbTotOccu (list-length (nth 1 (car infos_longests))))
                  (loop for info_longest in (cdr infos_longests) do;;;;
                        (setf string_infos_longests 
                              ;;;(concatenate 'string string_infos_longests "\\n" ;;;;
                              (concatenate 'string string_infos_longests ", " ;;;;
                                           ;(format nil "~D (~D)" (list-length (nth 1 info_longest)) (nth 0 info_longest) ))
                                           (format nil "~D" (nth 0 info_longest) ))
                                           ;)
                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13
                              nbTotOccu
                              (+ nbTotOccu (list-length (nth 1 info_longest)))));;;;
                  ;Graph : states linked by longest prexixes found in inTune
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13 (width, et longueur du suffixe ne figure plus)
                  ;(format out "\"~D:~a\" -> \"~D:~a\" [label=\"~D (~a)\" , color=\"red\", constraint=false, width=~D];~%" ;;;;
                  ;        (+ num_suffix 1) (nth num_suffix grid_tune) (+ num_suffix max_length_for_this_suffix) (nth (+ num_suffix (- max_length_for_this_suffix 1)) grid_tune);;;;
                  ;        max_length_for_this_suffix string_infos_longests (+ 0 (log (/ nbTotOccu 0.1))));;;;

                  
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 11/10/13 -> INTRODUCTION DE L'ARGUMENT OCCUMIN
                  (if (>= nbTotOccu nbOccuMin) 
                      (format out "\"~D:~a\" -> \"~D:~a\" [label=\"~a\" ,labeldistance = 50, labelfontsize=20,fontsize=20, color=\"red\", constraint=false, width=~D];~%" ;;;;
                              (+ num_suffix 1) (nth num_suffix grid_tune) (+ num_suffix max_length_for_this_suffix) (nth (+ num_suffix (- max_length_for_this_suffix 1)) grid_tune);;;;
                              string_infos_longests (* 20 (atan (/ nbTotOccu 20)))));;;;
                  )
              )

            ;Display other prefixes
            ;-----------------------
            (if (not max_mode)
                (loop for len_prefix in other_prefixes_to_display
                      when (not (= len_prefix max_length_for_this_suffix)) do
                      (let* ((infos (gethash len_prefix prefixes_of_this_suffix))
                             ;;;;;(string_infos (format nil "~D (~D)" (list-length (nth 1 (car infos))) (nth 0 (car infos)) ))
                             (string_infos (format nil "~D" (nth 0 (car infos)) ))
                             (nbTotOccu (list-length (nth 1 (car infos)))))
                        ;Prepare string to display the transpoS and nbS occurrences associated to the prefix
                        (loop for info in infos do;;;;
                              ;;;(setf string_infos (concatenate 'string string_infos "\\n" ;;;;
                              (setf string_infos (concatenate 'string string_infos ", " ;;;;
                                                                       ;(format nil "~D (~D)" (list-length (nth 1 info)) (nth 0 info) ))));;;;
                                                              (format nil "~D" (nth 0 info) ))))
                        ;Graph : states linked by longest prexixes found in inTune
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 11/10/13 -> INTRODUCTION DE L'ARGUMENT OCCUMIN
                        (if (>= nbTotOccu nbOccuMin) 
                            (format out "\"~D:~a\" -> \"~D:~a\" [label=\"~a\" ,labeldistance = 50, labelfontsize=20,fontsize=20, color=\"lightsalmon\", constraint=false, width=~D];~%" ;;;;
                                    (+ num_suffix 1) (nth num_suffix grid_tune) (+ num_suffix len_prefix) (nth (+ num_suffix (- len_prefix 1)) grid_tune);;;;
                                    string_infos (* 20 (atan (/ nbTotOccu 20)))));;;;
                        )))

            (incf num_suffix)
            )
      

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13 ? CHOSES A RAJOUTER A LA MAIN ?
      ;(format out
      ;       "subgraph cluster1000 {~% rank = same; cluster0; cluster1; cluster2; cluster3;~%}~%subgraph cluster1001 {~% rank = same; cluster4; cluster5; cluster6;~%}~%subgraph cluster1002 {~% rank = same; cluster7; cluster8; cluster9;~%}~%")
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 10/10/13 

      ;Graph : end
      (format out "}");;;;
      )))


#|
(draw-graph-tune-in-tune "BlueInGreen" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithALICEINWONDERLAND2.dot")

(draw-graph-tune-in-tune "BlueInGreen" "NightInTunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithNIGHTIN2.dot")

(draw-graph-tune-in-tune "BlueInGreen" "BluesForAlice" 1 10000 nil 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithBLUESFOR.dot")

(draw-graph-tune-in-tune "BlueInGreen" "Allthethingsyouare" 1 1000 nil 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithALLTHETHINGS.dot")

(draw-graph-tune-in-tune "BlueInGreen" "AutumnLeavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithAUTUMN2.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "Allthethingsyouare" 3 5 nil 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/ALLTHETHINGSinitself.dot")
|#

#|
;20/11/13 - MAX MODE
;-------------------

(draw-graph-tune-in-tune "AliceInWonderland" "NightinTunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruALICEINwithNIGHTIN.dot")

(draw-graph-tune-in-tune "Handfulofkeys" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruHANDFULwithAUTUMN.dot")

(draw-graph-tune-in-tune "Handfulofkeys" "Nightintunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruHANDFULwithNIGHTIN.dot")

(draw-graph-tune-in-tune "AuPrivave" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruAUPIVAVEwithMOMENTS.dot")

(draw-graph-tune-in-tune "BlueInGreen" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithAUTUMN.dot")

(draw-graph-tune-in-tune "Bagsgroove" "Nightintunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBAGSGROOVEwithNIGHTIN.dot")

(draw-graph-tune-in-tune "SongForMyFather" "AuPrivave" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruSONGFORwithAUPRIVAVE.dot")

(draw-graph-tune-in-tune "SongForMyFather" "Nightintunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruSONGFORwithNIGHTIN.dot")

(draw-graph-tune-in-tune "SongForMyFather" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruSONGFORwithAUTUMN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "BlueInGreen" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUESFORwithBLUEIN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUESFORwithMOMENTS.dot")

(draw-graph-tune-in-tune "AutumnleavesDoMin" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruAUTUMNwithALICEIN.dot")

(draw-graph-tune-in-tune "AutumnleavesDoMin" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruAUTUMNwithMOMENTS.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruALLTHEwithALICEIN.dot")

(draw-graph-tune-in-tune "CantelopeIsland" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruCANTELOPEwithALICEIN.dot")

(draw-graph-tune-in-tune "CantelopeIsland" "AuPrivave" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruCANTELOPEwithAUPRIVAVE.dot")
|#

#|
;20/11/13 - NON MAX MODE
;-------------------

(draw-graph-tune-in-tune "AliceInWonderland" "NightinTunisia" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruALICEINwithNIGHTIN.dot")

(draw-graph-tune-in-tune "Handfulofkeys" "AutumnleavesDoMin" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruHANDFULwithAUTUMN.dot")

(draw-graph-tune-in-tune "Handfulofkeys" "Nightintunisia" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruHANDFULwithNIGHTIN.dot")

(draw-graph-tune-in-tune "AuPrivave" "MomentsNotice" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruAUPIVAVEwithMOMENTS.dot")

(draw-graph-tune-in-tune "BlueInGreen" "AutumnleavesDoMin" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUEINwithAUTUMN.dot")

(draw-graph-tune-in-tune "Bagsgroove" "Nightintunisia" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBAGSGROOVEwithNIGHTIN.dot")

(draw-graph-tune-in-tune "SongForMyFather" "AuPrivave" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruSONGFORwithAUPRIVAVE.dot")

(draw-graph-tune-in-tune "SongForMyFather" "Nightintunisia" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruSONGFORwithNIGHTIN.dot")

(draw-graph-tune-in-tune "SongForMyFather" "AutumnleavesDoMin" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruSONGFORwithAUTUMN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "BlueInGreen" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUESFORwithBLUEIN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "MomentsNotice" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruBLUESFORwithMOMENTS.dot")

(draw-graph-tune-in-tune "AutumnleavesDoMin" "AliceInWonderland" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruAUTUMNwithALICEIN.dot")

(draw-graph-tune-in-tune "AutumnleavesDoMin" "MomentsNotice" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruAUTUMNwithMOMENTS.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "AliceInWonderland" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruALLTHEwithALICEIN.dot")

(draw-graph-tune-in-tune "CantelopeIsland" "AliceInWonderland" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruCANTELOPEwithALICEIN.dot")

(draw-graph-tune-in-tune "CantelopeIsland" "AuPrivave" 2 1000 nil 2 "/Users/jnika/Desktop/TablesCompareGrids020214/Exemples/gothruCANTELOPEwithAUPRIVAVE.dot")
|#

#|
;21/11/13 - ExemplesNouveaux BONNES MOYENNES
; BONNES DERIVEES ET DERIVEES SECONDES
; ((( MAX MODE)))
;-----------------------------------

(draw-graph-tune-in-tune "Allthethingsyouare" "BluesForALice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALLTHETHINGSwithBLUESFOR.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "Nightintunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALLTHETHINGSwithNIGHTIN.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALLTHETHINGSwithMOMENTS.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "AuPrivave" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALLTHETHINGSwithAUPRIVAVE.dot")

(draw-graph-tune-in-tune "Allthethingsyouare" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALLTHETHINGSwithALICEIN.dot")

;REGARDER VERSION MAXMODE = NIL ???
(draw-graph-tune-in-tune "AutumnleavesDoMin" "Allthethingsyouare" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUTUMNwithALLTHETHINGS.dot")

(draw-graph-tune-in-tune "AutumnleavesDoMin" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUTUMNwithMOMENTS.dot") ;;!!??

;REGARDER VERSION MAXMODE = NIL ???
(draw-graph-tune-in-tune "AutumnleavesDoMin" "SongForMyFather" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUTUMNwithSONGFOR.dot") ;;!!??

(draw-graph-tune-in-tune "AutumnleavesDoMin" "BlueInGreen" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUTUMNwithBLUEIN.dot") ;;!!??


(draw-graph-tune-in-tune "BluesForAlice" "Allthethingsyouare" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithALLTHETHINGS.dot")

;REGARDER VERSION MAXMODE = NIL ???
(draw-graph-tune-in-tune "BluesForAlice" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithAUTUMN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "Nightintunisia" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithNIGHTIN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "Bagsgroove" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithBAGSGROOVE.dot") ;;!!??

(draw-graph-tune-in-tune "BluesForAlice" "AuPrivave" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithAUPRIVAVE.dot") ;;!!??

(draw-graph-tune-in-tune "BluesForAlice" "Handfulofkeys" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithHANDFUL.dot") ;;!!??


(draw-graph-tune-in-tune "Nightintunisia" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruNIGHTINwithAUTUMN.dot")

;REGARDER VERSION MAXMODE = NIL ???
(draw-graph-tune-in-tune "MomentsNotice" "Allthethingsyouare" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruMOMENTSwithALLTHETHINGS.dot")


(draw-graph-tune-in-tune "SongForMyFather" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruSONGFORwithAUTUMN.dot")

(draw-graph-tune-in-tune "SongForMyFather" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruSONGFORwithALICEIN.dot")


(draw-graph-tune-in-tune "Bagsgroove" "BlueInGreen" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBAGSGROOVEwithBLUEIN.dot") ;;!!??

(draw-graph-tune-in-tune "Bagsgroove" "StraightNoChaser" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBAGSGROOVEwithSTRAIGHTNO.dot")


(draw-graph-tune-in-tune "BlueInGreen" "Allthethingsyouare" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithALLTHETHINGS.dot")

(draw-graph-tune-in-tune "BlueInGreen" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithAUTUMN.dot")


;REGARDER VERSION MAXMODE = NIL ???
(draw-graph-tune-in-tune "AuPrivave" "BluesForAlice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUPRIVAVEwithBLUESFOR.dot") ;;!!??


(draw-graph-tune-in-tune "StraightNoChaser" "Bagsgroove" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruSTRAIGHTNOwithBAGSGROOVE.dot")

;;;HANDFULOFKEYS IN ... ???

;REGARDER VERSION MAXMODE = NIL ???
(draw-graph-tune-in-tune "AliceInWonderland" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALICEINwithAUTUMN.dot")

|#



#|
;AUTRES TESTS 22/11/13

(draw-graph-tune-in-tune "Allthethingsyouare" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruALLTHETHINGSwithAUTUMN2.dot")

(draw-graph-tune-in-tune "AutumnleavesDoMin" "Allthethingsyouare" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUTUMNwithALLTHE2.dot")

(draw-graph-tune-in-tune "BlueInGreen" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithAUTUMN.dot")
|#


#|
;AUTRES TESTS 22/11/13 2

;;BLUEINGREEN

(draw-graph-tune-in-tune "BlueInGreen" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithMOMENTS.dot")

(draw-graph-tune-in-tune "BlueInGreen" "SongForMyFather" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithSONGFOR.dot")

(draw-graph-tune-in-tune "BlueInGreen" "Bagsgroove" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithBAGSGROOVE.dot")

(draw-graph-tune-in-tune "BlueInGreen" "AutumnleavesDoMin" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUEINwithAUTUMN.dot")

;;AUPRIVAVE

(draw-graph-tune-in-tune "AuPrivave" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUPRIVAVEwithALICEIN.dot")

(draw-graph-tune-in-tune "AuPrivave" "MomentsNotice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUPRIVAVEwithMOMENTS.dot")

(draw-graph-tune-in-tune "AuPrivave" "BluesForAlice" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUPRIVAVEwithBLUESFOR.dot")

(draw-graph-tune-in-tune "AuPrivave" "Goodbyeporkpiehat" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruAUPRIVAVEwithGOODBYE.dot")

;;BLUESFORALICE

(draw-graph-tune-in-tune "BluesForAlice" "AliceInWonderland" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithALICEIN.dot")

(draw-graph-tune-in-tune "BluesForAlice" "AuPrivave" 2 1000 t 1 "/Users/jnika/Desktop/TablesCompareGrids020214/ExemplesNouveaux/gothruBLUESFORwithAUPRIVAVE.dot")

;;GOODBYEPORK --> X

|#

; Draw the graph of the max length prefixes of inTune in tune (with length > min_length)
;----------------------------------------------------------------------------------------
; max_mode = t : only max length prefixes
;----------------------------------------------------------------------------------------
;(defun draw-graph-tune-in-tune (title_tune title_inTune min_length max_length max_mode nbOccuMin path)