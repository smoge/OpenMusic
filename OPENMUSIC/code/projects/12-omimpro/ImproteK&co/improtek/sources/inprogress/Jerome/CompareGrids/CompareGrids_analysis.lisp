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


(loop for tune1 being the hash-key of *table_length_longest_prefixes_in_corpus* do 
      (let ((tablefortune1 (table_compare-tune_max-len tune1)))
        (format *om-stream* "~%~% ****************** Studying ~a... ****************** ~%~%" tune1)
        (loop for tune2 being the hash-key of tablefortune1 do
              (let* ((listmaxlen12 (list_max-len-prefix-in-2-of-suffixes-of-1 tune1 tune2))
                    (deriv nil)
                    (deriv2 nil)
                    (somme 0)
                    (sommederiv 0)
                    (sommederiv2 0))
                (format *om-stream* "~%->Searching ~a in ~a : ~%" tune1 tune2)
                (setf deriv
                      (loop for i from 0 to (- (list-length listmaxlen12) 2)
                            collect (- (nth (+ i 1) listmaxlen12) (nth i listmaxlen12))))
                (setf deriv2
                      (loop for i from 0 to (- (list-length deriv) 2)
                            collect (- (nth (+ i 1) deriv) (nth i deriv))))

                (loop for j from 0 to (- (list-length listmaxlen12) 1) do
                      (setf somme (+ somme (nth j listmaxlen12))))
                (loop for j from 0 to (- (list-length deriv) 1) do
                      (setf sommederiv (+ sommederiv (nth j deriv))))
                (loop for j from 0 to (- (list-length deriv2) 1) do
                      (setf sommederiv2 (+ sommederiv2 (nth j deriv2))))
                (format *om-stream* "--- DERIV : ~a~%MoyenneDeriv : ~D~%--- DERIV2 : ~a~%MoyenneDeriv2 : ~D~%--- MOYENNE : ~D~%~%" deriv (/ (* sommederiv 1.0) (list-length deriv)) deriv2 (/ (* sommederiv2 1.0) (list-length deriv2)) (/ (* somme 1.0) (list-length listmaxlen12)))))))
                ;(format *om-stream* "SOMMEDERIV : ~D~%SOMMEDERIV / LONGUEUR : ~D~%" sommederiv (/ (* sommederiv 1.0) (list-length listmaxlen12)))))))



