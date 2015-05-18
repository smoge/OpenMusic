(in-package :om)

;table_prefixes_of_suffixes_in_corpus
;-------------------------------------------
;key : tune-studied_title
;value : table_prefixes_tune-studied_in_tunes-corpus
(setf table_prefixes_of_suffixes_in_corpus (make-hash-table :test #'string= ))
;table_length_longest_prefixes_in_corpus
;-------------------------------------------
;key : tune-studied_title
;value : table_length_longest_prefixes_tune-studied_in_tunes-corpus
(setf table_length_longest_prefixes_in_corpus (make-hash-table :test #'string= ))
(loop for tune-studied_title being the hash-key of *available-grids* using (hash-value tune-studied)
      do
      (format *om-stream* "~%~%Studying ~S...~%" tune-studied_title)
      ;table_prefixes_tune-studied_in_tunes-corpus
      ;-------------------------------------------
      ;key : tune-corpus_title
      ;value : list_results_for_suffixes_from_tune-studied
      (setf table_prefixes_tune-studied_in_tunes-corpus (make-hash-table :test #'string= ))
      ;table_length_longest_prefixes_tune-studied_in_tunes-corpus
      ;-------------------------------------------
      ;key : tune-corpus_title
      ;value : list-length-longest-for-all-transpos
      (setf table_length_longest_prefixes_tune-studied_in_tunes-corpus (make-hash-table :test #'string= ))
      (setf tune-studied_grid (expand_grid (grid tune-studied)))
      
      (loop for tune-corpus_title being the hash-key of *available-grids* using (hash-value tune-corpus)
            do
            (format *om-stream* "->Searching ~S in ~S : ~%" tune-studied_title tune-corpus_title)
            ;list_results_for_suffixes_from_tune-studied
            ;-------------------------------------------
            ; (nth i ...) -> table_current_studied_suffix_prefixes_from_tune-corpus
            (setf list_results_for_suffixes_from_tune-studied '())
            (setf tune-studied_grid_SUFFIX tune-studied_grid idx_current_suffix 0)
            ;list-length-longest-for-all-transpos
            ;-------------------------------------------
            ; (nth i ...) -> length of the longest prefix of the suffix nb i of tune-studied found in tune-corpus
            (setf list-length-longest-prefix-found-for-each-suffix '())
            (while tune-studied_grid_SUFFIX
              (progn
                ;table_current_studied_suffix_prefixes_from_tune-corpus
                ;------------------------------------------------------
                ;key : length
                ;value : list ( (transpo (idx*)) (transpo (idxs*))...)
                (setf table_current_studied_suffix_prefixes_from_tune-corpus (make-hash-table :test #'CompareEvents))
                (setf tune-corpus_grid (expand_grid (grid tune-corpus)))
                (setf length-longest-prefix-for-this-suffix 0)
                (loop for transpo in '(-5 -4 -3 -2 -1 0 1 2 3 4 5 6)
                      do
                      (setf transpo_tune-corpus_grid (TransposeGrid tune-corpus_grid transpo))
                      (setf resultMP (MorrisPratt_prefixes transpo_tune-corpus_grid tune-studied_grid_SUFFIX))
                      (setf tableMP (nth 0 resultMP) longest (nth 1 resultMP))
                      (loop for len being the hash-key of tableMP using (hash-value idxs)
                            do
                            (push (list transpo idxs) (gethash len table_current_studied_suffix_prefixes_from_tune-corpus)))
                      (if (> longest length-longest-prefix-for-this-suffix) (setf length-longest-prefix-for-this-suffix longest)))
              
                (setf list-length-longest-prefix-found-for-each-suffix (append list-length-longest-prefix-found-for-each-suffix (list length-longest-prefix-for-this-suffix)))
                (setf list_results_for_suffixes_from_tune-studied (append list_results_for_suffixes_from_tune-studied (list table_current_studied_suffix_prefixes_from_tune-corpus)))
                (pop tune-studied_grid_SUFFIX)
                (setf idx_current_suffix (1+ idx_current_suffix))
                )
              )
            
            (setf (gethash tune-corpus_title table_prefixes_tune-studied_in_tunes-corpus) list_results_for_suffixes_from_tune-studied)
            (format *om-stream* "For every suffix of ~S, length of the longest prefix found in ~S (all transpositions authorized)  : ~%~a~%~%" tune-studied_title tune-corpus_title list-length-longest-prefix-found-for-each-suffix)
            (setf (gethash tune-corpus_title table_length_longest_prefixes_tune-studied_in_tunes-corpus) list-length-longest-prefix-found-for-each-suffix)
            )
      (setf (gethash tune-studied_title table_prefixes_of_suffixes_in_corpus) table_prefixes_tune-studied_in_tunes-corpus)
      (setf (gethash tune-studied_title table_length_longest_prefixes_in_corpus) table_length_longest_prefixes_tune-studied_in_tunes-corpus)
      )



;(setf path_work_directory #P"/Users/Nika/Developpement/ImproteK/TableCompareGrids")
(setf name "/Users/jnika/Desktop/TablesCompareGrids020214/TableCompareGrids020214")
(ensure-directories-exist name)
(setf self table_prefixes_of_suffixes_in_corpus)
(WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))

(setf name "/Users/jnika/Desktop/TablesCompareGrids020214/TableLengthLongest020214")
(ensure-directories-exist name)
(setf self table_length_longest_prefixes_in_corpus)
(WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
