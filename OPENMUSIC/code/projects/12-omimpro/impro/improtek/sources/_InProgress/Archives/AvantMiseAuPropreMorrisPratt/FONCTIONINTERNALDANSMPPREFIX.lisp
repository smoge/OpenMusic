
;Sub-routine to index the internal prefixes within 
;the longest prefix ending at step (i,j) of the research phase
;-----------------------------------------------
;Arguments : 
;1) result table to complete
;2) prefixes of the word in itself (obtained during the analysis phase)
;3/4) Step in the research algorithm
;-----------------------------------------------
(defun add_internal_prefixes (prefixesWordInText internalPrefixesInWord j i)
  (if (= *print_info_MP* 1) (format *om-stream* "~%+++  ====Looking for internal prefixes within the last prefix found~%"))
  
  (let ((jFinPref (- j 1)) (iFinPref (- i 1)))
    (loop for i0 from 0 to iFinPref do
          (if (= *print_info_MP* 1) (format *om-stream* "+++ i0 = ~D -> looking for prefixes ending in i-i0=~D (word)-> j-i0=~D (text) ...+++~%" i0 (- iFinPref i0) (- jFinPref i0)))
          (loop for len in (gethash (- iFinPref i0) internalPrefixesInWord) do
                (let ((idx_in_text (+ (- (- jFinPref len) i0) 1)))
                  (progn
                    (if (= *print_info_MP* 1) (format *om-stream* "+++ ... one of length l=~D ! +++~%" len))
                    (if (not (member idx_in_text (gethash len prefixesWordInText))) (push idx_in_text (gethash len prefixesWordInText))) 
                    (if (= *print_info_MP* 1)(format *om-stream* "+++  ((>>Found prefix moving backward in the failure function (AFTER END OF THE TEXT) : i0 =~D ---> begins at idx j-i0-l+1= ~D : length l= ~D<<))~%"i0 idx_in_text len))
                    )))))
  (if (= *print_info_MP* 1) (format *om-stream* "+++  ====  END Looking for internal prefixes~%"))
  prefixesWordInText)


#|
; APPEL DANS LE CORPS DE LA FONCTION QUI PROCEDE AU PREFIX INDEXING :
;Look for internal prefixes in the found prefix
(setf prefix_idx (add_internal_prefixes prefix_idx lengthInternalPrefixes j i))
|#

#|
(defun test (arg)
  (push "ours" (gethash 1 arg)))

(setf table (make-hash-table :test #'equal))
(gethash 1 table)

(test table)
(gethash 1 table)
|#
