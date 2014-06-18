(defun init-testnika ()
  (setq *beat-dur* 330
        *start-indx* 0
        *rtimprovizer* (load-realtimeImprovizer-fromSavedImprovizer *db-path-solo1*)))

(defun run-testnika ()
  (setq *scenario-suffix* (nthcdr *start-indx* (expand_grid *scenario*))
        *result-beats* (improvize-loop-next-factor *rtimprovizer* *scenario-suffix* *beat-dur* *start-indx*)
        *result-chord-seq* (beats->chseq *result-beats* *beat-dur* 0))
  
  (incf *start-indx* (length *result-beats*)))


(defun play-test-nika ()
  (progn
    (push (list *result-chord-seq* 0 nil nil nil) *ms-list-to-play*)
    (player-start :midishare)
    (pop *ms-list-to-play*)))

(defun stop-testnika ()
  (player-stop :midishare))

;(init-testnika)
;(run-testnika)
;(play-test-nika)
;(stop-testnika)