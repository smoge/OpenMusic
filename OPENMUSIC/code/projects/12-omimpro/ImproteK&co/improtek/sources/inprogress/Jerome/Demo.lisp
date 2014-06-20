;========================================================================
;                   "ImproteK" Demo, European Lisp Simposium
;========================================================================
(in-package :om)

; SCENARIO : CHORD PROGRESSION OF "AUTUMN LEAVES"
;------------------------------------------------
(setf tune AutumnleavesDoMin_tune)
(setf scenario1 (expand_grid Autumnleaves1_grid))
(setf scenario2 (expand_grid Autumnleaves2_grid))





; MEMORY 1 : "AUTUMN LEAVES", played by Bernard Lubat
;----------------------------------------------------
(setf memory_solo 
      (load-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Oracles/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or")
      memory_accomp 
      (load-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Oracles/AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or"))

; GENERATE
;----------
(setf impro_solo 
      (ImprovizeOnHarmGrid memory_solo (length scenario1) scenario1)
      impro_accomp 
      (ImprovizeOnHarmGrid memory_accomp (length scenario1) scenario1)
      impro_mix
      (chseq-mix-melo-accomp impro_solo impro_accomp (beatduration tune) 4 4))

; PLAY
;----------
(Stop-Player *general-player*)
(play impro_mix)v





; MEMORY 2 : "BALAIO", Hermeto Pascoal, played by Jovino Santos Neto
;-------------------------------------------------------------------
(setf memory_solo 
      (load-improvizer "/Users/jnika/Google Drive/Dev/Max-Interface/_Oracles/AutumnleavesDoMin-JOVINOmixPlanant.or"))

; GENERATE
;----------
(setf impro_solo 
      (ImprovizeOnHarmGrid memory_solo (length scenario2) scenario2)
      impro_accomp 
      (PlayVoicings (voicings AutumnLeaves2_tune) 
                    (substitution AutumnLeaves2_tune) 
                    (beatduration tune))
      impro_mix
      (chseq-mix-melo-accomp impro_solo impro_accomp (beatduration tune) 4 4))

; PLAY
;----------
;lire
(progn 
  (push (list impro_mix 0 nil nil nil) *ms-list-to-play*)
  (player-start :midishare)
  (pop *ms-list-to-play*))
;stop
(player-stop :midishare)


(defun playchelou (trucajouer) 
  (progn 
  (push (list trucajouer 0 nil nil nil) *ms-list-to-play*)
  (player-start :midishare)
  (pop *ms-list-to-play*)))







