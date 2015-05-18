;========================================================================
;                   "ImproteK" Demo, European Lisp Simposium
;========================================================================
(in-package :om)



; SCENARIO : CHORD PROGRESSION OF "AUTUMN LEAVES"
;------------------------------------------------
(setf tune AutumnleavesDoMin_tune)
(setf scenario_part1 (NewHarmLabelList (expand_grid Autumnleaves1_grid)))
(setf scenario_part2 (NewHarmLabelList (expand_grid Autumnleaves2_grid)))
;(om-inspect scenario_part1)
;(om-inspect scenario_part2)

; MEMORY 1 : "AUTUMN LEAVES", played by Bernard Lubat
;----------------------------------------------------
(setf path_dir_saved_improvizers (append (pathname-directory *load-pathname*) (list "DataDemoMidi"))
      name_saved_improvizer_solo "AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
      name_saved_improvizer_accomp "AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or")

(setf memory_solo 
      (load-old-improvizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_solo))
      memory_accomp 
      (load-old-improvizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_accomp)))
;(om-inspect memory_solo)

; GENERATE
;----------
(setf impro_solo 
      (ImprovizeOnHarmGrid memory_solo (length scenario_part1) scenario_part1)
      impro_accomp 
      (ImprovizeOnHarmGrid memory_accomp (length scenario_part1) scenario_part1))
;(om-inspect impro_solo)
 (setf impro_mix (chseq-mix-melo-accomp impro_solo impro_accomp (beatduration tune) 4 4))

; PLAY
;----------
(play impro_mix)


; MEMORY 2 : "BALAIO", Hermeto Pascoal, played by Jovino Santos Neto
;-------------------------------------------------------------------
(setf name_saved_improvizer_solo "JSantosNeto-Balaio.or"
      memory_solo 
      (load-old-improvizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_solo)))

; GENERATE
;----------
(setf impro_solo 
      (ImprovizeOnHarmGrid memory_solo (length scenario_part2) scenario_part2)
      impro_accomp 
      (PlayVoicings (voicings AutumnLeaves2_tune) 
                    (substitution AutumnLeaves2_tune) 
                    (beatduration tune))
      impro_mix
      (chseq-mix-melo-accomp impro_solo impro_accomp (beatduration tune) 4 4))

; PLAY
;----------
;lire
(play impro_mix)

