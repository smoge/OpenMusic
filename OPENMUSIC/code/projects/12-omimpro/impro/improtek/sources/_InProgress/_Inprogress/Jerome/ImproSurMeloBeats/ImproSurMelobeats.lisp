(in-package :om)
(Stop-Player *general-player*)

(setf midi1 
      (midi-to-beatlist "/Users/jnika/Desktop/LAMSessionKilema/midibuff_19-Real-Tsoratsora-kazo+beats.mid")
      midi2 
      (midi-to-beatlist "/Users/jnika/Desktop/LAMSessionKilema/midibuff_20-Real-Aynene+beats.mid")
      midi3 
      (midi-to-beatlist "/Users/jnika/Desktop/LAMSessionKilema/midibuff_mohiafahariva.mid")
      midi4 
      (midi-to-beatlist "/Users/jnika/Desktop/LAMSessionKilema/midibuff_ratianavivo.mid"))

;==================================================================================
;==================================================================================
;**********************************************************************************
(setf
 ;=========
 midifromfile-memory
 (append nil
         midi1 
       ;midi2
       ;midi3 
       ;midi4
         )
 ;=========
 midifromfile-scenario
 midi1
 ;=========
 coeff-div-length-impro
 1
 ;=========
 recomb
 4
 ;=========
 path-4-saving
 "/Users/jnika/Google Drive/Dev/Max-Interface/Test/new/2sur1-pasRECOMB.txt"
 )
;**********************************************************************************
;==================================================================================
;==================================================================================
(setf beatlist-memory (first midifromfile-memory) beatdur-memory (second midifromfile-memory) beatlist-scenario (first midifromfile-scenario) beatdur-scenario (second midifromfile-scenario))
(setf melobeatsImprovizer (learn-melobeats-improvizer beatlist-memory beatdur-memory))
;==================================================================================
;==================================================================================
;**********************************************************************************
 (setf (max-continuity melobeatsImprovizer) recomb)
 (setf (bestTranspoMode melobeatsImprovizer) t)
 (setf (FirstWithoutTranspoMode melobeatsImprovizer) nil)
 (setf (randomPrefixOccurrenceMode melobeatsImprovizer) t)
(setf lengthfactorsfromgrid melobeatsImprovizer '(1 recomb))
;**********************************************************************************
;==================================================================================
;==================================================================================
(setf melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats beatlist-scenario beatdur-scenario))))
(setf impro_melobeatlist (ImprovizeOnHarmGrid melobeatsImprovizer (round (/ (length melolist) coeff-div-length-impro)) melolist))
(setf impro_chseq (beats->chseq impro_melobeatlist beatdur-scenario 0))
(Stop-Player *general-player*)
(pgmout 4 1)
(pgmout 5 2)
(play impro_chseq)
;(om-inspect impro_melobeatlist)
(save-for-antescofo2 impro_melobeatlist beatdur-scenario path-4-saving)

