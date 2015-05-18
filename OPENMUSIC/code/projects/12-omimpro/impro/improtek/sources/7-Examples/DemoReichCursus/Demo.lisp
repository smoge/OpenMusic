(in-package :om)

; -------------- MEMORY : NewYorkCounterpoint  -------------- 
; SCENARIO : Electric Counterpoint Part 1.
(osc-send-saved-impro p_impro3 1 0)

; SCENARIO : Electric Counterpoint Part 2.
(osc-send-saved-impro p_impro4 1 0)
















#|


(setf p_impro1 "/Users/jnika/Desktop/_SAVEDIMPROS_/1ScenNY_MemELEC1_tr2.or"
      p_impro2 "/Users/jnika/Desktop/_SAVEDIMPROS_/2ScenNY_MemELEC2_tr2.or"
      p_impro3 "/Users/jnika/Desktop/_SAVEDIMPROS_/3ScenELEC1_MemNY_tr2.or"
      p_impro4 "/Users/jnika/Desktop/_SAVEDIMPROS_/4ScenELEC2_MemNY_tr2.or"
      p_impro5 "/Users/jnika/Desktop/_SAVEDIMPROS_/5ScenELEC2_MemScenELEC1_tr2.or"
      p_impro6 "/Users/jnika/Desktop/_SAVEDIMPROS_/6ScenELEC1_MemELEC2_tr2.or")



; -------------- SCENARIO : NewYorkCounterpoint_1_tune -------------- 
 
;MEMORY : ElectricCounterpoint_Fast_tune ;1-40 ... 270-300... bof
(start-impro p_impro1 1 0)

;MEMORY : ElectricCounterpoint_Fast_2_tune... BOF
(start-impro p_impro2 1 0)

; -------------- SCENARIO : ElectricCounterpoint_Fast_tune -------------- 

;MEMORY : NewYorkCounterpoint_1_tune ;128 220 ou 250 à 181bpm et 150 crossfade OUI vers 300 AUTRE VOIX trous vers 900
(start-impro p_impro3 1 0)

;MEMORY : ElectricCounterpoint_Fast_2_tune
(start-impro p_impro6 1 0)

; -------------- SCENARIO : ElectricCounterpoint_Fast_2_tune -------------- 

;MEMORY : NewYorkCounterpoint_1_tune beat 48
(start-impro p_impro4 1 0)

;MEMORY : ElectricCounterpoint_Fast_tune
(start-impro p_impro5 1 0)



; -------------- SCENARIO : NewYorkCounterpoint_1_tune -------------- 
 
;MEMORY : ElectricCounterpoint_Fast_tune
(start-impro p_impro1 1 0)

;MEMORY : ElectricCounterpoint_Fast_2_tune
(start-impro p_impro2 1 0)

; -------------- SCENARIO : ElectricCounterpoint_Fast_tune -------------- 

;MEMORY : NewYorkCounterpoint_1_tune
(start-impro p_impro3 1 0)

;MEMORY : ElectricCounterpoint_Fast_2_tune
(start-impro p_impro6 1 0)

; -------------- SCENARIO : ElectricCounterpoint_Fast_2_tune -------------- 

;MEMORY : NewYorkCounterpoint_1_tune
(start-impro p_impro4 1 0)

;MEMORY : ElectricCounterpoint_Fast_tune
(start-impro p_impro5 1 0)

|#
