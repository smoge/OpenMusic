 (in-package :om)



(setf Transpos '(-2 -1 0 1 2))

(setf impro1
      (ImproTruK 1 NewYorkCounterpoint_1_tune ElectricCounterpoint_Fast_tune 1000 nil '(1 1000) Transpos size) ;;; marche bien à 370 250 800 730 ?
      )
(save-impro impro1 "/Users/jnika/Desktop/_SAVEDIMPROS_/1ScenNY_MemELEC1_tr2.or")


(setf impro2
      (ImproTruK 1 NewYorkCounterpoint_1_tune ElectricCounterpoint_Fast_2_tune 1000 nil '(1 1000) Transpos size) ;;; marche bien à 1, de 525 à 600 pourquoi pas 350
      )
(save-impro impro2 "/Users/jnika/Desktop/_SAVEDIMPROS_/2ScenNY_MemELEC2_tr2.or")


(setf impro3
      (ImproTruK 1 ElectricCounterpoint_Fast_tune NewYorkCounterpoint_1_tune 1000 nil '(1 1000) Transpos size) ;;; marche 270 longtemps
      )
(save-impro impro3 "/Users/jnika/Desktop/_SAVEDIMPROS_/3ScenELEC1_MemNY_tr2.or")


(setf impro4
      (ImproTruK 1 ElectricCounterpoint_Fast_2_tune NewYorkCounterpoint_1_tune 1000 nil '(1 1000) Transpos size) ;;; ???
      )
(save-impro impro4 "/Users/jnika/Desktop/_SAVEDIMPROS_/4ScenELEC2_MemNY_tr2.or")



(setf impro5
      (ImproTruK 1 ElectricCounterpoint_Fast_2_tune ElectricCounterpoint_Fast_tune 1000 nil '(1 1000) Transpos size) ;;; ???
      )
(save-impro impro5 "/Users/jnika/Desktop/_SAVEDIMPROS_/5ScenELEC2_MemScenELEC1_tr2.or")


(setf impro6
      (ImproTruK 1 ElectricCounterpoint_Fast_tune ElectricCounterpoint_Fast_2_tune 1000 nil '(1 1000) Transpos size) ;;; ???
      )
(save-impro impro6 "/Users/jnika/Desktop/_SAVEDIMPROS_/6ScenELEC1_MemELEC2_tr2.or")












