; J. Nika, Nov. 2011
;
; D�claration des grilles connues par le logiciel


(in-package :om)


(defparameter *available-grids* (make-hash-table :test #'string= ))

(defmethod! improtest-current-tune () 
            (beats->chseq (mix-poly-impro *current-tune* (oracle *current-tune*)) (beatduration *current-tune*) 0)) ;for OM patch 'improtest-current-tune'

;Test
;--------------------------------------------------------------------------------------------------------------------
(setf Test_grid '(
                        (c maj7 384)     ;384 = 3 x 128 -> 3 grilles de 32 mesures avec chapitres
)
                                                        
      Test_beatdur 400)   ;BPM= + que 150, beatdur= - que 400 ms
(setf Test_tune (make-instance 'tune :grid Test_grid :beatduration Test_beatdur :tunename "Test" :chapters '(1 33 65)))
(setf (gethash '"Test" *available-grids*) Test_tune)


;Valse (Test J�r�me)
;--------------------------------------------------------------------------------------------------------------------
(setf Valse_grid '(
                        (eb maj7 6) (db maj7 6)    
                        (eb maj7 6) (db maj7 6)
                        (eb maj7 6) (db maj7 6)    
                        (eb maj7 6) (db maj7 6)
)
                                                        
      Valse_beatdur 380)   ;BPM= + que 150, beatdur= - que 400 ms
(setf Valse_tune (make-instance 'tune :grid Valse_grid :chapters '(1) :beatduration Valse_beatdur :tunename "Valse" :NbBeatsPerMeasure 3))
(setf (gethash '"Valse" *available-grids*) Valse_tune)

;Helvella (Test J�r�me)
;--------------------------------------------------------------------------------------------------------------------
(setf Helvella_grid '(
                      (e m7 4) (b m7 4) (c maj7 4) (g maj7 4)                    
                      (e m7 2) (b m7 2) (c maj7 2) (g maj7 2) 
                      (e m7 2) (b m7 5) (e m7 1)
                            )
                                                        
      Helvella_beatdur 750)   ;BPM=80, beatdur=750 ms
(setf Helvella_tune (make-instance 'tune :grid Helvella_grid :chapters '(1) :beatduration Helvella_beatdur :tunename "Helvella" :NbBeatsPerMeasure 4))
(setf (gethash '"Helvella" *available-grids*) Helvella_tune)


#|
; Fausse grille pour sauvegarder les exemples du s�minaire avec Jovino
;--------------------------------------------------------------------------------------------------------------------
(setf SeminaireEHESSJovino_grid '(
                             (d maj7 4) (a m7 4) (d maj7 4) (a m7 2) (g maj7 2)
                             (d maj7 4) (b m7 4) (bb m7 4) (f m7 4)
                             (a m7 4) (d m7 2) (g 7 2) (c maj7 4) (g m7 4)
                             (c maj7 4) (g m7 4) (f m7 4) (eb m7 2) (ab 7 2) (db maj7 4)
                             (d m7 1) (eb m7 1) (ab 7 1) (g 7 1) (c maj7 4) (d 7 2) (g 7 2) 
                             (e m7 4) (bb 7 4) (c 7 4) (d 7 4) (e 7 2) (a 7 2)
                             (bb 7 4) (c 7 4) (d 7 2) (g 7 2) (c maj7 4) (e m7 2) (a 7 2)
)
                                                        
      SeminaireEHESSJovino_beatdur 400)   ;BPM=150, beatdur=400 ms
(setf SeminaireEHESSJovino_tune (make-instance 'tune :grid SeminaireEHESSJovino_grid :chapters '(1 33) :beatduration SeminaireEHESSJovino_beatdur :tunename "SeminaireEHESSJovino"))
(setf (gethash '"SeminaireEHESSJovino" *available-grids*) SeminaireEHESSJovino_tune)
|#



;Balaio (Hermeto)
;--------------------------------------------------------------------------------------------------------------------
(setf Balaio_grid '(
                             (d maj7 4) (a m7 4) (d maj7 4) (a m7 2) (g maj7 2)
                             (d maj7 4) (b m7 4) (bb m7 4) (f m7 4)
                             (a m7 4) (d m7 2) (g 7 2) (c maj7 4) (g m7 4)
                             (c maj7 4) (g m7 4) (f m7 4) (eb m7 2) (ab 7 2) (db maj7 4)
                             (d m7 1) (eb m7 1) (ab 7 1) (g 7 1) (c maj7 4) (d 7 2) (g 7 2) 
                             (e m7 4) (bb 7 4) (c 7 4) (d 7 4) (e 7 2) (a 7 2)
                             (bb 7 4) (c 7 4) (d 7 2) (g 7 2) (c maj7 4) (e m7 2) (a 7 2)
)
                                                        
      Balaio_beatdur 400)   ;BPM=150, beatdur=400 ms
(setf Balaio_tune (make-instance 'tune :grid Balaio_grid :chapters '(1 33) :beatduration Balaio_beatdur :tunename "Balaio"))
(setf (gethash '"Balaio" *available-grids*) Balaio_tune)


#|
;====
;TODO
;====
;23 de Junho de 1997 (Hermeto's calendario)
;--------------------------------------------------------------------------------------------------------------------
(setf 23deJunhode1997_grid '(
                           (bb m7 1) (gb 7 1)
                                                  ; REVOIR
                           (eb m7 2) (db maj7 2) (eb 7 1) (f# maj7 1)
                           (g 7 1) (ab m7 1) (bb m7) (f# maj7 1)
                           (bb m7 1) (f# maj7 1) (bb m7 1) (c 7 1)
                           (db maj7 1) (e 7 1) (ab maj7 1) (db 7 1)
                           (a 7 1) (bb m7 1) (g m7 1) (f# maj7 1) (bb m7 1)

                           (c m7 1) (db maj7 1) (d 7 1)
                           (bb m7 1) (c 7 1) (db maj7 1) (d m7 1) (e m7 1)
                           (g m7 1) (bb m7 1) (a 7 1) (bb m7) (a 7 1) (c maj7 1)
)
                                                        
      23deJunhode1997_beatdur 896)   ;BPM=67, beatdur=896 ms
(setf 23deJunhode1997_tune (make-instance 'tune :grid 23deJunhode1997_grid :chapters '(1) :beatduration 23deJunhode1997_beatdur :tunename "23deJunhode1997"))
(setf (gethash '"23deJunhode1997" *available-grids*) 23deJunhode1997_tune)


;23 de Junho de 1997 (Hermeto's calendario)
;--------------------------------------------------------------------------------------------------------------------
(setf 13deJunhode1997_grid '(
                            (e maj7 1) (d maj7 2) (e maj7 2) (d maj 7 1) (c maj7 1)
                            (a m7 2) (b m7 1) (g maj7 1) (e maj7 1) (eb 7 1) (d maj7 1) (db 7 1) (c maj7 2)
                            (a maj7 1) (ab m7 1) (g 7 1) (f# maj7 1) (e maj7 1) (c# m7 1) (d# m7 1) (e maj7 1) 
                            (a maj7 1) (ab m7 1) (g maj7 1) (f# maj7 1) (f maj7 1) (d maj7 1) (e maj7 1) (c# m7 1) (c# maj7 2)
)
                                                        
      13deJunhode1997_beatdur 896)   ;BPM=67, beatdur=896 ms
(setf 13deJunhode1997_tune (make-instance 'tune :grid 13deJunhode1997_grid :chapters '(1) :beatduration 13deJunhode1997_beatdur :tunename "23deJunhode1997"))
(setf (gethash '"23deJunhode1997" *available-grids*) 13deJunhode1997_tune)
|
|#


;Campinas (Hermeto's calendario p.21)
; CODA ?
;--------------------------------------------------------------------------------------------------------------------
(setf Campinas_grid '(
                      (c 7 4)
                      ;---A
                      (f maj7 4) (c 7 4) (f maj7 4)                 ;(A/F)
                      (d m7 2) (c 7 2) (a m7 2) (bb maj7 2) (c 7 4) (a maj7 2) (bb maj7 2)
                      (a m7 2) (d 7 2)
                      
                      ;---B              ;(A/F)
                      (bb m7 4) (eb 7 4) (a maj7 4)
                      ;(A/G)           ;(Dm/C)  ;(Dm/B)
                      (a 7 4) (d m7 4) (d m7 4) (d m7 4)
                      ;e13 puis e7#5
                      (e 7 4) (a maj7 4) (d maj7 4) (gb maj7 4)
                      (eb m7 4) (b maj7 2) (e maj7 2) (d maj7 2) (f# m7 2) (g maj7 4)
                      (c maj7 4)

                      ;---C
                      (f maj7 4) (bb maj7 2) (g m7 2) (a 7 4)
                      (f# 7 2) (b m7 2) (c# m7 4) (g maj7 2) (f maj7 2) (e m7 2) (f# m7 2)
                      (g# m7 2) (a# m7 2) (a maj7 4) (g maj7 4)

                      ;---D
                      (e maj7 4) (eb m7 4)
                      (c maj7 4) (e m7 4) (g maj7 4) (c maj7 4)
                                                        ;"last time only..."
                      (gb maj7 4) (f maj7 4) (b maj7 4) (ab m7 4)
                           
)
                                                        
      Campinas_beatdur 857)   ;BPM=70, beatdur=857 ms
(setf Campinas_tune (make-instance 'tune :grid Campinas_grid :chapters '(1 33) :beatduration Campinas_beatdur :tunename "Campinas"))
(setf (gethash '"Campinas" *available-grids*) Campinas_tune)


;Hermeto (Hermeto's calendario p.34)
;--------------------------------------------------------------------------------------------------------------------
(setf Hermeto_grid '(
                                           
                     (a maj7 4) (g maj7 2) (f# 7 2)
                     (b 7 2) (c 7 2) (b 7 2) (e 7 2) 

                     ;---A
                                                    ;B13
                     (a maj7 4) (g maj7 1) (f# 7 1) (b 7 2)
                     ;B13
                     (b 7 2) (b m7 2) (e 7 2)
                     (g# 7 2) (a maj7 2) (d# m7 2)
                     (g# 7 2) (c# m7 2) (f# 7 2)
                     (f maj7 2) (d m7 2) (b 7 2) (a# 7 2)

                     ;---B
                     (a m7 4) (g# 7 4)
                     (g m7 4) (gb 7 4)
                     (bb maj7 1) (a m7 1) (g m7 2) (a m7 2)
                     (ab maj7 2) (g m7 4)

                     ;---C
                     (b m7 2) (bb maj7 2) (a m7 4)
                     (a# m7 2) (a maj7 2)
                               ;C#13
                     (g# m7 2) (c# 7 2) (g# m7 2) (e 7 2)

                     ;---Coda
                               ;C#13
                     (g# m7 2) (c# 7 2)

                            )
                                                        
      Hermeto_beatdur 750)   ;BPM=80, beatdur=750 ms
(setf Hermeto_tune (make-instance 'tune :grid Hermeto_grid :chapters '(1 33) :beatduration Hermeto_beatdur :tunename "Hermeto" :NbBeatsPerMeasure 2))
(setf (gethash '"Hermeto" *available-grids*) Hermeto_tune)


;Samba do Belaqua (Hermeto's calendario p.50)
;--------------------------------------------------------------------------------------------------------------------
(setf SambaDoBelaqua_grid '(
                            (c m7 2) (f m7 2) (bb 7 2) (eb maj7 2)
                            (c m7 2) (f m7 2) (db maj7 1) (c m7 1) (g m7 1) (ab maj7 1) (g m7 1) (ab maj7 1)

                                                                        
                            (db maj7 1) (c m7 1) (bb m7 1) (ab maj7 1) (db maj7 1) (f maj7 1) (f maj7 2) (g maj7 2) 
                                                                         ;Bb13       ;A13       ;C13
                            (e m7 1) (a 7 1) (c maj7 1) (b 7 1) (e m7 1) (bb 7 1) (a 7 1) (c 7 1)
                                                                                 ;Dm7b5
                            (e m7 1) (a 7 1) (c maj7 1) (b 7 1) (e m7 1) (a 7 1) (d m7 1) (g 7 1)

                                                                         ;Bb13       ;A13       ;C13
                            (e m7 1) (a 7 1) (c maj7 1) (b 7 1) (e m7 1) (bb 7 1) (a 7 1) (c 7 1)
                            (a 7 1) (c maj7 3)

                            )
                                                        
      SambaDoBelaqua_beatdur 600)   ;BPM=100, beatdur=600 ms
(setf SambaDoBelaqua_tune (make-instance 'tune :grid SambaDoBelaqua_grid :chapters '(1) :beatduration SambaDoBelaqua_beatdur :tunename "SambaDoBelaqua" :NbBeatsPerMeasure 2))
(setf (gethash '"SambaDoBelaqua" *available-grids*) SambaDoBelaqua_tune)


;Santo Antonio (Hermeto's calendario p.51)
;--------------------------------------------------------------------------------------------------------------------
(setf SantoAntonio_grid '(
                          ;---A
                          (f m7 2) (c 7 2) (f m7 2) (c 7 2)
                          ;Db13       ;Bb13       ;Db13
                          (db 7 4) (bb 7 2) (db 7 1) (c 7 1)
                      
                          ;---B
                          ;F13
                          (f 7 8)
                          (e 7 8)
                                   ;D13
                          (a m7 6) (d 7 2)

                          ;---C
                          (f maj7 8)

                          ;---D
                          (f maj7 20)
)
                                                        
      SantoAntonio_beatdur 600)   ;BPM=100, beatdur=600 ms
(setf SantoAntonio_tune (make-instance 'tune :grid SantoAntonio_grid :chapters '(1 33) :beatduration SantoAntonio_beatdur :tunename "SantoAntonio" :NbBeatsPerMeasure 2))
(setf (gethash '"SantoAntonio" *available-grids*) SantoAntonio_tune)


;Moment's notice   BPM=240, beatdur=250 ms
;--------------------------------------------------------------------------------------------------------------------
(setf MomentsNotice_grid '(
                             (e m7 2) (a 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (ab m7 4) (db 7 4)
                             (d m7 2) (g 7 2) (eb m7 2) (ab 7 2) (db maj7 4) (d m7 2) (g 7 2)
                             (c m7 2) (b 7 2) (bb m7 2) (eb 7 2) (ab maj7 4) (ab m7 2) (db 7 2)
                             (g m7 2) (c 7 2) (ab m7 2) (db 7 2) (gb maj7 4) (f m7 2) (bb 7 2)
                             
                             (e m7 2) (a 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (ab m7 2) (db 7 2)
                             (d m7 2) (g 7 2) (eb m7 2) (ab 7 2) (db maj7 4) (d m7 2) (g 7 2)
                             (c m7 2) (b 7 2) (bb m7 2) (eb 7 2) (ab maj7 4) (ab m7 2) (db 7 2)
                             (g m7 2) (c 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (f m7 4) (g m7 4)
                             (f m7 4) (eb maj7 2) (f m7 2) (g m7 2) (f m7 2) (eb maj7 4) (eb maj7 4)
                             )
      MomentsNotice_beatdur 250)
(setf MomentsNotice_tune (make-instance 'tune :grid MomentsNotice_grid :beatduration MomentsNotice_beatdur :tunename "MomentsNotice"))

(setf (gethash '"MomentsNotice" *available-grids*) MomentsNotice_tune)


;Song for my father      ;BPM=120, beatdur=500 ms
;--------------------------------------------------------------------------------------------------------------------
(setf SongForMyFather_grid '(;A
                             (f m7 4) (f m7 4) (eb 7 4) (eb 7 4) (db 7 4) (c 7 4) (f m7 4) (f m7 4)
                             ;A
                             (f m7 4) (f m7 4) (eb 7 4) (eb 7 4) (db 7 4) (c 7 4) (f m7 4) (f m7 4)
                             ;B
                             (eb 7 4) (eb 7 4) (f m7 4) (f m7 4) (eb 7 2) (db 7 2) (c 7 4) (f m7 4) (f m7 4)
                             )
      SongForMyFather_beatdur 500)   ;BPM=120, beatdur=500 ms
(setf SongForMyFather_tune (make-instance 'tune :grid SongForMyFather_grid :beatduration SongForMyFather_beatdur :tunename "SongForMyFather"))

(setf (gethash '"SongForMyFather" *available-grids*) SongForMyFather_tune)


;Cantelope Island   ;BPM=100, beatdur=600 ms
;--------------------------------------------------------------------------------------------------------------------
(setf CantelopeIsland_grid '((f m7 4) (f m7 4) (f m7 4) (f m7 4)
                             (f m7 4) (f m7 4) (f m7 4) (f m7 4)
                             (db 7 4) (db 7 4) (db 7 4) (db 7 4)
                             (d m7 4) (d m7 4) (d m7 4) (d m7 4))
      CantelopeIsland_beatdur 600)
(setf CantelopeIsland_tune (make-instance 'tune :grid CantelopeIsland_grid :beatduration CantelopeIsland_beatdur :tunename "CantelopeIsland"))

(setf (gethash '"CantelopeIsland" *available-grids*) CantelopeIsland_tune)



;Cecile ma fille   ;BPM=160 (beatdur=375)
;--------------------------------------------------------------------------------------------------------------------
(setf Cecilemafille_grid '((c m7 3) (f m7 3) (g 7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3) (eb maj7 3) 
                           (f m7 3) (g 7 3) (c m7 3) (f 7 3) (bb m7 2) (eb 7 1) (ab maj7 3) (d m7 2) (g 7 1) (c m7 3) (g 7 3)
                           (c m7 3) (f m7 3) (g 7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3) (eb maj7 3) 
                           (f m7 3) (g 7 3) (c m7 3) (f 7 3) (bb m7 2) (eb 7 1) (ab maj7 3) (d m7 2) (g 7 1) (c m7 3)
                           (f m7 3) (g 7 3) (c m7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3)  (eb maj7 3)
                           (f m7 3) (g 7 3) (c m7 3) (d 7 3) (g m7 3) (d 7 3) (g 7 3) (g 7 3)
                           ;(c m7 3) (f m7 3) (g 7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3) (eb maj7 3) 
                           ;(f m7 3) (g 7 3) (c m7 3) (f 7 3) (bb m7 2) (eb 7 1) (ab maj7 3) (d m7 2) (g 7 1) (c m7 3)
                           )
      Cecilemafille_beatdur 375)
(setf Cecilemafille_tune (make-instance 'tune :grid Cecilemafille_grid :chapters '(1 33) :beatduration Cecilemafille_beatdur :tunename "Cecilemafille" :NbBeatsPerMeasure 3))

(setf (gethash '"Cecilemafille" *available-grids*) Cecilemafille_tune)



;Handful of keys   ;BPM=240 (beatdur=250)
;--------------------------------------------------------------------------------------------------------------------
(setf Handfulofkeys_intro '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4))        ;8 mes. 
      Handfulofkeys_theme1 '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)     ;32 mes. 
                             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)
                             (a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4) 
                             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4))
      Handfulofkeys_trans '((f maj7 4) (f maj7 2) (c 7 2) (c 7 4) (f 7 4) (f 7 4))                        ;5 mes. (-> bifurque sur derniere mes. Theme1 = C7)
      Handfulofkeys_theme2                                                                                ;32 mes.
                          '((bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) 
                            (d 7 4) (d 7 4) (g m7 4) (g 7 4) (c 7 4) (c 7 4) (f 7 4) (f 7 4)
                            (bb maj7 4) (bb maj7 4) (f 7 4) (f 7 4) (bb 7 4) (bb 7 4)      ;;;;; error (bb maj7 4) (bb maj7 4) 
                                                                    (eb maj7 4) (eb maj7 4) (eb maj7 4) (eb maj7 4)
                            (bb maj7 4) (bb maj7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 2) (c 7 2))
                            

                           ; (bb maj7 2) (bb 7 2) (eb maj7 2) (eb m7 2) (bb maj7 2) (eb m7 2) (bb maj7 2) (f 7 2) (bb maj7 2) (bb 7 2) (eb maj7 2) (eb m7 2) (bb maj7 2) (eb m7 2) (bb maj7 4)
                           ;  (d m7 2) (d 7 2) (d 7 4) (g m7 2) (eb m7 2) (g 7 4) (c 7 4) (c 7 4) (f 7 4) (f 7 4)
                           ;  (bb maj7 4) (bb maj7 2) (c# dim 2) (f 7 4) (bb 7 4) (bb 7 4) (eb maj7 4) (eb maj7 2) (bb 7 2) (eb maj7 2) (bb 7 2)
                           ;  (eb maj7 2) (eb m7 2) (bb maj7 4) (g 7 4) (c 7 4) (f 7 4) (bb maj7 2) (f 7 2) (bb maj7 2) (c 7 2))

      Handfulofkeys_coda   '((f maj7 4) (f maj7 4) (f maj7 4) (c 7 2) (f 7 2) (f 7 4) (f 7 4) (f 7 4) (f 7 4) (f 7 4))     ;7 mes. (-> bifurque sur 2 dernieres mes. Theme1 = Fmaj7 C7) 
                                                                                                                      ;6/1/2014 ajout 1 mes. coda = 9 mes. 
                                                                                          ;29/4/2014 num-beat = 4 * (num-mes - 1) + 1
                                                                                          ;INTRO:
                                                                                          ;          m. 1 -> beat1 = 1er beat mes. 1 = 4 * (1 - 1) + 1 = 0+1 = 1    
                                                                                          ;          m. 2 -> beat5 = 1er beat mes. 2 = 4 * (2 - 1) + 1 = 4+1 = 5
                                                                                          ;          m. 3 -> beat9 = 1er beat mes. 3 = 4 * (3 - 1) + 1 = 8+1 = 9

                                                                                          ;THEME1
                                                                                          ;          m. 9 -> beat33 = 4*8+1 = 33           (f maj7 4)
                                                                                          ;    
                                                                                          ;          m. 38 = 9+8+8+8+5 -> beat149 = 4*37+1 = 149         (c 7 4)  -> bifurque vers THEME2
                                                                                          ;          m. 39 = 9+8+8+8+6 -> beat153 = 4*38+1 = 153         (f maj7 4) -> bifurque vers CODA 
                                                                                          ;          m. 40 = 9+(32-1)  -> beat157          = 157         (c 7 4)                                          
                                                                                          ;CODA  
                                                                                          ;          m. 41             -> beat161 = 4*40+1 = 161         (f maj7 4)
                                                                                          ;          m. 42             -> beat165 = 4*41+1 = 165         (f maj7 4) 
                                                                                          ;          m. 43             -> beat169 = 4*42+1 = 169         (f maj7 4)
                                                                                          ;          m. 44             -> beat173 = 4*43+1 = 173         (c 7 2)
                                                                                          ;                            -> beat175          = 175         (f 7 2)
                                                                                          ;          m. 45             -> beat177 = 4*44+1 = 177         (f 7 4)
                           ;'((f maj7 2) (f 7 2) (bb m7 4) (b dim 4) (c 7 2) (f 7 2) (f 7 4) (f 7 4) (f 7 4))
   
      Handfulofkeys_grid (append Handfulofkeys_intro Handfulofkeys_theme1 Handfulofkeys_coda 
                                 Handfulofkeys_trans Handfulofkeys_theme2)   ;;;permutation of Theme2: pb of 'gridconnect' limited to 64 bars
                       
;      Handfulofkeys_grid (append Handfulofkeys_intro Handfulofkeys_theme1 Handfulofkeys_trans Handfulofkeys_theme2 Handfulofkeys_coda)

      Handfulofkeys_beatdur 250)
(setf Handfulofkeys_tune (make-instance 'tune :grid Handfulofkeys_grid :beatduration Handfulofkeys_beatdur :tunename "Handfulofkeys" :chapters '(1 9 41 50)))

(setf (gethash '"Handfulofkeys" *available-grids*) Handfulofkeys_tune)







#|
;Coco
;--------------------------------------------------------------------------------------------------------------------
;Cycle 8 pulsations =  2 x 4
;Forme la plus courante: solo tambour grave bombo ou zabumba (grosse caisse) trois types de frappes (m.d. = baguette peau ou bord, m.g. = tige)
;+ accompagnement fixe pandeiro (tambourin), tarol (caisse claire), ganza (hochet)
(setf Coco_grid '((c maj7 4) (c maj7 4))
      Coco_beatdur 666)   ;BPM=90
(setf Coco_tune (make-instance 'tune :grid Coco_grid :beatduration Coco_beatdur :tunename "Coco"))

(setf (gethash '"Coco" *available-grids*) Coco_tune)
|#


;Maracatu
;--------------------------------------------------------------------------------------------------------------------
;Solo tambour grave (un seul type de frappe) + accompagnement hochet, BPM=90
(setf Maracatu_grid '((c maj7 1) (c# maj7 1) (d maj7 1) (eb maj7 1)  ;;; necessite de distinguer les 8 temps (accents localises)
                      (e maj7 1) (f maj7 1) (f# maj7 1) (g maj7 1))
      Maracatu_beatdur 666)   ;BPM=90
(setf Maracatu_tune (make-instance 'tune :grid Maracatu_grid :beatduration Maracatu_beatdur :tunename "Maracatu"))

(setf (gethash '"Maracatu" *available-grids*) Maracatu_tune)




;;;;;;;;;;;;;;;;;;;; MARC 15/11/2011 ;;;;;;;;;;;;;;;;;;;;;;;
;ajout des oracles de juillet dans (oracle tune) pour "J'aime", "All the things", "Dici d'en bas", et "Goodbye"
;voir � la fin du fichier


;Blue in green      ;BPM=75 (beatdur=790 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf BlueInGreen_grid '((bb maj7 4) (a 7 4) (d m7 2) (db 7 2) (c m7 2) (f 7 2)
                         (bb maj7 4) (a 7 4) (d m7 4) (e 7 4) (a m7 4) (d m7 4)) 
      BlueInGreen_beatdur 790)
(setf BlueInGreen_tune (make-instance 'tune :grid BlueInGreen_grid :beatduration BlueInGreen_beatdur :tunename "BlueInGreen"))

(setf (gethash '"BlueInGreen" *available-grids*) BlueInGreen_tune)



;Straight no chaser     ;BPM=181 (beatdur=330 ms) 
;--------------------------------------------------------------------------------------------------------------------
(setf StraightNoChaser_grid '((bb 7 4) (eb 7 4) (bb 7 4) (bb 7 4) (eb 7 4) (eb 7 4)
                              (bb 7 4) (bb 7 4) (f 7 4) (f 7 4) (bb 7 4) (bb 7 4)) 
      StraightNoChaser_beatdur 330)
(setf StraightNoChaser_tune (make-instance 'tune :grid StraightNoChaser_grid :beatduration StraightNoChaser_beatdur :tunename "StraightNoChaser"))

(setf (gethash '"StraightNoChaser" *available-grids*) StraightNoChaser_tune)


;Autumn Leaves DoMin    ;BPM=181 (beatdur=330 ms)     ---> en fait SolMin, Cm7 est le premier accord
;--------------------------------------------------------------------------------------------------------------------
(setf AutumnleavesDoMin_grid '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)

                             (c m7 4) (f 7 4)    (bb maj7 4) (eb maj7 4)               (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                                              ;;;;SUBSTITUTION Bernard:
                             ;(c m7 4) (f 7 4)   (b m7 2) (e 7 2) (bb m7 2) (eb 7 2)   (a m7 4) (d 7 4) (g m7 4) (g m7 4)

                             (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4)     (bb maj7 4) (bb maj7 4)
                                                                                ;;;SUBSTITUTION Bernard:
                             ;(a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4)    (b m7 4) (e 7 4)

                              (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4)          (g m7 4) (g m7 4) ) 
                                                                        ;;;SUBSTITUTION Bernard:
                                                                      ;;;(bb 7 2) (eb maj7 4) (a m7 2) (d 7 2) 
                                              ;;;SUBSTITUTION Marc -> uniquement accords 7
                                              ;;;(g 7 2) (gb 7 2) (f 7 2) (e 7 2) (eb 7 4) (d 7 4)


      AutumnleavesDoMin_beatdur 330)
(setf AutumnleavesDoMin_tune (make-instance 'tune :grid AutumnleavesDoMin_grid :beatduration AutumnleavesDoMin_beatdur :tunename "AutumnleavesDoMin"))

(setf (gethash '"AutumnleavesDoMin" *available-grids*) AutumnleavesDoMin_tune)

#|
;Autumn Leaves
;--------------------------------------------------------------------------------------------------------------------
(setf Autumnleaves-en-mi_grid '((a m7 4) (d 7 4) (g maj7 4) (c maj7 4) (f# m7 4) (b 7 4) (e m7 4) (e m7 4)
                          (a m7 4) (d 7 4) (g maj7 4) (c maj7 4) (f# m7 4) (b 7 4) (e m7 4) (e m7 4)
                          (f# m7 4) (b 7 4) (e m7 4) (e m7 4) (a m7 4) (d 7 4) (g maj7 4) (g maj7 4)
                          (f# m7 4) (b 7 4) (e 7 2) (a 7 2) (d 7 2) (g 7 2) (c 7 4) (b 7 4) (e m7 4) (e m7 4) ) 
      Autumnleaves-en-mi_beatdur 330)
(setf Autumnleaves-en-mi_tune (make-instance 'tune :grid Autumnleaves-en-mi_grid :beatduration Autumnleaves-en-mi_beatdur :tunename "Autumnleaves-en-mi"))

(setf (gethash '"Autumnleaves-en-mi" *available-grids*) Autumnleaves-en-mi_tune)
|#

;Garnerloop             ;BPM=127 (beatdur=472 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Garnerloop_grid '((d m7 4) (g 7 4) (c maj7 4) (a 7 4)
) 
      Garnerloop_beatdur 472)
(setf Garnerloop_tune (make-instance 'tune :grid Garnerloop_grid :beatduration Garnerloop_beatdur :tunename "Garnerloop"))

(setf (gethash '"Garnerloop" *available-grids*) Garnerloop_tune)



;Reveeveille            ;BPM=126 (beatdur=476 ms)
;--------------------------------------------------------------------------------------------------------------------
;"126.Rev 3.wav", BPM=126, basse re-mi-sol-la, riff la#-si,  impro sur E7
;Dijon mars 2008: BPM=150 (beatdur=400 ms), basse sol-re-fa-do, grille Am7 Bm7 Gm7 Am7

(setf Reveeveille_grid '((e 7 16)) 
      Reveeveille_alternativegrid '((a m7 4) (b m7 4) (g m7 4) (a m7 4)) 
      Reveeveille_beatdur 476)
(setf Reveeveille_tune (make-instance 'tune :grid (append Reveeveille_grid Reveeveille_grid Reveeveille_grid Reveeveille_grid)
                                      :alternategrid (append Reveeveille_alternativegrid Reveeveille_alternativegrid 
                                                               Reveeveille_alternativegrid Reveeveille_alternativegrid)
                                      :beatduration Reveeveille_beatdur :tunename "Reveeveille"))

(setf (gethash '"Reveeveille" *available-grids*) Reveeveille_tune)




;BagsGroove (ou Au privave)     ;BPM=144 (beatdur=414 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Bagsgroove_grid '((f 7 4) (f 7 4) (f 7 4) (f 7 4) (bb 7 4) (bb 7 4) (f 7 4) (f 7 4) (g m7 4) (c 7 4) (f 7 4) (c 7 4)) 
      Bagsgroove_beatdur 414)
(setf Bagsgroove_tune (make-instance 'tune :grid Bagsgroove_grid :beatduration Bagsgroove_beatdur :tunename "Bagsgroove"))

(setf (gethash '"Bagsgroove" *available-grids*) Bagsgroove_tune)




#|
;Baleine
;--------------------------------------------------------------------------------------------------------------------
;BPM=67, beatdur=896 ms
(setf Baleine_grid '((d m7 4) (e m7 4))
      Baleine_beatdur 896)
(setf Baleine_tune (make-instance 'tune :grid Baleine_grid :beatduration Baleine_beatdur :tunename "Baleine"))

(setf (gethash '"Baleine" *available-grids*) Baleine_tune)
|#


;Night in Tunisia
;--------------------------------------------------------------------------------------------------------------------
;"Chansons enjazzees" BPM=240, beatduration=250 ms  
; A / chant: AABA+C AABA+C /  scat: AAB+AABA+C / chant: AABA+C
(setf Nightintunisia_grid '((eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (e m7 2) (a 7 2) (d m7 4)  ; A
                            (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (e m7 2) (a 7 2) (d m7 4)  ; A

                            (a m7 4) (d 7 4) (g m7 4) (g m7 4) (g m7 4) (c 7 4) (f maj7 4) (e m7 2) (a 7 2)  ; B

                            (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (e m7 2) (a 7 2) (d m7 4)  ; A

                            (e m7 4) (e m7 4) (eb 7 4) (eb 7 4) (d m7 4) (d m7 4) (g 7 4) (g 7 4)            ; C
                            (g m7 4) (g m7 4) (f# 7 4) (f# 7 4) (f maj7 4) (f maj7 4) (e m7 4) (a 7 4))
      Nightintunisia_beatdur 250)
(setf Nightintunisia_tune (make-instance 'tune :grid Nightintunisia_grid :beatduration Nightintunisia_beatdur :tunename "Nightintunisia"))

(setf (gethash '"Nightintunisia" *available-grids*) Nightintunisia_tune)



;D'ici d'en bas             ;BPM=188 (batdur=319 ms)
;--------------------------------------------------------------------------------------------------------------------
;"188.128dicirhytmic.aif"  BPM=188, beatdur=319 ms
;"Chansons enjazzees" BPM=220, beatdur=272 ms  (WARNING: quarter notes, not half notes !!!)
; chant: AABB AABB AABB / scat: AABB / chant: AABB AABB / basse: AABB AABB AABB "choeur": AABB AABB
(setf Dicidenbas_grid '((f m7 4) (g 7 4) (c m7 4) (c m7 4) (f m7 4) (g 7 4) (c m7 4) (c m7 4)  ; A
                        (f m7 4) (g 7 4) (c m7 4) (c m7 4) (f m7 4) (g 7 4) (c m7 4) (c 7 4)  ; A
                        (f m7 4) (bb 7 4) (eb maj7 4) (ab maj7 4) (d 7 4) (g 7 4) (c m7 4) (c 7 4) ; B
                        (f m7 4) (bb 7 4) (eb maj7 4) (ab maj7 4) (d 7 4) (g 7 4) (c m7 4) (c m7 4)) ; B
      Dicidenbas_beatdur 319)
(setf Dicidenbas_tune (make-instance 'tune :grid Dicidenbas_grid :beatduration Dicidenbas_beatdur :tunename "Dicidenbas"))

(setf (gethash '"Dicidenbas" *available-grids*) Dicidenbas_tune)




;J'aime pour la vie        BPM=100, beatdur=600 ms
;--------------------------------------------------------------------------------------------------------------------
;"Chansons enjazzees" BPM=100, beatdur=600 ms         -> structure AAB (faire tourner le A si besoin)
; chant: ABA / scat: AAB / chant: ABA
(setf Jaime_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                   (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                   (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                   (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                   (g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) ; B
                   (g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2))
 
      Jaime_beatdur 600)
(setf Jaime_tune (make-instance 'tune :grid Jaime_grid :beatduration Jaime_beatdur :tunename "Jaime"))

(setf (gethash '"Jaime" *available-grids*) Jaime_tune)


;Free        BPM=200, beatdur=300 ms
;--------------------------------------------------------------------------------------------------------------------
(setf Free_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) ; B
                  (g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2))
 
      Free_beatdur 300)
(setf Free_tune (make-instance 'tune :grid Free_grid :beatduration Free_beatdur :tunename "Free"))

(setf (gethash '"Free" *available-grids*) Free_tune)


;Goodbye Porkpie Hat (Mes nuits blanches)   BPM=70, beatdur=857 ms
;--------------------------------------------------------------------------------------------------------------------
;"Chansons enjazzees" BPM=70, beatdur=857 ms 
(setf Goodbyeporkpiehat_grid '((f 7 2) (db 7 2) (gb maj7 2) (b 7 2) (eb 7 2) (db 7 2) (eb 7 2) (f 7 2)
                     (bb m7 2) (ab 7 2) (g m7 2) (c 7 2) (d 7 2) (g 7 2) (db 7 2) (gb maj7 2)
                     (bb 7 2) (db 7 2) (c 7 2) (eb 7 2) (f 7 2) (db 7 2) (gb maj7 2) (b 7 2))
      Goodbyeporkpiehat_beatdur 857)
(setf Goodbyeporkpiehat_tune (make-instance 'tune :grid Goodbyeporkpiehat_grid :beatduration Goodbyeporkpiehat_beatdur :tunename "Goodbyeporkpiehat"))

(setf (gethash '"Goodbyeporkpiehat" *available-grids*) Goodbyeporkpiehat_tune)


#|
;Test
;--------------------------------------------------------------------------------------------------------------------
(setf Test_grid 
'((d m7 4) (g 7 2) (e m7 2)  
  (d m7 4) (a m7 4)
  (d m7 4) (g 7 2) (e m7 2)  
  (d m7 4) (a m7 4)
  (d m7 2) (g 7 2) (d m7 2) (g 7 2)
  (d m7 2) (eb maj7 2) (db maj7 4)
  (d m7 4) (a m7 4)
  (d m7 4) (g 7 4)
  (d m7 4) (g 7 4)
  (d m7 4) (g 7 4)
(c maj7 4) (c maj7 4)
)
Test_beatdur 500)
(setf Test_tune (make-instance 'tune :grid Test_grid :beatduration Test_beatdur :tunename "Test"))

(setf (gethash '"Test" *available-grids*) Test_tune)
|#

;All the things you are       BPM=180 (beatdur=333 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Allthethingsyouare_grid '((f m7 4) (bb m7 4) (eb 7 4) (ab maj7 4)
                                (db maj7 4) (d m7 2) (g 7 2) (c maj7 4) (c maj7 4)

                                (c m7 4) (f m7 4) (bb 7 4) (eb maj7 4)
                                (ab maj7 4) (a m7 2) (d 7 2) (g maj7 4) (g maj7 4)

                                (a m7 4) (d 7 4) (g maj7 4) (g maj7 4)
                                (f# m7 4) (b 7 4) (e maj7 4) (g m7 2) (c 7 2)
                                ;(f# m7b5 4) (b 7alt 4) (e maj7 4) (g m7b5 2) (c 7alt 2)

                                (f m7 4) (bb m7 4) (eb 7 4) (ab maj7 4)
                                (db maj7 4) (db m7 4) (c m7 4) (b m7 4)
                                ;(db maj7 4) (db m7 4) (c m7 4) (b dim7 4)
                                (bb m7 4) (eb 7 4) (ab maj7 4) (g m7 2) (c 7 2)
                                ;(bb m7 4) (eb 7 4) (ab maj7 4) (g m7b5 2) (c 7alt 2)


)
      Allthethingsyouare_beatdur 333)
(setf Allthethingsyouare_tune (make-instance 'tune :grid Allthethingsyouare_grid :chapters '(1 33) :beatduration Allthethingsyouare_beatdur :tunename "Allthethingsyouare"))

(setf (gethash '"Allthethingsyouare" *available-grids*) Allthethingsyouare_tune)

#|
;Test2
;--------------------------------------------------------------------------------------------------------------------

(setf Test2_grid 
'( 
  (c maj7 4) (c maj7 4) (c maj7 4) (c maj7 4)
  (a m7 4) (a m7 4) 
  (f maj7 4) (g 7 4) (c maj7 4) (f maj7 4) 
  (c maj7 2) (g 7 2)
  (c maj7 4) (c maj7 4)
)
Test2_beatdur 500)
(setf Test2_tune (make-instance 'tune :grid Test2_grid :beatduration Test2_beatdur :tunename "Test2"))

(setf (gethash '"Test2" *available-grids*) Test2_tune)
|#



;Alice in wonderland
;--------------------------------------------------------------------------------------------------------------------

(setf AliceInWonderland_grid 
      '( 
        (d m7 3) (g 7 3) (c maj7 3) (f maj7 3) (b m7 3) (e 7 3) 
  ;                                         m7b5
        (a m7 3) (eb 7 3) (d m7 3) (g 7 3) (e m7 3) (a m7 3)
        (d m7 3) (g 7 3) (e m7 2) (a 7 1) (d m7 2) (g 7 1)
        
        
        (d m7 3) (g 7 3) (c maj7 3) (f maj7 3) (b m7 3) (e 7 3) 
  ;                                         m7b5
        (a m7 3) (eb 7 3) (d m7 3) (g 7 3) (e m7 3) (a m7 3)
        (d m7 3) (g 7 3) (c maj7 3) (a m7 3) 
        
        
        (d m7 3) (g 7 3) (e m7 3) (a m7 3) (d m7 3)
        (g 7 3) (c maj7 3) (f maj7 3) (f# m7 3) (b 7 3)
  ;                                          b9
        (e m7 3) (a 7 3) (d m7 2) (a 7 1) (d m7 2) (a 7 1) (d m7 2) (ab 7 1) (g 7 3)
        (d m7 3) (g 7 3) (c maj7 3) (f maj7 3) (b m7 3)
  ;                                       m7b5
        (e 7 3) (a m7 3) (eb 7 3) (d m7 3) (g 7 3)
        (e m7 3) (a m7 3) (d m7 3) (g 7 3) (c maj7 3) (c maj7 3)
        
        )
      AliceInWonderland_beatdur 400)
(setf AliceInWonderland_tune (make-instance 'tune :grid AliceInWonderland_grid :beatduration AliceInWonderland_beatdur :NbBeatsPerMeasure 3 :tunename "AliceInWonderland"))

(setf (gethash '"AliceInWonderland" *available-grids*) AliceInWonderland_tune)



;AuPrivave
;--------------------------------------------------------------------------------------------------------------------

(setf AuPrivave_grid 
      '( 
        (f maj7 4) (g m7 2) (c 7 2) (f maj7 2) (g m7 2)
        (c m7 2) (f 7 2) (bb 7 4) (bb m7 2) (eb 7 2)
        ;           +7       7b9
        (f maj7 2) (g m7 2) (a m7 2) (d 7 2) (g m7 4)
        (g m7 2) (c 7 2) (f maj7 2) (d 7 2) (g m7 2) (c 7 2)
        ;                              7b9
        
        )
      
      AuPrivave_beatdur 460)
(setf AuPrivave_tune (make-instance 'tune :grid AuPrivave_grid :beatduration AuPrivave_beatdur :tunename "AuPrivave"))

(setf (gethash '"AuPrivave" *available-grids*) AuPrivave_tune)





;BluesForAlice
;--------------------------------------------------------------------------------------------------------------------

(setf BluesForAlice_grid 
      '( 
        (f maj7 4) (e m7 2) (a 7 2) (d m7 2) (g 7 2)
        ;             m7b5     7b9
        (c m7 2) (f 7 2) (bb 7 4) (bb m7 2) (eb 7 2)
        (a m7 2) (d 7 2) (ab m7 2) (db 7 2) (g m7 4)
        (c 7 4) (f maj7 2) (d m7 2) (g m7 2) (c 7 2)
        
        )
      
      BluesForAlice_beatdur 400)
(setf BluesForAlice_tune (make-instance 'tune :grid BluesForAlice_grid :beatduration BluesForAlice_beatdur :tunename "BluesForAlice"))

(setf (gethash '"BluesForAlice" *available-grids*) BluesForAlice_tune)





; Jerome 20/03/13
; (d 7 4) => (d 7)(d 7)(d 7)(d 7)
(defun expand_grid (grid)
  (setf expanded_grid 
        (loop for i from 0 to (1- (list-length grid)) 
              append 
(if (> (list-length (nth i grid)) 2)
                    (loop for j from 0 to (1- (nth 2 (nth i grid))) 
                          collect
                          (list (nth 0 (nth i grid)) (nth 1 (nth i grid)))
                          ) (list (nth i grid))))))

(defun make-oracle-from-beatlist (beatlist) (NewImprovizer beatlist))


#|
;St-Just Blues
;--------------------------------------------------------------------------------------------------------------------
;"Scatrap Jazzcogne" BPM=56, beatdur=1071 ms 
(setf St-Justblues_grid '((f 7 4) (db 7 4) (f 7 4) (c m7 2) (f 7 2)



      St-Justblues_beatdur 600)
(setf St-Justblues_tune (make-instance 'tune :grid St-Justblues_grid :beatduration St-Justblues_beatdur :tunename "St-Justblues"))
|#


;=====================
;DATA FOR SOLO ORACLES
;=====================
; MIDI Files:
;      - "Allthethingsyouare-solo+voicings.mid"  ---> Uzeste, November 19th 2011
;      - "Dicidenbas.soloBernard18nov2011.mid"   ---> Uzeste, November 18th 2011   BPM=168, beatdur=357
;      - "Dicidenbas.soloBernard19nov2011.mid"   ---> Uzeste, November 19th 2011
;      - "Jaime-solo2.mid" ("ticotico")          ---> Uzeste, November 17th 2011

; Jaime-nov2007_beatsfromfile ---> Uzeste, November 3rd, 2007 (BPM=88, beatdur=682, FROM ImprotekInprogress.lisp, folder "Bernard BAL")         
; Jaimesolo-juil2011_beatsfromfile ---> Uzeste, July 13th, 2011 (BPM=102, beatdur=589)
; Allthethingssolo-juil2011_beatsfromfile ---> Uzeste, July 13th, 2011 (BPM=180, beatdur=333, mean 326)
; Goodbyesolo-juil2011_beatsfromfile  ---> Uzeste, July 13th, 2011 (BPM=70, beatdur=857)
; Dicidenbassolo-juil2011_beatsfromfile  ---> Uzeste, July 13th, 2011 (BPM=204, beatdur=294)

; + from ImprotekData.lisp
; Dicidenbas_beatsfromfile  ---> Ircam, May 8th 2004 (BPM=188, beatdur=319)   ---> percu table "188.128dicirhytmic.aif"
; Goodbye_beatsfromfile   ---> Uzeste, October 1rst 2004 (BPM=77, beatdur=779) 
; Zistesolo_beatsfromfile   ---> Uzeste, April 4th 2003 (BPM=112, beatdur=536) 
; Billevans_beatsfromfile   ---> "Israel", Bill Evans, February 2nd 1961 (BPM=182, beatdur=328)

; + from Garner.lisp
; Garner-TeaForTwo_beatsfromfile
; Garner-CloseToYou_beatsfromfile
; ---> 2 oracles: garnerrightoracle, garnerleftoracle  (BPM=254, beatdur=236, half tempo BPM=127)


#|
;--------------------------------------------------------------
;Exploration of the oracles for preparing phrases to Antescofo:
;--------------------------------------------------------------

;;; For oracles on other tunes see below
(setf beatduroracle Dicidenbassolo-juil2011_beatdur           ;BPM=204 beatdur=294             
      beatsfromfile Dicidenbassolo-juil2011_beatsfromfile
      oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle))

(setf beatduroracle Garner-CloseToYou_beatdur           ;BPM=204 beatdur=294             
      beatsfromfile Garner-CloseToYou_beatsfromfile
      oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle))

;;; ... or open a MIDI file
(setf tmp (midi-to-beats) beatduroracle (second tmp) beatsfromfile (first tmp)
      oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle))

;for cumulating oracles: (setf oracle (add-improvizer oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle)))


(setf toto (make-instance 'tune))
(gethash 3 (oracletable toto))
(gethash 6 (oracletable toto))
(max-continuity (gethash 6 (oracletable toto)))
(max-continuity (gethash 3 (oracletable toto)))
(max-continuity (gethash 7 (oracletable )))

(max-continuity (gethash 3 (oracletable *current-tune*)))


(setf *current-tune* Dicidenbas_tune)                         ;BPM=188 beatdur=319
(setf (gethash 6 (oracletable *current-tune*)) oracle)                ;channel 6: solo oracle
(setf (gethash 3 (oracletable *current-tune*)) garnerleftoracle)      ;channel 3: comping oracle, max-continuity = 1000


;;;;;;;;;
(ImprovizeOnHarmGrid (gethash 6 (oracletable *current-tune*)) (length (simplegrid *current-tune*)) (simplegrid *current-tune*))
(mix-impro-multi-oracle *current-tune* 
                        (list (gethash 6 (oracletable *current-tune*)) (gethash 7 (oracletable *current-tune*)) (gethash 8 (oracletable *current-tune*))) 6)

(remove nil '(142 270 398 526 654 782) :test #'(lambda (x y) (remove-if-tabou-or-region y (gethash 6 (oracletable *current-tune*)))))
(remove nil '(1 2 3) :test (lambda (x) (= x 2)))
(loop for x in '(142 270 398 526 654 782) collect (remove-if-tabou-or-region x (gethash 6 (oracletable *current-tune*))))
(remove-if-tabou-or-region 142 (gethash 6 (oracletable *current-tune*)))
(start-region (gethash 6 (oracletable *current-tune*)))
(and (tabou-mode (gethash 6 (oracletable *current-tune*))) (gethash 142 (tabou (gethash 6 (oracletable *current-tune*)))))
(or (< 142 (first (start-region (gethash 6 (oracletable *current-tune*))))) (> 142 (second (start-region (gethash 6 (oracletable *current-tune*))))))))
(setf (tabou-mode (gethash 6 (oracletable *current-tune*))) t)

(max-continuity (gethash 6 (oracletable *current-tune*)))

(setf (max-continuity (gethash 6 (oracletable *current-tune*))) 1000)
(start-region (gethash 6 (oracletable *current-tune*)))
(tabou-mode (gethash 6 (oracletable *current-tune*)))
(setf (tabou-mode (gethash 6 (oracletable *current-tune*))) t)
(loop for x being the hash-key of (tabou (gethash 6 (oracletable *current-tune*))) using (hash-value q) do (print (list x q)))
;;;;;;;;



(progn (pgmout 4 6) (Stop-Player *general-player*)
(setf impro (mix-impro-multi-oracle *current-tune* (loop for i in '(6 7 8) collect (gethash i (oracletable *current-tune*))) 6))
(setf impro1 (merger (beats->chseq impro (beatduration *current-tune*) 0)  
                     (beats->chseq (make-clocks (length (simplegrid *current-tune*)) (beatduration *current-tune*) 2) 
                                   (beatduration *current-tune*) 0)))
(pgmout 4 3) (pgmout 4 4) (pgmout 4 5)
(play impro1))


(save-for-antescofo impro (beatduration *current-tune*))

(save-for-antescofo impro beatduroracle)  
;/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/buffer-for-antescofo.txt

(Stop-Player *general-player*)




;;; PLAY THE ORIGINAL ORACLE WITH ITS OWN BEAT DURATION

(progn (pgmout 4 2) (Stop-Player *general-player*)
(setf impro1 (merger (beats->chseq (thread-Beats (oracle->beatlist oracle) (RefTempo oracle)) (RefTempo oracle) 0)
                     (beats->chseq (make-clocks (maxetat oracle) (RefTempo oracle) 2) (RefTempo oracle) 0)))
(play impro1))

(Stop-Player *general-player*)
(my-save-as-midi impro1 beatduroracle)

;;; IMPROVIZE ON THE ORACLE ACCORDING TO ORIGINAL ORACLE BEAT DURATION                                                     

(progn  (Stop-Player *general-player*)
(setf impro (beats->chseq (mix-poly-impro *current-tune* garnerleftoracle) (beatduration *current-tune*) 0))
;(setf impro (merger impro
;                    (beats->chseq (mix-poly-impro *current-tune* oracle) (beatduration *current-tune*) 0)))
(setf impro1 (merger impro  (beats->chseq (make-clocks (length (simplegrid *current-tune*)) (beatduration *current-tune*) 2) 
                                          (beatduration *current-tune*) 0)))
(pgmout 4 3) (pgmout 4 4) (pgmout 4 5)
(play impro1))

(my-save-as-midi impro1 (beatduration *current-tune*))

(save-for-antescofo impro (beatduration *current-tune*))

(save-for-antescofo impro beatduroracle)  
;/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/buffer-for-antescofo.txt


;;; CHECK PARAMETERS:

(setf (bestSuffixMode oracle) nil)
(beatduration *current-tune*)

(set-start-point oracle 1)
(set-start-point oracle 200)
(setf (max-continuity oracle) 20)
(setf (max-continuity oracle) 50)
(setf (max-continuity oracle) 100)
(setf (max-continuity garnerleftoracle) 1000)
(maxetat oracle)


(setf (max-continuity (gethash 3 (oracletable *current-tune*))) 1000)
(setf (max-continuity (gethash 6 (oracletable *current-tune*))) 2)
(set-start-point (gethash 3 (oracletable *current-tune*)) 50)
(set-start-point (gethash 6 (oracletable *current-tune*)) 1000)
(maxetat (gethash 3 (oracletable *current-tune*)))
(maxetat (gethash 6 (oracletable *current-tune*)))

(tunename *current-tune*)
(beatduration *current-tune*)                         ;;;; beat duration of current live performance (given by Max)

(pathname-directory (tunedir *current-tune*))
(setf resfromfile (midi-to-beatlist) refbeatdur (second resfromfile) (beatduration *current-tune*) refbeatdur
      oracle (NewImprovizer (first resfromfile) refbeatdur))
(RefTempo oracle)
           
;;;;;;;;;;;!!!!!!!!!!!!!!! not in use anymore - !!!!!!!!!!!!!!!!!!!!!!
(setf (max-continuity liveoracle) 1000)
(maxetat (liveoracle *current-tune*))  
(setf (max-continuity (liveoracle *current-tune*)) 1000)
(reset-liveoracle *current-tune*)
(om-inspect (liveoracle *current-tune*))
(setf (maxpolyphony *current-tune*) 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(generate-offline-harmos *current-tune* (beatduration *current-tune*))
(load-realtime-data-and-generate-impros *current-tune* 1)


;;; OTHER TUNES

(setf beatduroracle Jaime-nov2007_beatdur   
      beatsfromfile Jaime-nov2007_beatsfromfile)

(setf beatduroracle Allthethingssolo-juil2011_beatdur  ;;;; beat duration of the original oracle             
      beatsfromfile Allthethingssolo-juil2011_beatsfromfile)

(setf beatduroracle Dicidenbas_beatdur           ;Ircam, May 8th 2004, BPM=188 beatdur=319          
      beatsfromfile Dicidenbas_beatsfromfile) 


|#






(setf Jaimesolo-juil2011_beatdur 589
      Jaimesolo-juil2011_beatsfromfile 
 '(      ;"J'aime pour la vie", BPM = 102, beatdur = 589, Bernard's solo recorded in Uzeste, July 13th, 2011
((d 7) nil)
((d 7) ((62 -36 130 113 2) (65 339 141 121 2)))
((d 7) ((66 -84 100 110 2) (67 42 88 127 2) (68 192 68 101 2) (69 308 150 95 2)))
((d 7) ((65 -88 62 103 2) (66 22 145 113 2) (65 214 114 97 2) (62 313 115 85 2)))
((d 7) ((65 -99 93 123 2) (66 20 188 106 2) (62 218 74 81 2) (60 224 10 127 2) (65 312 276 123 2)))
((d 7) ((-65 0 594 123 2) (66 324 119 118 2) (72 324 244 114 2)))
((d 7) ((-65 0 536 123 2) (66 -99 94 93 2) (72 219 89 113 2) (66 224 84 118 2)))
((d 7) nil)
((d 7) nil)
((g 7) ((54 46 84 113 2) (55 109 145 68 2) (65 380 135 123 2)))
((g 7) ((64 89 130 120 2) (62 266 93 86 2) (61 381 130 111 2)))
((g# 7) ((64 -26 115 120 2) (65 73 120 113 2) (64 177 94 91 2) (61 261 67 92 2) (62 375 156 110 2)))
((g# 7) ((61 -42 68 105 2) (62 83 161 113 2) (63 276 208 111 2)))
((g 7) ((55 282 88 111 2) (65 390 172 118 2)))
((g 7) ((55 -15 47 75 2) (64 94 204 120 2) (62 302 126 97 2)))
((f 7) ((55 -83 120 106 2) (62 204 104 124 2) (61 339 167 91 2)))
((f 7) ((53 -79 266 101 2)))
((g 7) ((55 42 120 98 2) (65 328 152 125 2)))
((g 7) ((65 31 74 121 2) (64 47 156 121 2) (62 213 116 81 2) (61 323 140 111 2)))
((g# 7) ((64 -78 104 120 2) (65 10 110 114 2) (64 114 94 77 2) (61 203 78 110 2) (62 318 135 114 2)))
((g# 7) ((61 -78 57 91 2) (62 15 120 114 2) (63 193 270 114 2) (62 203 47 108 2)))
((g 7) ((55 182 68 111 2) (65 291 177 124 2)))
((g 7) ((55 -94 74 75 2) (64 16 187 121 2) (62 193 135 106 2) (55 464 68 95 2) (53 469 15 103 2)))
((f 7) ((61 22 78 111 2) (62 37 146 114 2) (61 261 83 88 2) (62 302 204 106 2)))
((f 7) ((61 -99 427 121 2)))
((g 7) ((67 57 93 127 2) (77 344 140 123 2)))
((g 7) ((77 26 89 120 2) (76 68 156 78 2) (74 219 115 103 2) (73 339 125 123 2)))
((g# 7) ((76 -68 131 121 2) (77 37 108 98 2) (76 125 88 98 2) (73 219 68 101 2) (74 323 151 110 2)))
((g# 7) ((73 -79 68 111 2) (74 41 125 117 2) (75 213 177 121 2) (77 364 130 111 2) (75 452 136 54 2)))
((g 7) ((-75 0 83 54 2) (67 229 11 127 2) (77 338 198 117 2)))
((g 7) ((76 58 166 124 2) (74 250 120 103 2)))
((f 7) ((67 -63 104 127 2) (74 213 115 124 2) (73 339 172 97 2)))
((f 7) ((65 -68 359 120 2)))
((g 7) ((67 42 68 127 2) (77 334 166 121 2)))
((g 7) ((77 20 105 114 2) (76 46 168 106 2) (74 198 124 88 2) (73 322 120 120 2)))
((g# 7) ((76 -67 78 121 2) (77 -5 146 111 2) (76 131 67 68 2) (73 198 52 101 2) (74 318 135 111 2)))
((g# 7) ((73 -99 62 108 2) (74 0 119 118 2) (75 182 370 125 2)))
((g 7) ((67 239 15 127 2) (77 333 182 121 2)))
((g 7) ((67 -52 10 127 2) (76 42 224 123 2) (74 234 84 100 2) (67 464 52 127 2)))
((f 7) ((74 0 166 121 2) (73 0 56 113 2) (73 291 297 127 2) (72 354 146 123 2)))
((f 7) ((-73 0 250 127 2) (74 -26 156 34 2) (70 234 42 117 2) (72 250 20 64 2) (71 255 26 118 2) (68 266 26 114 2) (66 270 6 103 2) (69 276 20 82 2) (66 281 21 125 2) (63 281 47 120 2) (67 292 10 127 2) (65 302 26 113 2) (61 302 42 118 2) (64 312 32 120 2) (62 333 21 93 2) (56 344 10 106 2) (60 354 16 121 2)))
((d 7) ((50 83 115 100 2) (53 365 156 124 2) (54 385 11 111 2)))
((d 7) ((54 -46 98 85 2) (55 63 203 75 2) (56 240 78 114 2) (57 365 151 114 2) (56 370 32 65 2)))
((d 7) ((53 -41 72 91 2) (54 57 156 110 2) (53 255 110 100 2) (50 381 108 85 2)))
((d 7) ((53 -42 94 121 2) (54 83 187 95 2) (50 276 57 76 2) (48 286 16 78 2) (53 344 250 123 2)))
((d 7) ((-53 0 588 123 2)))
((d 7) ((-53 0 464 123 2)))
((d 7) nil)
((d 7) nil)
((d 7) ((50 -52 172 117 2) (53 328 156 120 2)))
((d 7) ((54 -83 94 74 2) (55 37 219 95 2) (56 224 52 113 2) (57 334 125 106 2) (55 339 89 110 2)))
((d 7) ((53 -67 62 98 2) (54 31 152 113 2) (53 219 94 68 2) (50 359 68 98 2)))
((d 7) ((54 -78 83 123 2) (53 41 157 97 2) (50 328 255 124 2)))
((d 7) ((-50 0 250 124 2)))
((d 7) nil)
((d 7) ((60 219 78 127 2) (54 224 135 123 2) (53 318 167 86 2) (50 479 89 89 2)))
((d 7) ((53 15 167 111 2) (60 15 104 127 2)))
((d 7) ((50 -84 297 121 2) (53 322 162 123 2)))
((d 7) ((54 -57 62 106 2) (55 37 203 97 2) (56 198 63 121 2) (57 323 151 113 2)))
((d 7) ((53 -73 52 97 2) (54 10 167 111 2) (53 218 94 101 2) (50 307 120 85 2)))
((d 7) ((53 -98 82 121 2) (54 10 198 111 2) (50 214 46 71 2) (53 302 287 125 2)))
((d 7) ((-53 0 593 125 2)))
((d 7) ((-53 0 298 125 2)))
((d 7) nil)
((d 7) nil)
((d 7) ((50 -67 151 113 2) (53 318 152 123 2)))
((d 7) ((54 -98 150 97 2) (55 48 140 82 2) (56 219 47 98 2) (55 308 109 101 2) (57 308 135 101 2) (53 485 63 95 2)))
((d 7) ((54 11 145 117 2) (53 182 99 74 2) (50 317 84 105 2) (54 474 99 118 2) (60 479 78 127 2)))
((d 7) ((53 10 172 98 2) (52 307 203 123 2) (50 307 245 123 2)))
((d 7) nil)
((d 7) nil)
((d 7) nil)
((d 7) ((55 230 46 118 2) (58 354 146 106 2)))
((g 7) ((59 47 187 120 2) (62 349 146 105 2) (60 354 11 127 2)))
((g 7) ((55 -42 68 78 2) (67 67 136 127 2) (65 339 171 113 2)))
((g# 7) ((72 -20 218 121 2)))
((g# 7) ((70 250 63 125 2) (71 381 119 110 2)))
((g 7) ((67 -68 52 127 2) (60 78 46 127 2) (62 83 250 91 2)))
((g 7) nil)
((f 7) ((69 -83 78 123 2) (72 31 115 111 2) (63 183 119 110 2) (60 328 79 127 2) (58 469 109 121 2)))
((f 7) ((55 15 135 77 2) (53 192 42 70 2) (58 296 162 123 2)))
((g 7) ((53 -88 36 81 2) (59 10 172 118 2) (53 208 74 92 2)))
((g 7) nil)
((g# 7) ((62 182 94 120 2) (60 296 126 127 2)))
((g# 7) ((56 -83 42 59 2) (66 -5 198 123 2) (65 21 146 127 2) (54 177 84 89 2) (60 302 125 127 2) (65 463 110 123 2)))
((g 7) ((63 15 115 98 2) (64 187 115 123 2) (62 302 124 108 2) (59 474 94 113 2)))
((g 7) ((55 26 130 70 2) (63 282 182 123 2) (55 480 56 86 2) (53 484 16 76 2)))
((f 7) ((62 0 157 114 2) (60 183 57 127 2) (62 272 192 108 2) (60 470 114 127 2)))
((f 7) ((-60 0 52 127 2)))
((g 7) ((59 214 56 110 2) (62 322 152 117 2)))
((g 7) ((67 -62 5 127 2) (70 42 156 121 2) (71 198 68 108 2) (74 323 120 120 2)))
((g# 7) ((77 -73 292 111 2) (76 -68 151 117 2) (75 193 98 103 2) (72 271 94 111 2) (66 343 188 124 2)))
((g# 7) ((65 42 214 106 2) (63 323 135 117 2)))
((g 7) ((61 -94 120 97 2) (62 20 136 103 2) (67 203 78 127 2) (65 208 16 117 2) (59 354 125 75 2)))
((g 7) ((55 -78 68 78 2) (62 32 94 120 2) (61 182 63 105 2) (60 308 166 127 2)))
((f 7) ((57 -83 94 108 2) (60 11 72 127 2) (63 109 63 110 2) (67 224 63 127 2) (69 229 94 105 2) (71 317 42 108 2) (72 317 48 103 2) (75 406 53 117 2)))
((f 7) ((79 -73 141 125 2) (73 448 15 117 2)))
((g 7) nil)
((g 7) ((62 218 99 123 2) (55 343 157 101 2)))
((g# 7) ((59 -78 68 111 2) (62 26 125 120 2) (65 218 142 124 2) (63 334 171 65 2)))
((g# 7) ((60 -78 68 127 2) (62 32 109 118 2) (63 141 94 108 2) (62 240 99 106 2) (60 328 84 127 2) (59 334 140 117 2)))
((g 7) ((55 -52 31 81 2) (58 36 136 117 2) (59 224 42 114 2) (62 312 115 120 2)))
((g 7) ((59 -98 124 120 2) (58 11 135 91 2) (55 182 63 83 2) (59 292 151 124 2) (55 480 41 65 2)))
((f 7) ((58 0 172 118 2) (57 307 146 121 2) (55 474 78 101 2)))
((f 7) ((56 30 178 118 2) (55 250 202 95 2)))
((d 7) ((53 375 172 125 2)))
((d 7) ((54 -32 68 89 2)))
((d 7) nil)
((d 7) ((50 -52 83 123 2) (48 73 162 110 2) (50 255 84 98 2) (53 359 120 118 2)))
((d 7) ((54 -52 94 111 2) (55 78 94 100 2) (56 240 150 121 2) (57 364 110 105 2)))
((d 7) ((58 -47 84 123 2) (57 -31 57 121 2) (59 78 141 121 2) (60 250 135 127 2) (57 370 119 108 2)))
((d 7) ((54 -57 109 89 2) (53 74 135 108 2)))
((d 7) nil)
((d 7) ((72 52 151 120 2) (69 52 156 118 2) (62 240 41 83 2) (60 244 6 127 2) (71 334 104 123 2) (67 338 74 127 2)))
((d 7) ((65 -79 57 124 2) (67 -79 5 127 2) (71 -79 120 120 2) (72 -79 229 117 2) (66 36 140 113 2) (72 223 131 95 2) (65 223 135 118 2) (62 354 98 98 2)))
((d 7) ((69 -78 104 106 2) (62 -78 16 108 2) (60 -78 124 127 2) (57 46 94 98 2) (61 177 21 110 2) (54 188 140 120 2) (60 198 42 127 2) (53 307 172 106 2)))
((d 7) ((50 -93 62 93 2) (60 16 135 127 2) (53 21 146 114 2) (50 198 57 100 2) (62 302 151 123 2) (56 313 130 124 2) (57 422 109 121 2)))
((d 7) ((62 -98 120 97 2) (57 -4 82 110 2) (62 52 94 101 2) (57 146 78 100 2) (62 224 78 111 2) (57 282 94 97 2) (62 365 83 101 2) (57 433 89 91 2)))
((d 7) ((62 -88 104 89 2) (57 -4 88 98 2) (62 58 114 95 2) (62 220 93 97 2) (57 287 99 91 2) (62 355 99 82 2) (57 438 94 98 2)))
((d 7) ((62 -99 115 77 2) (57 -6 84 108 2) (62 78 88 92 2) (57 136 72 95 2) (62 229 68 64 2) (55 297 57 97 2) (57 297 67 103 2) (62 364 100 72 2) (57 422 88 98 2)))
((d 7) ((62 -100 73 58 2) (57 -6 58 75 2) (62 52 239 103 2) (57 130 150 82 2) (60 280 48 127 2) (55 332 22 93 2) (54 332 6 100 2) (53 338 31 114 2) (50 343 109 123 2) (52 343 67 97 2) (48 358 89 124 2)))
((d 7) ((74 318 120 124 2) (72 412 78 100 2)))
((d 7) ((69 -98 72 101 2) (68 21 146 110 2) (66 219 89 123 2) (65 344 110 106 2)))
((d 7) ((63 -94 141 123 2) (62 21 187 106 2) (60 182 74 124 2) (59 302 141 117 2) (60 308 20 77 2)))
((d 7) ((62 -94 114 118 2) (60 4 146 127 2) (56 306 136 120 2)))
((d 7) ((54 -99 104 118 2) (53 37 98 74 2) (51 183 124 123 2) (50 292 203 105 2)))
((d 7) ((48 -94 52 97 2) (53 0 203 114 2) (48 208 36 55 2) (50 296 141 120 2) (53 484 104 110 2)))
((d 7) ((-53 0 16 110 2) (43 0 36 85 2) (45 0 78 77 2) (48 99 73 117 2) (53 198 47 114 2) (57 308 72 108 2) (59 313 31 118 2) (60 391 67 127 2)))
((d 7) ((65 -94 42 124 2) (69 10 88 108 2) (72 104 83 113 2) (77 208 57 113 2) (81 302 286 124 2)))
((d 7) ((-81 0 599 124 2)))
((d 7) ((-81 0 594 124 2)))
((d 7) ((-81 0 589 124 2)))
((d 7) ((-81 0 338 124 2)))
((d 7) ((84 219 130 124 2) (83 323 130 101 2) (82 433 78 121 2)))
((d 7) ((81 -84 152 108 2) (80 52 114 121 2) (78 198 104 114 2) (77 338 89 106 2) (75 484 84 127 2)))
((d 7) ((74 15 63 114 2) (75 83 89 98 2) (74 156 83 101 2) (72 218 52 101 2) (72 306 131 64 2) (71 317 167 120 2)))
((d 7) ((69 -83 46 88 2) (69 52 187 106 2) (71 52 198 108 2)))
((g 7) ((72 298 10 123 2) (74 298 72 123 2)))
((g 7) ((67 -36 36 127 2) (77 162 161 127 2) (76 167 11 118 2) (69 402 62 95 2) (67 402 56 127 2)))
((g# 7) ((76 16 68 124 2) (77 26 177 124 2) (75 297 104 127 2) (72 412 135 106 2)))
((g# 7) ((66 0 94 123 2) (65 104 116 101 2) (63 261 141 98 2) (62 376 140 88 2)))
((g 7) ((60 -26 36 127 2) (61 57 135 123 2) (62 240 67 114 2) (67 360 93 127 2)))
((g 7) ((59 -58 104 103 2) (55 62 120 91 2) (62 234 104 120 2) (57 344 130 91 2)))
((f 7) ((60 -94 89 127 2) (62 -88 68 117 2) (64 58 57 82 2) (61 62 11 117 2) (65 198 73 114 2) (67 204 62 127 2) (71 312 100 106 2) (75 480 56 123 2)))
((f 7) ((77 10 151 120 2) (67 192 16 127 2) (78 296 188 121 2)))
((g 7) ((79 0 374 127 2) (78 15 11 105 2) (77 306 120 114 2) (74 416 104 113 2)))
((g 7) ((72 -67 37 114 2) (71 -62 36 105 2) (67 32 57 127 2) (62 214 58 92 2) (71 302 142 121 2) (70 485 104 114 2)))
((g# 7) ((-70 0 57 114 2) (71 26 125 118 2) (72 183 88 120 2) (66 313 156 113 2)))
((g# 7) ((65 -88 52 108 2) (63 11 141 108 2) (62 193 105 103 2) (60 298 130 127 2) (61 464 84 110 2)))
((g 7) ((62 -11 131 110 2) (55 146 130 120 2) (57 151 36 124 2)))
((g 7) nil)
((f 7) ((53 -63 67 108 2) (55 -53 5 92 2) (57 26 93 101 2) (60 98 58 127 2) (63 202 120 114 2) (53 312 104 95 2) (63 426 100 114 2)))
((f 7) ((52 -67 73 101 2) (53 -67 89 98 2) (63 42 214 113 2)))
((g 7) ((63 235 114 123 2) (62 333 84 91 2) (60 443 78 127 2)))
((g 7) ((57 -73 37 103 2) (59 -73 73 105 2) (60 46 131 127 2) (61 203 125 113 2) (62 328 125 81 2)))
((g# 7) ((64 -89 146 127 2) (65 62 125 121 2) (67 218 68 127 2) (69 333 125 114 2)))
((g# 7) ((70 -94 120 120 2) (71 31 114 108 2) (72 182 109 118 2) (73 323 120 123 2) (74 484 105 114 2)))
((g 7) ((-74 0 30 114 2) (71 30 172 103 2) (70 322 136 120 2)))
((g 7) ((71 -93 93 114 2) (72 31 188 114 2) (67 229 16 127 2) (71 328 141 123 2)))
((f 7) ((70 -94 94 106 2) (69 46 167 106 2) (67 218 89 127 2) (65 348 105 111 2)))
((f 7) ((63 -73 120 124 2) (62 41 68 114 2) (63 130 99 95 2) (62 198 57 83 2) (60 235 62 124 2) (59 339 140 118 2)))
((g 7) ((60 -78 84 127 2) (61 47 125 117 2) (62 214 146 114 2) (59 344 114 105 2)))
((g 7) ((55 -62 88 72 2) (52 47 109 100 2) (60 219 5 127 2) (62 219 119 117 2) (54 334 254 120 2)))
((g# 7) ((-54 0 594 120 2)))
((g# 7) ((-54 0 594 120 2)))
((g 7) ((-54 0 94 120 2) (60 62 100 127 2) (61 68 57 91 2) (58 94 46 97 2) (56 136 41 91 2) (57 140 11 124 2) (59 146 36 123 2) (54 151 37 114 2) (57 162 31 124 2) (51 167 47 118 2) (47 177 11 117 2) (55 177 37 101 2) (45 182 16 108 2) (53 203 27 63 2) (46 214 5 125 2) (52 276 16 36 2)))
((g 7) nil)
((f 7) ((57 187 131 114 2) (55 192 36 123 2) (60 296 89 127 2) (63 396 62 93 2)))
((f 7) ((69 -94 136 120 2) (65 10 120 121 2) (63 188 57 113 2) (62 240 83 124 2) (67 292 31 127 2) (65 302 68 108 2) (63 318 46 95 2) (64 323 63 72 2) (61 323 67 88 2) (62 349 57 56 2) (60 349 73 106 2) (58 360 62 98 2) (59 370 10 51 2) (56 380 37 118 2) (54 401 16 123 2) (59 401 37 117 2) (57 422 42 110 2) (52 422 10 100 2) (54 422 16 120 2) (56 427 11 66 2) (55 443 89 85 2) (53 448 52 58 2) (48 458 84 125 2) (50 464 10 76 2)))
((d 7) ((52 -84 68 39 2) (50 -68 58 97 2) (50 68 150 108 2) (53 344 161 127 2)))
((d 7) ((54 -48 116 114 2) (55 57 198 111 2) (56 228 74 118 2) (57 354 172 117 2)))
((d 7) ((53 -42 68 100 2) (54 58 166 110 2) (53 256 130 117 2) (50 360 150 89 2)))
((d 7) ((53 -41 120 120 2) (54 68 204 100 2) (50 276 58 89 2) (53 339 250 125 2)))
((d 7) ((-53 0 594 125 2)))
((d 7) ((-53 0 589 125 2)))
((d 7) ((-53 0 213 125 2)))
((d 7) nil)
((d 7) ((60 -58 100 127 2) (53 -58 115 114 2) (59 -47 5 97 2) (50 0 322 105 2) (53 348 157 121 2)))
((d 7) ((54 -42 58 111 2) (55 47 167 105 2) (56 224 63 113 2) (57 331 42 111 2)))
((d 7) ((53 -52 36 85 2) (54 36 141 113 2) (53 234 74 106 2) (50 338 94 103 2)))
((d 7) ((54 -74 110 123 2) (53 41 141 121 2) (50 343 245 127 2)))
((d 7) ((-50 0 594 127 2)))
((d 7) ((-50 0 234 127 2)))
((d 7) nil)
((d 7) nil)
((d 7) ((50 -73 193 124 2) (53 333 146 127 2) (60 333 125 127 2)))
((d 7) ((54 -58 78 101 2) (55 46 152 110 2) (60 62 68 117 2) (56 224 56 120 2) (60 234 229 127 2) (57 333 137 108 2)))
((d 7) ((53 -63 63 89 2) (60 15 104 127 2) (54 26 167 120 2) (53 208 109 82 2) (50 323 135 98 2)))
((d 7) ((53 -68 83 118 2) (60 -63 135 127 2) (54 41 219 106 2) (48 234 26 91 2) (50 234 52 72 2) (53 322 266 127 2)))
((d 7) ((-53 0 588 127 2)))
((d 7) ((-53 0 594 127 2)))
((d 7) ((-53 0 224 127 2)))
((d 7) nil)
((d 7) ((50 -57 276 123 2) (51 -46 41 121 2) (53 350 161 127 2)))
((d 7) ((54 -46 82 110 2) (55 68 146 103 2) (56 234 58 121 2) (57 344 151 114 2)))
((d 7) ((53 -47 47 105 2) (54 47 166 114 2) (53 265 115 89 2) (50 354 89 105 2)))
((d 7) ((60 -74 126 127 2) (54 -74 130 123 2) (53 36 146 106 2) (50 322 266 124 2)))
((d 7) ((-50 0 412 124 2)))
((d 7) nil)
((d 7) nil)
((d 7) nil)
((g 7) ((54 58 98 121 2) (55 151 125 103 2) (65 396 162 124 2) (64 401 83 124 2)))
((g 7) ((65 89 109 125 2) (64 120 167 117 2) (62 324 94 111 2) (61 418 108 117 2)))
((g# 7) ((64 0 115 124 2) (65 89 124 120 2) (64 203 84 100 2) (61 302 57 113 2) (62 401 136 118 2)))
((g# 7) ((61 -10 68 111 2) (62 84 114 118 2) (63 250 344 123 2)))
((g 7) ((-63 0 58 123 2) (55 250 68 113 2) (65 365 182 124 2) (64 370 16 118 2)))
((g 7) ((55 -27 53 98 2) (64 57 234 127 2) (62 265 100 118 2)))
((f 7) ((55 -88 162 117 2) (61 152 41 123 2) (62 152 104 127 2) (61 287 156 113 2) (53 454 130 120 2)))
((f 7) ((-53 0 250 120 2)))
((g 7) ((55 62 130 66 2) (65 354 166 127 2)))
((g 7) ((65 30 100 124 2) (64 41 161 123 2) (62 250 78 68 2) (61 312 120 118 2)))
((g# 7) ((64 -72 109 124 2) (65 22 104 113 2) (64 89 94 98 2) (61 209 63 108 2) (62 308 136 117 2)))
((g# 7) ((61 -94 68 118 2) (62 5 99 117 2) (63 166 131 123 2) (62 172 36 114 2) (65 271 151 123 2) (63 344 214 100 2)))
((g 7) ((55 204 46 113 2) (65 308 172 125 2) (55 485 41 91 2)))
((g 7) ((64 15 224 125 2) (62 239 94 114 2)))
((f 7) ((55 -100 84 113 2) (61 52 78 123 2) (62 57 156 124 2) (61 302 98 124 2) (62 307 224 123 2)))
((f 7) ((61 -89 432 121 2) (62 135 100 85 2) (63 135 172 70 2) (55 265 1 46 2) (64 287 52 68 2) (62 307 78 111 2) (60 333 100 98 2) (59 339 94 22 2) (57 349 57 48 2)))
((g 7) ((67 58 124 127 2) (77 360 166 125 2)))
((g 7) ((77 57 114 125 2) (76 78 161 98 2) (74 250 109 111 2) (73 349 120 127 2)))
((g# 7) ((74 -48 78 120 2) (76 -48 84 114 2) (77 36 99 81 2) (76 104 109 93 2) (73 224 52 114 2) (74 322 156 120 2)))
((g# 7) ((73 -62 120 113 2) (74 27 130 123 2) (75 194 400 127 2)))
((g 7) ((-75 0 16 127 2) (75 172 21 78 2) (67 246 46 127 2) (77 360 177 124 2)))
((g 7) ((67 -52 15 127 2) (76 47 192 127 2) (74 261 109 113 2)))
((f 7) ((67 -42 78 127 2) (73 182 42 123 2) (74 187 1 125 2) (73 322 162 123 2) (74 328 20 65 2)))
((f 7) ((65 -83 374 124 2)))
((g 7) ((67 62 99 127 2) (77 338 177 127 2)))
((g 7) ((77 47 99 121 2) (76 79 161 74 2) (74 240 125 101 2) (73 344 115 125 2)))
((g# 7) ((76 -58 266 123 2) (77 26 109 95 2) (73 229 52 113 2) (74 328 151 120 2)))
((g# 7) ((73 -62 42 106 2) (74 10 116 118 2) (75 172 422 124 2) (77 286 162 110 2)))
((g 7) ((-75 0 5 124 2) (67 214 20 127 2) (79 323 161 127 2) (77 323 182 120 2)))
((g 7) ((67 -79 21 127 2) (76 26 213 127 2) (74 213 93 100 2)))
((f 7) ((67 -84 26 127 2) (74 20 194 125 2) (73 20 74 125 2) (73 318 270 127 2)))
((f 7) ((-73 0 245 127 2) (70 156 116 43 2) (74 167 68 66 2) (72 172 84 41 2) (71 172 100 52 2) (69 188 94 52 2) (67 224 63 118 2) (68 230 52 42 2) (65 240 62 111 2) (66 245 5 117 2) (66 250 42 120 2) (64 256 57 110 2) (62 272 52 110 2) (63 272 30 100 2) (60 282 52 124 2) (61 282 36 103 2) (57 282 10 103 2) (59 292 47 114 2) (55 292 10 106 2) (58 298 30 103 2) (57 302 48 113 2) (56 308 31 110 2) (55 313 41 120 2) (54 318 32 117 2) (53 324 36 124 2) (52 328 42 123 2) (51 334 26 120 2) (50 339 41 106 2) (48 350 41 121 2) (46 354 6 127 2) (46 365 15 123 2) (44 365 11 120 2) (45 370 10 91 2) (44 380 6 105 2) (47 380 22 127 2) (42 386 5 111 2) (45 396 36 111 2)))
((d 7) ((50 104 68 95 2) (53 376 156 123 2)))
((d 7) ((54 -31 104 103 2) (55 84 176 89 2) (57 94 26 88 2) (56 250 126 120 2) (57 354 172 114 2)))
((d 7) ((53 -26 47 88 2) (54 47 182 114 2) (53 260 151 113 2) (50 375 140 103 2)))
((d 7) ((53 -37 93 120 2) (54 78 213 92 2) (50 302 56 64 2) (53 354 234 127 2)))
((d 7) ((-53 0 594 127 2)))
((d 7) ((-53 0 594 127 2)))
((d 7) ((-53 0 588 127 2)))
((d 7) ((-53 0 99 127 2)))
((d 7) ((53 -67 99 118 2) (50 -20 307 93 2) (52 334 135 120 2) (53 334 140 121 2)))
((d 7) ((54 -83 156 101 2) (55 42 203 95 2) (56 214 83 120 2) (56 334 15 64 2) (57 334 166 118 2)))
((d 7) ((53 -68 52 105 2) (54 26 182 117 2) (53 245 135 97 2) (50 343 94 93 2)))
((d 7) ((54 -79 125 123 2) (53 30 157 113 2) (50 312 270 127 2)))
((d 7) ((-50 0 272 127 2)))
((d 7) nil)
((d 7) nil)
((d 7) nil)
((d 7) ((53 -94 136 124 2) (54 -88 120 123 2) (50 -15 250 71 2) (53 302 146 123 2)))
((d 7) ((54 -99 114 113 2) (55 15 146 113 2) (56 187 68 118 2) (57 297 140 111 2)))
((d 7) ((53 -94 52 110 2) (54 0 156 108 2) (53 208 114 97 2) (50 312 94 110 2) (60 478 110 127 2) (53 478 110 127 2)))
((d 7) ((-60 0 32 127 2) (-53 0 26 127 2) (54 10 198 105 2) (48 198 52 111 2) (50 203 53 88 2) (53 297 291 127 2) (60 297 291 127 2)))
((d 7) ((-53 0 594 127 2) (-60 0 594 127 2)))
((d 7) ((-53 0 594 127 2) (-60 0 594 127 2)))
((d 7) ((-53 0 480 127 2) (-60 0 432 127 2)))
((d 7) nil)
((d 7) ((62 -46 161 123 2) (64 -42 16 121 2) (65 350 135 127 2)))
((d 7) ((66 -52 78 121 2) (67 58 93 127 2) (68 219 151 127 2) (69 338 157 110 2)))
((d 7) ((65 -46 42 120 2) (66 48 150 124 2) (65 235 115 113 2) (62 329 130 105 2)))
((d 7) ((66 -89 79 125 2) (72 -83 140 118 2) (65 21 146 124 2) (62 318 271 127 2)))
((d 7) ((-62 0 494 127 2)))
))







(setf Jaime-nov2007_beatdur 682
      Jaime-nov2007_beatsfromfile
'(
;-------------------------------------------------------

; Lineaire
; BEAT 1
((d 7 1 1) ((33 156 115 104 2) (26 281 208 90 2) (57 526 94 82 2)))
((d 7 1 2) ((60 -52 109 81 2) (65 83 58 81 2) (67 193 67 65 2) (69 203 89 101 2) (72 297 94 104 2) (77 391 78 104 2) (81 516 99 115 2)))
((d 7 1 3) ((75 -37 240 77 2) (69 177 42 63 2) (38 229 63 16 2) (74 292 177 117 2) (69 505 63 84 2) (71 536 52 45 2)))
((d 7 1 4) ((66 -47 193 108 2) (65 182 42 58 2) (72 297 151 100 2) (63 490 99 94 2)))
;-------------------------------------------------------
; BEAT 5
((d 7 1 1) ((65 -52 172 118 2) (72 -47 188 99 2) (62 161 146 72 2) (45 182 141 79 2) (38 333 203 90 2) (50 552 125 79 2)))
((d 7 1 2) ((-50 0 10 79 2) (74 208 94 127 2) (75 354 172 103 2) (48 562 115 85 2) (36 568 130 81 2) (72 568 119 120 2)))
((d 7 1 3) ((74 10 172 113 2) (69 203 99 107 2) (49 219 166 80 2) (68 354 208 92 2) (66 531 146 114 2) (37 542 135 97 2)))
((d 7 1 4) ((-66 0 16 114 2) (-37 0 94 97 2) (65 5 104 89 2) (49 47 52 31 2) (63 167 135 119 2) (45 172 130 87 2) (62 312 198 77 2) (60 505 37 59 2)))
;-------------------------------------------------------
; BEAT 9
((d 7 1 1) ((50 -88 120 78 2) (65 -15 208 127 2) (63 198 479 124 2) (50 214 141 68 2)))
((d 7 1 2) ((50 -15 193 34 2) (-63 0 683 124 2) (45 235 114 76 2) (38 355 187 79 2) (50 553 145 101 2)))
((d 7 1 3) ((-50 0 21 101 2) (62 21 120 105 2) (60 120 115 95 2) (57 256 166 72 2) (45 292 109 58 2) (59 360 161 110 2) (62 469 130 106 2) (36 521 115 27 2) (48 537 73 29 2)))
((d 7 1 4) ((60 -88 120 101 2) (59 11 114 94 2) (48 79 109 72 2) (57 125 99 89 2) (56 240 130 107 2) (54 360 146 102 2) (53 490 99 76 2) (51 594 104 105 2)))
;-------------------------------------------------------
; BEAT 13
((d 7 1 1) ((50 -10 578 115 2) (-51 0 52 105 2) (33 204 109 84 2) (26 334 192 99 2) (38 542 135 74 2) (51 568 104 98 2)))
((d 7 1 2) ((50 -41 114 84 2) (-38 0 6 74 2) (48 21 188 95 2) (38 214 141 82 2) (50 355 177 122 2) (53 553 145 105 2)))
((d 7 1 3) ((38 -93 109 37 2) (36 -73 68 29 2) (48 -15 187 97 2) (-53 0 16 105 2) (50 209 203 102 2) (33 209 109 88 2) (26 318 198 92 2) (38 511 120 110 2)))
((d 7 1 4) ((48 318 167 112 2) (36 323 157 114 2) (45 542 120 87 2)))
;-------------------------------------------------------

; Rythmic
; BEAT 17
((d 7 1 1) ((49 -10 203 115 2) (37 0 182 102 2) (65 235 88 118 2) (72 240 198 102 2) (45 245 57 55 2) (66 380 198 75 2) (50 573 104 65 2)))
((d 7 1 2) ((65 -83 120 81 2) (72 -31 104 27 2) (-50 0 5 65 2) (62 68 146 41 2) (45 255 105 100 2) (50 396 151 98 2) (65 396 120 109 2) (38 401 157 89 2) (72 401 183 100 2) (42 459 78 19 2)))
((d 7 1 3) ((45 -57 41 59 2) (48 21 167 96 2) (36 31 146 97 2) (66 47 177 105 2) (72 63 130 59 2) (45 261 52 64 2) (49 365 229 101 2) (37 365 229 94 2) (65 375 182 101 2) (72 386 166 76 2)))
((d 7 1 4) ((66 -99 167 72 2) (45 -52 109 74 2) (62 52 162 73 2) (50 250 177 93 2) (38 261 218 84 2) (65 271 89 101 2) (72 281 157 105 2) (66 370 141 82 2) (65 469 167 66 2)))
;-------------------------------------------------------
; BEAT 21
((d 7 1 1) ((60 -99 53 85 2) (62 -99 47 58 2) (69 0 125 66 2) (60 11 213 87 2) (67 16 36 75 2) (57 235 47 79 2) (45 266 73 58 2) (53 360 213 92 2) (60 386 99 52 2) (45 558 52 95 2) (54 573 104 91 2)))
((d 7 1 2) ((60 -93 62 59 2) (-54 0 27 91 2) (50 21 177 82 2) (33 230 99 82 2) (53 250 42 72 2) (60 360 193 103 2) (57 370 177 99 2) (55 391 57 38 2) (26 433 57 22 2) (50 594 57 44 2)))
((d 7 1 3) ((55 -20 161 115 2) (59 -15 161 108 2) (53 193 156 108 2) (60 198 167 101 2) (38 245 78 32 2) (54 349 188 60 2) (38 563 114 49 2) (53 573 104 64 2)))
((d 7 1 4) ((60 -83 89 42 2) (-38 0 183 49 2) (-53 0 53 64 2)))
;-------------------------------------------------------

; Lineaire
; BEAT 25
((d 7 1 1) ((40 -58 52 20 2) (-50 0 104 103 2) (59 0 46 49 2) (60 5 41 93 2) (65 119 42 76 2) (64 130 47 52 2) (69 208 88 103 2) (72 338 182 124 2) (62 541 136 88 2) (57 541 136 93 2) (71 546 126 124 2) (51 552 125 98 2)))
((d 7 1 2) ((-62 0 21 88 2) (-57 0 31 93 2) (-51 0 93 98 2) (70 31 172 98 2) (52 192 506 73 2) (69 208 130 112 2) (58 208 490 83 2) (63 213 485 86 2) (68 380 146 84 2) (69 562 104 86 2)))
((d 7 1 3) ((70 -11 182 125 2) (-52 0 62 73 2) (-58 0 15 83 2) (-63 0 83 86 2) (53 187 412 99 2) (59 192 381 68 2) (71 192 105 101 2) (64 192 485 94 2) (62 218 58 50 2) (56 250 52 20 2) (72 343 141 94 2) (74 395 63 24 2) (73 531 114 117 2)))
((d 7 1 4) ((74 -11 172 105 2) (-64 0 93 94 2) (75 177 146 121 2) (54 187 500 122 2) (65 192 422 101 2) (60 198 479 90 2) (61 229 78 29 2) (76 349 182 104 2) (74 406 78 37 2) (77 567 146 127 2)))
;-------------------------------------------------------
; BEAT 29
((d 7 1 1) ((74 16 146 124 2) (58 89 89 27 2) (75 193 125 121 2) (62 204 474 58 2) (51 209 469 89 2) (57 214 464 69 2) (74 344 120 112 2) (75 438 94 101 2) (74 542 125 87 2)))
((d 7 1 2) ((72 -37 104 102 2) (-62 0 140 58 2) (-51 0 390 89 2) (-57 0 307 69 2) (69 67 99 84 2) (71 156 120 115 2) (74 244 115 117 2) (72 328 109 102 2) (71 395 110 81 2) (69 479 88 105 2) (68 567 115 109 2)))
((d 7 1 3) ((66 -41 104 95 2) (65 42 63 83 2) (63 131 114 92 2) (62 214 89 83 2) (60 282 52 83 2) (63 370 136 115 2) (60 480 88 96 2) (62 568 110 126 2) (56 573 105 99 2)))
((d 7 1 4) ((50 -99 651 88 2) (55 -79 547 74 2) (-62 0 697 126 2) (-56 0 515 99 2)))
;-------------------------------------------------------

; Rythmic
; BEAT 33
((d 7 1 1) ((68 5 130 102 2) (69 146 78 52 2) (60 286 172 84 2) (74 286 110 125 2) (50 286 308 85 2) (57 286 63 75 2) (65 422 114 100 2) (66 547 125 65 2)))
((d 7 1 2) ((60 -42 52 50 2) (57 -16 105 44 2) (65 73 177 107 2) (57 255 37 91 2) (54 281 52 46 2) (50 281 58 39 2) (62 297 52 58 2) (60 385 230 101 2)))
((d 7 1 3) ((46 -83 140 103 2) (57 -68 47 47 2) (59 -68 47 61 2) (65 36 193 113 2) (39 42 635 107 2) (69 255 162 112 2) (49 281 209 99 2) (61 422 130 73 2)))
((d 7 1 4) ((65 -88 119 106 2) (46 -88 234 74 2) (-39 0 42 107 2) (63 47 94 92 2) (65 135 73 28 2) (63 193 99 86 2) (39 255 328 12 2) (60 260 84 97 2) (62 281 42 69 2) (46 359 68 34 2) (58 385 204 107 2)))
;-------------------------------------------------------
; BEAT 37
((d 7 1 1) ((46 -68 115 83 2) (56 -37 89 91 2) (38 52 536 87 2) (57 94 270 56 2) (45 286 292 113 2)))
((d 7 1 2) ((50 -47 162 88 2) (45 -21 94 51 2) (38 62 282 101 2) (57 312 115 120 2) (50 323 213 100 2) (60 432 136 108 2) (65 552 104 88 2)))
((d 7 1 3) ((46 -94 141 100 2) (67 -37 162 121 2) (63 83 188 108 2) (61 276 187 127 2) (39 276 401 107 2) (49 307 52 46 2) (60 443 130 95 2)))
((d 7 1 4) ((46 -63 245 94 2) (58 -52 130 117 2) (-39 0 42 107 2) (53 88 146 65 2) (55 115 52 67 2) (39 266 390 100 2) (56 286 125 107 2) (46 307 89 34 2) (57 411 230 100 2) (60 604 104 121 2)))
;-------------------------------------------------------
; BEAT 41
((d 7 1 1) ((46 -88 203 99 2) (-60 0 58 121 2) (53 26 651 111 2) (38 125 58 19 2) (50 219 172 106 2) (38 224 344 95 2)))
((d 7 1 2) ((45 -73 110 96 2) (-53 0 329 111 2) (38 42 354 106 2) (50 266 214 88 2) (53 381 317 101 2) (46 594 104 101 2)))
((d 7 1 3) ((-53 0 214 101 2) (39 16 448 112 2) (51 245 287 100 2) (53 271 406 119 2)))
((d 7 1 4) ((51 -78 183 102 2) (39 -73 433 108 2) (53 58 286 113 2) (51 219 130 85 2) (43 245 68 36 2) (45 261 67 31 2) (53 396 177 111 2)))
;-------------------------------------------------------

; Pont
; BEAT 45
((g 7 1 1) ((38 -88 192 96 2) (43 0 240 118 2) (31 5 308 122 2) (43 287 119 69 2) (58 385 32 90 2) (53 396 141 124 2) (59 396 172 117 2) (64 396 208 120 2) (55 417 36 91 2)))
((g 7 1 2) ((43 57 365 94 2) (31 63 208 88 2) (71 266 135 117 2) (76 271 146 114 2) (65 271 141 126 2) (73 302 47 50 2) (39 422 182 98 2)))
((g# 7 1 3) ((72 -88 208 113 2) (66 -88 177 125 2) (77 -88 224 108 2) (44 240 218 122 2) (32 245 167 125 2)))
((g# 7 1 4) ((89 -88 192 127 2) (65 229 146 104 2) (60 240 166 113 2) (54 245 198 114 2) (87 406 172 109 2) (86 620 141 127 2) (50 620 135 107 2)))
;-------------------------------------------------------
; BEAT 49
((g 7 1 1) ((-86 0 46 127 2) (43 0 276 96 2) (83 46 131 67 2) (53 203 135 125 2) (79 218 125 106 2) (77 375 62 52 2) (76 380 62 51 2)))
((g 7 1 2) ((79 0 177 127 2) (53 187 146 99 2) (62 192 89 85 2) (57 203 114 66 2) (59 208 68 79 2) (74 213 42 67 2) (56 234 63 34 2) (80 307 161 122 2) (81 359 68 32 2) (81 515 125 127 2)))
((f 7 1 3) ((84 -6 94 78 2) (83 15 37 85 2) (75 88 125 107 2) (51 109 224 85 2) (41 130 114 65 2) (72 187 68 107 2) (67 317 26 111 2)))
((f 7 1 4) ((51 270 266 114 2) (43 333 52 19 2) (42 505 114 106 2)))
;-------------------------------------------------------
; BEAT 53
((g 7 1 1) ((59 -52 136 92 2) (43 -31 281 74 2) (67 -21 136 61 2) (50 198 84 114 2) (53 198 104 119 2) (71 313 161 107 2) (74 318 161 82 2) (79 318 198 119 2) (67 323 146 99 2) (76 323 167 91 2) (65 334 31 104 2)))
((g 7 1 2) ((59 -57 286 101 2) (53 -52 198 114 2) (64 -52 167 108 2) (57 -52 198 82 2) (62 -36 47 69 2) (71 183 187 110 2) (67 183 213 126 2) (76 193 115 90 2) (79 193 130 108 2) (74 209 57 63 2) (64 282 187 107 2) (53 282 260 109 2) (56 308 52 57 2) (59 516 47 82 2) (57 521 42 70 2)))
((g# 7 1 3) ((72 -67 192 101 2) (77 -67 177 105 2) (66 -67 229 120 2) (51 16 73 59 2) (54 136 172 120 2) (51 281 224 102 2) (66 422 89 22 2) (44 490 114 84 2)))
((g# 7 1 4) ((72 -47 178 127 2) (65 -47 94 127 2) (54 0 58 57 2) (66 37 203 90 2) (54 141 276 104 2) (44 151 365 99 2) (51 157 52 78 2) (63 287 203 124 2) (72 292 156 109 2) (60 490 94 107 2) (51 495 167 84 2)))
;-------------------------------------------------------
; BEAT 57
((g 7 1 1) ((68 -52 94 91 2) (58 -42 250 99 2) (67 5 89 55 2) (43 182 334 93 2) (67 187 152 83 2) (53 187 32 92 2) (57 193 125 84 2) (59 193 203 99 2) (65 219 62 23 2) (60 495 57 14 2) (50 536 141 102 2)))
((g 7 1 2) ((53 -16 193 91 2) (43 -5 172 95 2) (62 -5 187 106 2) (67 -5 224 109 2) (46 31 105 27 2) (50 203 94 93 2) (52 313 182 113 2) (42 313 244 112 2) (67 339 145 95 2) (63 349 146 95 2) (48 542 146 102 2)))
((f 7 1 3) ((51 -31 245 107 2) (56 -26 104 108 2) (57 68 130 85 2) (60 203 89 112 2) (48 208 136 78 2) (63 333 172 127 2) (67 333 167 127 2) (51 401 68 46 2) (67 542 135 106 2) (63 552 104 110 2)))
((f 7 1 4) ((-67 0 47 106 2) (65 11 135 84 2) (62 16 140 87 2) (51 177 203 117 2) (41 193 146 89 2) (67 193 177 113 2) (63 198 198 111 2) (60 339 130 86 2) (62 339 213 83 2) (65 354 177 81 2) (48 526 120 121 2)))
;-------------------------------------------------------
; BEAT 61
((g 7 1 1) ((43 -21 208 112 2) (53 203 130 76 2) (58 328 114 127 2) (67 333 297 122 2) (59 416 172 89 2) (60 520 79 15 2)))
((g 7 1 2) ((50 -47 187 110 2) (53 -37 162 100 2) (67 187 146 119 2) (65 187 141 127 2) (43 198 224 103 2) (53 198 265 102 2) (51 494 58 56 2) (65 531 83 127 2) (72 531 167 127 2) (66 547 151 110 2)))
((g# 7 1 3) ((-72 0 125 127 2) (-66 0 67 110 2) (54 156 198 127 2) (44 156 224 127 2) (51 494 136 34 2) (77 500 104 127 2) (84 500 161 127 2) (78 572 105 57 2)))
((g# 7 1 4) ((-78 0 52 57 2) (54 156 318 127 2) (44 156 380 120 2) (72 286 203 120 2) (65 286 172 127 2) (66 411 146 86 2) (51 531 135 85 2)))
;-------------------------------------------------------
; BEAT 65
((g 7 1 1) ((53 -31 229 94 2) (43 -31 328 99 2) (65 -31 188 126 2) (71 -31 260 117 2) (-51 0 6 85 2) (50 240 239 69 2) (67 542 135 122 2) (71 547 130 108 2) (53 552 125 121 2) (43 558 119 111 2)))
((g 7 1 2) ((-67 0 11 122 2) (-71 0 37 108 2) (-53 0 417 121 2) (-43 0 459 111 2) (65 6 224 87 2) (50 250 125 77 2) (67 339 182 124 2) (71 339 182 117 2) (50 552 79 83 2)))
((f 7 1 3) ((42 -26 235 118 2) (52 -20 109 119 2) (68 -5 120 112 2) (64 0 167 106 2) (50 6 52 69 2) (48 214 109 82 2) (51 339 151 118 2) (41 344 255 107 2) (63 360 93 109 2) (69 365 104 86 2) (67 370 110 100 2) (62 433 57 38 2) (52 453 68 18 2) (50 459 119 32 2) (48 516 130 37 2)))
((f 7 1 4) ((53 21 141 127 2) (38 94 68 35 2) (45 245 73 93 2) (47 245 37 93 2) (38 292 63 20 2) (54 349 193 127 2) (33 537 130 97 2)))
;-------------------------------------------------------

; Rythmic
; BEAT 69
((d 7 1 1) ((26 -10 265 122 2) (-33 0 31 97 2) (53 375 188 126 2) (60 375 188 114 2)))
((d 7 1 2) ((54 -78 68 82 2) (55 47 162 94 2) (56 250 110 114 2) (60 297 375 29 2) (57 386 218 91 2)))
((d 7 1 3) ((53 -62 78 77 2) (54 37 197 94 2) (60 52 57 55 2) (53 261 124 80 2) (50 375 188 93 2)))
((d 7 1 4) ((53 -68 136 103 2) (60 -68 141 94 2) (54 57 303 83 2) (50 281 105 66 2) (53 406 344 113 2) (33 646 104 55 2)))
;-------------------------------------------------------
; BEAT 73
((d 7 1 1) ((-53 0 677 113 2) (-33 0 5 55 2) (26 21 208 83 2) (38 245 104 67 2) (54 354 151 109 2) (60 365 57 83 2)))
((d 7 1 2) ((-53 0 698 113 2) (38 26 214 63 2) (60 235 130 66 2) (54 235 140 76 2) (26 339 208 102 2) (38 589 109 74 2)))
((d 7 1 3) ((-53 0 677 113 2) (-38 0 21 74 2) (54 21 172 79 2) (60 26 167 66 2) (38 380 172 97 2) (26 380 198 86 2)))
((d 7 1 4) ((54 -67 104 92 2) (60 -52 57 53 2) (36 47 188 101 2) (24 47 193 107 2) (33 282 119 79 2) (37 406 209 114 2) (25 412 213 104 2) (50 625 125 114 2)))
;-------------------------------------------------------

; Lineaire
; BEAT 77
((d 7 1 1) ((63 -73 359 76 2) (-57 0 677 32 2) (-62 0 677 29 2) (-51 0 677 43 2) (65 286 219 49 2) (63 516 161 50 2)))
((d 7 1 2) ((-57 0 698 32 2) (-62 0 636 29 2) (-51 0 698 43 2) (-63 0 698 50 2)))
((d 7 1 3) ((-57 0 375 32 2) (-51 0 443 43 2) (-63 0 115 50 2) (62 177 109 102 2) (63 245 156 20 2) (62 385 94 40 2) (60 495 73 55 2)))
((d 7 1 4) ((65 -10 244 77 2) (57 203 104 44 2) (62 318 94 69 2) (63 375 229 48 2)))
;-------------------------------------------------------
; BEAT 81
((d 7 1 1) ((62 -46 307 50 2) (51 -46 359 49 2) (57 -41 719 36 2) (60 266 120 49 2) (51 412 266 35 2) (62 422 219 58 2)))
((d 7 1 2) ((60 -21 120 42 2) (-57 0 698 36 2) (-51 0 698 35 2) (62 198 177 49 2) (60 395 303 48 2)))
((d 7 1 3) ((-57 0 677 36 2) (-51 0 677 35 2) (-60 0 677 48 2)))
((d 7 1 4) ((-57 0 567 36 2) (-51 0 552 35 2) (-60 0 541 48 2) (62 599 119 108 2)))
;-------------------------------------------------------
; BEAT 85
((d 7 1 1) ((63 32 177 77 2) (57 245 198 38 2) (65 256 135 91 2) (62 407 161 41 2) (57 568 109 35 2) (63 573 104 76 2)))
((d 7 1 2) ((-57 0 63 35 2) (-63 0 47 76 2) (62 47 209 38 2) (60 261 52 53 2) (57 287 78 35 2) (62 381 203 56 2)))
((d 7 1 3) ((57 -93 88 44 2) (60 -88 99 58 2)))
((d 7 1 4) ((69 276 105 72 2) (72 401 105 65 2) (77 521 42 80 2)))
;-------------------------------------------------------
; BEAT 89
((d 7 1 1) ((81 -99 109 121 2) (89 78 135 110 2) (87 302 145 107 2) (86 484 125 99 2)))
((d 7 1 2) ((85 -26 156 83 2) (65 -21 239 41 2) (60 -21 255 45 2) (54 -16 490 50 2) (84 156 161 59 2) (60 312 110 67 2) (65 343 105 34 2) (82 427 166 83 2)))
((d 7 1 3) ((83 -63 188 79 2) (79 187 109 100 2) (82 349 145 86 2) (80 515 120 104 2) (65 520 115 55 2) (60 520 73 68 2) (61 526 109 43 2)))
((d 7 1 4) ((75 -16 172 65 2) (71 161 78 48 2) (61 182 57 50 2) (60 182 47 58 2) (73 297 156 108 2) (51 479 177 79 2) (74 489 104 115 2) (57 489 167 58 2) (62 489 167 55 2)))
;-------------------------------------------------------
; BEAT 93
((d 7 1 1) ((75 -5 146 79 2) (-57 0 94 58 2) (-62 0 99 55 2) (72 167 109 95 2) (57 177 99 67 2) (69 338 131 59 2) (66 489 167 85 2) (57 495 135 52 2) (51 526 99 48 2)))
((d 7 1 2) ((64 -15 145 79 2) (-66 0 31 85 2) (51 172 99 53 2) (57 172 68 66 2) (72 188 88 104 2) (74 224 73 53 2) (67 302 115 88 2) (73 427 94 95 2)))
((d 7 1 3) ((77 -99 52 78 2) (76 -94 37 72 2) (61 -16 110 30 2) (55 0 94 32 2) (77 16 41 38 2) (79 16 88 79 2) (85 94 93 101 2) (89 198 83 93 2) (65 271 120 52 2) (60 276 63 52 2) (61 276 83 28 2) (93 292 104 107 2) (55 323 73 23 2) (79 401 198 102 2)))
((d 7 1 4) ((62 511 187 61 2) (51 521 177 82 2) (57 531 167 61 2)))
;-------------------------------------------------------
; BEAT 97
((d 7 1 1) ((-62 0 125 61 2) (-51 0 31 82 2) (-57 0 188 61 2) (81 182 110 127 2) (77 297 89 86 2) (74 422 47 48 2) (57 505 58 42 2) (69 511 104 107 2)))
((d 7 1 2) ((65 5 157 31 2) (51 172 188 61 2) (57 193 125 49 2) (72 282 151 114 2) (74 479 105 115 2)))
((d 7 1 3) ((70 -36 99 59 2) (71 68 104 75 2) (52 146 312 50 2) (63 151 333 36 2) (58 156 365 32 2) (75 198 115 93 2) (82 370 135 124 2) (80 552 146 112 2)))
((d 7 1 4) ((63 -52 52 14 2) (-80 0 10 112 2) (79 21 140 74 2) (78 182 146 104 2) (63 188 67 50 2) (58 214 151 61 2) (77 344 104 61 2) (75 500 146 84 2) (51 500 177 82 2) (62 531 146 27 2) (57 542 104 20 2)))
;-------------------------------------------------------

; Peches
; BEAT 101
((d 7 1 1) ((65 167 125 107 2) (84 167 125 119 2) (54 172 156 112 2) (72 172 94 125 2) (77 172 78 105 2) (60 172 214 106 2) (81 177 110 104 2) (60 521 57 105 2)))
((d 7 1 2) ((60 0 208 111 2) (54 0 348 122 2) (72 0 203 127 2) (84 0 229 124 2) (77 5 245 115 2) (65 5 208 110 2) (81 5 229 113 2) (61 15 37 91 2) (72 281 156 75 2) (77 286 156 36 2) (60 286 104 76 2) (84 302 88 25 2) (81 328 67 55 2)))
((d 7 1 3) ((83 -89 735 118 2) (55 -89 688 121 2) (71 -89 735 127 2) (66 -89 656 124 2) (80 -89 735 118 2) (76 -89 735 114 2) (61 -89 735 111 2)))
((d 7 1 4) ((-83 0 166 118 2) (-71 0 104 127 2) (-80 0 166 118 2) (-76 0 177 114 2) (-61 0 182 111 2) (60 416 208 111 2) (54 416 208 118 2) (65 416 208 100 2) (77 416 208 113 2) (84 416 208 104 2) (81 416 208 110 2) (72 421 203 117 2) (61 437 36 72 2) (71 447 47 55 2)))
;-------------------------------------------------------
; BEAT 105
((d 7 1 1) ((-60 0 32 111 2) (-54 0 115 118 2) (-65 0 53 100 2) (-77 0 105 113 2) (-84 0 89 104 2) (-81 0 99 110 2) (-72 0 115 117 2) (53 334 245 117 2) (64 339 203 107 2) (59 339 266 111 2) (76 339 146 104 2) (71 339 240 110 2) (80 339 260 97 2) (83 344 183 100 2)))
((d 7 1 2) ((70 99 281 120 2) (58 99 375 115 2) (75 104 297 102 2) (63 104 250 114 2) (82 104 313 105 2) (52 104 287 111 2) (79 104 229 81 2)))
((d 7 1 3) ((62 -21 292 104 2) (51 -16 396 114 2) (57 -16 313 115 2) (74 -11 276 102 2) (81 -11 292 77 2) (79 21 57 38 2) (50 442 177 114 2) (60 442 177 110 2) (56 448 171 109 2) (80 448 171 104 2) (70 448 171 98 2) (73 453 166 99 2)))
((d 7 1 4) ((-50 0 261 114 2) (-60 0 105 110 2) (-56 0 131 109 2) (-80 0 542 104 2) (-70 0 511 98 2) (-73 0 542 99 2)))
;-------------------------------------------------------

; Pont
; BEAT 109
((g 7 1 1) ((43 -15 713 127 2) (31 -15 713 127 2) (59 490 208 121 2) (53 490 208 124 2) (64 490 208 122 2) (55 506 31 103 2)))
((g 7 1 2) ((-43 0 354 127 2) (-31 0 323 127 2) (-59 0 349 121 2) (-53 0 313 124 2) (-64 0 344 122 2) (39 485 187 97 2)))
((g# 7 1 3) ((44 -31 729 112 2) (54 -31 302 125 2) (65 -31 323 114 2) (60 -31 354 100 2) (32 -15 99 85 2) (32 287 375 117 2)))
((g# 7 1 4) ((77 -5 260 112 2) (72 -5 234 104 2) (-44 0 11 112 2) (66 0 172 114 2) (73 37 62 30 2) (77 360 73 20 2) (59 396 172 69 2) (57 412 156 48 2) (70 443 182 122 2)))
;-------------------------------------------------------
; BEAT 113
((g 7 1 1) ((71 -26 724 115 2) (59 -26 724 111 2) (64 -21 719 122 2) (53 -21 719 120 2) (57 -16 714 99 2)))
((g 7 1 2) ((81 -31 291 127 2) (-71 0 359 115 2) (-59 0 172 111 2) (-64 0 146 122 2) (-53 0 146 120 2) (-57 0 166 99 2) (64 302 114 92 2) (59 307 266 96 2) (53 318 41 59 2) (62 318 46 64 2) (57 333 110 52 2) (56 354 62 38 2) (79 427 130 112 2) (77 458 47 58 2)))
((f 7 1 3) ((51 -52 750 119 2) (62 -52 302 119 2) (57 -47 313 104 2) (73 -47 125 126 2) (74 94 166 104 2) (71 286 125 117 2) (62 302 385 120 2) (57 312 386 70 2) (56 359 73 26 2) (67 448 172 90 2)))
((f 7 1 4) ((65 -63 68 85 2) (-51 0 130 119 2) (-57 0 177 70 2) (63 52 141 112 2) (65 224 94 52 2) (51 234 120 105 2) (67 260 63 26 2) (63 265 110 94 2) (57 276 57 25 2) (62 323 229 99 2) (50 562 131 106 2) (60 578 83 108 2)))
;-------------------------------------------------------
; BEAT 117
((g 7 1 1) ((50 -26 146 106 2) (65 -5 307 102 2) (58 0 105 113 2) (59 94 167 60 2) (43 120 328 102 2) (53 323 167 120 2) (52 339 31 101 2) (79 433 234 125 2) (71 443 224 120 2) (74 443 193 110 2) (69 448 188 113 2)))
((g 7 1 2) ((-79 0 57 125 2) (-71 0 16 120 2) (59 182 84 25 2) (62 214 46 13 2) (62 318 265 108 2) (86 318 312 102 2) (91 323 172 118 2) (83 323 125 114 2) (57 323 292 87 2) (53 328 214 105 2) (81 328 156 111 2) (59 328 42 85 2) (56 359 73 45 2) (60 583 94 82 2)))
((g# 7 1 3) ((89 -57 250 113 2) (84 -52 255 101 2) (78 -47 313 102 2) (80 -36 98 77 2) (65 255 250 105 2) (60 255 349 115 2) (54 255 396 121 2)))
((g# 7 1 4) ((77 -47 287 112 2) (66 -47 344 117 2) (72 -42 276 105 2) (51 292 151 120 2) (54 417 260 112 2) (44 422 255 108 2)))
;-------------------------------------------------------
; BEAT 121
((g 7 1 1) ((58 -31 146 125 2) (67 -31 573 112 2) (51 -31 255 84 2) (50 -31 62 39 2) (-54 0 11 112 2) (-44 0 167 108 2) (59 99 156 87 2) (43 234 443 24 2) (53 266 344 114 2) (60 276 182 117 2) (61 422 125 101 2) (50 573 104 102 2)))
((g 7 1 2) ((62 -78 167 111 2) (67 -67 333 102 2) (-43 0 146 24 2) (-50 0 188 102 2) (59 68 271 107 2) (53 255 146 113 2) (67 396 224 88 2) (55 396 182 104 2) (57 417 47 76 2) (48 589 109 120 2)))
((f 7 1 3) ((-48 0 52 120 2) (51 21 359 127 2) (41 52 271 80 2) (42 73 63 29 2) (60 266 104 118 2) (67 266 349 119 2) (63 266 172 106 2) (59 396 240 69 2) (60 427 63 39 2)))
((f 7 1 4) ((51 -83 167 92 2) (48 -57 187 43 2) (60 21 188 113 2) (67 37 177 88 2) (63 37 229 105 2) (51 214 218 121 2) (41 224 42 63 2) (59 344 167 119 2) (67 344 146 105 2)))
;-------------------------------------------------------

; Rythmic
; BEAT 125
((d 7 1 1) ((38 -5 244 71 2) (-50 0 16 111 2) (45 234 89 99 2) (53 344 182 126 2) (60 344 213 115 2) (54 568 109 81 2)))
((d 7 1 2) ((55 -10 208 94 2) (56 224 104 107 2) (60 339 291 3 2) (57 354 203 93 2) (53 594 88 76 2)))
((d 7 1 3) ((54 10 172 91 2) (60 15 198 49 2) (53 198 166 79 2) (50 359 167 78 2) (60 411 68 18 2) (53 568 109 107 2)))
((d 7 1 4) ((60 -68 235 48 2) (-53 0 36 107 2) (54 42 239 90 2) (50 255 120 68 2) (53 385 261 105 2)))
;-------------------------------------------------------
; BEAT 129
((d 7 1 1) ((-53 0 677 105 2) (60 385 68 61 2) (54 385 130 74 2)))
((d 7 1 2) ((-53 0 698 105 2) (50 26 167 68 2) (54 265 152 99 2) (60 286 141 72 2) (38 364 204 79 2)))
((d 7 1 3) ((50 -78 130 58 2) (-53 0 526 105 2) (54 41 167 88 2) (60 47 172 70 2) (45 349 250 98 2) (48 448 83 21 2)))
((d 7 1 4) ((54 -89 125 95 2) (53 -89 58 36 2) (60 -83 140 84 2) (36 10 167 112 2) (48 16 171 115 2) (45 213 120 87 2) (49 338 120 127 2) (37 344 208 115 2) (50 547 130 111 2)))
;-------------------------------------------------------
; BEAT 133
((d 7 1 1) ((52 -94 73 32 2) (26 -21 182 106 2) (38 213 115 117 2) (53 354 193 124 2) (60 354 323 110 2) (54 567 99 84 2)))
((d 7 1 2) ((-60 0 573 110 2) (55 0 177 100 2) (33 109 94 34 2) (56 203 104 111 2) (38 255 104 60 2) (26 307 115 15 2) (57 333 219 101 2) (53 573 125 97 2)))
((d 7 1 3) ((33 -99 73 29 2) (38 -89 104 59 2) (54 -6 183 114 2) (-53 0 5 97 2) (53 192 125 80 2) (38 208 182 73 2) (50 317 157 88 2) (52 369 89 26 2) (54 541 136 125 2) (60 552 120 107 2) (36 552 125 109 2) (24 562 115 109 2)))
((d 7 1 4) ((53 -5 255 81 2) (-54 0 15 125 2) (-36 0 119 109 2) (-24 0 47 109 2) (33 172 125 94 2) (38 302 234 107 2) (50 312 177 114 2) (52 354 57 45 2) (33 515 120 110 2)))
;-------------------------------------------------------
; BEAT 137
((d 7 1 1) ((26 -11 234 124 2) (38 223 79 109 2) (60 328 198 127 2) (53 333 187 127 2) (38 489 63 35 2) (54 557 109 94 2)))
((d 7 1 2) ((38 -21 203 108 2) (53 192 94 120 2) (60 198 130 111 2) (26 296 256 127 2) (38 515 183 124 2)))
((d 7 1 3) ((53 -58 130 120 2) (60 -53 579 104 2) (-38 0 20 124 2) (54 135 125 92 2) (38 161 120 122 2) (26 171 53 52 2) (53 270 151 105 2) (34 489 63 18 2) (36 489 83 92 2)))
((d 7 1 4) ((29 -73 166 127 2) (41 -73 192 127 2) (37 -32 63 25 2) (38 140 110 96 2) (42 255 229 124 2) (30 260 203 112 2) (38 463 84 74 2)))
))





(setf Allthethingssolo-juil2011_beatdur 326
      Allthethingssolo-juil2011_beatsfromfile 
'(      ;"All the things you are", BPM = 180, beatdur = 333 (mean 326), Bernard's solo recorded in Uzeste, July 13th, 2011
((f m7) ((68 220 108 97 2)))
((f m7) ((-68 0 2 97 2) (65 38 114 66 2)))
((f m7) ((68 -22 162 114 2)))
((f m7) ((65 -21 99 105 2)))
((bb m7) ((70 -41 167 114 2) (72 204 120 100 2)))
((bb m7) ((-72 0 41 100 2) (73 182 135 113 2)))
((bb m7) ((70 161 104 114 2)))
((bb m7) ((65 -20 130 106 2) (61 152 104 103 2)))
((eb 7) ((60 -16 98 124 2) (58 145 83 105 2)))
((eb 7) ((56 -52 182 66 2)))
((eb 7) ((58 -10 151 103 2) (55 214 68 74 2)))
((eb 7) ((56 6 151 105 2) (58 220 78 111 2)))
((g# maj7) ((55 36 140 81 2)))
((g# maj7) ((60 -83 57 120 2) (63 21 109 89 2) (67 193 78 127 2) (66 214 16 105 2)))
((g# maj7) ((65 15 130 110 2) (63 187 99 100 2)))
((g# maj7) ((60 -6 172 123 2)))
((c# maj7) ((65 -5 213 113 2) (56 214 120 89 2)))
((c# maj7) ((-56 0 78 89 2)))
((c# maj7) nil)
((c# maj7) nil)
((d m7) nil)
((d m7) ((65 0 156 111 2) (69 187 78 98 2)))
((g 7) ((67 15 68 127 2) (71 209 98 103 2)))
((g 7) ((74 -21 136 100 2) (77 157 172 118 2)))
((c maj7) ((-77 0 30 118 2) (76 4 282 70 2)))
((c maj7) nil)
((c maj7) nil)
((c maj7) ((72 -10 104 89 2) (71 94 94 81 2) (69 220 104 65 2)))
((c maj7) ((67 -4 56 127 2) (69 136 78 56 2) (71 209 120 98 2)))
((c maj7) ((-71 0 5 98 2) (67 31 42 127 2) (69 125 109 64 2)))
((c maj7) ((67 -94 203 127 2)))
((c maj7) nil)
((c m7) nil)
((c m7) ((75 -99 151 125 2)))
((c m7) ((74 -63 120 100 2) (73 130 109 101 2)))
((c m7) ((72 -41 130 105 2) (70 131 108 88 2)))
((f m7) ((68 -16 115 105 2) (67 114 100 127 2)))
((f m7) ((65 -10 57 93 2) (63 120 146 97 2)))
((f m7) ((62 -11 120 77 2) (60 172 156 118 2)))
((f m7) ((-60 0 11 118 2)))
((bb 7) nil)
((bb 7) nil)
((bb 7) nil)
((bb 7) nil)
((eb maj7) nil)
((eb maj7) ((62 -78 98 113 2) (67 46 94 127 2) (58 213 109 105 2)))
((eb maj7) ((-58 0 22 105 2) (55 37 141 70 2)))
((eb maj7) ((62 -94 109 117 2) (60 56 183 123 2)))
((g# maj7) ((56 -74 246 70 2)))
((g# maj7) nil)
((g# maj7) nil)
((g# maj7) nil)
((a m7) ((57 -100 37 70 2) (60 5 135 125 2) (63 192 115 111 2)))
((a m7) ((62 16 134 58 2) (60 203 109 125 2)))
((d 7) ((57 15 167 76 2) (58 202 115 101 2)))
((d 7) ((59 4 198 108 2)))
((g maj7) ((66 -10 188 101 2)))
((g maj7) ((67 -22 115 127 2) (69 187 119 95 2)))
((g maj7) ((66 -6 136 72 2) (62 176 110 85 2)))
((g maj7) ((59 -21 141 86 2) (64 161 115 106 2)))
((g maj7) ((62 -20 156 77 2) (64 188 136 103 2)))
((g maj7) ((62 26 198 64 2)))
((g maj7) ((64 -73 99 98 2) (73 203 68 114 2) (74 219 104 110 2)))
((g maj7) ((-74 0 328 110 2)))
((a m7) ((-74 0 333 110 2)))
((a m7) ((-74 0 187 110 2)))
((a m7) ((72 -84 52 64 2) (74 46 151 65 2)))
((a m7) ((77 46 172 114 2)))
((d 7) ((77 -68 105 78 2) (75 63 177 68 2)))
((d 7) ((77 -46 114 110 2) (74 130 194 68 2)))
((d 7) ((-74 0 15 68 2) (72 46 198 97 2)))
((d 7) ((71 -58 380 86 2)))
((g maj7) ((-71 0 48 86 2) (72 42 110 101 2) (73 152 72 105 2)))
((g maj7) ((74 -74 141 106 2) (71 78 124 97 2)))
((g maj7) ((66 -74 110 118 2) (62 62 188 81 2)))
((g maj7) ((64 -68 68 105 2) (66 63 187 106 2)))
((g maj7) ((62 -63 63 88 2) (64 46 198 100 2)))
((g maj7) ((62 -58 177 72 2)))
((g maj7) nil)
((g maj7) nil)
((f# m7) nil)
((f# m7) nil)
((f# m7) ((71 -57 114 114 2) (69 171 116 100 2)))
((f# m7) ((68 -16 151 77 2) (66 177 89 118 2)))
((b 7) ((65 0 105 106 2) (63 157 104 110 2)))
((b 7) ((62 -10 176 95 2) (60 188 88 127 2)))
((b 7) ((59 0 136 111 2) (60 136 114 127 2)))
((b 7) ((59 -83 104 101 2) (57 31 182 74 2) (56 224 104 92 2)))
((e maj7) ((-56 0 329 92 2)))
((e maj7) ((-56 0 322 92 2)))
((e maj7) ((-56 0 334 92 2)))
((e maj7) ((-56 0 36 92 2) (52 213 89 74 2)))
((g m7) ((56 22 82 78 2) (58 104 68 103 2)))
((g m7) ((64 -79 83 117 2) (68 4 94 98 2) (70 108 63 106 2) (75 202 104 111 2)))
((c 7) ((70 -11 94 105 2) (68 78 124 88 2) (64 213 115 71 2)))
((c 7) ((-64 0 83 71 2)))
((f m7) nil)
((f m7) nil)
((f m7) ((68 -68 110 121 2) (65 62 156 105 2)))
((f m7) ((67 -68 74 127 2) (68 52 151 81 2)))
((bb m7) ((70 -78 89 101 2) (72 57 126 91 2)))
((bb m7) ((73 -82 88 111 2) (75 37 146 110 2)))
((bb m7) ((77 -74 126 111 2) (72 124 79 29 2)))
((bb m7) ((75 -89 151 120 2)))
((eb 7) ((72 -78 68 97 2) (70 58 130 100 2)))
((eb 7) ((68 -83 131 101 2) (67 52 74 127 2)))
((eb 7) ((63 74 140 92 2)))
((eb 7) ((61 -68 104 97 2) (60 78 130 127 2)))
((g# maj7) ((63 -52 57 93 2) (67 57 125 127 2)))
((g# maj7) ((67 -57 57 127 2) (63 42 204 81 2)))
((g# maj7) ((67 -42 57 127 2) (72 78 146 111 2)))
((g# maj7) ((72 -58 141 91 2) (67 72 104 127 2)))
((c# maj7) ((72 -68 110 113 2) (68 104 157 68 2)))
((c# maj7) ((65 -46 72 105 2) (61 63 141 95 2)))
((c# maj7) ((60 -56 109 125 2) (58 116 213 63 2)))
((c# maj7) ((-58 0 57 63 2) (56 41 224 93 2)))
((c# m7) ((54 -42 344 89 2)))
((c# m7) nil)
((c# m7) nil)
((c# m7) nil)
((c m7) nil)
((c m7) ((55 -83 67 74 2) (60 37 141 127 2)))
((c m7) ((63 -88 73 106 2) (65 48 108 120 2) (66 209 99 120 2)))
((c m7) ((67 46 104 127 2)))
((b m7) ((65 -88 88 120 2) (63 63 177 64 2)))
((b m7) ((62 -41 93 65 2) (65 52 126 92 2)))
((b m7) ((68 -84 125 114 2) (70 78 124 98 2)))
((b m7) ((71 -68 83 101 2) (73 67 151 100 2)))
((bb m7) ((74 -62 57 100 2) (77 48 166 106 2)))
((bb m7) ((75 -88 94 111 2) (73 42 130 83 2)))
((bb m7) ((72 -98 62 86 2) (70 37 161 101 2)))
((bb m7) ((68 -89 83 76 2) (67 41 79 127 2)))
((eb 7) ((65 -89 57 100 2) (63 42 171 89 2)))
((eb 7) ((65 -58 58 98 2) (67 26 73 127 2) (63 224 104 117 2)))
((eb 7) ((-63 0 10 117 2) (61 47 99 97 2) (63 146 114 81 2)))
((eb 7) ((61 -63 110 74 2) (60 47 187 127 2)))
((g# maj7) ((58 -73 401 97 2)))
((g# maj7) ((-58 0 328 97 2)))
((g# maj7) ((-58 0 229 97 2)))
((g# maj7) ((55 -72 68 86 2) (58 32 83 86 2) (60 141 47 127 2)))
((g m7) ((61 -88 88 113 2) (60 63 157 125 2)))
((g m7) ((58 -79 116 98 2) (55 67 183 91 2)))
((c 7) ((60 -47 84 127 2) (58 67 250 70 2)))
((c 7) ((55 63 161 106 2)))
((f m7) ((56 62 167 98 2)))
((f m7) ((58 58 208 110 2)))
((f m7) ((60 73 239 125 2)))
((f m7) nil)
((bb m7) ((61 -72 88 111 2) (65 94 214 91 2)))
((bb m7) ((72 52 193 108 2)))
((bb m7) ((68 -62 162 93 2)))
((bb m7) nil)
((eb 7) ((65 -84 62 110 2) (61 36 146 82 2)))
((eb 7) ((67 26 125 127 2)))
((eb 7) ((66 -93 109 123 2) (65 53 108 100 2)))
((eb 7) ((63 -99 79 113 2) (61 42 83 63 2)))
((g# maj7) ((60 -78 47 127 2) (63 32 182 103 2)))
((g# maj7) ((67 16 146 127 2)))
((g# maj7) ((63 -79 146 92 2)))
((g# maj7) ((60 -94 88 127 2) (56 72 172 71 2)))
((c# maj7) ((53 -52 260 98 2)))
((c# maj7) nil)
((c# maj7) nil)
((c# maj7) ((56 -82 72 120 2) (60 42 152 127 2)))
((d m7) ((53 -83 98 85 2) (55 41 146 110 2) (56 224 93 101 2)))
((d m7) ((58 37 130 88 2) (59 213 104 108 2)))
((g 7) ((60 10 146 127 2)))
((g 7) ((62 -99 57 111 2) (63 16 146 106 2) (64 224 78 110 2)))
((c maj7) ((67 -16 126 127 2) (67 193 99 127 2)))
((c maj7) ((64 -5 182 91 2) (67 203 73 127 2)))
((c maj7) ((64 37 93 74 2) (67 126 78 127 2) (64 208 116 95 2)))
((c maj7) ((-64 0 4 95 2) (59 -6 151 108 2) (62 187 89 106 2) (64 192 78 105 2)))
((c maj7) ((59 -6 146 82 2) (62 156 72 88 2) (59 218 84 111 2)))
((c maj7) ((55 0 146 93 2) (59 198 125 111 2)))
((c maj7) ((-59 0 5 111 2) (55 27 302 65 2)))
((c maj7) ((-55 0 62 65 2)))
((c m7) ((60 224 78 125 2)))
((c m7) ((62 36 151 117 2) (63 218 110 101 2)))
((c m7) ((-63 0 16 101 2) (62 42 182 72 2)))
((c m7) ((60 -82 72 127 2) (58 52 178 100 2)))
((f m7) ((57 -94 109 100 2) (56 47 161 83 2)))
((f m7) ((58 -73 84 93 2) (60 63 130 121 2)))
((f m7) ((62 -89 94 113 2) (63 41 152 68 2)))
((f m7) ((65 -88 145 110 2) (67 62 94 127 2)))
((bb 7) ((68 -89 94 114 2) (70 52 135 88 2)))
((bb 7) ((71 -94 131 93 2) (74 26 151 106 2) (75 203 110 111 2)))
((bb 7) ((77 5 141 108 2) (79 192 105 121 2)))
((bb 7) ((74 21 140 89 2) (77 203 125 117 2)))
((eb maj7) ((-77 0 15 117 2) (75 15 141 78 2) (70 198 125 110 2)))
((eb maj7) ((-70 0 5 110 2) (67 36 94 127 2) (65 198 125 120 2)))
((eb maj7) ((-65 0 5 120 2) (63 5 120 95 2) (65 130 83 108 2) (63 213 110 88 2)))
((eb maj7) ((-63 0 10 88 2) (62 10 188 89 2)))
((g# maj7) ((60 -78 150 127 2) (60 198 72 127 2) (59 203 41 106 2)))
((g# maj7) ((72 -4 114 100 2) (67 110 47 127 2)))
((g# maj7) ((63 -78 26 66 2) (59 0 83 93 2) (60 5 145 103 2)))
((g# maj7) ((62 -83 57 91 2) (63 16 182 101 2)))
((a m7) ((63 37 235 117 2)))
((a m7) ((62 37 105 110 2)))
((d 7) ((63 -83 105 110 2) (62 74 202 85 2)))
((d 7) ((60 -16 240 127 2)))
((g maj7) ((59 -63 209 97 2)))
((g maj7) ((59 -98 130 93 2) (62 11 78 81 2) (66 126 52 110 2)))
((g maj7) ((69 -94 187 114 2) (67 56 120 127 2)))
((g maj7) ((66 -98 140 123 2) (62 63 182 89 2)))
((g maj7) ((59 -73 37 65 2) (64 -5 177 113 2) (66 157 140 105 2)))
((g maj7) ((64 -74 115 103 2) (59 26 187 91 2)))
((g maj7) ((62 -57 265 88 2) (64 -57 67 93 2)))
((g maj7) nil)
((a m7) ((74 42 172 106 2) (72 42 16 111 2)))
((a m7) ((74 -78 172 100 2) (72 126 203 74 2)))
((a m7) ((74 99 214 108 2)))
((a m7) ((72 46 162 85 2)))
((d 7) ((74 -26 114 105 2) (75 52 276 45 2)))
((d 7) ((-75 0 47 45 2) (74 84 218 98 2)))
((d 7) ((72 -16 339 92 2)))
((d 7) ((74 41 209 100 2)))
((g maj7) ((71 -42 370 88 2)))
((g maj7) ((-71 0 166 88 2)))
((g maj7) ((66 -62 94 117 2) (62 73 219 64 2)))
((g maj7) ((59 -57 62 89 2) (64 31 162 110 2)))
((g maj7) ((66 -88 156 117 2) (62 47 198 85 2)))
((g maj7) ((64 -83 104 110 2) (59 99 109 23 2)))
((g maj7) ((62 -78 281 108 2)))
((g maj7) nil)
((f# m7) ((71 219 104 114 2)))
((f# m7) ((-71 0 11 114 2) (69 57 167 82 2)))
((f# m7) ((68 -78 177 98 2) (69 198 125 88 2)))
((f# m7) ((-69 0 16 88 2) (68 120 198 111 2)))
((b 7) ((66 15 188 95 2)))
((b 7) ((68 -83 161 110 2) (66 156 141 121 2)))
((b 7) ((64 21 187 78 2)))
((b 7) ((66 -100 141 118 2) (64 134 152 105 2)))
((e maj7) ((63 -6 219 68 2)))
((e maj7) ((64 -98 150 108 2) (63 141 151 106 2)))
((e maj7) ((61 -41 83 91 2) (63 42 136 82 2) (61 178 124 81 2)))
((e maj7) ((60 -26 140 123 2) (63 161 93 118 2)))
((g m7) ((64 -16 146 111 2) (60 166 94 127 2)))
((g m7) ((58 -15 193 74 2) (55 193 79 76 2)))
((c 7) ((53 -10 188 86 2) (52 204 68 63 2)))
((c 7) ((53 -26 178 110 2) (55 178 83 105 2)))
((f m7) ((56 -21 198 97 2) (58 187 141 106 2)))
((f m7) ((-58 0 15 106 2) (53 15 172 93 2)))
((f m7) ((55 -93 108 103 2) (56 31 146 75 2)))
((f m7) ((58 -94 110 105 2) (60 52 140 127 2)))
((bb m7) ((61 -99 115 108 2) (63 47 151 88 2)))
((bb m7) ((65 -88 140 113 2) (67 42 93 127 2)))
((bb m7) ((68 -93 99 108 2) (70 32 177 103 2)))
((bb m7) ((65 -78 36 100 2) (66 31 182 114 2)))
((eb 7) ((67 -58 188 127 2)))
((eb 7) ((72 -78 135 111 2) (67 41 68 127 2) (65 150 100 110 2)))
((eb 7) ((61 -58 94 85 2) (60 98 115 123 2)))
((eb 7) ((58 -48 84 92 2) (56 78 202 69 2)))
((g# maj7) ((55 -48 68 83 2) (60 46 130 127 2)))
((g# maj7) ((60 -83 115 127 2) (56 37 187 88 2)))
((g# maj7) ((60 -63 93 127 2) (56 82 246 82 2)))
((g# maj7) ((-56 0 4 82 2) (55 41 187 93 2)))
((c# maj7) ((53 -63 271 105 2) (56 198 88 93 2)))
((c# maj7) ((60 -16 36 127 2) (65 57 213 120 2)))
((c# maj7) ((60 -46 177 127 2)))
((c# maj7) ((53 -93 156 58 2) (56 22 94 93 2) (60 131 67 127 2) (59 131 26 101 2)))
((c# m7) ((65 -100 79 106 2) (64 -94 68 110 2) (63 20 308 110 2)))
((c# m7) ((61 36 208 97 2)))
((c# m7) ((60 -84 266 127 2) (61 188 119 81 2)))
((c# m7) ((60 -21 73 127 2) (58 42 224 101 2)))
((c m7) ((60 -52 380 127 2)))
((c m7) ((-60 0 47 127 2) (60 136 83 108 2)))
((c m7) ((63 203 126 121 2)))
((c m7) ((60 0 156 127 2) (62 4 100 78 2) (63 208 83 114 2)))
((b m7) ((61 10 126 77 2) (62 125 193 77 2)))
((b m7) ((65 42 119 121 2)))
((b m7) ((67 -78 68 127 2) (68 73 167 95 2)))
((b m7) ((70 -63 145 106 2) (71 114 104 68 2)))
((bb m7) ((74 -46 119 108 2) (73 126 130 111 2)))
((bb m7) ((75 -31 141 108 2) (73 120 115 100 2)))
((bb m7) ((72 -58 84 98 2) (70 93 146 106 2)))
((bb m7) ((68 -31 83 83 2) (67 68 140 127 2)))
((eb 7) ((65 -26 104 118 2) (63 115 177 64 2)))
((eb 7) ((61 -41 141 100 2) (60 130 105 127 2)))
((eb 7) ((58 -41 130 111 2) (56 78 209 82 2)))
((eb 7) ((55 -46 88 88 2) (56 68 156 93 2)))
((g# maj7) ((58 -57 52 110 2) (60 57 182 127 2)))
((g# maj7) ((55 -62 156 93 2)))
((g# maj7) ((55 73 240 103 2)))
((g# maj7) nil)
((g m7) ((55 -52 56 106 2) (58 67 141 92 2)))
((g m7) ((63 -74 89 120 2) (61 52 192 92 2)))
((c 7) ((60 -74 68 127 2) (58 42 72 105 2)))
((c 7) ((58 -78 100 70 2) (55 42 167 93 2)))
((f m7) ((56 63 167 100 2)))
((f m7) ((58 57 171 118 2)))
((f m7) ((60 63 140 127 2)))
((f m7) ((53 57 182 98 2)))
((bb m7) ((58 42 224 105 2)))
((bb m7) ((60 -26 109 127 2)))
((bb m7) ((61 -78 135 114 2)))
((bb m7) ((60 -73 78 127 2) (58 31 167 83 2)))
((eb 7) ((56 -83 104 91 2) (55 67 141 93 2)))
((eb 7) ((56 -68 84 105 2) (58 52 156 98 2)))
((eb 7) ((60 -78 78 127 2) (61 52 120 89 2)))
((eb 7) ((63 -100 68 91 2) (65 0 156 114 2)))
((g# maj7) ((67 -94 63 127 2) (63 22 187 64 2) (60 224 52 127 2)))
((g# maj7) ((58 22 166 83 2) (56 214 99 88 2)))
((g# maj7) ((55 20 130 81 2) (56 156 130 82 2)))
((g# maj7) ((55 -52 115 82 2) (53 32 234 110 2)))
((c# maj7) nil)
((c# maj7) ((53 -72 98 113 2) (56 16 100 77 2) (60 142 67 127 2)))
((c# maj7) ((63 -73 99 113 2) (60 26 93 127 2) (56 156 78 76 2)))
((c# maj7) ((53 -47 93 88 2) (51 36 104 120 2) (49 156 88 91 2)))
((d m7) ((50 -47 83 85 2) (52 36 94 95 2) (53 156 73 101 2)))
((d m7) ((55 -57 88 75 2) (57 47 98 103 2) (59 156 73 121 2)))
((g 7) ((60 -63 73 127 2) (62 31 89 105 2) (64 150 74 113 2)))
((g 7) ((65 -78 98 110 2) (67 26 68 127 2) (69 140 74 100 2)))
((c maj7) ((71 -78 84 93 2) (72 16 89 108 2) (74 105 73 113 2) (76 209 115 110 2)))
((c maj7) ((-76 0 333 110 2)))
((c maj7) ((-76 0 209 110 2) (74 177 104 106 2)))
((c maj7) ((72 -73 99 108 2) (71 16 166 98 2) (74 208 104 103 2)))
((c maj7) ((72 16 135 88 2) (74 151 104 81 2)))
((c maj7) ((72 -88 88 95 2) (71 21 161 81 2) (72 219 104 105 2)))
((c maj7) ((-72 0 41 105 2) (74 209 109 111 2)))
((c maj7) ((75 172 162 106 2)))
((c m7) ((-75 0 134 106 2) (73 -26 88 40 2) (72 192 94 113 2)))
((c m7) ((70 -20 176 77 2) (67 178 46 127 2)))
((c m7) ((65 -26 126 117 2) (63 162 99 111 2)))
((c m7) ((62 -11 171 85 2) (60 187 83 124 2)))
((f m7) ((58 -6 208 93 2) (56 198 72 89 2)))
((f m7) ((55 -26 140 85 2) (56 114 136 82 2)))
((f m7) ((55 -94 94 89 2) (53 16 172 93 2)))
((f m7) ((55 -100 104 121 2) (56 46 130 92 2)))
((bb 7) ((58 -98 104 95 2) (59 52 141 108 2)))
((bb 7) ((61 -94 105 105 2) (62 48 134 95 2)))
((bb 7) ((67 -94 57 127 2) (70 36 135 105 2)))
((bb 7) ((71 -94 52 92 2) (74 16 166 113 2)))
((eb maj7) ((67 -78 10 127 2) (70 26 167 105 2) (63 214 110 88 2)))
((eb maj7) ((67 36 114 127 2)))
((eb maj7) ((62 -42 37 55 2) (63 62 172 100 2)))
((eb maj7) ((58 -67 78 70 2) (62 63 150 98 2) (60 68 109 127 2)))
((g# maj7) ((55 -47 73 81 2) (56 62 209 98 2)))
((g# maj7) ((60 -26 47 127 2) (63 78 172 120 2)))
((g# maj7) ((63 -21 152 106 2) (60 209 114 127 2)))
((g# maj7) ((56 26 203 70 2)))
((a m7) ((57 -52 276 95 2)))
((a m7) ((60 -62 56 127 2) (63 52 146 117 2)))
((d 7) ((62 -68 89 98 2) (60 62 120 123 2) (62 188 93 85 2)))
((d 7) ((60 -31 94 127 2) (59 57 167 97 2)))
((g maj7) ((62 -58 380 114 2)))
((g maj7) ((-62 0 235 114 2) (59 209 83 100 2)))
((g maj7) ((54 -57 83 101 2) (50 26 177 83 2)))
((g maj7) ((52 -78 78 110 2) (54 42 182 92 2)))
((g maj7) ((52 26 188 95 2)))
((g maj7) ((50 -99 177 101 2) (48 -94 41 95 2)))
((g maj7) nil)
((g maj7) ((52 -94 68 106 2) (55 5 83 100 2) (59 88 68 111 2) (62 203 67 106 2)))
((a m7) ((62 -4 265 114 2)))
((a m7) ((60 -73 1 127 2) (62 151 1 98 2)))
((a m7) ((60 47 203 125 2)))
((a m7) ((62 -26 151 110 2) (62 213 89 114 2)))
((d 7) ((63 -94 297 105 2)))
((d 7) ((62 -94 141 91 2) (60 58 140 127 2)))
((d 7) ((63 -83 135 117 2) (62 52 172 93 2)))
((d 7) ((60 -78 109 127 2) (59 47 104 92 2) (60 167 73 127 2)))
((g maj7) ((61 -57 63 103 2) (62 52 151 117 2)))
((g maj7) ((59 -78 1 120 2) (54 63 156 52 2)))
((g maj7) ((52 21 140 97 2) (54 177 89 108 2)))
((g maj7) ((52 -21 79 89 2) (50 52 182 95 2)))
((g maj7) ((54 -41 187 121 2)))
((g maj7) ((50 198 89 118 2)))
((g maj7) ((52 21 141 103 2) (54 188 83 95 2)))
((g maj7) ((55 -11 172 103 2) (57 203 73 101 2)))
((f# m7) ((59 -21 156 123 2) (60 146 141 127 2)))
((f# m7) ((59 -73 94 106 2) (57 21 145 101 2) (56 214 88 108 2)))
((f# m7) ((57 37 135 88 2) (59 213 104 121 2)))
((f# m7) ((60 11 114 127 2) (62 187 78 110 2)))
((b 7) ((63 5 172 105 2) (63 214 114 92 2)))
((b 7) ((-63 0 16 92 2) (62 6 124 89 2) (63 182 68 106 2) (64 224 26 97 2)))
((b 7) ((62 -26 88 113 2) (63 73 125 105 2) (62 178 145 101 2)))
((b 7) ((-62 0 5 101 2) (60 -26 151 127 2) (59 193 135 118 2)))
((e maj7) ((59 141 62 105 2) (57 146 57 103 2)))
((e maj7) ((63 -68 78 78 2) (64 47 57 106 2) (68 146 88 121 2)))
((e maj7) ((64 -31 166 106 2) (63 151 104 101 2)))
((e maj7) ((57 -37 105 68 2) (59 -15 67 56 2) (63 151 104 114 2)))
((g m7) ((64 -21 141 103 2) (64 182 78 106 2)))
((g m7) ((66 -26 141 117 2) (66 182 100 120 2)))
((c 7) ((67 -10 120 127 2) (67 204 72 127 2)))
((c 7) ((68 22 160 108 2)))
((f m7) ((68 -89 83 106 2) (70 36 151 113 2)))
((f m7) ((70 -84 78 106 2) (72 26 156 110 2)))
((f m7) ((72 -84 58 106 2) (73 52 146 110 2) (75 166 32 55 2)))
((f m7) ((73 -63 63 108 2) (75 46 152 117 2)))
((bb m7) ((75 -62 62 106 2) (77 53 161 106 2)))
((bb m7) ((77 -68 48 64 2) (79 63 119 121 2)))
((bb m7) ((79 -63 63 114 2) (80 56 136 120 2)))
((bb m7) ((80 -63 73 106 2) (82 52 172 118 2)))
((eb 7) ((82 -41 72 110 2) (84 89 156 111 2)))
((eb 7) ((84 -26 114 106 2) (82 88 157 108 2)))
((eb 7) ((82 -15 119 120 2) (80 109 130 105 2)))
((eb 7) ((80 -10 124 110 2) (79 99 130 121 2)))
((g# maj7) ((84 -47 131 120 2) (79 78 141 114 2)))
((g# maj7) ((75 -37 152 105 2) (74 89 124 81 2)))
((g# maj7) ((70 -52 141 120 2) (68 83 115 101 2) (70 219 78 95 2)))
((g# maj7) ((68 -36 124 103 2) (67 68 114 127 2)))
((c# maj7) ((65 -73 229 121 2) (72 219 109 114 2)))
((c# maj7) ((-72 0 21 114 2) (68 0 94 89 2) (65 115 104 91 2) (61 219 104 85 2)))
((c# maj7) ((-61 0 5 85 2) (60 42 239 114 2)))
((c# maj7) ((58 -16 192 98 2) (63 208 120 121 2)))
((c# m7) ((-63 0 317 121 2)))
((c# m7) ((61 -22 183 92 2) (63 208 120 120 2)))
((c# m7) ((-63 0 20 120 2) (61 31 156 65 2) (63 203 89 108 2)))
((c# m7) ((61 -41 115 106 2) (60 22 145 127 2)))
((c m7) ((51 -67 391 105 2)))
((c m7) ((-51 0 328 105 2)))
((c m7) ((-51 0 198 105 2)))
((c m7) ((48 -78 62 114 2) (51 36 162 101 2)))
((b m7) ((54 -94 120 110 2) (55 -37 365 92 2)))
((b m7) ((-55 0 328 92 2)))
((b m7) ((-55 0 172 92 2)))
((b m7) nil)
((bb m7) ((65 -78 94 123 2) (61 78 198 82 2)))
((bb m7) ((61 52 162 117 2)))
((bb m7) ((65 37 192 117 2)))
((bb m7) ((68 42 167 117 2)))
((eb 7) ((76 46 84 114 2) (77 88 240 110 2)))
((eb 7) ((-77 0 328 110 2)))
((eb 7) ((-77 0 20 110 2) (67 36 292 127 2)))
((eb 7) ((-67 0 124 127 2) (68 218 110 117 2)))
((g# maj7) ((-68 0 26 117 2) (70 10 78 88 2) (68 109 94 82 2) (67 192 48 127 2)))
((g# maj7) ((68 -21 349 123 2)))
((g# maj7) ((-68 0 328 123 2)))
((g# maj7) ((-68 0 334 123 2)))
((g m7) ((-68 0 98 123 2) (70 5 57 44 2) (66 10 120 76 2) (63 72 104 48 2) (61 104 98 58 2) (62 109 21 36 2) (60 114 42 103 2) (59 135 47 65 2) (58 140 99 101 2) (57 161 52 55 2) (56 166 99 82 2) (55 187 47 58 2) (54 202 89 61 2)))
((g m7) ((53 -99 42 58 2) (51 -78 52 108 2) (52 -68 21 55 2) (51 -10 26 108 2) (44 47 15 55 2) (46 47 26 124 2) (42 62 32 54 2) (39 68 244 114 2) (44 78 21 106 2) (37 99 15 114 2) (42 104 32 113 2) (41 110 26 36 2) (37 125 161 111 2) (40 146 20 83 2) (35 208 37 43 2)))
((c 7) ((-39 0 16 114 2) (36 -98 68 44 2) (38 -67 67 45 2) (40 -67 89 58 2) (41 -15 63 29 2) (43 11 57 31 2) (42 16 42 56 2) (45 22 67 56 2) (47 32 78 61 2) (48 42 94 86 2) (50 68 94 64 2) (52 89 99 64 2) (53 115 94 71 2) (55 141 89 64 2) (57 183 67 48 2) (59 204 57 81 2)))
((c 7) ((60 -89 42 95 2) (64 -21 32 43 2) (65 -11 32 65 2) (69 31 47 61 2) (71 47 57 65 2) (72 68 63 58 2) (74 94 63 56 2) (76 109 78 44 2) (77 120 99 54 2) (79 157 88 37 2)))
((f m7) ((81 -89 47 29 2) (83 -79 63 56 2) (84 -73 88 54 2)))
))








(setf Goodbyesolo-juil2011_beatdur 857
      Goodbyesolo-juil2011_beatsfromfile
'(      ;"Goodbye Porkpie Hat", BPM = 70, beatdur = 857, Bernard's solo recorded in Uzeste, July 13th, 2011
((c# 7) ((68 -271 68 114 2) (65 -146 73 105 2) (63 -15 124 117 2) (60 135 84 121 2) (58 265 126 117 2) (56 411 104 91 2) (53 537 124 95 2) (65 719 109 100 2)))
((f# maj7) ((58 5 140 86 2) (59 171 100 100 2) (71 500 78 48 2)))
((f# maj7) ((64 635 26 114 2) (65 635 120 118 2)))
((b 7) ((63 -88 94 83 2) (61 35 94 83 2) (58 318 115 114 2) (56 480 104 82 2) (54 620 146 72 2)))
((b 7) ((53 -41 36 68 2) (51 42 99 117 2) (52 589 31 110 2) (53 589 93 105 2)))
((f 7) ((56 -11 93 103 2) (57 119 187 91 2) (60 306 230 125 2) (62 578 228 118 2)))
((f 7) ((60 11 239 124 2) (65 313 146 120 2) (58 594 115 101 2) (59 709 140 91 2)))
((c# 7) ((-59 0 32 91 2) (58 130 26 34 2)))
((c# 7) ((70 323 52 95 2) (71 328 109 117 2) (70 474 99 117 2) (68 615 109 98 2)))
((f# maj7) ((65 -99 63 120 2) (63 10 110 118 2) (59 172 78 103 2) (58 297 141 111 2) (56 458 110 92 2) (53 584 150 81 2)))
((f# maj7) ((56 -84 68 77 2) (58 36 152 100 2) (53 229 67 65 2) (56 344 135 113 2) (58 516 135 105 2) (56 620 150 85 2)))
((b 7) ((53 -30 30 64 2) (56 63 146 89 2) (58 350 146 105 2) (53 516 47 83 2) (59 610 120 121 2)))
((b 7) ((58 -98 114 74 2) (56 26 178 77 2)))
((eb 7) ((72 360 140 118 2) (65 495 99 108 2) (67 604 84 127 2)))
((eb 7) ((70 -99 130 106 2) (65 47 146 105 2) (61 229 115 64 2) (60 349 42 125 2) (58 459 78 91 2) (55 594 167 101 2)))
((c# 7) ((53 -78 42 65 2) (58 36 151 105 2) (53 198 62 70 2) (55 302 120 111 2) (56 437 105 93 2) (58 568 130 86 2) (59 734 104 103 2)))
((c# 7) ((-59 0 21 103 2) (61 6 124 108 2) (63 178 78 86 2) (65 292 104 100 2) (67 438 72 127 2) (68 563 109 110 2) (70 714 68 86 2)))
((eb 7) ((72 -15 135 91 2) (73 120 94 110 2) (75 245 125 111 2) (77 401 89 103 2) (79 537 146 123 2)))
((eb 7) ((80 -36 88 117 2) (81 94 68 56 2) (84 246 119 120 2) (80 422 152 105 2) (81 500 131 70 2) (84 704 98 118 2) (83 709 89 125 2)))
((f 7) ((80 32 114 117 2) (81 172 94 92 2) (77 313 130 110 2) (72 506 130 66 2) (74 682 157 89 2)))
((f 7) ((-74 0 63 89 2)))
((bb m7) ((77 505 115 123 2) (72 672 109 117 2)))
((bb m7) ((75 -16 131 124 2) (68 156 141 114 2) (65 333 104 93 2) (67 473 79 127 2) (68 615 88 106 2)))
((g# 7) ((70 -94 98 110 2) (68 -89 47 64 2) (71 67 135 114 2) (68 244 84 106 2) (63 380 120 72 2) (59 546 110 70 2) (58 650 198 98 2)))
((g# 7) ((-58 0 73 98 2)))
((g m7) ((57 297 63 75 2) (62 406 89 105 2) (65 505 63 121 2) (68 640 110 118 2)))
((g m7) ((69 -73 73 89 2) (70 36 136 120 2) (71 198 94 108 2) (72 333 115 120 2) (70 458 104 110 2) (68 594 156 97 2)))
((c# 7) ((64 -78 72 85 2) (63 36 136 108 2) (61 208 114 66 2) (60 317 68 127 2) (58 463 99 114 2) (54 572 141 65 2)))
((c# 7) ((52 -94 63 78 2) (53 -94 58 76 2) (51 16 146 118 2) (48 193 57 77 2) (50 297 130 105 2) (48 474 99 88 2) (51 568 166 118 2)))
((d 7) ((48 -82 72 106 2) (53 16 136 108 2) (48 194 78 93 2) (54 302 157 117 2)))
((d 7) ((56 282 114 110 2) (57 428 114 68 2) (60 542 146 127 2) (62 730 83 124 2)))
((g 7) ((65 10 120 124 2) (58 130 136 85 2) (59 302 130 106 2) (61 438 114 97 2) (62 578 115 105 2) (67 740 57 127 2)))
((g 7) ((69 5 63 105 2) (71 5 89 106 2) (74 141 120 114 2) (62 265 152 98 2) (70 531 120 123 2) (69 563 94 114 2) (71 703 136 108 2)))
((c# 7) ((-71 0 20 108 2) (70 31 245 71 2)))
((c# 7) ((68 68 162 113 2) (66 287 109 127 2) (68 490 125 123 2) (70 678 166 120 2)))
((f# maj7) ((-70 0 26 120 2) (66 52 100 113 2) (61 266 177 86 2) (63 443 135 88 2) (61 615 235 68 2)))
((f# maj7) ((-61 0 67 68 2)))
((bb 7) ((67 411 58 127 2) (68 422 172 124 2) (65 687 152 113 2)))
((bb 7) ((67 0 89 127 2) (62 161 172 93 2) (65 339 213 118 2)))
((c# 7) nil)
((c# 7) nil)
((c# 7) ((72 276 172 121 2) (70 417 109 114 2) (67 552 99 127 2)))
((c# 7) ((63 -15 156 103 2) (60 172 37 105 2) (64 266 162 103 2) (65 532 120 118 2) (66 694 82 123 2)))
((eb 7) ((67 -5 83 127 2) (66 141 130 120 2) (65 282 94 120 2) (63 428 124 118 2) (61 552 156 83 2) (60 724 47 111 2) (59 730 46 88 2)))
((eb 7) ((58 0 115 101 2) (56 193 172 52 2) (53 333 58 47 2) (58 469 146 114 2) (53 619 79 75 2)))
((f 7) ((56 -88 104 97 2) (58 68 120 114 2) (59 146 42 75 2) (56 230 125 92 2) (57 376 104 70 2) (60 532 83 127 2) (62 646 152 114 2)))
((f 7) ((65 100 150 123 2) (69 402 166 111 2) (70 672 152 117 2)))
((c# 7) ((71 104 282 108 2) (77 547 135 120 2) (71 672 114 97 2)))
((c# 7) ((75 -10 109 120 2) (65 151 52 65 2) (63 281 131 114 2) (59 412 124 114 2) (55 578 78 70 2) (53 578 78 83 2) (65 656 188 120 2)))
((f# maj7) ((59 -11 37 66 2) (63 88 136 117 2) (58 239 79 74 2) (54 385 183 68 2)))
((f# maj7) ((56 250 84 113 2) (58 355 119 98 2) (59 526 52 95 2) (60 532 42 108 2) (61 626 135 111 2)))
((b 7) ((63 -46 72 103 2) (65 58 146 113 2) (66 219 94 120 2) (68 344 146 100 2) (69 511 119 91 2) (70 646 182 89 2)))
((b 7) ((69 -10 98 95 2) (70 94 136 75 2) (71 276 156 100 2)))
((f 7) ((68 328 120 117 2) (69 609 151 108 2)))
((f 7) ((72 26 135 118 2) (68 208 63 81 2) (69 328 130 97 2) (65 495 98 118 2) (60 630 89 117 2)))
((c# 7) ((70 -46 94 106 2) (71 84 125 97 2) (70 365 109 123 2) (63 532 99 103 2) (59 652 94 82 2)))
((c# 7) ((58 -52 93 108 2) (56 72 156 82 2)))
((f# maj7) ((54 78 254 101 2) (58 348 234 98 2) (61 630 218 111 2)))
((f# maj7) ((-61 0 6 111 2) (65 58 198 120 2) (66 334 244 124 2) (68 625 177 121 2)))
((b 7) ((68 58 108 101 2) (69 177 667 63 2)))
((b 7) ((-69 0 354 63 2)))
((eb 7) ((66 250 74 121 2) (67 324 78 127 2) (77 568 152 120 2)))
((eb 7) ((67 -73 10 127 2) (75 79 187 123 2) (73 323 141 101 2) (72 557 120 103 2)))
((c# 7) ((70 -99 110 108 2) (67 93 53 127 2) (68 313 120 100 2) (70 479 114 105 2) (71 646 193 110 2)))
((c# 7) ((68 -10 73 89 2) (63 136 198 76 2) (59 350 203 89 2)))
((eb 7) nil)
((eb 7) ((63 312 120 124 2) (61 468 156 72 2) (60 619 94 127 2)))
((f 7) ((58 -83 73 93 2) (57 42 136 88 2) (60 198 100 123 2) (62 328 157 106 2) (65 626 166 124 2)))
((f 7) ((62 78 245 111 2) (60 370 104 114 2) (60 651 187 127 2)))
((bb m7) ((61 47 182 113 2) (72 370 479 110 2) (71 380 10 111 2)))
((bb m7) ((-72 0 563 110 2) (70 563 140 100 2)))
((g# 7) ((72 -37 115 105 2) (75 187 187 124 2) (73 416 172 105 2) (72 635 203 103 2)))
((g# 7) ((70 11 141 81 2) (72 240 244 97 2)))
((g m7) ((69 662 114 113 2) (70 740 104 105 2)))
((g m7) ((69 47 114 106 2) (65 198 130 111 2) (62 339 78 101 2) (60 474 94 127 2) (58 615 109 92 2) (57 739 105 93 2)))
((c# 7) ((-57 0 41 93 2) (55 47 150 70 2) (57 213 94 113 2) (58 343 94 93 2) (60 484 99 127 2) (61 589 140 117 2)))
((c# 7) ((63 -73 131 114 2) (60 52 167 127 2) (61 214 187 91 2) (60 349 167 127 2)))
((d 7) ((56 230 99 113 2) (57 350 114 108 2) (62 474 105 118 2) (54 605 119 82 2)))
((d 7) ((50 -78 89 88 2) (53 37 146 98 2) (50 224 37 82 2) (54 329 166 118 2) (50 505 52 58 2) (53 609 11 105 2) (55 609 168 117 2)))
((g 7) ((56 -89 83 106 2) (58 52 162 91 2) (59 240 124 123 2) (61 364 47 103 2) (62 385 63 93 2) (62 500 88 97 2) (64 635 120 114 2)))
((g 7) ((65 -58 89 117 2) (67 78 83 127 2) (68 213 109 113 2) (70 328 130 111 2) (71 484 94 88 2) (73 604 120 120 2)))
((c# 7) ((75 -78 104 113 2) (77 47 125 88 2) (79 193 89 125 2) (80 308 171 113 2) (82 448 78 120 2) (83 584 145 114 2) (80 729 115 114 2)))
((c# 7) ((75 10 146 110 2) (70 296 110 117 2) (68 458 84 105 2) (66 572 110 117 2)))
((f# maj7) ((77 -98 109 113 2) (70 -5 172 93 2) (75 172 115 118 2) (68 292 156 95 2) (73 458 110 108 2) (66 568 162 105 2)))
((f# maj7) ((70 -100 104 106 2) (63 10 135 66 2) (68 166 99 97 2) (61 286 140 72 2) (66 452 68 111 2) (58 567 135 81 2) (63 724 93 98 2)))
((bb 7) ((56 15 142 110 2) (57 183 124 71 2) (59 198 104 66 2) (60 297 110 123 2) (65 453 58 117 2) (69 573 156 114 2) (72 719 83 100 2)))
((bb 7) ((77 10 120 118 2) (81 166 115 114 2) (69 286 256 100 2)))
((c# 7) ((67 32 72 127 2) (68 48 187 113 2) (67 282 120 127 2) (65 448 162 111 2) (60 574 93 123 2) (62 578 74 76 2) (67 724 104 127 2)))
((c# 7) ((65 5 287 113 2)))
((c# 7) ((63 282 56 106 2) (64 292 192 121 2) (72 578 271 117 2)))
((c# 7) ((-72 0 573 117 2) (70 594 99 103 2) (71 651 198 105 2)))
((eb 7) ((-71 0 52 105 2) (70 62 787 120 2)))
((eb 7) ((-70 0 354 120 2) (68 391 150 120 2) (63 677 151 124 2)))
((f 7) ((60 11 42 124 2) (58 105 135 120 2) (56 276 105 89 2) (53 396 152 93 2)))
((f 7) ((60 130 125 127 2) (63 135 63 111 2) (53 307 89 89 2) (58 400 146 114 2) (62 406 99 120 2) (56 552 109 83 2) (60 598 178 127 2) (57 672 150 92 2)))
((c# 7) ((56 6 93 111 2) (53 115 119 97 2) (52 120 31 95 2) (58 276 136 95 2) (59 417 161 110 2) (65 594 78 114 2) (59 704 67 110 2)))
((c# 7) ((65 -68 68 106 2) (59 10 68 95 2) (65 99 57 110 2) (59 162 88 111 2) (65 255 73 106 2) (59 318 98 110 2) (65 406 78 97 2) (59 484 94 108 2) (59 646 72 100 2) (65 724 120 110 2)))
((f# maj7) ((-65 0 31 110 2) (58 -16 99 98 2) (65 124 74 61 2) (58 187 115 89 2) (65 276 88 110 2) (58 354 109 89 2) (65 448 114 105 2) (58 526 83 72 2) (65 588 115 56 2) (58 677 167 78 2)))
((f# maj7) ((-58 0 62 78 2) (65 -84 58 65 2) (65 62 94 108 2) (58 119 105 82 2) (65 202 100 74 2) (58 270 89 76 2) (58 422 124 85 2) (65 572 271 66 2) (58 583 260 46 2)))
((b 7) ((-65 0 37 66 2) (-58 0 73 46 2) (56 83 89 78 2) (63 125 88 89 2) (56 224 287 65 2) (63 313 78 93 2) (63 485 83 92 2) (56 573 88 65 2) (63 641 120 89 2) (56 729 110 63 2)))
((b 7) ((-56 0 109 63 2) (63 0 198 89 2) (53 604 83 118 2)))
((f 7) ((56 4 94 106 2) (57 146 442 83 2) (60 609 47 124 2)))
((f 7) ((62 318 230 113 2) (60 594 84 121 2)))
((c# 7) ((65 47 287 124 2) (59 318 286 89 2) (63 620 234 97 2)))
((c# 7) ((-63 0 26 97 2) (53 84 536 81 2) (56 636 214 71 2)))
((f# maj7) ((-56 0 854 71 2)))
((f# maj7) ((-56 0 338 71 2) (53 370 312 121 2) (51 661 182 98 2)))
((b 7) ((-51 0 135 98 2) (53 161 694 100 2)))
((b 7) ((-53 0 213 100 2) (48 572 266 111 2)))
((eb 7) ((52 52 120 78 2) (53 167 396 92 2) (56 631 198 91 2)))
((eb 7) nil)
((c# 7) nil)
((c# 7) nil)
((eb 7) nil)
((eb 7) nil)
((f 7) nil)
((f 7) nil)
((bb m7) ((59 110 104 125 2) (60 151 401 103 2) (63 630 219 98 2)))
((bb m7) ((-63 0 52 98 2) (53 359 204 118 2) (56 615 187 111 2)))
((g# 7) ((58 57 187 97 2) (60 255 151 127 2) (58 380 214 86 2) (56 594 255 81 2)))
((g# 7) ((-56 0 11 81 2) (53 323 250 103 2) (56 578 193 106 2)))
((g m7) ((59 11 120 100 2) (60 100 426 114 2) (65 610 240 118 2)))
((g m7) ((-65 0 68 118 2) (53 276 276 118 2) (58 572 250 105 2)))
((c# 7) ((62 37 130 124 2) (63 178 338 91 2) (59 604 94 120 2) (60 610 5 120 2) (60 667 177 121 2)))
((c# 7) ((-60 0 68 121 2) (56 52 250 95 2) (52 302 256 98 2) (49 573 235 103 2)))
((d 7) ((54 -5 104 103 2) (56 104 458 85 2) (59 578 266 124 2)))
((d 7) ((-59 0 333 124 2) (53 296 553 110 2)))
((g 7) ((-53 0 10 110 2) (52 5 542 86 2) (57 463 110 108 2) (58 521 146 110 2)))
((g 7) ((55 296 552 98 2)))
((c# 7) ((-55 0 48 98 2) (56 11 839 97 2)))
((c# 7) ((-56 0 307 97 2) (67 708 47 127 2) (55 718 100 106 2)))
((f# maj7) ((54 -52 104 85 2) (66 -52 104 111 2) (65 52 796 91 2) (53 67 781 85 2)))
((f# maj7) ((-65 0 182 91 2) (-53 0 58 85 2) (58 276 63 68 2) (59 292 302 123 2) (70 292 104 124 2) (71 297 292 123 2) (58 542 302 103 2) (70 563 281 103 2)))
((bb 7) ((-58 0 21 103 2) (-70 0 10 103 2) (56 -5 849 98 2) (68 26 818 86 2)))
((bb 7) ((-56 0 234 98 2) (-68 0 214 86 2) (56 286 563 110 2) (67 292 67 127 2) (68 307 542 118 2)))
((c# 7) ((-56 0 26 110 2) (-68 0 21 118 2) (53 26 823 103 2) (65 52 797 101 2)))
((c# 7) ((-53 0 677 103 2) (-65 0 666 101 2) (57 672 104 65 2) (56 672 114 54 2)))
((c# 7) ((58 -32 626 103 2) (69 -26 98 108 2) (70 0 239 111 2) (70 270 302 98 2) (56 520 328 103 2) (68 552 239 105 2)))
((c# 7) ((-56 0 26 103 2) (53 -15 265 110 2) (65 0 136 121 2) (68 110 109 95 2) (65 230 83 97 2) (51 276 287 114 2) (63 302 230 69 2) (58 542 114 118 2) (70 568 115 123 2) (59 620 219 114 2) (71 672 167 103 2)))
((eb 7) ((-59 0 354 114 2) (-71 0 224 103 2) (71 297 114 114 2) (58 297 552 113 2)))
((eb 7) ((-58 0 36 113 2) (56 -10 614 111 2) (70 10 167 105 2) (68 73 495 71 2) (53 578 260 103 2) (65 594 244 98 2)))
((f 7) ((-53 0 104 103 2) (-65 0 74 98 2) (56 63 787 83 2) (67 68 94 127 2) (68 141 709 58 2)))
((f 7) ((-56 0 26 83 2) (-68 0 588 58 2) (56 72 548 103 2) (53 593 250 101 2) (65 604 239 88 2)))
((c# 7) ((-53 0 849 101 2) (-65 0 849 88 2)))
((c# 7) ((-53 0 854 101 2) (-65 0 854 88 2)))
((f# maj7) ((-53 0 16 101 2) (-65 0 16 88 2) (59 -52 162 88 2) (71 58 109 106 2) (60 78 672 121 2) (72 130 584 91 2) (70 698 152 100 2) (58 698 152 98 2)))
((f# maj7) ((-70 0 400 100 2) (-58 0 510 98 2) (56 384 1 86 2) (68 400 443 82 2)))
((b 7) ((-68 0 73 82 2) (53 83 584 92 2) (65 172 489 98 2) (51 620 229 93 2) (63 635 214 77 2)))
((b 7) ((-51 0 318 93 2) (-63 0 323 77 2) (48 312 485 111 2) (60 323 432 124 2)))
((f 7) ((53 -10 317 92 2) (65 16 280 92 2)))
((f 7) nil)
((c# 7) nil)
((c# 7) ((59 255 52 101 2) (63 370 140 86 2) (67 614 94 127 2)))
((f# maj7) ((71 -79 126 110 2) (70 31 130 103 2) (63 187 115 68 2) (59 307 125 98 2) (58 443 130 113 2) (56 593 126 88 2)))
((f# maj7) ((54 -89 83 63 2)))
))





(setf Dicidenbassolo-juil2011_beatdur 294
      Dicidenbassolo-juil2011_beatsfromfile
 '(      ;"D'ici d'en bas", BPM = 204, beatdur = 294, Bernard's solo recorded in Uzeste, July 13th, 2011
((g 7) ((60 32 135 125 2)))
((g 7) ((65 -89 161 88 2) (63 156 94 82 2)))
((g 7) ((58 -5 187 82 2)))
((g 7) ((53 -99 78 66 2) (59 26 125 110 2)))
((c m7) ((62 -99 130 83 2) (64 -94 125 83 2) (55 36 146 86 2)))
((c m7) ((60 -78 162 127 2)))
((c m7) nil)
((c m7) nil)
((c 7) nil)
((c 7) nil)
((c 7) nil)
((c 7) nil)
((f m7) ((72 187 109 98 2)))
((f m7) ((-72 0 52 98 2) (67 172 68 127 2)))
((f m7) ((70 20 120 103 2) (68 177 120 85 2)))
((f m7) ((-68 0 21 85 2) (65 36 109 106 2)))
((bb 7) ((63 -99 130 97 2) (61 47 125 89 2)))
((bb 7) ((62 -88 88 97 2) (65 26 141 114 2)))
((bb 7) ((58 -94 110 101 2) (56 20 162 74 2)))
((bb 7) ((53 -78 78 65 2) (58 37 120 101 2)))
((eb maj7) ((54 -73 99 66 2) (55 68 130 66 2)))
((eb maj7) ((58 -73 94 37 2) (62 47 125 103 2)))
((eb maj7) ((65 -84 94 114 2) (67 46 89 127 2)))
((eb maj7) ((62 -52 32 64 2) (65 52 115 118 2)))
((g# maj7) ((63 -62 46 74 2) (62 36 146 39 2)))
((g# maj7) ((59 -42 156 66 2)))
((g# maj7) ((56 -56 234 85 2)))
((g# maj7) nil)
((d 7) ((54 188 98 89 2)))
((d 7) ((57 5 156 105 2) (60 172 73 125 2)))
((d 7) ((65 11 78 111 2) (66 172 58 100 2) (65 183 21 103 2) (64 183 26 100 2)))
((d 7) ((69 36 68 61 2) (72 99 57 76 2)))
((g 7) ((75 -47 130 124 2) (73 135 100 101 2)))
((g 7) ((74 -5 140 93 2) (71 156 120 93 2)))
((g 7) ((67 16 99 127 2) (65 193 105 48 2)))
((g 7) ((-65 0 20 48 2) (63 0 156 92 2) (65 182 94 106 2)))
((c m7) ((67 47 57 127 2) (63 187 104 78 2)))
((c m7) ((-63 0 11 78 2) (60 32 182 120 2)))
((c m7) nil)
((c m7) ((62 -26 120 98 2) (63 167 62 85 2)))
((c 7) ((64 -10 172 76 2)))
((c 7) ((67 -84 11 127 2) (72 16 150 110 2)))
((c 7) ((64 -88 47 82 2) (70 11 157 95 2)))
((c 7) ((68 0 178 93 2)))
((f m7) ((67 -63 157 127 2)))
((f m7) nil)
((f m7) nil)
((f m7) ((65 -68 99 88 2) (64 78 136 83 2)))
((bb 7) ((65 -5 11 63 2) (70 99 151 110 2)))
((bb 7) ((68 26 151 85 2)))
((bb 7) ((67 -21 88 127 2)))
((bb 7) ((65 -57 167 101 2)))
((eb maj7) ((67 -78 104 127 2) (62 193 104 93 2)))
((eb maj7) ((-62 0 15 93 2) (58 41 183 78 2)))
((eb maj7) ((55 -41 245 72 2)))
((eb maj7) nil)
((g# maj7) nil)
((g# maj7) ((56 -89 83 75 2) (60 46 110 118 2)))
((g# maj7) ((63 -83 83 76 2) (65 93 142 113 2)))
((g# maj7) ((63 42 120 64 2)))
((d 7) ((65 -36 130 108 2)))
((d 7) ((63 -94 84 61 2) (65 52 120 108 2) (64 58 114 88 2) (66 166 126 98 2)))
((d 7) ((-66 0 52 98 2) (65 62 204 101 2)))
((d 7) ((63 -16 140 71 2)))
((g 7) ((62 -94 386 83 2)))
((g 7) ((-62 0 42 83 2) (63 151 135 95 2)))
((g 7) ((65 41 151 98 2)))
((g 7) ((67 -52 89 127 2) (62 177 120 98 2)))
((c m7) ((-62 0 73 98 2) (63 114 178 70 2)))
((c m7) ((-63 0 26 70 2) (60 78 218 118 2)))
((c m7) ((-60 0 32 118 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c m7) ((67 57 84 127 2)))
((f m7) ((70 52 188 110 2)))
((f m7) ((68 -48 126 74 2)))
((f m7) ((70 -94 224 101 2)))
((f m7) nil)
((g 7) nil)
((g 7) nil)
((g 7) ((68 -84 110 103 2) (67 88 162 127 2)))
((g 7) ((65 11 171 106 2)))
((c m7) ((63 -84 220 69 2) (65 177 115 110 2)))
((c m7) ((-65 0 20 110 2) (67 166 104 127 2)))
((c m7) ((60 145 135 121 2)))
((c m7) ((63 52 245 64 2)))
((c m7) ((-63 0 47 64 2)))
((c m7) nil)
((c m7) nil)
((c m7) ((67 -94 53 127 2) (66 11 141 113 2)))
((f m7) ((67 -88 42 127 2) (70 52 224 113 2)))
((f m7) ((68 -26 109 86 2)))
((f m7) ((70 -62 141 101 2) (70 183 109 95 2)))
((f m7) ((71 -32 245 101 2)))
((g 7) ((70 -68 364 65 2)))
((g 7) ((-70 0 78 65 2)))
((g 7) ((68 182 99 111 2)))
((g 7) ((70 37 161 98 2)))
((c m7) ((68 -100 157 83 2) (67 88 99 127 2)))
((c m7) ((67 27 82 127 2)))
((c m7) ((74 -78 119 110 2) (75 41 120 64 2) (74 130 161 74 2)))
((c m7) ((72 -58 204 86 2) (74 177 115 95 2)))
((c m7) ((-74 0 296 95 2)))
((c m7) ((-74 0 120 95 2)))
((c m7) nil)
((c m7) nil)
((f m7) nil)
((f m7) ((72 -89 167 91 2) (70 166 120 106 2)))
((f m7) ((68 94 187 92 2)))
((f m7) ((67 10 104 127 2)))
((g 7) ((65 -42 136 106 2)))
((g 7) ((63 -94 172 101 2) (62 152 146 97 2)))
((g 7) ((-62 0 10 97 2) (60 62 177 111 2)))
((g 7) ((59 -37 110 113 2) (62 172 125 110 2)))
((c m7) ((-62 0 16 110 2) (63 26 114 51 2) (62 114 156 89 2)))
((c m7) ((60 -53 204 127 2) (55 182 109 65 2)))
((c m7) ((-55 0 298 65 2)))
((c m7) ((-55 0 296 65 2)))
((c m7) ((-55 0 297 65 2)))
((c m7) ((-55 0 291 65 2)))
((c m7) ((-55 0 126 65 2) (58 188 110 105 2)))
((c m7) ((-58 0 57 105 2) (56 172 124 105 2)))
((f m7) ((-56 0 22 105 2) (53 42 151 52 2) (55 167 125 93 2)))
((f m7) ((-55 0 10 93 2) (56 32 104 92 2) (58 172 104 97 2)))
((f m7) ((60 26 146 125 2) (62 162 104 111 2)))
((f m7) ((63 26 124 103 2) (65 166 125 98 2)))
((g 7) ((-65 0 52 98 2) (67 16 78 127 2) (68 172 120 82 2)))
((g 7) ((-68 0 67 82 2) (70 21 150 93 2) (71 177 73 111 2)))
((g 7) ((74 10 172 113 2) (72 172 120 88 2)))
((g 7) ((-72 0 10 88 2) (67 15 104 127 2) (65 192 73 88 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c 7) nil)
((c 7) nil)
((c 7) ((60 -83 78 127 2) (62 31 126 98 2) (64 172 104 105 2)))
((c 7) ((65 27 130 111 2) (67 178 83 127 2)))
((f m7) ((68 20 104 93 2) (70 161 125 93 2)))
((f m7) ((72 20 84 103 2) (74 176 110 117 2)))
((f m7) ((75 6 104 91 2) (74 110 98 64 2)))
((f m7) ((73 -99 145 111 2) (74 99 193 93 2)))
((bb 7) ((-74 0 156 93 2)))
((bb 7) nil)
((bb 7) nil)
((bb 7) nil)
((eb maj7) nil)
((eb maj7) ((79 -89 125 117 2) (77 187 109 111 2)))
((eb maj7) ((-77 0 110 111 2) (74 84 192 97 2)))
((eb maj7) ((70 10 182 95 2)))
((g# maj7) ((67 -52 172 127 2)))
((g# maj7) nil)
((g# maj7) nil)
((g# maj7) ((75 -83 130 117 2) (74 156 130 97 2)))
((d 7) ((-74 0 16 97 2) (72 63 229 65 2)))
((d 7) ((-72 0 6 65 2) (74 -15 161 105 2)))
((d 7) ((75 -73 229 100 2) (77 193 104 91 2)))
((d 7) ((-77 0 119 91 2) (74 156 135 55 2)))
((g 7) ((-74 0 126 55 2)))
((g 7) nil)
((g 7) ((71 -78 99 111 2) (67 68 78 127 2)))
((g 7) ((65 -88 109 108 2) (63 32 160 86 2)))
((c m7) ((65 -94 104 110 2) (67 52 57 127 2) (63 166 126 81 2)))
((c m7) ((-63 0 36 81 2) (60 36 120 127 2)))
((c m7) nil)
((c m7) nil)
((c 7) nil)
((c 7) ((60 -46 72 124 2) (62 63 104 103 2)))
((c 7) ((63 -94 104 89 2) (64 30 131 82 2) (65 166 125 114 2)))
((c 7) ((-65 0 5 114 2) (67 15 78 127 2) (68 161 136 110 2)))
((f m7) ((-68 0 5 110 2) (70 10 156 106 2) (67 166 26 127 2)))
((f m7) ((68 0 136 100 2) (70 182 115 85 2)))
((f m7) ((-70 0 37 85 2)))
((f m7) nil)
((bb 7) nil)
((bb 7) ((70 172 78 111 2) (71 178 120 113 2)))
((bb 7) ((-71 0 15 113 2) (70 62 172 89 2)))
((bb 7) ((68 -37 152 97 2) (67 183 93 127 2)))
((eb maj7) ((70 135 162 108 2)))
((eb maj7) ((-70 0 10 108 2) (74 83 146 103 2)))
((eb maj7) ((75 -6 214 108 2)))
((eb maj7) ((74 -67 202 101 2) (72 187 110 70 2)))
((g# maj7) ((-72 0 172 70 2)))
((g# maj7) ((70 166 130 117 2)))
((g# maj7) ((-70 0 22 117 2) (68 52 240 69 2)))
((g# maj7) ((67 -20 202 127 2) (66 193 104 106 2)))
((d 7) ((-66 0 291 106 2)))
((d 7) ((-66 0 292 106 2)))
((d 7) ((-66 0 208 106 2)))
((d 7) ((65 -5 88 113 2) (66 31 261 105 2)))
((g 7) ((-66 0 52 105 2) (65 37 254 105 2)))
((g 7) ((-65 0 298 105 2)))
((g 7) ((-65 0 213 105 2)))
((g 7) ((64 -11 74 81 2) (65 41 256 74 2)))
((c m7) ((-65 0 5 74 2) (63 10 282 72 2)))
((c m7) ((-63 0 15 72 2) (60 15 229 127 2)))
((c m7) ((60 22 228 125 2)))
((c m7) ((60 26 270 124 2)))
((c m7) ((-60 0 286 124 2)))
((c m7) ((67 5 115 127 2)))
((c m7) ((70 31 136 118 2)))
((c m7) ((68 42 161 103 2)))
((f m7) ((67 48 82 127 2)))
((f m7) ((65 20 89 101 2)))
((f m7) ((70 -78 130 111 2) (68 193 104 89 2)))
((f m7) ((-68 0 52 89 2) (68 172 88 89 2) (67 177 115 127 2)))
((g 7) ((-67 0 42 127 2)))
((g 7) ((65 141 89 100 2)))
((g 7) ((63 21 209 91 2)))
((g 7) ((65 -31 135 110 2) (67 187 100 127 2)))
((c m7) ((67 162 88 127 2)))
((c m7) ((60 141 115 108 2)))
((c m7) ((63 26 271 75 2)))
((c m7) ((-63 0 52 75 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c m7) ((67 -89 42 127 2) (70 31 115 117 2)))
((f m7) ((68 -94 131 85 2) (67 47 94 127 2) (68 167 124 71 2)))
((f m7) ((-68 0 6 71 2) (67 -10 88 127 2) (65 115 141 93 2)))
((f m7) ((70 6 172 103 2)))
((f m7) ((68 -63 188 78 2)))
((g 7) ((68 -93 67 78 2) (67 -78 266 127 2)))
((g 7) nil)
((g 7) ((65 -79 115 117 2) (63 72 224 77 2)))
((g 7) ((-63 0 10 77 2) (65 36 188 111 2)))
((c m7) ((63 -62 358 58 2)))
((c m7) ((-63 0 292 58 2)))
((c m7) ((-63 0 152 58 2) (60 182 115 121 2)))
((c m7) ((-60 0 5 121 2) (58 16 161 97 2) (60 177 78 127 2)))
((c m7) ((63 21 104 95 2) (63 172 119 91 2)))
((c m7) ((-63 0 32 91 2) (60 22 124 125 2) (63 183 109 97 2)))
((c m7) ((-63 0 52 97 2)))
((c m7) nil)
((f m7) nil)
((f m7) ((60 -89 109 124 2) (62 42 108 100 2)))
((f m7) ((63 -100 115 89 2) (65 30 136 106 2) (67 171 89 127 2)))
((f m7) ((68 26 94 89 2) (70 161 100 98 2)))
((g 7) ((72 22 218 100 2)))
((g 7) ((67 -88 26 127 2) (70 32 120 114 2) (68 182 115 92 2)))
((g 7) ((-68 0 31 92 2) (67 37 120 127 2)))
((g 7) ((66 21 130 123 2) (67 182 84 127 2)))
((c m7) ((65 26 120 111 2) (63 177 119 92 2)))
((c m7) ((60 16 99 121 2) (58 152 130 110 2)))
((c m7) ((55 -11 193 81 2) (53 187 73 93 2)))
((c m7) ((60 5 156 127 2) (55 183 56 83 2)))
((c m7) ((58 -5 104 108 2) (60 104 104 127 2)))
((c m7) ((58 -100 136 66 2) (53 16 108 68 2) (55 120 93 105 2)))
((c m7) ((58 4 63 101 2) (60 93 105 124 2)))
((c m7) ((62 -89 74 117 2) (63 -15 62 108 2) (65 89 104 103 2) (67 177 68 127 2)))
((f m7) ((68 -36 84 105 2) (70 42 47 108 2) (72 120 110 71 2)))
((f m7) ((74 -72 83 117 2) (75 -5 63 111 2) (77 84 104 81 2) (79 167 78 124 2)))
((f m7) ((80 -68 109 103 2) (82 15 104 97 2) (84 104 193 120 2)))
((f m7) ((-84 0 292 120 2)))
((g 7) ((-84 0 140 120 2) (82 161 89 113 2)))
((g 7) ((83 -88 130 113 2) (82 130 162 114 2)))
((g 7) ((-82 0 31 114 2) (80 0 94 101 2) (79 136 145 114 2)))
((g 7) ((77 -6 136 89 2) (75 140 115 105 2)))
((c m7) ((79 -10 172 120 2) (74 152 52 81 2)))
((c m7) ((75 -20 140 111 2)))
((c m7) ((79 -10 171 106 2) (76 151 119 83 2)))
((c m7) ((72 -11 193 88 2)))
((c 7) nil)
((c 7) nil)
((c 7) nil)
((c 7) ((79 -99 94 121 2) (76 -11 74 105 2) (72 83 146 97 2)))
((f m7) ((70 -78 63 98 2) (69 58 156 45 2)))
((f m7) ((68 -100 104 93 2) (72 20 162 101 2)))
((f m7) ((70 16 115 113 2) (69 26 89 113 2) (68 172 125 93 2)))
((f m7) ((-68 0 21 93 2) (65 21 93 91 2) (63 177 89 105 2)))
((bb 7) ((60 11 145 125 2) (61 178 88 81 2)))
((bb 7) ((62 0 120 100 2) (65 162 120 117 2)))
((bb 7) ((58 0 167 86 2) (56 167 124 76 2)))
((bb 7) ((53 22 124 37 2) (54 152 114 105 2)))
((eb maj7) ((55 16 208 88 2) (58 172 120 101 2)))
((eb maj7) ((62 16 130 105 2) (65 172 120 123 2)))
((eb maj7) ((63 31 120 61 2)))
((eb maj7) ((62 -98 94 92 2) (60 16 130 123 2) (67 172 74 127 2)))
((g# maj7) ((72 6 124 85 2) (71 16 99 81 2) (63 178 82 65 2)))
((g# maj7) ((67 10 104 127 2)))
((g# maj7) ((60 -99 72 117 2) (63 10 167 120 2) (56 171 110 82 2)))
((g# maj7) ((60 32 120 127 2)))
((d 7) ((53 -99 94 83 2)))
((d 7) ((54 167 88 113 2)))
((d 7) ((57 -4 78 100 2) (60 79 57 127 2) (65 183 93 120 2)))
((d 7) ((63 0 150 110 2) (61 176 89 103 2)))
((g 7) ((62 31 136 93 2) (67 187 68 127 2)))
((g 7) ((71 0 120 105 2) (74 146 83 81 2)))
((g 7) ((75 10 182 124 2)))
((g 7) ((77 -63 224 66 2) (79 172 124 111 2)))
((c m7) ((-79 0 162 111 2)))
((c m7) nil)
((c m7) nil)
((c m7) ((63 188 84 117 2)))
((c 7) ((64 20 120 100 2) (66 187 78 117 2)))
((c 7) ((67 6 109 127 2)))
((c 7) ((64 -99 57 77 2) (68 26 140 111 2)))
((c 7) ((64 -94 47 77 2) (70 15 178 117 2)))
((f m7) ((71 16 140 117 2)))
((f m7) ((72 -100 152 114 2) (68 36 114 69 2)))
((f m7) ((67 -94 94 127 2) (65 52 83 93 2) (63 187 104 113 2)))
((f m7) ((-63 0 26 113 2) (60 32 114 127 2) (61 172 115 83 2)))
((bb 7) ((62 10 126 105 2) (65 162 135 121 2)))
((bb 7) ((58 0 120 108 2) (65 94 109 103 2) (58 167 88 38 2)))
((bb 7) ((59 -58 214 68 2) (58 161 135 100 2)))
((bb 7) ((-58 0 26 100 2) (56 130 146 110 2)))
((eb maj7) ((55 58 182 82 2)))
((eb maj7) ((58 -47 130 89 2) (62 177 114 113 2)))
((eb maj7) ((-62 0 11 113 2) (65 74 187 120 2)))
((eb maj7) ((63 -31 187 93 2) (60 193 104 127 2)))
((g# maj7) ((-60 0 297 127 2)))
((g# maj7) ((-60 0 292 127 2)))
((g# maj7) ((-60 0 83 127 2) (63 182 110 117 2)))
((g# maj7) ((62 6 146 103 2) (60 178 68 127 2)))
((d 7) ((63 16 115 120 2) (65 120 119 113 2)))
((d 7) ((63 -83 109 83 2) (62 26 156 101 2)))
((d 7) ((60 -94 79 127 2) (62 52 89 105 2) (63 141 120 91 2)))
((d 7) ((62 -73 135 83 2) (60 42 120 127 2)))
((g 7) ((62 -73 115 106 2) (63 5 135 98 2)))
((g 7) ((62 -84 167 93 2) (60 130 135 124 2)))
((g 7) ((59 78 120 95 2)))
((g 7) ((62 -21 63 58 2) (63 0 182 91 2)))
((c m7) ((62 -78 250 101 2) (60 187 109 125 2)))
((c m7) ((-60 0 115 125 2) (55 178 120 66 2)))
((c m7) ((-55 0 291 66 2)))
((c m7) ((-55 0 297 66 2)))
((c m7) ((-55 0 136 66 2) (55 192 105 97 2)))
((c m7) ((-55 0 5 97 2) (56 15 152 100 2) (58 187 110 93 2)))
((c m7) ((-58 0 5 93 2) (60 42 104 127 2) (62 177 104 120 2)))
((c m7) ((63 30 120 103 2) (65 176 120 114 2)))
((f m7) ((67 26 115 127 2) (68 193 104 98 2)))
((f m7) ((-68 0 57 98 2)))
((f m7) nil)
((f m7) nil)
((g 7) nil)
((g 7) ((67 -98 62 127 2) (66 52 94 65 2)))
((g 7) ((67 -99 37 127 2) (70 26 130 121 2) (68 166 131 89 2)))
((g 7) ((-68 0 10 89 2) (67 10 109 127 2) (65 167 98 101 2)))
((c m7) ((63 27 140 97 2) (65 187 94 103 2)))
((c m7) ((67 10 126 127 2) (63 182 104 93 2)))
((c m7) ((-63 0 100 93 2)))
((c m7) nil)
((c m7) ((53 182 84 113 2)))
((c m7) ((55 26 115 106 2) (56 161 100 100 2)))
((c m7) ((58 16 124 100 2) (60 166 115 125 2)))
((c m7) ((62 11 115 111 2) (63 162 114 100 2)))
((f m7) ((65 16 130 110 2) (67 151 89 127 2)))
((f m7) ((68 0 141 92 2) (70 172 119 118 2)))
((f m7) ((-70 0 94 118 2)))
((f m7) nil)
((g 7) nil)
((g 7) ((70 -78 188 100 2)))
((g 7) ((70 -73 99 114 2) (71 0 162 72 2)))
((g 7) ((70 -72 177 105 2) (68 157 135 106 2)))
((c m7) ((67 68 99 127 2)))
((c m7) ((67 -26 68 127 2) (75 177 115 121 2)))
((c m7) ((-75 0 31 121 2) (75 120 120 117 2)))
((c m7) ((74 -88 214 95 2) (72 126 172 103 2)))
((c m7) ((-72 0 146 103 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((f m7) ((70 -98 78 124 2) (68 37 141 78 2) (70 167 120 74 2)))
((f m7) ((67 16 120 127 2) (68 172 120 103 2)))
((f m7) ((-68 0 20 103 2) (65 20 162 82 2) (67 182 114 127 2)))
((f m7) ((-67 0 16 127 2) (63 22 140 97 2) (65 168 124 114 2)))
((g 7) ((-65 0 21 114 2) (62 0 172 81 2) (63 162 130 110 2)))
((g 7) ((-63 0 47 110 2) (60 6 156 127 2) (62 172 104 108 2)))
((g 7) ((58 -5 135 101 2) (60 135 146 125 2)))
((g 7) ((55 -4 119 93 2) (58 141 157 118 2)))
((c m7) ((56 -16 114 89 2) (58 88 125 101 2)))
((c m7) ((56 -99 115 91 2) (55 26 198 89 2)))
((c m7) ((53 -62 265 86 2)))
((c m7) ((55 -94 281 91 2) (58 182 114 93 2)))
((c m7) ((-58 0 298 93 2)))
((c m7) ((-58 0 291 93 2)))
((c m7) ((-58 0 131 93 2) (56 193 104 110 2)))
((c m7) ((-56 0 47 110 2) (55 36 162 74 2) (53 177 115 98 2)))
((f m7) ((-53 0 57 98 2) (51 42 140 111 2) (53 156 114 76 2)))
((f m7) ((51 -57 141 93 2) (48 89 172 76 2)))
((f m7) ((50 -57 182 93 2)))
((f m7) nil)
((g 7) ((51 10 146 95 2) (53 187 109 100 2)))
((g 7) ((-53 0 42 100 2) (55 16 136 114 2) (56 162 120 101 2)))
((g 7) ((58 10 83 93 2) (59 140 120 117 2)))
((g 7) ((60 -15 166 127 2) (62 161 89 118 2)))
((c m7) ((63 -6 152 97 2) (60 146 130 124 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c 7) ((60 167 78 124 2)))
((c 7) ((62 6 109 108 2) (64 141 105 106 2)))
((c 7) ((65 -16 125 108 2) (67 146 108 127 2)))
((c 7) ((68 11 88 108 2) (70 146 146 114 2)))
((f m7) ((72 21 219 105 2)))
((f m7) ((74 31 193 114 2)))
((f m7) ((75 16 172 123 2)))
((f m7) ((77 -58 146 106 2)))
((bb 7) ((77 -99 89 105 2) (79 0 182 110 2)))
((bb 7) ((78 -62 167 114 2) (77 162 136 111 2)))
((bb 7) ((-77 0 10 111 2) (75 62 203 105 2)))
((bb 7) ((74 4 162 101 2)))
((eb maj7) ((79 -73 188 124 2) (74 193 57 82 2)))
((eb maj7) ((70 -47 89 113 2) (67 78 177 127 2)))
((eb maj7) ((65 36 172 113 2) (67 41 5 127 2)))
((eb maj7) ((63 -37 172 93 2) (62 183 114 106 2)))
((g# maj7) ((-62 0 21 106 2) (63 36 78 31 2) (62 78 114 92 2) (60 188 104 127 2)))
((g# maj7) ((-60 0 104 127 2)))
((g# maj7) ((62 131 99 117 2)))
((g# maj7) ((63 -16 110 86 2) (65 115 115 103 2)))
((d 7) ((66 -26 78 127 2) (68 94 114 85 2) (66 172 120 110 2)))
((d 7) ((-66 0 5 110 2) (63 -6 94 111 2) (65 135 109 124 2)))
((d 7) ((67 -41 68 127 2) (65 21 140 111 2) (63 161 63 105 2)))
((d 7) ((66 -31 120 127 2) (68 93 100 75 2) (66 167 124 114 2)))
((g 7) ((-66 0 32 114 2) (63 0 115 81 2) (65 157 93 121 2)))
((g 7) ((67 -58 42 127 2) (65 36 78 108 2) (66 41 21 95 2) (63 140 52 118 2)))
((g 7) ((66 0 115 125 2) (67 120 89 127 2)))
((g 7) ((72 0 99 121 2) (66 166 94 121 2)))
((c m7) ((67 -20 72 127 2) (66 94 68 91 2) (65 162 78 106 2) (67 167 11 127 2)))
((c m7) ((63 10 151 92 2) (60 171 115 127 2)))
((c m7) ((-60 0 16 127 2) (58 -10 135 105 2) (55 166 130 82 2)))
((c m7) ((-55 0 16 82 2) (53 16 126 70 2) (52 22 20 69 2) (60 162 130 127 2)))
((c 7) ((-60 0 21 127 2) (47 26 74 105 2) (48 26 162 91 2) (50 162 135 105 2)))
((c 7) ((-50 0 11 105 2) (52 21 130 101 2) (53 151 141 110 2)))
((c 7) ((-53 0 26 110 2) (55 -5 166 111 2) (56 151 110 114 2)))
((c 7) ((58 0 136 108 2) (60 157 141 125 2)))
((f m7) ((-60 0 20 125 2) (62 10 125 117 2) (63 145 146 92 2)))
((f m7) ((-63 0 11 92 2) (65 11 146 117 2) (67 161 120 127 2)))
((f m7) ((68 0 131 101 2) (70 152 146 106 2)))
((f m7) ((-70 0 26 106 2) (60 10 182 123 2) (62 166 130 113 2)))
((bb 7) ((-62 0 6 113 2) (63 11 145 85 2) (65 156 136 118 2)))
((bb 7) ((-65 0 42 118 2) (67 10 110 127 2) (68 141 125 106 2)))
((bb 7) ((70 0 157 106 2) (72 178 120 81 2)))
((bb 7) ((-72 0 41 81 2) (74 20 126 120 2) (75 166 125 108 2)))
((eb maj7) ((-75 0 21 108 2) (65 31 193 97 2)))
((eb maj7) ((67 -94 99 127 2) (68 26 130 100 2) (70 171 126 103 2)))
((eb maj7) ((-70 0 5 103 2) (72 26 130 100 2) (74 156 130 117 2)))
((eb maj7) ((75 15 151 105 2) (77 166 130 91 2)))
((g# maj7) ((-77 0 21 91 2) (79 21 135 123 2) (80 167 109 111 2)))
((g# maj7) ((82 16 104 117 2)))
((g# maj7) ((82 -84 193 117 2) (80 -78 41 113 2) (80 176 115 117 2)))
((g# maj7) ((-80 0 53 117 2) (79 135 157 120 2)))
((d 7) ((-79 0 21 120 2) (78 57 130 97 2)))
((d 7) ((79 -67 109 98 2) (78 100 192 103 2)))
((d 7) ((-78 0 297 103 2)))
((d 7) ((-78 0 105 103 2) (77 177 78 108 2) (78 187 105 105 2)))
((g 7) ((-78 0 99 105 2) (77 89 202 95 2)))
((g 7) ((-77 0 26 95 2) (75 6 172 92 2)))
((g 7) ((77 -73 235 85 2) (75 177 115 95 2)))
((g 7) ((-75 0 31 95 2) (77 130 104 106 2)))
((c m7) ((78 -4 150 106 2) (79 120 141 110 2)))
((c m7) ((75 21 172 118 2)))
((c m7) ((72 26 151 118 2)))
((c m7) ((70 41 136 118 2) (69 78 83 106 2)))
((c m7) ((67 31 99 127 2)))
((c m7) ((72 -98 72 111 2) (66 48 130 127 2) (65 162 125 103 2)))
((c m7) ((63 67 120 108 2)))
((c m7) ((60 -94 84 127 2) (58 37 145 97 2) (55 182 105 83 2)))
((f m7) ((63 32 140 113 2) (58 172 125 88 2)))
((f m7) ((-58 0 21 88 2) (55 21 124 71 2)))
((f m7) ((56 -98 109 111 2)))
((f m7) ((58 -100 156 114 2) (60 187 104 127 2)))
((g 7) ((-60 0 63 127 2) (62 193 104 111 2)))
((g 7) ((-62 0 84 111 2)))
((g 7) ((63 -94 178 113 2)))
((g 7) ((65 -94 198 117 2)))
((c m7) ((67 -83 244 127 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c m7) ((55 -74 100 108 2) (57 -68 78 105 2) (60 52 108 127 2)))
((c m7) ((63 -94 68 105 2) (67 31 10 127 2) (65 31 88 120 2) (72 172 89 101 2) (71 177 32 105 2)))
((c m7) ((75 -15 115 101 2) (79 162 136 125 2)))
((c m7) ((-79 0 10 125 2) (78 0 120 101 2) (77 150 141 113 2)))
((f m7) ((-77 0 37 113 2) (75 161 100 121 2)))
((f m7) ((72 0 136 103 2) (68 162 78 108 2)))
((f m7) ((67 -6 74 127 2) (68 94 98 82 2)))
((f m7) ((67 -94 78 127 2) (65 4 89 117 2) (61 140 140 121 2)))
((g 7) ((62 0 141 92 2) (67 172 93 127 2)))
((g 7) ((59 22 114 66 2)))
((g 7) ((55 -98 72 76 2) (62 21 109 121 2) (59 172 125 111 2)))
((g 7) ((-59 0 5 111 2) (55 11 109 78 2) (57 21 26 72 2) (53 151 94 117 2)))
((c m7) ((51 26 270 51 2) (53 124 89 82 2)))
((c m7) ((-51 0 37 51 2) (48 22 187 85 2) (50 188 104 110 2)))
((c m7) ((-50 0 230 110 2)))
((c m7) nil)
((c m7) nil)
((c m7) ((48 -94 78 108 2) (50 0 94 110 2) (51 104 88 106 2)))
((c m7) ((53 -84 99 86 2) (55 15 99 114 2) (56 114 73 111 2)))
((c m7) ((58 -73 73 110 2) (60 26 99 127 2) (62 130 79 121 2)))
((f m7) ((63 -67 83 106 2) (65 32 104 113 2) (67 126 72 127 2)))
((f m7) ((68 -100 78 110 2) (70 10 73 106 2) (72 114 99 100 2)))
((f m7) ((74 -78 78 118 2) (75 -10 104 105 2) (77 104 73 97 2) (79 172 125 120 2)))
((f m7) ((-79 0 21 120 2) (74 -16 73 106 2) (77 78 78 108 2) (75 161 130 117 2)))
((g 7) ((74 0 74 93 2) (72 94 63 69 2) (71 178 104 105 2)))
((g 7) ((74 -31 78 118 2) (67 68 57 127 2) (65 73 52 105 2) (71 188 62 77 2)))
((g 7) ((74 -42 62 117 2) (81 57 57 105 2) (79 57 73 114 2) (67 125 73 127 2)))
((g 7) ((79 -72 72 110 2) (67 0 53 127 2) (79 74 52 100 2) (81 79 47 86 2) (67 152 42 127 2)))
((c m7) ((79 -57 62 108 2) (67 21 58 127 2) (79 105 104 106 2)))
((c m7) ((67 -94 68 127 2) (79 0 37 82 2)))
((c m7) ((78 10 36 64 2)))
((c m7) ((78 -89 57 127 2) (66 -89 57 127 2) (70 -74 16 111 2) (77 -32 198 95 2) (65 -11 1 52 2) (75 168 57 127 2)))
((c m7) ((63 -98 56 127 2) (72 36 126 108 2) (60 42 120 127 2) (72 188 104 89 2)))
((c m7) ((-72 0 26 89 2) (60 -94 52 113 2) (70 16 213 98 2) (58 52 146 77 2)))
((c m7) ((60 31 130 127 2) (72 36 162 117 2)))
((c m7) ((63 15 83 118 2) (75 15 83 121 2) (76 82 63 23 2)))
((f m7) ((64 -99 73 92 2) (76 -94 146 86 2) (65 -94 99 101 2) (77 -89 157 86 2) (78 172 124 111 2) (68 177 57 97 2) (66 177 89 120 2)))
((f m7) ((-78 0 16 111 2) (79 -10 151 100 2) (67 0 146 127 2)))
((f m7) ((77 21 182 113 2) (65 21 83 111 2) (76 26 183 108 2)))
((f m7) ((75 -84 115 72 2) (63 -62 114 101 2) (63 166 58 88 2) (73 166 42 117 2) (61 166 58 93 2) (74 177 109 120 2)))
((g 7) ((-74 0 26 120 2) (62 -57 131 92 2) (72 167 120 101 2) (60 178 78 124 2)))
((g 7) ((70 46 120 110 2) (58 56 63 103 2) (59 140 57 83 2) (71 166 125 106 2)))
((g 7) ((-71 0 26 106 2) (70 0 119 103 2) (58 11 119 97 2)))
((g 7) ((68 -93 213 111 2) (56 -82 244 108 2) (67 162 136 127 2) (55 162 136 100 2) (69 162 68 88 2)))
((c m7) ((-67 0 291 127 2) (-55 0 291 100 2)))
((c m7) ((-67 0 297 127 2) (-55 0 297 100 2)))
((c m7) ((-67 0 292 127 2) (-55 0 292 100 2)))
((c m7) ((-67 0 240 127 2) (-55 0 292 100 2)))
((c 7) ((-55 0 20 100 2) (65 -89 83 68 2) (64 -84 114 68 2) (62 -74 126 63 2) (60 -63 130 123 2) (57 52 36 66 2) (55 67 31 58 2) (53 72 42 75 2) (52 114 10 52 2) (50 119 16 56 2)))
((c 7) nil)
((c 7) nil)
((c 7) ((60 187 99 127 2)))
((f m7) ((66 -84 21 106 2) (62 26 124 103 2) (64 176 105 110 2)))
((f m7) ((65 16 125 118 2) (67 172 110 127 2)))
((f m7) ((68 10 105 110 2) (70 156 84 114 2) (68 162 42 93 2)))
((f m7) ((72 5 167 101 2) (65 136 82 113 2)))
((bb 7) ((68 -22 110 120 2) (70 88 120 97 2) (68 182 109 95 2)))
((bb 7) ((-68 0 11 95 2) (65 31 78 88 2) (66 157 135 120 2)))
((bb 7) ((-66 0 26 120 2) (67 5 94 127 2) (68 145 152 113 2)))
((bb 7) ((-68 0 10 113 2) (66 -26 146 120 2) (67 135 73 127 2)))
((eb maj7) ((66 -58 84 110 2) (65 30 94 100 2) (63 172 124 125 2)))
((eb maj7) ((-63 0 21 125 2) (62 10 157 103 2) (60 182 84 127 2)))
((eb maj7) ((58 5 141 100 2) (55 156 136 93 2)))
((eb maj7) ((-55 0 31 93 2) (53 15 115 89 2) (60 182 114 127 2)))
((g# maj7) ((-60 0 6 127 2) (55 0 156 92 2) (55 188 104 92 2)))
((g# maj7) ((-55 0 36 92 2) (53 36 162 74 2)))
((g# maj7) ((55 -78 187 97 2)))
((g# maj7) nil)
((d 7) nil)
((d 7) ((53 -99 110 88 2) (55 31 110 110 2) (58 161 131 118 2)))
((d 7) ((55 26 161 78 2) (56 161 115 113 2)))
((d 7) ((58 11 120 103 2) (59 167 115 123 2)))
((g 7) ((61 10 120 113 2) (62 167 130 106 2)))
((g 7) ((67 0 99 127 2) (71 151 94 105 2)))
((g 7) ((74 10 187 106 2) (72 15 126 117 2)))
((g 7) ((73 -89 219 92 2) (72 176 116 106 2)))
((c m7) ((-72 0 82 106 2) (67 145 73 127 2)))
((c m7) ((63 5 151 91 2) (60 141 104 125 2)))
((c m7) ((72 -15 146 117 2) (60 152 83 127 2)))
((c m7) ((62 -16 120 120 2) (63 130 104 105 2)))
((c 7) ((64 -21 162 110 2) (67 146 89 127 2)))
((c 7) ((68 -21 115 105 2) (70 136 120 103 2)))
((c 7) ((72 -10 145 108 2) (74 140 110 117 2)))
((c 7) ((75 -20 130 111 2) (77 131 161 105 2)))
((f m7) ((-77 0 10 105 2) (79 10 287 123 2)))
((f m7) ((-79 0 292 123 2)))
((f m7) ((-79 0 104 123 2) (78 177 120 123 2)))
((f m7) ((-78 0 62 123 2) (79 124 110 124 2)))
((bb 7) ((81 11 183 114 2) (82 172 68 91 2)))
((bb 7) ((80 6 146 110 2)))
((bb 7) ((79 -84 183 117 2) (77 114 156 93 2)))
((bb 7) ((75 26 176 118 2)))
((eb maj7) ((74 -31 192 88 2) (76 -26 37 89 2)))
((eb maj7) ((74 -62 182 110 2) (75 167 125 120 2)))
((eb maj7) ((-75 0 42 120 2) (77 83 183 105 2)))
((eb maj7) ((79 6 203 118 2)))
((g# maj7) ((72 -78 219 105 2) (72 188 109 103 2)))
((g# maj7) ((-72 0 53 103 2) (74 113 36 89 2) (75 151 141 108 2)))
((g# maj7) ((-75 0 37 108 2) (74 15 235 56 2)))
((g# maj7) ((72 -41 224 108 2) (75 167 131 110 2)))
((d 7) ((-75 0 291 110 2)))
((d 7) ((-75 0 291 110 2)))
((d 7) ((-75 0 146 110 2) (74 178 120 111 2) (75 183 15 68 2)))
((d 7) ((-74 0 52 111 2) (74 161 73 110 2) (72 161 78 105 2)))
((g 7) ((74 12 34 105 2) (75 30 240 105 2)))
((g 7) ((74 21 250 106 2)))
((g 7) ((72 26 266 92 2)))
((g 7) ((-72 0 47 92 2) (71 32 265 93 2)))
((c m7) ((72 21 271 83 2)))
((c m7) ((-72 0 297 83 2)))
((c m7) ((-72 0 296 83 2)))
((c m7) ((-72 0 298 83 2)))
((c m7) ((-72 0 36 83 2)))
((c m7) ((60 47 68 127 2)))
((c m7) ((63 47 104 121 2)))
((c m7) ((65 36 94 124 2) (67 130 162 127 2)))
((f m7) nil)
((f m7) nil)
((f m7) nil)
((f m7) ((60 32 78 127 2)))
((g 7) ((63 26 125 120 2)))
((g 7) ((65 11 88 124 2) (66 52 83 113 2)))
((g 7) ((65 16 136 127 2) (63 178 83 89 2)))
((g 7) ((65 16 136 123 2) (63 178 82 106 2)))
((c m7) ((65 20 277 117 2)))
((c m7) ((-65 0 15 117 2) (63 15 94 101 2) (66 167 124 123 2)))
((c m7) ((-66 0 11 123 2) (67 22 72 127 2)))
((c m7) ((60 -99 73 121 2) (63 26 250 108 2)))
((c m7) nil)
((c m7) ((60 32 104 127 2)))
((c m7) ((63 20 136 121 2)))
((c m7) ((66 11 98 124 2) (67 135 115 127 2)))
((f m7) nil)
((f m7) nil)
((f m7) ((60 194 46 127 2)))
((f m7) ((63 26 140 108 2)))
((g 7) ((60 -99 32 117 2) (66 21 172 124 2) (60 183 30 120 2)))
((g 7) ((65 5 162 124 2) (66 171 110 124 2)))
((g 7) ((65 11 130 114 2) (63 193 105 101 2)))
((g 7) ((-63 0 78 101 2) (60 26 78 127 2)))
((c m7) ((65 -99 88 113 2) (67 -67 82 127 2) (65 177 114 125 2)))
((c m7) ((63 32 136 85 2) (60 188 68 123 2)))
((c m7) ((58 11 135 111 2) (55 172 89 98 2)))
((c m7) ((63 10 146 118 2)))
((c m7) ((58 11 156 92 2) (60 182 109 127 2)))
((c m7) ((-60 0 48 127 2) (60 183 67 127 2)))
((c m7) ((63 10 152 98 2)))
((c m7) ((60 -99 36 124 2) (65 11 124 125 2) (67 109 183 127 2)))
((f m7) ((-67 0 10 127 2)))
((f m7) nil)
((f m7) nil)
((f m7) ((60 -99 68 127 2) (63 42 114 105 2)))
((g 7) ((66 16 166 127 2)))
((g 7) ((60 -94 46 124 2) (65 4 136 123 2) (67 166 73 127 2)))
((g 7) ((65 0 115 91 2) (63 152 140 81 2)))
((g 7) ((60 -5 115 127 2) (58 151 131 110 2)))
((c m7) ((55 0 78 55 2) (63 135 152 108 2)))
((c m7) ((60 -10 115 127 2) (61 -4 72 52 2) (58 141 125 110 2)))
((c m7) ((55 162 135 113 2)))
((c m7) ((-55 0 16 113 2) (56 141 42 68 2) (55 146 120 97 2)))
((c m7) ((57 10 109 101 2) (58 99 140 85 2) (57 187 104 95 2)))
((c m7) ((-57 0 22 95 2) (55 22 93 93 2)))
((c m7) ((57 -99 83 111 2) (58 -26 162 86 2) (57 84 109 78 2) (55 188 104 97 2)))
((c m7) ((-55 0 5 97 2) (57 26 114 114 2) (58 140 146 83 2)))
((f m7) ((57 -63 78 82 2) (55 41 109 68 2) (56 176 100 118 2)))
((f m7) ((58 -26 141 88 2) (56 74 124 86 2)))
((f m7) ((53 -99 88 85 2) (55 31 88 101 2) (56 115 161 95 2)))
((f m7) ((55 -73 115 81 2) (53 31 115 105 2)))
((g 7) ((55 -100 89 98 2) (56 -22 152 75 2) (55 78 104 83 2)))
((g 7) ((53 -94 115 92 2) (51 47 187 117 2)))
((g 7) ((53 -32 220 69 2)))
((g 7) ((51 -89 152 103 2) (50 135 99 106 2)))
((c m7) ((50 16 110 98 2) (51 100 130 97 2) (50 178 120 81 2)))
((c m7) ((-50 0 4 81 2) (48 -11 302 89 2)))
((c m7) ((-48 0 291 89 2)))
((c m7) ((-48 0 298 89 2)))
((c 7) ((-48 0 176 89 2) (50 166 110 100 2)))
((c 7) ((52 0 120 93 2) (53 135 162 101 2)))
((c 7) ((-53 0 26 101 2) (55 0 125 113 2) (56 125 109 111 2)))
((c 7) ((58 -26 119 101 2) (60 115 150 127 2)))
((f m7) ((62 -32 120 120 2) (63 88 141 93 2)))
((f m7) ((65 -63 151 120 2) (67 93 105 127 2)))
((f m7) ((62 -52 130 100 2) (65 78 136 117 2)))
((f m7) ((63 -68 141 88 2) (60 68 135 127 2)))
((bb 7) ((61 -89 167 120 2) (62 57 125 113 2)))
((bb 7) ((63 -93 141 105 2) (62 48 124 106 2) (61 172 115 108 2)))
((bb 7) ((62 -11 105 101 2) (61 67 110 88 2) (60 167 130 127 2)))
((bb 7) ((-60 0 31 127 2) (58 16 109 117 2) (56 120 120 92 2)))
((eb maj7) ((55 -68 364 68 2)))
((eb maj7) ((-55 0 292 68 2)))
((eb maj7) ((-55 0 151 68 2) (58 172 125 118 2)))
((eb maj7) ((-58 0 5 118 2) (62 11 130 111 2) (65 167 109 120 2)))
((g# maj7) ((63 11 57 118 2) (65 146 37 56 2) (63 168 119 91 2)))
((g# maj7) ((60 16 188 124 2)))
((g# maj7) nil)
((g# maj7) ((64 177 1 120 2) (65 177 1 120 2)))
((d 7) ((66 -98 208 125 2) (65 162 120 121 2)))
((d 7) ((63 4 131 88 2) (60 156 78 127 2)))
((d 7) ((66 5 183 120 2)))
((d 7) ((65 -15 187 106 2) (63 152 146 106 2)))
((g 7) ((-63 0 15 106 2) (61 15 131 83 2)))
((g 7) ((62 -100 141 91 2) (67 20 84 127 2) (71 150 84 110 2)))
((g 7) ((74 -4 141 91 2) (62 177 78 82 2)))
((g 7) ((74 10 162 111 2)))
((c m7) ((62 -100 48 93 2) (74 -11 161 106 2) (75 176 1 110 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
((c 7) nil)
((c 7) ((63 -78 88 118 2) (64 46 115 106 2)))
((c 7) ((66 -99 114 120 2) (67 42 78 127 2) (68 183 114 120 2)))
((c 7) ((-68 0 5 120 2) (70 21 135 106 2) (72 182 110 93 2)))
((f m7) ((74 26 120 120 2) (75 172 114 105 2)))
((f m7) ((77 6 140 110 2) (79 183 109 120 2)))
((f m7) ((-79 0 36 120 2) (75 178 119 125 2)))
((f m7) ((-75 0 5 125 2) (74 15 131 92 2) (71 157 130 97 2)))
((bb 7) ((67 16 74 127 2) (65 152 120 121 2)))
((bb 7) ((63 -20 135 92 2) (62 152 135 85 2)))
((bb 7) ((60 10 115 125 2) (62 177 120 118 2)))
((bb 7) ((-62 0 15 118 2) (63 11 93 89 2) (65 156 109 117 2)))
((eb maj7) ((67 11 89 127 2) (62 162 84 117 2)))
((eb maj7) ((63 -10 177 105 2) (60 178 72 123 2)))
((eb maj7) ((62 5 136 105 2) (58 172 94 77 2)))
((eb maj7) ((60 -11 136 127 2) (56 145 105 83 2)))
((g# maj7) ((58 0 156 114 2) (55 172 78 83 2)))
((g# maj7) ((58 -10 120 118 2) (56 146 146 93 2)))
((g# maj7) ((55 -31 146 97 2) (53 135 156 95 2)))
((g# maj7) ((51 -20 140 121 2) (50 146 146 95 2)))
((d 7) ((-50 0 292 95 2)))
((d 7) ((-50 0 292 95 2)))
((d 7) ((-50 0 182 95 2)))
((d 7) ((51 -83 83 98 2) (53 136 156 111 2)))
((g 7) ((-53 0 16 111 2) (55 16 229 105 2)))
((g 7) ((56 -47 161 103 2) (58 146 140 92 2)))
((g 7) ((59 36 203 120 2)))
((g 7) ((60 -41 202 127 2) (62 183 114 100 2)))
((c m7) ((-62 0 167 100 2) (63 140 152 98 2)))
((c m7) ((-63 0 140 98 2) (60 140 156 127 2)))
((c m7) ((-60 0 292 127 2)))
((c m7) ((-60 0 302 127 2)))
((c m7) ((-60 0 156 127 2)))
((c m7) ((55 -88 88 105 2) (60 42 98 127 2)))
((c m7) ((63 -99 73 114 2) (66 57 120 127 2)))
((c m7) ((67 -99 104 127 2) (66 46 152 120 2)))
((f m7) ((65 -56 234 117 2)))
((f m7) ((63 -84 94 111 2) (60 36 125 124 2)))
((f m7) ((56 -99 73 92 2) (55 27 119 113 2) (56 146 115 86 2)))
((f m7) ((55 -57 114 89 2) (53 52 115 89 2)))
((g 7) ((55 -78 104 105 2)))
((g 7) ((55 -89 115 117 2) (59 26 124 118 2) (62 172 93 105 2)))
((g 7) ((65 21 140 117 2)))
((g 7) ((55 -94 62 68 2) (64 26 130 101 2)))
((c m7) ((55 -100 79 91 2) (63 31 172 98 2)))
((c m7) ((55 -79 63 68 2) (65 15 130 123 2)))
((c m7) ((67 -99 62 127 2) (60 37 109 117 2) (65 177 120 123 2)))
((c m7) ((-65 0 26 123 2) (63 10 214 83 2)))
((c m7) ((55 187 89 101 2)))
((c m7) ((60 21 99 127 2) (63 156 89 100 2)))
((c m7) ((67 16 187 127 2) (55 16 88 105 2)))
((c m7) ((66 -99 135 114 2) (54 -73 197 86 2) (53 172 120 61 2) (65 187 105 103 2)))
((f m7) ((-53 0 82 61 2) (-65 0 98 103 2)))
((f m7) ((60 -94 94 127 2) (56 52 136 97 2) (53 188 94 92 2)))
((f m7) ((55 36 110 103 2) (56 140 131 89 2)))
((f m7) ((55 -57 88 83 2) (53 21 182 72 2)))
((g 7) ((55 -94 152 100 2)))
((g 7) ((55 -100 110 117 2) (59 26 130 114 2) (62 166 99 98 2)))
((g 7) ((65 31 152 123 2)))
((g 7) ((55 -98 104 85 2) (57 -93 57 74 2) (64 26 162 106 2) (55 178 94 77 2)))
((c m7) ((63 10 281 108 2)))
((c m7) ((-63 0 27 108 2) (55 5 120 92 2) (63 177 120 106 2)))
((c m7) ((-63 0 42 106 2)))
((c m7) nil)
((c m7) ((55 188 84 103 2)))
((c m7) ((57 -98 62 101 2) (60 26 104 127 2) (63 182 78 106 2)))
((c m7) ((67 42 140 127 2)))
((c m7) ((66 -63 105 123 2) (67 -6 141 127 2)))
((f m7) ((68 -96 63 123 2) (68 166 130 97 2)))
((f m7) ((-68 0 52 97 2)))
((f m7) nil)
((f m7) nil)
((g 7) ((67 164 43 127 2)))
((g 7) ((66 16 136 117 2) (67 152 72 127 2)))
((g 7) ((70 6 62 121 2) (70 120 36 105 2) (71 151 105 117 2)))
((g 7) ((67 -5 114 127 2) (65 146 151 117 2)))
((c m7) ((-65 0 109 117 2) (67 109 63 127 2) (65 188 104 121 2)))
((c m7) ((-65 0 10 121 2) (63 4 183 103 2) (66 156 120 121 2)))
((c m7) ((67 -15 312 127 2)))
((c m7) ((-67 0 297 127 2)))
((c m7) ((-67 0 63 127 2) (55 156 109 103 2)))
((c m7) ((60 5 99 127 2) (63 156 94 105 2)))
((c m7) ((55 10 130 108 2) (67 20 204 127 2)))
((c m7) ((66 -78 172 117 2) (54 -52 187 98 2) (53 172 120 47 2) (52 177 36 81 2) (65 177 115 114 2)))
((f m7) ((-53 0 156 47 2) (-65 0 177 114 2)))
((f m7) ((60 -84 115 127 2) (56 42 161 78 2)))
((f m7) nil)
((f m7) nil)
((g 7) ((55 194 72 113 2)))
((g 7) ((59 20 110 113 2) (62 182 83 95 2)))
((g 7) ((65 26 193 113 2)))
((g 7) ((55 -73 105 97 2) (64 36 168 111 2)))
((c m7) ((55 -78 78 86 2) (63 26 209 103 2)))
((c m7) ((65 -67 203 103 2) (63 142 150 101 2)))
((c m7) ((-63 0 204 101 2)))
((c m7) nil)
((c 7) ((60 151 68 125 2)))
((c 7) ((64 0 120 111 2) (67 161 57 127 2)))
((c 7) ((72 0 272 120 2)))
((c 7) ((69 -37 120 106 2) (70 67 183 110 2)))
((f m7) ((68 -5 224 114 2)))
((f m7) ((67 -26 177 127 2)))
((f m7) ((65 -78 260 86 2) (63 182 115 91 2)))
((f m7) ((-63 0 151 91 2) (62 182 110 98 2)))
((bb 7) ((-62 0 109 98 2) (65 166 114 121 2)))
((bb 7) ((68 -5 94 98 2) (70 141 104 111 2)))
((bb 7) ((71 -21 79 106 2) (73 88 110 114 2)))
((bb 7) ((71 -99 99 92 2) (70 15 115 108 2) (68 135 146 106 2)))
((eb maj7) ((67 11 151 127 2)))
((eb maj7) ((70 -72 161 121 2) (65 141 78 120 2)))
((eb maj7) ((67 -73 104 127 2) (63 89 150 103 2)))
((eb maj7) ((62 -16 219 105 2)))
((g# maj7) ((60 -84 182 125 2) (63 150 142 108 2)))
((g# maj7) ((-63 0 46 108 2) (67 108 136 127 2)))
((g# maj7) ((70 16 208 118 2)))
((g# maj7) ((68 -78 187 103 2) (67 141 130 127 2)))
((d 7) ((66 46 245 110 2)))
((d 7) ((-66 0 297 110 2)))
((d 7) ((-66 0 266 110 2)))
((d 7) ((65 11 93 117 2) (66 57 234 120 2)))
((g 7) ((-66 0 52 120 2) (65 48 250 111 2)))
((g 7) ((-65 0 291 111 2)))
((g 7) ((-65 0 255 111 2)))
((g 7) ((64 0 94 123 2) (65 58 234 100 2)))
((c m7) ((63 -6 230 105 2)))
((c m7) ((65 -79 255 113 2) (63 156 135 103 2)))
((c m7) ((-63 0 297 103 2)))
((c m7) ((-63 0 88 103 2)))
((c 7) nil)
((c 7) ((60 -89 83 127 2) (64 26 114 111 2) (67 176 63 127 2)))
((c 7) ((72 15 167 120 2) (60 182 114 125 2)))
((c 7) ((-60 0 16 125 2) (70 6 176 117 2) (60 156 68 124 2)))
((f m7) ((68 -16 125 111 2) (70 136 93 113 2)))
((f m7) ((68 -84 120 100 2) (67 20 146 127 2)))
((f m7) ((65 -67 228 108 2) (63 131 166 95 2)))
((f m7) ((-63 0 125 95 2) (62 162 135 74 2)))
((bb 7) ((-62 0 109 74 2) (65 161 115 120 2)))
((bb 7) ((68 -4 114 110 2) (70 152 94 113 2)))
((bb 7) ((71 -20 83 103 2) (73 89 99 108 2) (71 182 110 83 2)))
((bb 7) ((-71 0 10 83 2) (70 -10 109 105 2) (68 130 162 76 2)))
((eb maj7) ((-68 0 5 76 2) (67 5 130 127 2)))
((eb maj7) ((70 -100 182 120 2) (65 140 78 118 2)))
((eb maj7) ((67 -99 105 127 2) (63 68 156 105 2)))
((eb maj7) ((62 -32 198 114 2) (60 188 104 125 2)))
((g# maj7) ((-60 0 109 125 2) (63 161 135 108 2)))
((g# maj7) ((-63 0 26 108 2) (67 115 104 127 2)))
((g# maj7) ((70 0 157 118 2)))
((g# maj7) ((68 -99 183 110 2) (67 140 105 127 2)))
((d 7) ((66 16 270 125 2)))
((d 7) ((-66 0 10 125 2)))
((d 7) nil)
((d 7) ((65 0 68 124 2) (66 10 261 124 2)))
((g 7) ((65 16 276 121 2)))
((g 7) ((-65 0 291 121 2)))
((g 7) ((-65 0 255 121 2)))
((g 7) ((65 26 203 120 2)))
((c m7) ((63 -16 78 111 2) (65 62 110 106 2) (63 140 156 91 2)))
((c m7) ((-63 0 16 91 2) (60 37 255 127 2)))
((c m7) ((-60 0 166 127 2) (63 151 89 110 2)))
((c m7) ((67 -26 31 127 2) (72 67 105 111 2) (75 146 93 121 2)))
((c m7) ((79 -72 62 111 2) (84 -4 202 124 2)))
((c m7) nil)
((c m7) nil)
((c m7) nil)
))
