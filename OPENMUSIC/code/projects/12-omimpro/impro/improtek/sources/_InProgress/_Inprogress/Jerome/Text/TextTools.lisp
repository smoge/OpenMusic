(in-package :om)
;http://cl-cookbook.sourceforge.net/strings.html#find-el

;(defvar vow '(a e y u i o A E Y U I O))
;(defvar cons'(z r t p q s d f g h j k l m w x c v b n Z R T P Q S D F G H J K L M W X C V B N))

;(setf texttest "Elle, danse. Le lapin de ce quoi ? Chat, dans... une prairie? Oui, pour un. lapin     ")
;(setf texttest "Lapin des bois et des champs")
;(parsetext texttest)
;(ConvertToFrenchVowelsClasses texttest)


;(parsetext (concatenate 'string BonneDefunte "     "))
(defun parsetext (texte)
  (let ((i 0) (i_prec 0) (s 0) (s_prec 0) (text (concatenate 'string " " texte "      "))
        (result nil))  
    (loop while (< i (- (length text) 5)) do
          (progn
            (setf un (subseq text i (+ i 1))
                  deux (subseq text i (+ i 2))
                  trois (subseq text i (+ i 3))
                  quatre (subseq text i (+ i 4))
                  cinq (subseq text i (+ i 5)))
            
            (cond
             
             ((or 
               (equalp cinq " mes ")
               (equalp cinq " les ")
               (equalp cinq " tes ")
               (equalp cinq " ses ")
               (equalp cinq " des ")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 5))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list " ") (list (list "é" (subseq text s_prec i)))))))
             ((or 
               (equal quatre "Mes ")
               (equal quatre "Les ")
               (equal quatre "Tes ")
               (equal quatre "Ses ")
               (equal quatre "Des ")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 4))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list " ") (list (list "é" (subseq text s_prec i)))))))

             
             ((or 
               (equalp quatre " le ")
               (equalp quatre " te ")
               (equalp quatre " se ")
               (equalp quatre " de ")
               (equalp quatre " me ")
               (equalp quatre " ce ")
               (equalp quatre " ne ")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 3));;---------->(setf i (+ i 4))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list " ") (list (list "eu" (subseq text s_prec i)))))
                ))
             ((or 
               (equal trois "Je ")
               (equal trois "Le ")
               (equal trois "Te ")
               (equal trois "Se ")
               (equal trois "De ")
               (equal trois "Me ")
               (equal trois "Ce ")
               (equal trois "Ne ")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 2));;---------->(setf i (+ i 4))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "eu" (subseq text s_prec i)))))
                ))

             ((or 
               (equalp deux "e ")
               (equalp deux "e,")
               (equalp deux "e?")
               (equalp deux "e;")
               (equalp deux "e.")
               (equalp deux "e!")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 1));;;;!!!!!!!!!! ------->   (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "e-muet" (subseq text s_prec i)))))
                ))


             ;+ "e" muets ?
             ((or
               (equalp deux "qu")
               (equalp deux "qu"))
              (progn
                (setf i (+ i 2))
                (setf result (append result (list deux)))))
             
                   
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;--- AFFINES
             ((or
               (equalp trois "onn")
               (equalp trois "omm")
               (equalp trois "eau"))
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "o" (subseq text s_prec i)))))))


             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;ET LE E MUET ???
             ;--- A AFFINER 
             ((equalp trois "ine")
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "i" (subseq text s_prec i)))))))
             ;ET LA DIPHTONGUE DU I ???
             ((equalp trois "ien")
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "un" (subseq text s_prec i)))))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

             ((equalp deux "oi")
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "oi" (subseq text s_prec i)))))))

             
             ((or
               (equalp deux "an")
               (equalp deux "en"))
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "an" (subseq text s_prec i)))))))
             ((or
               (equalp trois "emp")
               (equalp trois "amp"))
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "an" (subseq text s_prec i)))))))
             
             
             ((equalp deux "on")
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "on" (subseq text s_prec i)))))))
             
             
             ((or 
               (equalp deux "ou")
               (equalp deux "où")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "ou" (subseq text s_prec i)))))))
             
             
             ((or
               (equalp deux "ez")
               (equalp deux "et"))
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "é" (subseq text s_prec i)))))))
             ((equalp un "é")
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "é" (subseq text s_prec i)))))))
             
             


             ((equalp cinq "aient")
              (progn
                (setf i_prec i)
                (setf i (+ i 5))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "è" (subseq text s_prec i)))))))
             ((or
               (equalp trois "ais")
               (equalp trois "ait")
               (equalp trois "err")
               (equalp trois "ett")
               (equalp trois "est")
               (equalp trois "eff")
               (equalp trois "ess")
               (equalp trois "ell")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "è" (subseq text s_prec i)))))))
             ((or
               (equalp deux "ai")
               (equalp deux "aî"))
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "è" (subseq text s_prec i)))))))
             ((or
               (equalp un "è")
               (equalp un "ê"))
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "è" (subseq text s_prec i)))))))
             
             
             ((equalp deux "ez")
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "é" (subseq text s_prec i)))))))
             ((equalp un "é")
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "é" (subseq text s_prec i)))))))
             
             
             ((equalp deux "eu")
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "eu" (subseq text s_prec i)))))))
             ((equalp un "e")
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "eu" (subseq text s_prec i)))))))
             
             
             
             
            
     
             ((or
               (equalp deux "un")
               (equalp deux "in")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "un" (subseq text s_prec i)))))))
             ((or
               (equalp trois "ein")
               (equalp trois "ain")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "un" (subseq text s_prec i)))))))

             ((equalp un "u")
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "u" (subseq text s_prec i)))))))
             ((equalp trois " eu")
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "u" (subseq text s_prec i)))))))
             ((equalp deux "ue")
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "u" (subseq text s_prec i)))))))
             
             
             ((equalp deux "au")
              (progn
                (setf i_prec i)
                (setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "o" (subseq text s_prec i)))))))
             ((equalp trois "eau")
              (progn
                (setf i_prec i)
                (setf i (+ i 3))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "o" (subseq text s_prec i)))))))
             ((or
               (equalp un "o")
               (equalp un "ô"))
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "o" (subseq text s_prec i)))))))
             
             
             ((or
               (equalp un "a")
               (equalp un "à")
               (equalp un "â")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "a" (subseq text s_prec i)))))))
             
             
             ((or
               (equalp un "i")
               (equalp un "y")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 1))
                (setf s_prec s)
                (setf s i)
                (setf result (append result (list (list "i" (subseq text s_prec i)))))))

             ((or
               (equalp un " ")
               (equalp un "-"))
              (progn
                (setf i (+ i 1))
                (setf s i)
                (setf result (append result (list un)));" "
                ))

             ;===============================
             ; PONCTUATIONS
             ;===============================
             ((equalp trois "...")
              (progn
                (setf i_prec i)
                (setf i (+ i 3));;;;;;;;;;;(setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                ;CLASSE
                ;(setf result (append result (list (list "." (subseq text s_prec i)))))
                ;ESPACE
                (setf result (append result (list (subseq text s_prec i))))
                ))
             ((or
               (equalp un ".");(equalp deux ". ")
               (equalp un ",");(equalp deux ", ")
               (equalp un "!");(equalp deux "! ")
               (equalp un "?");(equalp deux "? ")
               (equalp un ";");(equalp deux "; ")
               )
              (progn
                (setf i_prec i)
                (setf i (+ i 1));;;;;;;;;;;(setf i (+ i 2))
                (setf s_prec s)
                (setf s i)
                ;CLASSE
                ;(setf result (append result (list (list "." (subseq text s_prec i)))))
                ;ESPACE
                (setf result (append result (list un)))
                ))


           
             (t
              (progn
                (setf i (+ i 1))
                (setf result (append result (list un)))))

             ;((equalp un " ")
             ; (progn
             ;   (setf i (+ i 1))
             ;   (list "ESPACE")))
             
             ;(t (setf i (+ i 1)))
             
             )))
    result))


;(ConvertToFrenchVowelsClasses BonneDefunte)
(defun ConvertToFrenchVowelsClasses (text)
  (let ((parsed (parsetext text)) 
        (RencontreEspaceDepuisDerniereSyllabe t)
        (lastsyllabe nil)
        (syllabecount 0)
        (concat nil)
        (result '()))

    (loop for e in parsed do
       
          (cond 
           
           ((and 
             (equalp (type-of e) 'cons)
             (= 2 (list-length e)))
            (progn
              (if (and lastsyllabe (not RencontreEspaceDepuisDerniereSyllabe)) 
                  (setf result (append result (list (list (nth 0 lastsyllabe) (nth 1 lastsyllabe) syllabecount)))))
              (setf lastsyllabe e)
              (incf syllabecount)
              (setf RencontreEspaceDepuisDerniereSyllabe nil concat nil)))
           

           ((or (equalp "«" e) (equalp "»" e) (equalp " " e) (equalp "." e) (equalp "," e) (equalp "." e) (equalp "?" e) (equalp "!" e) (equalp "
" e) (equalp "..." e))
            (progn
              (setf RencontreEspaceDepuisDerniereSyllabe t)
              (if lastsyllabe 
                  (setf result (append result (list (list (nth 0 lastsyllabe) (concatenate 'string (nth 1 lastsyllabe) concat e) syllabecount)))
                        concat nil
                        syllabecount 0
                        lastsyllabe nil)
                )))

           ((not RencontreEspaceDepuisDerniereSyllabe) 
            (setf concat (concatenate 'string concat e))) 
           ))result))

