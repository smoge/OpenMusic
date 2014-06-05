;TestTR2.lisp
;J.Nika

;DEPLACE DANS TUNE.LISP

(in-package :om)

(defmethod! generate-grid-param ((self tune) beatdur)
            
  ;from "generate-grid"
            (let* ((harmgrid (simplegrid self))
                   (gridlength (list-length harmgrid))
                   (beatlist (loop for x in harmgrid collect (make-instance 'beat :HarmLabel x)))
                   (beatduration (beatduration self))
                   (absolute-path-filename (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "realtime"))
                                                          :name (format nil "~a-grid-param.txt" (tunename self))))
                   (suffixes "")
                   (midicodes1 "")
                   (midicodes2 "")
                   (suffix nil)
                   (midicode nil)
                   (j 0))
              
  ;from "save-for-antescofo2"
              (with-open-file 
                  (ss absolute-path-filename 
                      :direction :output :if-exists :supersede)
                (setf beatlist (transfer-syncopated-event beatlist beatduration))
                (setf beatlist (add-grid-to-beatlist beatlist beatduration))
                
                (loop with c-beatlist = (mapcar #'clone-object beatlist)
                      for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
                      do (progn 
                           (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second)) 
                           (setf j 0)
                           (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                                 do 
                                 (progn 
                                   (incf j)
                                   (setf midicode (format nil "(~d, \"~a ~a 0.0.~a ~a\")" 
                                                         i 
                                                         (MEPitch event)     
                                                         (MEVel event) 
                                                         (round (* (/ (MEDur event) beatduration) 480))
                                                         (MEChannel event)))
                                   (if (= j 1)
                                       (setf midicodes1 (concatenate 'string midicodes1 midicode))
                                     (if (= j 2)
                                       (setf midicodes2 (concatenate 'string midicodes2 midicode))
                                     ))))
                                 (setf suffix (format nil "(~d,\"\'~a\")" i harmgrid))
                                 (setf suffixes (concatenate 'string suffixes suffix))
                                 ; JEROME 01/09/13
                                 ; POUR POUVOIR FAIRE IMPROS SUR PLUSIEURS GRILLES DE SUITES : CIRCULAIRE !
                                 ;(pop harmgrid)
                                 ;(if harmgrid (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                                 (setf harmgrid (rotate harmgrid))
                                 (if nextbeat (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                      ;(format ss "EVENT 1.0~%")
                      ;(format ss "group {~%")
                      (format ss "$suffixes := MAP{")
                      ;QUOTE AU DEBUT ??
                      (format ss "~a}~%~%" suffixes)
                      (format ss "$midicodes1 := MAP{")
                      (format ss "~a}~%~%" midicodes1)
                      (format ss "$midicodes2 := MAP{")
                      (format ss "~a}~%~%" midicodes2)
                      (format ss "$gridlength := ~d~%" gridlength)
                      ;(format ss "}~%")
                      )))
            
#|
(setf tune Jaime_tune)
(generate-grid-param tune (beatduration tune))
|#

(defmethod string-group-for-antescofo ((self beat) (beatduration integer) (numAntescofo integer))
  (let ((c-beat (clone-object self))
        (midicode nil)
        (midicodes ""))
    (setf (MidiSet c-beat) (sort (MidiSet c-beat) #'<= :key #'second)) 
    
    (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet c-beat)) for event in (MidiSet c-beat)
          do 
          (progn 
            ; 06/08/13 : RAJOUTÉ "@local" pour synchronisation évènements arrivés potentiellement trop tard
            ; (cf Delainegatif.asco.txt)
            (setf midicode (format nil "    ~a   mnote~D ~a ~a \"0.0.~a\" ~a @local~%" 
                                   (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))
                                   numAntescofo
                                   (MEPitch event)     
                                   (MEVel event) 
                                   (round (* (/ (MEDur event) beatduration) 480))
                                   (MEChannel event)))
            (setf midicodes (concatenate 'string midicodes midicode))))
    (format nil "group{~%~a}" midicodes)))

#|

; A METTRE ENSUITE DANS "Tune.lisp"
;=========================================
;(( UTILITAIRES POUR UTILISER PLUS FACILEMENT AVEC ORACLETABLE DE LA CLASSE TUNE))      
;(((mis ici car classe 'Tune' devait déjà être définie)))
(defmethod launch-realtime-on-oraclechan ((self tune) (numoraclechan integer) (host_send t) (port_send_ante integer) (numAntescofo integer))
  (launch-realtime (gethash numoraclechan (oracletable *current-tune*))
                                 (beatduration *current-tune*)
                                 host_send
                                 port_send_ante
                                 numAntescofo))
(defmethod kill-realtime-on-oraclechan ((self tune) (numoraclechan integer))
  (kill-realtime (gethash numoraclechan (oracletable *current-tune*))))

(defmethod reset-realtime-oraclechan ((self tune) (numoraclechan integer) (host_send t) (port_send_ante integer) (numAntescofo integer)) 
  (kill-realtime-on-oraclechan self numoraclechan)
  (setf (gethash numoraclechan (oracletable self)) (NewRealTimeImprovizer))
  ;Jerome 29/04/2013
  ;The oracle has been modified. The associated impro in improtable has to be deleted because it is not up to date.
  ; 2 lignes suivantes non pertinentes en TR
  ;-----------------------------------------
  ;(remhash numoraclechan (improtable self))
  ;(when (member numoraclechan '(3 4 5)) (setf (max-continuity (gethash numoraclechan (oracletable self))) 1000))
  (launch-realtime-on-oraclechan self numoraclechan host_send port_send_ante numAntescofo)
  (gethash numoraclechan (oracletable self)))

|#
