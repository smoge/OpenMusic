(in-package :om)

;===============
; SEND
;===============

;MidiHarmBeat
;--------------

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent midiharmbeat) startidxinimpro hostsend portsend adresssend numVoice)
  sequence)

(defmethod write-midiharmbeat-as-group-for-antescofo ((self midiharmbeat) (beatduration integer) (numAntescofo integer))
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


;AudioHarmBeat & AudioDescrBeat
;------------------------------

(defmethod write-as-antescofo-tab ((ev event)) "")

(defmethod write-impro-as-map-for-antescofo ((impro list) (startidxinimpro integer) )
  (let (( s "MAP {") (i 0))
    (loop for el in impro do
          (progn
            (setf s 
                  (concatenate 'string s 
                               (format nil "(~a, ~a)" (+ i startidxinimpro) (write-as-antescofo-tab el)))) 
            (setf i (+ i 1))
            (if (nth i impro)
                (setf s (concatenate 'string s (format nil ", ") )) 
              (setf s (concatenate 'string s (format nil " }") )) )
            ))s))

;AudioHarmBeat
;-------------

(defmethod write-as-antescofo-tab ((ev AudioHarmBeat)) (format nil "TAB [~a, ~a]" (IdxInBuffer ev) (CurrentTransfo ev)))

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent audioharmbeat) startidxinimpro hostsend portsend adresssend numVoice)
  (let ((s (write-impro-as-map-for-antescofo sequence startidxinimpro)))
    (osc-send (list adresssend numvoice s) hostsend portsend)))
;(osc-send (list (format nil "~a" numVoice) s) "127.0.0.1" 7657)



;AudioDescrBeat
;-------------
(defmethod write-as-antescofo-tab ((ev AudioDescrBeat)) (format nil "TAB [~a, ~a]" (IdxInBuffer (data ev)) (CurrentTransfo (data ev))))

#|
(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent AudioDescrBeat) startidxinimpro hostsend portsend adresssend numVoice)
  (let ((s (write-impro-as-map-for-antescofo sequence startidxinimpro)))
    (osc-send (list adresssend numvoice s) hostsend portsend)))
|#

(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent AudioDescrBeat) startidxinimpro hostsend portsend adresssend numVoice)
  (let ((pos startidxinimpro) (seq sequence) (sizemess 40)
        (fragseq nil) (s nil) (i 0) (j 0))
    (loop while seq do
          (progn
            (setf i 0)
            (setf fragseq (loop while (and seq (< (incf i) (+ sizemess 1))) collect (pop seq)))
            (setf s (write-impro-as-map-for-antescofo fragseq pos))
            (osc-send (list adresssend numvoice s) hostsend portsend)
            (incf j)
            ;(if *print-navig-basics* (format *om-stream* "~%Message part ~a : idx=~a / sent=~a~%" j pos s))
            (setf pos (+ pos (- i 1)))
            (if seq (sleep 0.051)))
          )
    ;(if *print-navig-basics* (format *om-stream* "Message sent in ~a part~%----------~%~%" j))
    ))

#|
  (let ((pos 10) (seq '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)) (sizemess 4)
        (fragseq nil) (s nil) (i 0))
    (loop while seq do
          (progn
            (setf i 0)
            (setf fragseq (loop while (and seq (< (incf i) (+ sizemess 1))) collect (pop seq)))
            (format *om-stream* "fragseq = ~a, posdebut = ~a~%" fragseq pos)
            (setf pos (+ pos (- i 1))))))
|#




;===============
; WRITE
;===============

(defmethod save-impro ((self list) (name t))
  (WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
  t)

(defmethod load-impro ((name t))
  (WITH-OPEN-FILE (in name :direction :input  );:if-does-not-exist) ;:nil)
    (eval (read in)) (eval (read in))))

(defun osc-send-saved-impro (path_impro numVoice shift)
  (let ((impro (load-impro path_impro)))
       (loop for j from 0 to (- (list-length impro) 1) do
             
             (osc-send-sequence-fragment (list (nth j impro)) (+ j shift) "127.0.0.1" 7657 "/modify" numVoice)
             (sleep 0.1)
             ;(format *om-stream* "Pos impro ~a : Idx ~a ; Label ~a ; RMStransf ~a ~%" j 
             ;        (IdxInBuffer (nth j impro))
             ;        (Label (nth j impro))
             ;        (TranspoRMSratio (nth j impro))
             ;        )
             )))









