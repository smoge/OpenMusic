;Antescofo.lisp
;by Marc Chemillier 2011 (additional code by J. Nika)

(in-package :om)

;----------------------------------------------------------------
;IMPROVISING WITH 'ANTESCOFO' IN MAX


(defun save-for-antescofo (beatlist beatduration &optional absolute-path-filename)  ;&optional slower-ratio)   
  (when (null absolute-path-filename)                                               ; slower-ratio = 2 -> beat = half notes instead of quarter notes
    (setf absolute-path-filename path_bufferAntescofo))

  (ensure-directories-exist absolute-path-filename)
  (with-open-file 
      (ss absolute-path-filename 
          :direction :output :if-exists :supersede)
    ;(format ss "BPM ~a~%" (float (/ 60000 beatduration)))
    ;Since tempo may change during impro, it should not be included in the impro files
    ;(but adapted during performance by just modifying the tempo value of Antescofo through the [tempo $1] message using a float in BPM
    (setf beatlist (transfer-syncopated-event beatlist beatduration))   ;MARC 2011/6/27
    (setf beatlist (add-grid-to-beatlist beatlist beatduration))
    (loop with c-beatlist = (mapcar #'clone-object beatlist)
          for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
          do (progn 
               (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second))   ;BUG 2011/6/14 'sort' is needed for deltatimes
               ;(if (or (null slower-ratio) (= (mod i slower-ratio) 1)) 
               (format ss "NOTE  60 1.0 beat~a~%" i)     ;'beat' is used as a cue to allow the placement at the current beat when loading a file in Antescofo 
               (format ss "	GFWD for-killall {~%")  ;Marc 1/3/2012 "killall" need GFWD in Antescofo
               ;  (format ss "NOTE  0 1.0~%"))    ;a silence is not an event (= not activated by 'nextevent' in Antescofo)
               (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                     do (format ss "    ~a   mnote ~a ~a \"0.0.~a\" ~a~%" 
                                (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))   ; deltatime in floats
                                (MEPitch event)     
                                (MEVel event) 
                                (round (* (/ (MEDur event) beatduration) 480))   ; durations in ticks (= 1/480th of a beat)
                                (MEChannel event)))
               (format ss "    }~%")
))))

;If no file name is given, sequence is saved in:
;"/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/buffer-for-antescofo.txt"


;MARC 15/11/2011
; writes with "mnote2" instead of "mnote" -> for the second Antescofo with fixed tempo (accompaniment)

(defun save-for-antescofo2 (beatlist beatduration &optional absolute-path-filename)    ;;; ATTENTION = save-for-antescofo2
  (when (null absolute-path-filename)                                              
    (setf absolute-path-filename path_bufferAntescofo))

  (ensure-directories-exist absolute-path-filename)
  (with-open-file 
      (ss absolute-path-filename 
          :direction :output :if-exists :supersede)
    (setf beatlist (transfer-syncopated-event beatlist beatduration))
    (setf beatlist (add-grid-to-beatlist beatlist beatduration))
    (loop with c-beatlist = (mapcar #'clone-object beatlist)
          for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
          do (progn 
               (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second)) 
               (format ss "NOTE  60 1.0 beat~a~%" i) 
               (format ss "	GFWD for-killall {~%")
               (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                     do (format ss "    ~a   mnote2 ~a ~a \"0.0.~a\" ~a~%"        ;;;;;; MNOTE2 instead of MNOTE  (for a second Antescofo devoted to comping)
                                (float (/ (- (MEOnset event) (MEOnset previousevent)) beatduration))   ; deltatime in floats
                                (MEPitch event)     
                                (MEVel event) 
                                (round (* (/ (MEDur event) beatduration) 480))   ; durations in ticks (= 1/480th of a beat)
                                (MEChannel event)))
               (format ss "    }~%")
))))



(defun add-grid-to-beatlist (beatlist beatduration)   
  (loop for beat in beatlist for c-beat = (clone-object beat)
        do (setf (MidiSet c-beat) (append (make-grid-event-for-beat c-beat beatduration) (MidiSet c-beat))) 
        collect c-beat))

(defun transfer-syncopated-event (beatlist beatduration)
  (loop with c-beatlist = (mapcar #'clone-object beatlist)
        for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil))
        do (let ((transferredevent (if (null nextbeat) nil
                                     (loop for event in (MidiSet nextbeat)      ; syncopated events must be transferred to the previous beat
                                           when (< (MEOnset event) 0) do (setf (MidiSet nextbeat) ;BUG 2011/5/31 delete does NOT make physical modif. here!
                                                                               (delete event (MidiSet nextbeat)))
                                           and collect (progn (setf (MEOnset event) (+ (MEOnset event) beatduration)) event)))))
             (setf (MidiSet beat) (append (MidiSet beat) transferredevent)))
        finally return (progn (loop for evt in (MidiSet (first c-beatlist)) do (setf (MEOnset evt) (max (MEOnset evt) 0)))
                         c-beatlist)))

;WARNING: 'beats' must have the same duration (whereas it might not be the case in all examples, see ImprotekTutorial)

;(save-for-antescofo (thread-Beats beats12) 682)

; DELTATIME IN MAX: (http://www.cycling74.com/docs/max5/vignettes/core/maxtime_syntax.html)
;    - makenote object: duration can be a "Tempo-Relative Time Value" represented in bars/beats/units as a single value, 
;      with three integers separated by periods, for example 2.3.240, which is 2 measures (bars), 3 beats, and 240 ticks 
;      (ticks represent 1/480th of a quarter note).
; DELTATIME IN 'ANTESCOFO': 
;    - deltatimes between 'events' are represented as floats indicating the number of beats or a fraction of a beat.
;    - 'events' of the "score language" (i.e. NOTE 60 1.0.) are passed each time the space key is pressed in Max.
;    - 'actions' activated by the 'events' are notes to be played, which are coded by a deltime and a "send message" received by a Max r-object "mnote", 
;      which activates a makenote object by sending it pitch velocity duration (tempo-relative time) and channel.
;    The initial tempo is given by an event named BPM. In Max, user activating each event (for instance by tapping the space key) must fit the initial tempo value.
;    Then one can modify the tempo provided variations are done smoothly.





;;;;; --------------------------------------
;;;;; --------------------------------------
;PRECEDEMMENT DANS ANTESCOFOTR.LISP ?????????
;;;;; --------------------------------------
;;;;; --------------------------------------

; NEW 20/05/14
;--------------

(defmethod send-beatlist-as-string-group-for-antescofo ((list-of-beats list) (beatduration integer) (beatIdxInImpro integer) (hostsend t) (portsend integer) (adresssend simple-base-string) (numAntescofo integer))
  
  (let ((beatIdx beatIdxInImpro))
    (if (and (boundp '*server*) (not (null *server*)))
         ; Traduction en group antescofo et envoi
        (progn
          (loop for beat in list-of-beats do 
                (progn
                  (setf beatforAntescofo (beat-as-string-group-for-antescofo beat beatduration numAntescofo)); "string-group...." = celui dans "AntescofoTR.lisp" ???
                ;(osc-send (list "/OM/improbeats" beatIdx beatforAntescofo) hostsend portsend)
                  (osc-send (list adresssend beatIdx beatforAntescofo) hostsend portsend)
            ;(format *om-stream* "~a~%" (list "/OM/improbeats" beatIdx beatforAntescofo))
                  (incf beatIdx)))
          (format *om-stream* "Generated from beat ~D to ~D ~%" beatIdxInImpro beatIdx))
      (format *om-stream* "Impossible to send the groups strings : NO SERVER OPEN !~%"))))







(defmethod beat-as-string-group-for-antescofo ((self beat) (beatduration integer) (numAntescofo integer))
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