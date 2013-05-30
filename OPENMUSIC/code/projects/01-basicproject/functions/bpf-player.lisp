
(in-package :om)

(defclass! bpf-control (simple-container BPF) 
   ((c-action :initform nil :accessor c-action :initarg :c-action)
    (faust-control :initform nil :accessor faust-control :initarg :faust-control :documentation "A list of a Faust Effect (or Synth) and a name of a parameter (e.g : (<faust-synth-console> \"freq\"))")))

(defmethod make-one-instance ((self bpf-control) &rest slots-vals)
  (let ((bpf (call-next-method)))
    (setf (c-action bpf) (nth 3 slots-vals))
    (setf (faust-control bpf) (nth 4 slots-vals))
    bpf))

(defmethod omng-copy ((self bpf-control))
  (let ((bpf (eval (call-next-method))))
    (setf (c-action bpf) (c-action self))
    bpf))

(defmethod print-object ((self bpf-control) stream)
  (call-next-method))
;  (format stream "BPF-CONTROL: ~A ~D" (c-action self) (slot-value self 'offset)))

(defmethod play-obj? ((self bpf-control)) t)
(defmethod allowed-in-maq-p ((self bpf-control)) t)
(defmethod get-obj-dur ((self bpf-control)) (last-elem (x-points self)))
;(defclass bpf-player (omplayer) ())
;(defmethod class-from-player-type ((type (eql :bpfplayer))) 'bpf-player)

(defmethod prepare-to-play ((self (eql :bpfplayer)) (player omplayer) (object bpf-control) at interval)
  ;(player-unschedule-all self)
  (let ((faustfun (if (faust-control object)
                      (get-function-from-faust-control (faust-control object)))))
    (print faustfun)
    (when (or (c-action object) (faust-control object))
      (if interval
          (mapcar #'(lambda (point) 
                      (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                          (progn
                            (if (c-action object) 
                                (schedule-task player 
                                               #'(lambda () (funcall (c-action object) (cadr point))) 
                                               (+ at (car point))))
                            (if (faust-control object) 
                                (schedule-task player
                                               #'(lambda () (funcall faustfun (cadr point))) 
                                               (+ at (car point)))))))
                  (point-pairs object))
        (mapcar #'(lambda (point)
                    (if (c-action object)
                        (schedule-task player 
                                       #'(lambda () (funcall (c-action object) (cadr point))) 
                                       (+ at (car point))))
                    (if (faust-control object) 
                        (schedule-task player
                                       #'(lambda () (funcall faustfun (cadr point))) 
                                       (+ at (car point)))))
                (point-pairs object))))))

;(defmethod player-stop ((player bpf-player) &optional object)
;  (call-next-method)
;  (player-unschedule-all player))

(defmethod default-edition-params ((self bpf-control)) 
  (pairlis '(player) '(:bpfplayer) (call-next-method)))


;;;=======================================================

(defmethod get-editor-class ((self bpf-control)) 'bpfcontroleditor)

(defclass bpfcontroleditor (bpfeditor play-editor-mixin) ())
;;(defmethod get-score-player ((self bpfcontroleditor)) :bpfplayer)
;(defmethod get-edit-param ((self bpfcontroleditor) (param (eql 'player))) :bpfplayer)
(defmethod cursor-panes ((self bpfcontroleditor)) (list (panel self)))

(defclass bpfcontrolpanel (bpfpanel cursor-play-view-mixin) ())
(defmethod view-turn-pages-p ((self bpfcontrolpanel)) nil)
(defmethod get-panel-class ((Self bpfcontroleditor)) 'bpfcontrolpanel)

(defmethod time2pixel ((self bpfcontrolpanel) time)
  (call-next-method self (* time (expt 10 (decimals (object (editor self)))))))

(defmethod handle-key-event ((Self bpfcontrolpanel) Char)
  (cond ((equal char #\SPACE) (editor-play/stop (editor self)))
        (t (call-next-method))))

(defun get-function-from-faust-control (faust-control)
  (print 123)
  (let* ((name (cadr faust-control))
         (console (car faust-control))
         (ptr (if (typep console 'faust-effect-console) (effect-ptr console) (synth-ptr console)))
         (maxnum (las-faust-get-control-count ptr))
         found)
    (loop for i from 0 to (- maxnum 1) do
          (if (string= name (car (las-faust-get-control-params ptr i)))
              (setf found i)))
    #'(lambda (val) (las-faust-set-control-value ptr found (float val)))))
