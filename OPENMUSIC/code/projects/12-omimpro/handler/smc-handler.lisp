(in-package :om)

(defclass impro-handler ()
  ((name :initform "Improvizer-Handler" :accessor name :initarg :name :type string)
   ;;;Improvizer
   (rtimprovizer :initform nil :accessor rtimprovizer :initarg :rtimprovizer)
   (scenario :initform nil :accessor scenario :initarg :scenario :type list)
   (expanded-scenario :initform nil :accessor expanded-scenario :initarg :expanded-scenario :type list)
   ;(db-path :initform nil :accessor db-path :initarg :db-path :type (or null string))
   ;;;Slice Data
   (slice-list :initform nil :accessor slice-list :type list)
   ;(slice-index :initform 0.0 :accessor slice-index :type single-sloat)
   (perf-time :initform 0 :accessor perf-time :type integer)
   (empty-pos :initform 0 :accessor empty-pos :type integer)
   ;;;Handler
   (output-slice-fun :initform nil :accessor output-slice-fun :initarg :output-slice-fun :type (or null function))
   (epsilon :initform 3 :accessor epsilon :initarg :epsilon :type integer)
   (play-pos :initform 0 :accessor play-pos :type integer)
   (queries :initform '() :accessor queries :initarg :queries :type list)
   (waiting-processes :initform '() :accessor waiting-processes :type list))
  (:documentation "
A handler for Improtek (Copyright 2013 (C) J.Nika).
This object can automate improvization generation based on the rtimprovizer class from Improtek."))

;;;Build an Improvizer Handler from a scenario and a database
(defun build-impro-handler (&key name scenario rtimprovizer epsilon output-fun expanded-scenario)
  (let ((handler (make-instance 'impro-handler 
                                :name (or name "Improvizer-Handler")
                                :scenario scenario
                                :expanded-scenario expanded-scenario;(expand_grid scenario)
                                :rtimprovizer (or rtimprovizer (NewRealtimeImprovizer))
                                :epsilon (or epsilon 3)
                                :output-slice-fun output-fun))) 
    (setf (gen-callback (rtimprovizer handler))
          #'(lambda (val) 
              (loop for proc in (waiting-processes handler) do
                    (mp:process-poke proc))
              ))
    handler))

(defmethod slice-max-pos ((self impro-handler))
  (length (expanded-scenario self)))

;;;Run the first generation step
(defmethod init-impro-handler ((self impro-handler) &optional (start-pos 0))
  (loop while (<= (empty-pos self) start-pos) do
        (proceed-impro-handler self start-pos)))

;;;Run one generation step
(defmethod proceed-impro-handler ((self impro-handler) gen-start)
  (let* ((scenario-suffix (nthcdr gen-start (expanded-scenario self)))
         result-slice-list
         result-length 
         output-list
         (i -1)) ;(print (formatlabellist scenario-suffix))
    ;;;When improvization is not over
    (when (< gen-start (slice-max-pos self))
      ;;;Run improvization as far as possible
      (setq result-slice-list (improvize_onephase (rtimprovizer self)
                                                  (length scenario-suffix)
                                                  scenario-suffix
                                                  gen-start)
            result-length (length result-slice-list))
      ;;;Add the generated slice list to the handler slice-list, set the new empty position and the next generation index
      (setf (slice-list self) (append (nthcar gen-start (slice-list self)) result-slice-list)
            (empty-pos self) (length (slice-list self))
            ;(slice-index self) (- (empty-pos self) result-length) ;slice-index result-length))
            ) 
      ;;;When the generation gave a non-null result
      (when result-slice-list
        (setq output-list result-slice-list
              ;(loop for slice in result-slice-list collect
              ;      (funcall (output-slice-fun self)
              ;               slice 
              ;               (+ gen-start (incf i)) 
              ;               (reduce #'+ (nthcar (+ gen-start i) (slice-list self)) :key #'duration)
              ;               ))
              )
        ))
    output-list))

(defmethod! modify-scenario ((self impro-handler) new-fragment start-index)
  (let ((new-scenario (scenario self)))
    (setf (nthcdr start-index new-scenario) (new-fragment))
    (setf (scenario self) new-scenario)))

(defmethod force-proceed-impro-handler ((self impro-handler))
  (proceed-impro-handler self (setf (slice-index self) (+ (perf-time self) (epsilon self)))))


(defmacro get-impro-slot-callback (slot)
  (case slot 
    ('perf-time
     `#'(lambda (self)
          (when (and (< (- (empty-pos self) new-val) (epsilon self))
                     (< (empty-pos self) (slice-max-pos self)))
            (setf (slice-index self) (empty-pos self))
            ;(proceed-impro-handler self)
            )))
    ('scenario
     `#'(lambda (self)
          (let* ((new-expanded-scenario (expand_grid new-val))
                 (pos (perf-time self))
                 (switch-pos (position nil (mapcar 'equal (nthcdr pos (expanded-scenario self)) (nthcdr pos new-expanded-scenario)))))
            (when switch-pos
              (incf switch-pos pos)
              (setf (slot-value handler 'expanded-scenario) new-expanded-scenario)
              (setf (slot-value handler 'slice-index) switch-pos)
              (setf (slot-value handler 'slice-max-pos) (length new-expanded-scenario)))
              ;(proceed-impro-handler self)
            )))
    (otherwise #'(lambda (self)))))


;;;Macros to enable and disable slot reactivity
(defmethod set-improvizer-param ((self impro-handler) (slots list) (new-vals list) &optional gen-start)
  (query-push (query-alloc :inputs slots
                           :vals new-vals
                           :handler self
                           :gen-start (or gen-start (perf-time self)))))

#|
;;;a mettre dans l'output
 ;;;Stretch the result beat list to the desired tempo
      (loop for st in result-slice-list do
            (setf (MidiSet st) (timestretch (MidiSet st) (/ sdur (duration st)))
                  (duration st) sdur))
`
(defmacro enable-slot-reactivity (slot callback)
  `(defmethod (set-param ,slot) (new-val (self impro-handler))
     (setf (slot-value (rtimprovizer self) ',slot) new-val) ;;;MARCHE QUE POUR LE HANDLER
     (funcall ,callback self)
     new-val))

(defmacro disable-slot-reactivity (slot class)
  `(defmethod (setf ,slot) (new-val (self ,class))
     (setf (slot-value self ',slot) new-val)))

;;;scheduling midi
(when result-slice-list
        ;;;Turn the result beat list into a scheduling list
        (when (setq result-schedlist (midi->schedlist (beats->midi result-slice-list sdur 0 (* slice-index sdur)) self))
          ;;;Set the player-scheduler queue accrding to new values
          (mp:with-lock ((om-get-scheduler-lock (player-scheduler self)))
            ;;;If the player scheduler has a non-null queue, cut the non-processed queue at the right place, keeping only the note-off midi events, and mix with the new queue + sort.
            (if (setq queue (om-get-scheduler-queue (player-scheduler self)))
                (let* ((indx (or (position (caar result-schedlist) queue :test '<= :key 'car) (length queue)))
                       (queue-prefix (nthcar indx queue))
                       (queue-suffix (nthcdr indx queue))
                       evt notes-off)
                  (loop for elem in queue-suffix do
                        (setq evt (caar (last elem)))
                        (if (and (eq (type-of evt) 'om-midi::midi-evt) (eq (om-midi:midi-evt-type evt) :keyoff))
                            (push elem notes-off)))
                  (setq result-schedlist (sort (append result-schedlist notes-off) '< :key #'car))
                  (om-set-scheduler-queue (player-scheduler self) (append (nthcar indx queue) result-schedlist)))
              ;;;If the player scheduler has no queue, set it to the result queue
              (om-set-scheduler-queue (player-scheduler self) result-schedlist)))))


|#