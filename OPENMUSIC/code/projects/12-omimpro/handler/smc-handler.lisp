(in-package :om)

(defclass impro-handler ()
  ((name :initform "Improvizer-Handler" :accessor name :initarg :name :type string)
   ;;;Improvizer
   (rtimprovizer :initform nil :accessor rtimprovizer)
   (scenario :initform nil :accessor scenario :initarg :scenario :type list)
   (expanded-scenario :initform nil :accessor expanded-scenario :type list)
   (db-path :initform nil :accessor db-path :initarg :db-path :type (or null string))
   ;;;Slice Data
   (slice-list :initform nil :accessor slice-list :type list)
   ;(slice-index :initform 0.0 :accessor slice-index :type single-sloat)
   (slice-max-pos :initform 0 :accessor slice-max-pos :type integer)
   (slice-pos :initform 0 :accessor slice-pos :type integer)
   (slice-date :initform 0 :accessor slice-date :type integer)
   (empty-pos :initform 0 :accessor empty-pos :type integer)
   ;;;Handler
   (output-slice-fun :initform nil :accessor output-slice-fun :initarg :output-slice-fun :type (or null function))
   (epsilon :initform 3 :accessor epsilon :initarg :epsilon :type integer)
   (play-pos :initform 0 :accessor play-pos :type integer)
   (queries :iniform '() :accessor queries :type list)
   (waiting-processes :initform '() :accessor waiting-processes :type list))
  (:documentation "
A handler for Improtek (Copyright 2013 (C) J.Nika).
This object can automate improvization generation based on the rtimprovizer class from Improtek."))

;;;Build an Improvizer Handler from a scenario and a database
(defun build-impro-handler (&key name scenario db-path epsilon output-fun)
  (let ((handler (make-instance 'impro-handler 
                                :name (or name "Improvizer-Handler")
                                :scenario scenario
                                :expanded-scenario (expand_grid scenario)
                                :db-path db-path
                                :epsilon (or epsilon 3)
                                :output-fun output-fun)))
    (setf (rtimprovizer handler) (if db-path
                                     (load-realtimeImprovizer-fromSavedImprovizer (db-path handler))
                                   (NewRealtimeImprovizer))
          (slice-max-pos handler) (length (expanded-scenario handler)))
    handler))

;;;Run the first generation step
(defmethod init-impro-handler ((self impro-handler) &optional (start-pos 0))
  (loop while (<= (empty-pos self) start-pos) do
        (proceed-impro-handler self)))

;;;Run one generation step
(defmethod proceed-impro-handler ((self impro-handler) gen-start)
  (let* ((slice-index gen-start)
         (scenario-suffix (nthcdr slice-index (expanded-scenario self)))
         result-slice-list
         result-length 
         output-list)
    ;;;When improvization is not over
    (when (< slice-index (slice-max-pos self))
      ;;;Run improvization as far as possible
      (setq result-slice-list (improvize-loop-next-factor (rtimprovizer self)
                                                          scenario-suffix
                                                          slice-index)
            result-length (length result-slice-list))
      ;;;Add the generated slice list to the handler slice-list, set the new empty position and the next generation index
      (setf (slice-list self) (append (nthcar slice-index (slice-list self)) result-slice-list)
            (empty-pos self) (length (slice-list self))
            ;(slice-index self) (- (empty-pos self) result-length) ;slice-index result-length))
            ) 
      ;;;When the generation gave a non-null result
      (when result-slice-list
        (setq output-list
              (loop for slice in result-slice-list collect
                    (let ((res (funcall (output-slice-fun self) slice slice-index slice-date)))
                      (incf (slice-date self) (duration slice))
                      res)))))
    output-list))

(defmethod! modify-scenario ((self impro-handler) new-fragment start-index)
  (let ((new-scenario (scenario self)))
    (setf (nthcdr start-index new-scenario) (new-fragment))
    (setf (scenario self) new-scenario)))

(defmethod force-proceed-impro-handler ((self impro-handler))
  (setf (slice-index self) (+ (slice-pos self) (epsilon self)))
  (proceed-impro-handler self))


(defmacro get-impro-slot-callback (slot)
  (case slot 
    ('slice-pos
     `#'(lambda (self)
          (when (and (< (- (empty-pos self) new-val) (epsilon self))
                     (< (empty-pos self) (slice-max-pos self)))
            (setf (slice-index self) (empty-pos self))
            ;(proceed-impro-handler self)
            )))
    ('scenario
     `#'(lambda (self)
          (let* ((new-expanded-scenario (expand_grid new-val))
                 (pos (slice-pos self))
                 (switch-pos (position nil (mapcar 'equal (nthcdr pos (expanded-scenario self)) (nthcdr pos new-expanded-scenario)))))
            (when switch-pos
              (incf switch-pos pos)
              (slot-value handler 'expanded-scenario) new-expanded-scenario
              (slot-value handler 'slice-index) switch-pos
              (slot-value handler 'slice-max-pos) (length new-expanded-scenario))
              ;(proceed-impro-handler self)
            )))
    (otherwise #'(lambda (self)))))


;;;Macros to enable and disable slot reactivity
(defmacro enable-slot-reactivity (slot class callback)
  `(defmethod (setf ,slot) (new-val (self ,class))
     (setf (slot-value self ',slot) new-val)
     (funcall ,callback self)
     new-val))

(defmacro disable-slot-reactivity (slot class)
  `(defmethod (setf ,slot) (new-val (self ,class))
     (setf (slot-value self ',slot) new-val)))


#|
;;;a mettre dans l'output
 ;;;Stretch the result beat list to the desired tempo
      (loop for st in result-slice-list do
            (setf (MidiSet st) (timestretch (MidiSet st) (/ sdur (duration st)))
                  (duration st) sdur))
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