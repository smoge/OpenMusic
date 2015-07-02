(in-package :om)

(defstruct (impro-query
            (:print-object
             (lambda (q stream)
               (print-unreadable-object (q stream :type t :identity t)
                 (princ `(:query :=,(q-name q) :at ,(q-gen-start q) :. :from :=,(q-inputs q)) stream))))
            (:conc-name q-))
  (name "Improvisation-Query" :type string)
  (handler nil :type (or null impro-handler))
  (inputs '() :type list)
  (vals '() :type list)
  (gen-start 0 :type integer)
  (process nil :type (or null mp:process))
  ;(will-be-relayed nil :type (or null impro-query))
  ;(waiting-for nil :type (or null impro-query))
  (output nil)
  (waiting-process nil)
  (:documentation ""))

(defmacro om-ignore&print-error (&rest body)
  `(let ((win nil))
     (multiple-value-bind (a b) 
         (ignore-errors
           ,@body)
       (if b (print (format nil "Error: ~A" b)))
       a)))

(defmethod q-curpos ((self impro-query))
  (1- (if (q-process self)
          (currentimproidx (rtimprovizer (q-handler self)))
        (q-gen-start self))))

(defmethod same-inputs ((q1 impro-query) (q2 impro-query))
  (let ((res t))
    (loop for input in (q-inputs q1) do
          (setq res (and res (find input (q-inputs q2)))))
    res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Query pool;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun query-pool (size)
  (let ((list (make-list size)))
    (loop
     for n on list do
     (setf (car n) (make-impro-query)))
    list))

(let* ((cache-lock (mp:make-lock))
       (cache-size 64)
       (cache-list '()))
  (declare (type fixnum cache-size))

  (mp:with-lock (cache-lock)
    (setf cache-list (query-pool cache-size)))

  (defun query-alloc (&key (name "Improvisation-Query") (inputs '()) (vals '()) (gen-start 0) process handler)
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (query-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((query (pop cache-list)))
	(setf (q-name query) name
              (q-handler query) handler
              (q-inputs query) inputs
              (q-vals query) vals
              (q-gen-start query) gen-start
              (q-process query) process
              ;(q-will-be-relayed query) nil
              )
        (if (eq (q-inputs query) '(scenario))
            (setf (scenario (q-handler query)) (car (q-vals query))))
        query)))

  (defmethod query-free ((self impro-query))
    (mp:with-lock (cache-lock)
      (setf (q-name self) "Improvisation-Query"
            (q-handler self) nil
            (q-inputs self) '()
            (q-vals) '()
            (q-gen-start self) 0
            (q-process self) nil
            (q-output self) nil
            ;(q-will-be-relayed self) nil
            )
      (push self cache-list)
      nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod reset-query ((self impro-query)) (print (list "RESET QUERY" self))
  (if (q-process self)
      (mp:process-kill (q-process self)))
  (if (q-will-be-relayed self) 
      (reset-query (q-will-be-relayed self)))
  (setf (q-output self) '()))
  

(defmethod run ((self impro-query)) (print (list "RUN" self))
  (loop for inp in (q-inputs self)
        for val in (q-vals self) do
        (if (not (eq inp 'scenario))
            (setf (slot-value (rtimprovizer (q-handler self)) inp) val))) ;;;warning: slot dans le rtimpro plutot que dans l'handler: faire une redirection EZ
  (setf (q-process self) (mp:process-run-function (q-name self) nil 
                                                  #'(lambda (hnd gnstrt) 
                                                      (setf (q-output self) (proceed-impro-handler hnd gnstrt))
                                                      (loop for slice in (q-output self)
                                                            for i from 0 do
                                                            (funcall (output-slice-fun (q-handler self))
                                                                     slice
                                                                     (+ (q-gen-start self) i)
                                                                     (reduce #'+ (nthcar (+ (q-gen-start self) i) (slice-list (q-handler self))) :key #'duration)
                                                                     )))
                                                  (q-handler self) 
                                                  (q-gen-start self))))

(defmethod kill ((self impro-query)) (print (list "KILL" self))
  (if (q-process self)
      (mp:process-kill (q-process self)))
  ;(query-free self)
  )

(defmethod %relay ((q1 impro-query) (q2 impro-query)) (print (list "RELAY" q1 q2))
  ;(setf (q-gen-start q2) pivot)
  (let* ((n 0)
         (out (output (rtimprovizer (q-handler q1))))
         (max (max 0 (min (abs (- (q-gen-start q2) (q-gen-start q1))) (1- (length out))))))
    (loop for slice in (subseq out 0 max)
          do
          (funcall (output-slice-fun (q-handler q1))
                   slice
                   (+ (q-gen-start q1) n)
                   (+ (* (q-gen-start q1) (duration slice))
                      (reduce #'+ (nthcar n
                                          (subseq out 0 max))
                              :key #'duration)))
          (incf n))
    (kill q1)
    (run q2)))

(defmethod relay ((q1 impro-query) (q2 impro-query))
  (push q2 (queries (q-handler q2)))
  (%relay q1 q2))

(defmethod %wait-for-relay ((q1 impro-query) (q2 impro-query)) ;;;VERIFIER SI RUNNING
  (let ((q2waswaiting (q-waiting-for q2)))
    (setf (q-will-be-relayed q1) q2
          (q-waiting-for q2) q1)
    (if (not q2waswaiting)
        (progn
          (print (list "WAIT-FOR-RELAY" q1 q2))
          (push (mp:process-run-function "Wait-For-Relay" nil
                                         #'(lambda (query)
                                             (mp:process-wait "Waiting..."
                                                              #'(lambda () (>= (q-curpos (q-waiting-for query)) (q-gen-start query)))
                                                      ;#'(lambda () (not (mp:process-alive-p (q-process q1))))
                                                              )
                                             (relay (q-waiting-for query) query))
                                         q2)
                (waiting-processes (q-handler q2)))))
    (if (not (or (q-running-p q1) (q-running-p q2)))
        (run q1))
    ))

               


;(defmethod wait-for-relay ((q1 impro-query) (q2 impro-query))
;  (if (and (q-will-be-relayed q1) (> (q-gen-start (q-will-be-relayed q1)) (q-gen-start q2)))
;      (progn 
;        (push q2 (queries (q-handler q2)))
;        (setf (q-waiting-for (q-will-be-relayed q1)) q2)
;        (%wait-for-relay q1 q2))
;    (if (not (q-will-be-relayed q1))
;        (progn 
;           (push q2 (queries (q-handler q2)))
;           (%wait-for-relay q1 q2)))))

;(defmethod merge-query ((q1 impro-query) (q2 impro-query)) (print (list "MERGE" q1 q2))
;  (let ((inputs (append (q-inputs q1) (q-inputs q2)))
;        (vals (append (q-vals q1) (q-vals q2))))
;  (kill q1)
;  (process-new-query (query-alloc :inputs inputs :vals vals :gen-start (q-gen-start q2)))))

;(defmethod process-new-query ((self impro-query))
;  (print (list "PROCESS" self))
;  (if (not (queries (q-handler self)))
;      (progn
;        (push self (queries (q-handler self)))
;        (run self))
;    (loop for qi in (queries (q-handler self)) do
;          (cond ((= (q-gen-start self) (q-gen-start qi))
;                 (if (same-inputs self qi)
;                     (progn
;                       (kill qi)
;                       (push self (queries (q-handler self)))
;                       (process-new-query self))
;                   (merge-query self qi)))
;                ((> (q-gen-start self) (q-gen-start qi))
;                 (if (< (q-gen-start self) (q-curpos qi))
;                     (relay self qi)
;                   (wait-for-relay qi self)))
;                ((< (q-gen-start self) (q-gen-start qi))
;                 (progn
;                   (reset-query qi)
;                   (wait-for-relay self qi)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod pos-in-queue ((self impro-query))
  (position self (queries (q-handler self))))

(defmethod query-push ((self impro-query))
  (push self (queries (q-handler self)))
  (process-query self))

(defmethod process-query ((self impro-query))
  (if (= (length (queries (q-handler self))) 1)
      (run self)
    (let (pos
          same-start-min
          same-start-max)
      (sort (queries (q-handler self)) '< :key 'q-gen-start)
      (setq pos (position self (queries (q-handler self))))
      (setq same-start-min (position (q-gen-start self)
                                     (queries (q-handler self))
                                     :key 'q-gen-start))
      (setq same-start-max (position (q-gen-start self)
                                     (queries (q-handler self))
                                     :key 'q-gen-start
                                     :from-end t))
      (loop for i from same-start-min to same-start-max
            do
            (if (not (= i pos))
                (process-eqstart-query self (nth i (queries (q-handler self))))))
      (if (>= (1- same-start-min) 0)
          (process-supstart-query self (nth (1- same-start-min) (queries (q-handler self)))))
      (if (< (1+ same-start-max) (length (queries (q-handler self))))
          (process-infstart-query self (nth (1+ same-start-max) (queries (q-handler self))))))))

(defmethod process-infstart-query ((self impro-query) (qi impro-query)) (print "INF")
  (reset-query qi)
  (wait-for-relay self qi)
  (if (eq (car (queries (q-handler self))) self)
      (run self)))

(defmethod process-supstart-query ((self impro-query) (qi impro-query)) (print "SUP")
  (if (< (q-gen-start self) (q-curpos qi))
      (relay self qi)
    (wait-for-relay qi self)))

(defmethod process-eqstart-query ((self impro-query) (qi impro-query)) (print "EQ")
  (if (same-inputs self qi)
      (progn
        (kill qi)
        (delete qi (queries (q-handler self)) :test 'eq)
        (process-query self))
    (merge-query self qi)))

(defmethod reset-query ((self impro-query)) (print (list "RESET QUERY" self))
  (if (mp:process-p (q-process self))
      (mp:process-kill (q-process self)))
  (if (not (eq (last-elem (queries (q-handler self))) self))
      (reset-query (next-query self)))
  (setf (q-output self) '()))

(defmethod run ((self impro-query)) (print (list "RUN" self))
  (loop for inp in (q-inputs self)
        for val in (q-vals self) do
        (if (not (eq inp 'scenario))
            (setf (slot-value (rtimprovizer (q-handler self)) inp) val))) ;;;warning: slot dans le rtimpro plutot que dans l'handler: faire une redirection EZ
  (setf (q-process self) (mp:process-run-function (q-name self) nil 
                                                  #'(lambda (hnd gnstrt)
                                                      (setf (q-output self) (proceed-impro-handler hnd gnstrt))
                                                      (loop for slice in (q-output self)
                                                            for i from 0 do
                                                            (funcall (output-slice-fun (q-handler self))
                                                                     slice
                                                                     (+ (q-gen-start self) i)
                                                                     (reduce #'+ (nthcar (+ (q-gen-start self) i) (slice-list (q-handler self))) :key #'duration)
                                                                     ))
                                                      (let ((pos (+ (q-gen-start self) (length (q-output self))))
                                                            new-query)
                                                        (when (and (next-query self)
                                                                   (< pos (q-gen-start (next-query self))))
                                                          (setq new-query (query-alloc :gen-start pos :handler (q-handler self)))
                                                          (push new-query (queries (q-handler new-query)))
                                                          (sort (queries (q-handler new-query)) '< :key 'q-gen-start)
                                                          (run new-query))))
                                                  (q-handler self) 
                                                  (q-gen-start self))))

(defmethod kill ((self impro-query)) (print (list "KILL" self))
  (if (mp:process-p (q-waiting-process self))
      (mp:process-kill (q-waiting-process self)))
  (if (mp:process-p (q-process self))
      (mp:process-kill (q-process self))))

(defmethod %relay ((q1 impro-query) (q2 impro-query)) (print (list "RELAY" q1 q2))
  (let* ((n 0)
         (out (output (rtimprovizer (q-handler q1))))
         (max (max 0 (min (abs (- (q-gen-start q2) (q-gen-start q1))) (1- (length out))))))
    (loop for slice in (subseq out 0 max)
          do
          (funcall (output-slice-fun (q-handler q1))
                   slice
                   (+ (q-gen-start q1) n)
                   (+ (* (q-gen-start q1) (duration slice))
                      (reduce #'+ (nthcar n
                                          (subseq out 0 max))
                              :key #'duration)))
          (incf n))
    (kill q1)
    (run q2)))

(defmethod relay ((q1 impro-query) (q2 impro-query))
  (%relay q1 q2))

(defmethod %wait-for-relay ((q1 impro-query) (q2 impro-query)) ;;;VERIFIER SI RUNNING
  (print (list "WAIT-FOR-RELAY" q1 q2))
  (setf (q-waiting-process q2)
        (mp:process-run-function "Wait-For-Relay" nil
                                 #'(lambda (query)
                                     (mp:process-wait "Waiting..." 
                                                      #'(lambda () (>= (q-curpos (previous-query query))
                                                                       (q-gen-start query))))
                                     (relay (previous-query query) query))
                                 q2)))
               
(defmethod wait-for-relay ((q1 impro-query) (q2 impro-query))
  (if (not (q-waiting-process q2))
      (%wait-for-relay q1 q2)))

(defmethod merge-query ((q1 impro-query) (q2 impro-query)) (print (list "MERGE" q1 q2))
  (let ((inputs (append (q-inputs q1) (q-inputs q2)))
        (vals (append (q-vals q1) (q-vals q2))))
  (kill q1)
  (process-new-query (query-alloc :inputs inputs :vals vals :gen-start (q-gen-start q2)))))

(defmethod q-running-p ((self impro-query))
  (mp:process-p (q-process self)))

(defmethod previous-query ((self impro-query))
  (if (>= (1- (pos-in-queue self)) 0)
      (nth (1- (pos-in-queue self)) (queries (q-handler self)))))

(defmethod next-query ((self impro-query))
  (if (< (1+ (pos-in-queue self)) (length (queries (q-handler self))))
      (nth (1+ (pos-in-queue self)) (queries (q-handler self)))))