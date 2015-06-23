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
  (will-be-relayed nil :type (or null impro-query))
  (waiting-for nil :type (or null impro-query))
  (output nil)
  (:documentation ""))

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
(defun q2-pool (size)
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
    (setf cache-list (q2-pool cache-size)))

  (defun query-alloc (&key (name "Improvisation-Query") (inputs '()) (vals '()) (gen-start 0) process handler)
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (q2-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((query (pop cache-list)))
	(setf (q-name query) name
              (q-handler query) handler
              (q-inputs query) inputs
              (q-vals query) vals
              (q-gen-start query) gen-start
              (q-process query) process
              (q-will-be-relayed query) nil)
        (if (eq (q-inputs query) '(scenario))
            (setf (scenario (handler query)) (car (q-vals query))))
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
            (q-will-be-relayed self) nil)
      (push self cache-list)
      nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod run ((self impro-query)) (print (list "RUN" self))
  (loop for inp in (q-inputs self)
        for val in (q-vals self) do
        (if (not (eq inp 'scenario))
            (setf (slot-value (handler self) inp) val))) ;;;warning: slot dans le rtimpro plutot que dans l'handler: faire une redirection EZ
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
                                                      )
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
         (max (min (abs (- (q-gen-start q2) (q-gen-start q1))) (1- (length out)))))
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
                (waiting-processes (q-handler q2)))))))




(defmethod wait-for-relay ((q1 impro-query) (q2 impro-query))
  ;(print (list "OLD" q1 "NEW" q2 "RELAYBY" (q-will-be-relayed q1)))
  (if (and (q-will-be-relayed q1) (> (q-gen-start (q-will-be-relayed q1)) (q-gen-start q2)))
      (progn 
        (push q2 (queries (q-handler q2)))
        (setf (q-waiting-for (q-will-be-relayed q1)) q2)
        (%wait-for-relay q1 q2))
    (if (not (q-will-be-relayed q1))
        (progn 
           (push q2 (queries (q-handler q2)))
           (%wait-for-relay q1 q2)))))

(defmethod merge-query ((q1 impro-query) (q2 impro-query)) (print (list "MERGE" q1 q2))
  (let ((inputs (append (q-inputs q1) (q-inputs q2)))
        (vals (append (q-vals q1) (q-vals q2))))
  (kill q1)
  (process-q2 (query-alloc :inputs inputs :vals vals :gen-start (q-gen-start q2)))))

(defmethod process-new-query ((self impro-query))
  (if (not (queries (q-handler self)))
      (progn
        (push self (queries (q-handler self)))
        (run self))
    (loop for qi in (queries (q-handler self)) do
          (cond ((= (q-gen-start self) (q-gen-start qi))
                 (if (same-inputs self qi)
                     (progn
                       (kill qi)
                       (push self (queries (q-handler self)))
                       (process-new-query self))
                   (merge-query self qi)))

                ((> (q-gen-start self) (q-gen-start qi))
                 (if (< (q-gen-start self) (q-curpos qi))
                     (relay self qi)
                   (wait-for-relay qi self)))

                ((< (q-gen-start self) (q-gen-start qi))
                 (wait-for-relay self qi))))))