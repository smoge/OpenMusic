(in-package :om)

(defvar *current-queries* '())

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
(defun new-query-pool (size)
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
    (setf cache-list (new-query-pool cache-size)))

  (defun query-alloc (&key (name "Improvisation-Query") (inputs '()) (vals '()) (gen-start 0) process handler)
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (new-query-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((query (pop cache-list)))
	(setf (q-name query) name
              (q-handler query) handler
              (q-inputs query) inputs
              (q-vals query) vals
              (q-gen-start query) gen-start
              (q-process query) process)
        (if (eq (q-inputs query) '(scenario))
            (setf (scenario (handler query)) (car (q-vals query))))
        query)))

  (defmethod query-free ((self impro-query))
    (remove self *current-queries*)
    (mp:with-lock (cache-lock)
      (setf (q-name self) "Improvisation-Query"
            (q-handler self) nil
            (q-inputs self) '()
            (q-vals) '()
            (q-gen-start self) 0
            (q-process self) nil
            (q-output self) nil)
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
                                                      (print (list "BEFORE" (currentimproidx (rtimprovizer (q-handler self)))))
                                                      (setf (q-output self) (proceed-impro-handler hnd gnstrt))
                                                      (print (list "AFTER" (currentimproidx (rtimprovizer (q-handler self)))))
                                                      ) 
                                                  (q-handler self) 
                                                  (q-gen-start self))))

(defmethod kill ((self impro-query)) (print (list "KILL" self))
  (if (q-process self)
      (mp:process-kill (q-process self)))
  ;(query-free self)
  )

(defmethod relay ((old-query impro-query) (new-query impro-query) pivot) (print (list "RELAY" old-query new-query))
  (setf (q-output new-query) (subseq (q-output old-query) 0 (- pivot (q-gen-start old-query)))
        (q-gen-start new-query) pivot)
  (kill old-query)
  (run new-query))

(defmethod wait-for-relay ((old-query impro-query) (new-query impro-query) pivot) (print (list "WAIT-FOR-RELAY" old-query new-query))
  (push (mp:process-run-function "Wait-For-Relay" nil
                                 #'(lambda (q1 q2 p)
                                     (mp:process-wait "Waiting..." 
                                                      ;#'(lambda () (>= (q-curpos q1) p)))
                                                      #'(lambda () (not (mp:process-alive-p (q-process q1)))))
                                     (relay q1 q2 p))
                                 old-query new-query pivot)
        (waiting-processes (q-handler old-query))))

(defmethod merge-query ((old-query impro-query) (new-query impro-query)) (print (list "MERGE" old-query new-query))
  (let ((inputs (append (q-inputs old-query) (q-inputs new-query)))
        (vals (append (q-vals old-query) (q-vals new-query))))
  (kill old-query)
  (process-new-query (query-alloc :inputs inputs :vals vals :gen-start (q-gen-start new-query)))))

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
                     (progn
                       (push self (queries (q-handler self)))
                       (relay self qi (q-gen-start self)))
                   (progn
                     (push self (queries (q-handler self)))
                     (wait-for-relay qi self (q-gen-start self)))))

                ((< (q-gen-start self) (q-gen-start qi))
                 (progn
                     (push self (queries (q-handler self)))
                     (wait-for-relay qi self (q-gen-start self))))))))