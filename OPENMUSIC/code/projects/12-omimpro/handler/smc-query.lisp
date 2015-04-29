(in-package :om)

(defvar *current-queries* '())

(defstruct (impro-query
            (:print-object
             (lambda (q stream)
               (print-unreadable-object (q stream :type t :identity t)
                 (princ `(:query :=,(q-name q) :at ,(q-gen-start q) :. :from :=,(q-origin q)) stream))))
            (:conc-name q-))
  (name "Improvisation-Query" :type string)
  (inputs '() :type list)
  (vals '() :type list)
  (gen-start 0 :type integer)
  (process nil :type (or null mp:process))
  (curpos 0 :type integer)
  (output nil)
  (:documentation ""))

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
       (cache-size 1024)
       (cache-list '()))
  (declare (type fixnum cache-size))

  (mp:with-lock (cache-lock)
    (setf cache-list (new-query-pool cache-size)))

  (defun query-alloc (&key (name "Improvisation-Query") (origin '()) (gen-start 0) process)
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (new-query-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((query (pop cache-list)))
	(setf (q-name query) name
              (q-origin-inputs query) origin
              (q-gen-start query) gen-start
              (q-process query) process)
        (push query *current-queries*)
        query)))

  (defmethod query-free ((self impro-query))
    (remove self *current-queries*)
    (mp:with-lock (cache-lock)
      (setf (q-name self) "Improvisation-Query"
            (q-origin-inputs self) '()
            (q-gen-start self) 0
            (q-process self) nil
            (q-output self) nil)
      (push self cache-list)
      nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod run ((self impro-query))
  ;;;start generation at (q-gen-start self)
  ;;;apply (q-vals self) on (q-inputs self)
  ;;;(setf (q-output self) ...)
  )

(defmethod kill ((self impro-query))
  (if (q-process self)
      (mp:process-kill (q-process self)))
  (query-free self))

(defmethod relay ((old-query impro-query) (new-query impro-query) pivot)
  (setf (q-output new-query) (subseq (q-output old-query) 0 (- pivot (q-gen-start old-query)))
        (q-gen-start new-query) pivot)
  (kill old-query)
  (run new-query))

(defmethod wait-for-relay ((old-query impro-query) (new-query impro-query) pivot)
  (loop while (not (>= (q-curpos old-query) pivot))
        ;;;wait
        )
  (relay old-query new-query pivot))

(defmethod merge ((q1 impro-query) (q2 impro-query))
  )

(defmethod same-inputs ((q1 impro-query) (q2 impro-query))
  (let ((res t))
    (loop for input in (q-inputs q1) do
          (setq res (and res (find input (q-inputs q2)))))
    res))

(defmethod process-new-query ((self impro-query))
  (loop for qi in *current-queries* do
        (cond ((= (q-gen-start self) (q-gen-start qi))
               (if (same-inputs self qi)
                   (kill qi)
                 (merge self qi)))

              ((> (q-gen-start self) (q-gen-start qi))
               (if (< (q-gen-start self) (q-curpos qi))
                   (relay self qi (q-gen-start self))
                 (wait-for-relay self qi (q-gen-start self))))

              ((< (q-gen-start self) (q-gen-start qi))
               (wait-for-relay qi self (q-gen-start self))))))