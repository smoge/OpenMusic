#|
Static Thread Pool with automatic optimization

The thread-pool is made of worker threads and one "dispatcher" thread which distribute the pending tasks to available workers.
Worker threads are made of a thread and a task slot, which can hold on task.
Tasks are made of a routine (lambda function) and data (arguments passed to the routine).
Tasks are pushed in a thread-pool-queue, which is a locked list.

inspired by : http://software.intel.com/fr-fr/articles/Thread-pool
|#

(in-package :sch)

(export
 '(;;;Structure
   build-t-task

   init-thread-pool
   abort-thread-pool
   pause-thread-pool
   resume-thread-pool

   ;;;Tools
   send-task-thread-pool-bottom
   send-task-thread-pool-top

   ;;;Variables
   *thread-pool*) :sch)

(defvar *thread-pool* nil)
(defvar *thread-pool-queue* nil)
(defvar *t-task-pool* nil)
(defvar *optimum-worker-count* nil)

(defconstant *max-worker-count* (expt 2 6))
(defconstant *min-worker-count* (expt 2 1))
(defconstant *default-worker-count* (expt 2 3))

(defstruct (t-task)
  (name "t-task" :type string)
  (state :init)
  (routine #'(lambda ()) :type (or null function))
  (data nil)
  (result nil)
  (callback nil :type (or null function)))

(defstruct (t-tree)
 ; (node nil :type (or null t-node))
  (packs nil :type list)
  (current-level 0 :type integer)
  (callback nil :type (or null function))
  (dispatcher nil :type (or null mp::process))
  (guard nil :type boolean))

(defstruct (thread)
  (process nil :type (or null mp::process))
  (task nil :type (or t-task null)))

(defstruct (thread-pool)
  (lock (mp::make-lock :name "thread_pool_lock") :type mp::lock)
  (dispatcher nil :type (or null mp::process))
  (threads (list) :type list)
  (count 0 :type integer)
  (init 0 :type integer)
  (state :running))

(defstruct (thread-pool-queue)
  (lock (mp::make-lock :name "thread_pool_queue_lock") :type mp::lock)
  (tasks (list) :type list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Transport functions
(defun init-thread-pool (&optional worker-count)
  (setq *thread-pool* (make-thread-pool)
        *thread-pool-queue* (make-thread-pool-queue)
        *optimum-worker-count* (get-cpu-count))
  (setf (thread-pool-threads *thread-pool*) (loop for i from 1 to (or worker-count *default-worker-count*) collect
                                                  (let ((thread (make-thread)))
                                                    (setf (thread-process thread)
                                                          (mp:process-run-function
                                                           (format nil "OM - TP Worker ~d" i) nil
                                                           'thread-exec thread))
                                                    thread))
        (thread-pool-count *thread-pool*) (or worker-count *optimum-worker-count*)
        (thread-pool-dispatcher *thread-pool*) (mp:process-run-function "OM - TP Dispatcher" nil 'dispatch-work))
  (dotimes (i 20) (push (make-t-task) *t-task-pool*)))

(defun abort-thread-pool ()
  (loop for thr in (thread-pool-threads *thread-pool*) do
        (mp:process-kill (thread-process thr)))
  (mp:process-kill (thread-pool-dispatcher *thread-pool*))
  (setq *thread-pool* nil
        *t-task-pool* nil))

(defun pause-thread-pool ()
  (setf (thread-pool-state *thread-pool*) :pause)
  (loop for thr in (thread-pool-threads *thread-pool*) do
        (mp:process-stop (thread-process thr))))

(defun resume-thread-pool ()
  (setf (thread-pool-state *thread-pool*) :running)
  (loop for thr in (thread-pool-threads *thread-pool*) do
        (mp:process-unstop (thread-process thr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Thread methods and functions
(defmethod thread-exec ((self thread))
  (loop
   (when (and (thread-task self) (t-task-routine (thread-task self)))
     (incf (thread-pool-init *thread-pool*))
     (setf (t-task-state (thread-task self)) :running)
     (setf (t-task-result (thread-task self))
           (if (t-task-data (thread-task self))
               (apply (t-task-routine (thread-task self)) (t-task-data (thread-task self)))
             (funcall (t-task-routine (thread-task self)))))
     (if (t-task-callback (thread-task self)) (funcall (t-task-callback (thread-task self)) (t-task-result (thread-task self))))
     (push (clean-t-task (thread-task self)) *t-task-pool*)
     (setf (t-task-state (thread-task self)) :stop
           (thread-task self) nil)
     (decf (thread-pool-init *thread-pool*)))
   (mp::process-wait "Waiting for a task" #'(lambda (thr) (and (eq (thread-pool-state *thread-pool*) :running)
                                                               (thread-task thr))) self)))

(defun dispatch-work ()
  (loop
   (mp::with-lock ((thread-pool-queue-lock *thread-pool-queue*))
     (when (thread-pool-queue-tasks *thread-pool-queue*)
       (let ((thr (find-available-thread)))
         (if thr 
             (assign-task thr (pop (thread-pool-queue-tasks *thread-pool-queue*)))))))
   (mp:process-wait "Waiting for tasks to assign" #'(lambda () (and (eq (thread-pool-state *thread-pool*) :running)
                                                                    (thread-pool-queue-tasks *thread-pool-queue*) 
                                                                    (find-available-thread))))))

(defun dispatcher-alarm ()
  (and (thread-pool-queue-tasks *thread-pool-queue*) (find-available-thread)))

(defun find-available-thread ()
  (let ((i (position t (mapcar 'thread-available? (thread-pool-threads *thread-pool*)))))
    (if i (nth i (thread-pool-threads *thread-pool*)))))

(defun add-thread ()
  (let ((thread (make-thread)))
    (setf (thread-process thread)
          (mp:process-run-function
           (format nil "OM - TP Worker ~d" (1+ (length (thread-pool-threads *thread-pool*)))) nil
           'thread-exec thread))
    (push thread (nthcdr (length (thread-pool-threads *thread-pool*)) (thread-pool-threads *thread-pool*)))
    (incf (thread-pool-count *thread-pool*))))

(defmethod thread-available? ((self thread))
  (if (thread-task self) nil t))

(defmethod assign-task ((self thread) (task t-task))
  (setf (thread-task self) task)
  (mp:process-poke (thread-process self)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;User Tools
(defun build-t-task (&key (name "t-task") routine data callback)
  (if (and (= (length data) (length (cadr (function-lambda-expression routine)))) (= (length (cadr (function-lambda-expression callback))) 1))
      (let ((task (or (pop *t-task-pool*) (make-t-task))))
        (setf (t-task-name task) name
              (t-task-routine task) routine
              (t-task-data task) data
              (t-task-callback task) callback)
        task)
    :error))

(defmethod clean-t-task ((self t-task))
  (setf (t-task-name self) "t-task"
        (t-task-state self) :init
        (t-task-routine self) nil
        (t-task-data self) nil
        (t-task-result self) nil
        (t-task-callback self) nil)
  self)

(defmethod send-task-thread-pool-top ((self t-task))
  (mp::with-lock ((thread-pool-queue-lock *thread-pool-queue*))
    (push self (thread-pool-queue-tasks *thread-pool-queue*)))
  (mp:process-poke (thread-pool-dispatcher *thread-pool*)))

(defmethod send-task-thread-pool-bottom ((self t-task))
  (mp::with-lock ((thread-pool-queue-lock *thread-pool-queue*))
    (if (thread-pool-queue-tasks *thread-pool-queue*)
        (push self (nthcdr (length (thread-pool-queue-tasks *thread-pool-queue*)) (thread-pool-queue-tasks *thread-pool-queue*)))
      (push self (thread-pool-queue-tasks *thread-pool-queue*))))
  (mp:process-poke (thread-pool-dispatcher *thread-pool*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tree Tools
(defmethod set-children-callbacks ((self t-tree) tree)
  (setf (t-task-state (first tree)) :init)
        ;(t-task-data (first tree)) (make-list (length (cdr tree)) :initial-element nil))
  (loop for child in (cdr tree)
        for i from 0 do
        (if (atom child)
            (let ((fun (t-task-callback child))
                  (n i)) 
              (setf (t-task-state child) :init
                    (t-task-callback child)
                    #'(lambda (obj)
                        (setf (nth n (t-task-data (first tree))) 
                              (t-task-result obj))
                        (funcall fun obj)
                        (if (every #'identity (t-task-data (first tree)))
                          (setf (t-tree-guard self) t)
                          (mp:process-poke (t-tree-dispatcher self))))))
          (let ((fun (t-task-callback (first child)))
                (n i))
            (setf (t-task-state (first child)) :init
                  (t-task-callback (first child))
                  #'(lambda (obj)
                      (setf (nth n (t-task-data (first tree))) 
                            (t-task-result obj))
                      (funcall fun obj)
                      (when (every #'identity (t-task-data (first tree)))
                        (setf (t-tree-guard self) t)
                        (mp:process-poke (t-tree-dispatcher self)))))
            (set-children-callbacks self child)))))

(defmethod compute-tree ((self t-tree))
  (setf (t-tree-dispatcher self) (mp:process-run-function "OM - TP tree Dispatcher" nil 'dispatch-tree self)))

(defmethod dispatch-tree ((self t-tree))
  (let ((max (reduce #'max (mapcar #'length (t-tree-packs self)))))
    (loop
     (setf (t-tree-guard self) nil)
     (compute-tree-pack self)
     (incf (t-tree-current-level self))
     (if (>= (t-tree-current-level self) max) (mp:process-kill (t-tree-dispatcher self)))
     (mp:process-wait "Waiting for results" #'(lambda () (t-tree-guard self))))))

(defmethod compute-tree-pack ((self t-tree))
  (loop for task in (nth (t-tree-current-level self) (t-tree-packs self)) do
        (send-task-thread-pool-bottom task)))

(defun paths (tree)
  (if (atom tree)
      (list (list tree))
    (mapcan (lambda (node)
              (mapcar (lambda (path)
                        (cons (car tree) path))
                      (paths node)))
            (cdr tree))))

(defun tree-depth (list &optional (prior-depth 0))
  "Maximum depth of list structure"
  (if (consp list)
      (reduce #'max (mapcar #'(lambda (l) (tree-depth l (1+ prior-depth))) list))
    prior-depth))


(defun get-tree-packs (tree)
  (let* ((ways (paths tree))
         (max (reduce #'max (mapcar #'length ways)))
         tree-packs)
    (dotimes (i max)
      (push (remove nil (remove-duplicates (loop for path in ways collect (nth i path)))) tree-packs))
    tree-packs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Extra
(defun get-cpu-count ()
  (let (pipe cpu)
    (setq pipe 
          #+macosx (sys:open-pipe "sysctl -n hw.ncpu")
          #+linux (sys:open-pipe "cat /proc/cpuinfo | grep processor | wc -l"))
    (setq cpu (parse-integer (read-line pipe nil nil)))
    (close pipe)
    cpu))

(defun thread-pool-automatic-optimization (&key (test-only t))
  (oa:om-beep)
  (om-print "

;;;=============================================
;;; Thread Pool Optimization.
;;;=============================================
")
  (om-api:om-message-dialog (format nil "Warning : 
Thread Pool automatic optimization runs tests with different numbers of workers and find the best match for your computer.
~%To get good results, please close any running application.~%~%When ready, press OK."))
  (hcl:avoid-gc)
  (let (task-list t1 t2 (optimum *min-worker-count*) elapsed-time (n-workers *min-worker-count*))

    (setq task-list (loop for i from 0 to 9999 collect
                          (if (= i 9999)
                              (make-t-task 
                               :routine #'(lambda (l) (let ((a 0))
                                                        (car l)
                                                        (dotimes (z 100000) (* z (expt (incf a) 3)))
                                                        (setq t2 (get-internal-real-time))))
                               :data (list 1 2 3))
                            (make-t-task 
                             :routine #'(lambda (l) (let ((a 0)) (car l) (dotimes (z 100000) (* z (expt (incf a) 3)))))
                             :data (list 1 2 3)))))

    (loop while (<= n-workers *max-worker-count*) do
          (ignore-errors (abort-thread-pool))
          (print (format nil "Running the test with ~d worker..." n-workers))
          (init-thread-pool n-workers)
          (setq t1 (get-internal-real-time))
          (loop for task in task-list do
                (send-task-thread-pool-bottom task))
          (loop while (not t2) do
                (sleep 1))
          (if (or (not elapsed-time) (< (* 1.2 (- t2 t1)) elapsed-time))
              (setq elapsed-time (- t2 t1)
                    optimum n-workers))
          (setq t2 nil
                n-workers (* n-workers 2)))
    (setq *optimum-worker-count* optimum)
    (print (format nil "Test ran the best with ~d workers." *optimum-worker-count*))
    (hcl:normal-gc)
    (ignore-errors (abort-thread-pool))
    (when (not test-only)
      (print (format nil "Thread pool is starting with ~d worker threads..." *optimum-worker-count*))
      (init-thread-pool *optimum-worker-count*)
      (print "Thread Pool successfully started!"))
    *optimum-worker-count*))

(defun build-t-tree (tree)
  (let (result)
    (setq result (make-t-tree))
    (set-children-callbacks result tree)
    (setf ;(t-tree-node result) (build-node-tree tree)
          (t-tree-packs result) (get-tree-packs tree))
    result))
;(thread-pool-automatic-optimization)
;;;=======================================================================================================================================
;;;=======================================================================================================================================
;;;=======================================================================================================================================
#|
;Trees using a node structure
(defstruct (t-node)
  (task nil)
  (children nil :type list))

(defun build-node-tree (tree)
  (if (listp tree)
      (make-t-node 
       :task (first tree)
       :children (loop for node in (cdr tree) collect
                       (build-node-tree node)))
    (make-t-node 
       :task tree)))



(defmethod set-children-node-callbacks ((self t-node))
  (setf (t-task-state (t-node-task self)) :init)
  (when (t-node-children self)
    (setf (t-task-data (t-node-task self)) (make-list (length (t-node-children self)) :initial-element nil))
    (loop for child in (t-node-children self)
          for i from 0 do
          (let ((fun (t-task-callback (t-node-task child))))
            (setf (t-task-callback (t-node-task child))
                  #'(lambda (obj)
                      (setf (nth i (t-task-data (t-node-task self))) 
                            (t-task-result obj))
                      (funcall fun obj)))
            (set-children-callbacks child)))))



;;;=======================================================================================================================================
(setq *val-tree-1* (list 0 1 (list 2 (list 3 7 (list 5 6)) 4)))
(setq *val-tree-2* (list 0 (list 2 (list 3 7 (list 5 6)) 4) 10))
(setq *val-tree-3* (list "end" 1 (list 2 3 4)))
init-thread-pool
(setq *task-tree* (list
                   (make-t-task :name "END"
                                :routine #'(lambda (a) (print "je termine") (sleep 3)
                                             (print (list "resultat" a)))
                                :data (list nil nil) 
                                :callback #'(lambda (self) (print "END ALL") ))
                   (list
                    (make-t-task :name "1"
                                 :routine #'(lambda (a) (print "START 1")
                                              a)
                                 :data '(nil nil) 
                                 :callback #'(lambda (self) (print "END 1")))
                    (make-t-task :name "5"
                                 :routine #'(lambda (a) (print "START 5")
                                              (sleep 2.5)
                                              a)
                                 :data '(5) 
                                 :callback #'(lambda (self) (print "END 5")))
                    (make-t-task :name "6"
                                 :routine #'(lambda (a) (print "START 6")
                                              (sleep 2)
                                              a)
                                 :data '(6) 
                                 :callback #'(lambda (self) (print "END 6"))))
                   (list (make-t-task :name "2"
                                      :routine #'(lambda (a) (print "START 2") 
                                                   (sleep 3) a)
                                      :data '(nil nil) 
                                      :callback #'(lambda (self) (print "END 2")))
                         (make-t-task :name "3"
                                      :routine #'(lambda (a) (print "START 3") 
                                                   (sleep 1) a)
                                      :data '(3) 
                                      :callback #'(lambda (self) (print "END 3")))
                         (make-t-task :name "4"
                                      :routine #'(lambda (a) (print "START 4")
                                                   (sleep 6) a)
                                      :data '(4) 
                                      :callback #'(lambda (self) (print "END 4"))))))

;(init-thread-pool 8)
;(ignore-errors (abort-thread-pool))
;(compute-tree *treetest*)
;(setf *treetest* (build-t-tree *task-tree*))
;(funcall #'(lambda (lvl) (every #'identity (mapcan #'t-task-data (nth lvl (t-tree-packs *treetest*))))) 1)
;(progn (print "------") (loop for a in (flat *task-tree*) do (print a)) (print "------"))
;(set-children-callbacks *task-tree*)

|#