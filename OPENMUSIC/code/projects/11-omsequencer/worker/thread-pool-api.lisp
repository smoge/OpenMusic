#|
Thread Pool API (cf. thread-pool.lisp)

Author : D.Bouche
Ircam (C) 2014
|#

(in-package :om)

;;;=============================================================================================Thread Pool Structure
(defun om-init-thread-pool (&optional worker-count)
  (sch:init-thread-pool worker-count))

(defun om-abort-thread-pool ()
  (sch:abort-thread-pool))

(defun om-pause-thread-pool ()
  (sch:pause-thread-pool))

(defun om-resume-thread-pool ()
  (sch:resume-thread-pool))

(defun om-get-thread-pool ()
  sch:*thread-pool*)

(defmethod om-get-thread-pool-count ((self sch::thread-pool))
  (sch::thread-pool-count self))

(defmethod om-get-thread-pool-state ((self sch::thread-pool))
  (sch::thread-pool-state self))

;;;=============================================================================================Thread Task Structure
(defun om-build-t-task (&key (name "t-task") routine data callback)
  (sch:build-t-task :name name :routine routine :data data :callback callback))

(defmethod om-get-t-task-name ((self sch::t-task))
  (sch::t-task-name self))
(defmethod om-set-t-task-name ((self sch::t-task) name)
  (setf (sch::t-task-name self) name))

(defmethod om-get-t-task-state ((self sch::t-task))
  (sch::t-task-state self))

(defmethod om-get-t-task-routine ((self sch::t-task))
  (sch::t-task-routine self))
(defmethod om-set-t-task-routine ((self sch::t-task) routine)
  (setf (sch::t-task-routine self) routine))

(defmethod om-get-t-task-data ((self sch::t-task))
  (sch::t-task-data self))
(defmethod om-set-t-task-data ((self sch::t-task) data)
  (setf (sch::t-task-data self) data))

(defmethod om-get-t-task-result ((self sch::t-task))
  (sch::t-task-result self))

(defmethod om-get-t-task-callback ((self sch::t-task))
  (sch::t-task-callback self))
(defmethod om-set-t-task-callback ((self sch::t-task) callback)
  (setf (sch::t-task-callback self) callback))

(defmacro om-build-t-task-callback (&body body)
  "Builds a callback to be called after a task execution. Use the 'self' variable name to use the result of the task in your callback" 
  `#'(lambda (result) ,@body))

;;;=============================================================================================Thread Pool Entry Points
(defmethod om-send-t-task-bottom ((self sch::t-task))
  (sch:send-task-thread-pool-bottom self))
(defmethod om-send-t-task-bottom ((self (eql :error)))
  (om-message-abort "Error : The task you submitted is invalid."))

(defmethod om-send-t-task-top ((self sch::t-task))
  (sch:send-task-thread-pool-top self))
(defmethod om-send-t-task-top ((self (eql :error)))
  (om-message-abort "Error : The task you submitted is invalid."))

(defmethod om-with-thread-pool ((routine function) (data list) (callback function) &optional (asap nil))
  "The 'routine' function is called with the 'data' list, so the number of arguments of 'routine' must be equal to the length of 'data'.
The 'callback' function will be called after 'routine' execution and must be a 1 arg function. Use the result of 'routine' in 'callback' with the variable name 'result'."
  (if asap
      (om-send-t-task-top (om-build-t-task :routine routine :data data :callback callback))
    (om-send-t-task-bottom (om-build-t-task :routine routine :data data :callback callback))))