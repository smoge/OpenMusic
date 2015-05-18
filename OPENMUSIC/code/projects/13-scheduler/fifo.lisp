(in-package :om)

(declaim (inline fifo-make fifo-push fifo-pop fifo-empty-p fifo-to-list)
         (optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))

(defun fifo-make (&optional initial-contents)
  (let ((que (cons nil initial-contents)))
    (setf (car que) (last que))
    que))

(defun fifo-push (x que)
  (setf (car que)
        (setf (cdar que) (list x)))
  que)

(defun fifo-pop (que)
  (prog1 (common-lisp:pop (cdr que))
    (when (null (cdr que))
      (setf (car que) que))))

(defun fifo-empty-p (que)
  (eq (car que) que))

(defun fifo-to-list (que)
  (cdr que))