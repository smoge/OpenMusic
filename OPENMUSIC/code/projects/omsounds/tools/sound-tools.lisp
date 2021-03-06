
(in-package :om)

;;;============================
;;; FILE UTILS

(defvar *tmpparfiles* nil)

(defun add-tmp-file (file)
  (push file *tmpparfiles*))

(defun clean-tmp-files ()
  (when *tmpparfiles*
    (om-print "Removing files:")
    (loop for file in *tmpparfiles* do (when (and file (probe-file file))
                                       (om-print (format nil "   ~s" file))
                                       (om-delete-file file)))
    (setf *tmpparfiles* nil)))
  

;;; FINDS A GOOD (UNIQUE) PATH FOR NAME IN DIR
(defun unique-pathname (dir name &optional (ext ""))
  (let ((pathname (om-make-pathname :directory dir :name name :type ext)))
    (loop while (probe-file pathname)
          for i = 1 then (+ i 1) do
          (setf pathname (make-pathname :device (pathname-device dir) :directory (pathname-directory dir) :name (string+ name (format nil "~D" i)) :type ext)))
    pathname))

(defun auto-rename (path)
  (unique-pathname path (pathname-name path) (pathname-type path)))


;;; IF AUTOMATIC-RENAME OPTION IS ON AND FILE EXISTS, FINDS A NEW NAME
(defun handle-new-file-exists (newpath)
  (when (and newpath (probe-file newpath))
    (gc-all)
    (if *automatic-rename*
        (setf newpath (unique-pathname (make-pathname :directory (pathname-directory newpath)
                                                      :host (pathname-host newpath) :device (pathname-device newpath))
                                       (pathname-name newpath) (pathname-type newpath)))
      (delete-file newpath)
      ))
  newpath)

;;; RETURNS A PATHNAME FROM PATHNAME OR SIMPLE NAME + FOLDER
(defun corrige-sound-filename (file folder)
  (unless (pathnamep file) 
    (setf file (make-pathname 
                :host (pathname-host folder) :device (pathname-device folder)
                :directory (pathname-directory folder) :name (pathname-name file)
                :type (pathname-type file))))
  
  file)


;;;========================
;;; CONVERSIONS

;;; DB / LIN
(defmethod! dB->lin ((x t))
  :icon 141
  :indoc '("a value or list of values in dB")
  :initvals '(-20)
  :doc "Converts <x> from dB to linear value."
  (cond ((numberp x) (db-to-lin x))
        ((listp x) (mapcar #'(lambda (y) (dB->lin y)) x))
        (t (error "illegal arg ~a" x))))

(defmethod! lin->dB ((x t))
  :icon 141
  :indoc '("a value or list of values")
  :initvals '(0.1)
  :doc "Converts <x> from linear to dB."
  (cond((numberp x) (lin-to-db x))
       ((listp x) (mapcar #'lin->dB x))
       (t (error "illegal arg ~a" x))))

;;; SAMPLES / SECONDS
(defmethod! samples->sec ((samples number) samplerate)
           :icon 141 
           :initvals '(0 nil)
           :indoc '("number of samples" "sample rate (Hz)")
           :numouts 1
           :doc "Converts <samples> to a time (or duration) in seconds depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the time."
           (float (/ samples (or samplerate *audio-sr*)))
           )

(defmethod! samples->sec ((samples list) samplerate)
    (mapcar #'(lambda (input)
                (samples->sec input samplerate)) samples))

(defmethod! sec->samples ((secs number) samplerate) 
           :icon 141  
           :initvals '(0 nil)
           :indoc '("duration (s)" "sample rate (Hz)")
           :numouts 1
           :doc "Converts <secs> to a number of samples depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the samples."
           (float (* secs (or samplerate *audio-sr*))))

(defmethod! sec->samples ((secs list) (samplerate number)) 
           (mapcar #'(lambda (input)
                       (sec->samples input samplerate)) secs)
           )

;;; SAMPLES / SECONDS
(defmethod! sec->ms ((n number))
           :icon 141 
           :initvals '(0)
           :indoc '("seconds")
           :numouts 1
           :doc "Converts <n> (seconds / floats) to milliseconds (intergers)."
           (round (* n 1000)))

(defmethod! sec->ms ((n list))
    (mapcar #'(lambda (s) (sec->ms s)) n))

(defmethod! ms->sec ((n number))
           :icon 141 
           :initvals '(0)
           :indoc '("milliseconds")
           :numouts 1
           :doc "Converts <n> (milliseconds / integers) to seconds (floats)."
           (/ n 1000.0))

(defmethod! ms->sec ((n list))
    (mapcar #'(lambda (s) (ms->sec s)) n))



;;;======== FROM OM2CSOUND ===========

(defun deep-mapcar/1 (fun list? &rest args)
  (labels ((map-structure (str accum)
             (cond ((null str) (reverse accum))
                   ((not (consp str))
                    (if accum (reverse (cons (apply fun str args) accum)) (apply fun str args)))
                   (t (map-structure (cdr str) (cons (map-structure (car str) ()) accum))))))
    (map-structure list? nil)))

(defun LLdecimals (list nbdec)
  "Arrondit liste de profondeur quelconque avec <nbdec> decimales"
  (let ((ndec 
         (if (> nbdec 0 ) (float (expt 10 nbdec)) (expt 10 nbdec))))
    (deep-mapcar/1 '/  
                   (deep-mapcar/1 'round list (/ 1 ndec)) ndec )))