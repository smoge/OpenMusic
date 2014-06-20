;J. Nika, Nov. 2011
;
; Vérifie que le path défini dans "WorkDirectory.lisp" existe bien. Si ce n'est pas le cas demande la sélection d'un nouveau répertoire de travail.
; A venir : regroupement de toutes les variables globales


(in-package :om)


;GLOBAL VARIABLES
;----------------

(setf file_defining_path (merge-pathnames
                    (make-pathname :name "WorkDirectory" :type "lisp" 
                                   :directory '(:relative))
                    *load-pathname*))


(defvar prtRcv 7413)
(defvar prtSnd 7414)
(defvar host_server "127.0.0.1") 
;(defvar *server* nil)

(defparameter channel_melo 2)
(defparameter channel_harmo 1)

;=============================================================================================================================================================================================================

(defun change_path_directory ()
  (progn
    (format *om-stream* "Select work directory~%")
    (setf new_path_work_directory (om-choose-directory-dialog :directory (make-pathname :directory oa::*api-directory*)))
    (setf path_work_directory new_path_work_directory))
       ;On écrit dans le fichier le bon path
  (WITH-OPEN-FILE (out file_defining_path :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (terpri out)
    (write-char #\( out)
    (prin1 'setf out)
    (write-char #\Space out)
    (prin1 'path_work_directory out)
    (write-char #\Space out)
    (prin1 new_path_work_directory out) 
    (write-char #\) out)
    (terpri out)
    (terpri out)
    (let (( comment ";To modify the path of the work directory, close this file and call (change_path_directory)"))
    (write-string comment out))
    )
  )


;CHANGE THE PATH OF THE WORK DIRECTORY IF THE ONE STORED IN DefineWorkDirectory.lisp DOESN'T EXIST
;-------------------------------------------------------------------------------------------------

;Modif Jérôme 12/12/12
(defun check_path_work_directory ()
  
    ;S'il n'existe pas on ouvre une fenetre pour le choisir
  (when (or (not path_work_directory)(not (directory path_work_directory)))
    (change_path_directory)
    ))


(check_path_work_directory)

;(change_path_directory)
