(in-package :om)

; LoadImprotek.lisp
;------------------------------------------------------------------------------------------------------------
; Load the library (without starting the server to listen to the Max/MSP interface)
; If it is not defined, the path of the work directory (containing the interface, the midi buffers, and the 
; saved oracles and antescofo scores) will be asked.
;
; Jérôme Nika - March 11th 2013
;------------------------------------------------------------------------------------------------------------


;================================================================================================================================
; (Compile and) load "Improtek.lisp" in the parent directory
; to (compile and) load all the files listed in it.

(let ((file-lib-improtek (make-pathname :directory (reverse (cdr (reverse (pathname-directory *load-pathname*)))) :name "Improtek")))
  (format *om-stream* "loading ~a~%" file-lib-improtek)
  (if (om-standalone-p) (load file-lib-improtek) (compile&load file-lib-improtek)))
;=================================================================================================================================

(defun tune-initialization (s) 
  (print s)
  (setf req_tune (gethash s *available-grids*))
  (setf *current-tune* (clone req_tune))                      
  (generate-grid *current-tune* (beatduration *current-tune*)))

(defun meloharm_oracle_initialization (s) 
  (if (probe-file s)
      (progn (setf *current-MeloHarmOracle* (load-improvizer s)))
        (print "MeloHarm Oracle not found")))


(defun voicings_oracle_initialization (s) 
  (if (probe-file s)
      (progn (setf *current-VoicingOracle* (load-improvizer s)))
    (print "MeloHarm Oracle not found")))


(defvar *flag-loaded-lib* t)