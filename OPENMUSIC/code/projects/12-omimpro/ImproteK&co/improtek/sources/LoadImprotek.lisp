(in-package :om)

; LoadImprotek.lisp
;------------------------------------------------------------------------------------------------------------
; Load the library (without starting the server to listen to the Max/MSP interface)
; If it is not defined, the path of the work directory (containing the interface, the midi buffers, and the 
; saved oracles and antescofo scores) will be asked.
;
; Jérôme Nika - March 11th 2013
;------------------------------------------------------------------------------------------------------------

;GLOBAL VARIABLES FOR NAVIGATION INFO DISPLAY
;---------------------------------------------
;Jerome 26/10/13
(defparameter *print-navig-basics* 1) ; Print labels and min. info during navigation
(defparameter *print_info_MP* 0) ;Print every step in "Morris&Pratt"
(defparameter *print_info_find* 0) ;Print every step in "find-prefix-label-match"
(defparameter *print_info_navig* 0) ;Print every step in "Improvize"


;================================================================================================================================
; (Compile and) load "Improtek.lisp" in the parent directory
; to (compile and) load all the files listed in it.

(let ((file-lib-improtek (make-pathname :directory (reverse (cdr (reverse (pathname-directory *load-pathname*)))) :name "Improtek")))
  (format *om-stream* "loading ~a~%" file-lib-improtek)
  (if (om-standalone-p) (load file-lib-improtek) (compile&load file-lib-improtek)))
;=================================================================================================================================

(defun set-current-tune (s) 
  (print s)
  (setf req_tune (gethash s *available-grids*))
  (setf *current-tune* (clone req_tune))                      
  (generate-grid *current-tune* (beatduration *current-tune*))
  ;Jerome 26/10/13
  (generate-grid-param *current-tune* (beatduration *current-tune*)))

;Jerome 29/04/2013 : ex "meloharm_oracle_initialization"
(defun load-saved-oracle-in-meloharm-oracle (s) 
  (if (probe-file s)
      (progn (setf *current-MeloHarmOracle* (load-improvizer s)))
        (print "MeloHarm Oracle not found")))

;Jerome 29/04/2013 : ex "voicings_oracle_initialization"
(defun load-saved-oracle-in-voicings-oracle (s) 
  (if (probe-file s)
      (progn (setf *current-VoicingOracle* (load-improvizer s)))
    (print "MeloHarm Oracle not found")))


(defvar *flag-loaded-lib* t)