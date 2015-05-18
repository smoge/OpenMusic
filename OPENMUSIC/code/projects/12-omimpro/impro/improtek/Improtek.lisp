;;;;============================================================================
;;;;               Improtek.lib
;;;;
;;;; Improtek library (formerly named OMax 2.0)
;;;; authors: G. Assayag, M. Chemillier, J. Nika
;;;; date: 2012 (2004 for the OMax 2.0 version)
;;;;============================================================================ 

(in-package :om)


(require-library "RepMus")

(load (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) '("bordeaux-threads")) :name "bordeaux-threads.lisp"))
(require-library "bordeaux-threads")

(defvar *improtek-source-dir* nil)
(setf *improtek-source-dir* (append (pathname-directory *load-pathname*) (list "sources")))


(defvar *improtek-lib-files* nil)
(setf *improtek-lib-files* '(
                             "WorkDirectory"
                             "WorkDirectory_Paths"
                             "Oracle"
                             "Labels"
                             "MorrisPratt"
                             "Improvizer"
                             "InitImprotek"
                             "Beatlist"             
                             "ImprotekData" 
                             "HarmoArrang/Substitution"
                             "Antescofo"
                             "Tune"
                             ;---------
                             ;"RealTime/AntescofoTR"          
                             "RealTime/AsynchEventDrivenProdCons"
                             "RealTime/MidiToolsForRealTime"
                             "RealTime/RealTimeImprovizer"
                             ;---------
                             
                             ;---------
                             "Melobeatlist"
                             ;"Styles/Hermeto"
                             "GridData"
                             ;"Styles/Garner"
                             "Styles/Fats"
                             "HarmoArrang/HarmonizationNew"
                             "HandleMaxMessages"
                             ))


(mapc #'(lambda (file) (compile&load (make-pathname :directory *improtek-source-dir* :name file))) *improtek-lib-files*)



