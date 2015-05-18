; J. Nika, Nov. 2011
;
; Définition des paths des différents fichiers et dossiers dans le répertoire de travail



(in-package :om)


; Tunes directory (used in Antescofo.lisp)
(defvar path_tunessavedir path_work_directory)
; Midibuff (used in Antescofo.lisp, CommunicationMax.lisp)
(defvar path_midibuff (format nil "~a~a" path_work_directory "midibuff.mid"))


; Path fichier d'apprentissage substitution (used in Antescofo.lisp)
(defvar path_labelized (format nil "~a~a" path_work_directory "labelized.mid"))


; Polyphonic midibuff for the simultaneous creation of the "Melo-Harmo" oracles and the "Voicings" oracles (used in CommunicationMax.lisp)
(defvar path_midibuff_melo-harmo (format nil "~a~a" path_work_directory "midibuff_melo-harmo.mid"))
; Default path for Antescofo (used in Antescofo.lisp)
(defvar path_bufferAntescofo (format nil "~a~a" path_work_directory "buffer-for-antescofo.txt"))

; Corpus calculated from live sessions
(defvar path_dir_oracle_melo_harmo (format nil "~a~a" path_work_directory "_Corpus/Oracles_melo_harmo"))
(defvar path_dir_oracle_voicings (format nil "~a~a" path_work_directory "_Corpus/Oracles_voicings"))
(defvar path_dir_live_oracles (format nil "~a~a" path_work_directory "_Oracles"))


(ensure-directories-exist path_dir_oracle_melo_harmo)
(ensure-directories-exist path_dir_oracle_voicings)
(ensure-directories-exist path_dir_live_oracles)

(print path_tunessavedir)
(print path_midibuff)
(print path_midibuff_melo-harmo)
(print path_bufferAntescofo)
(print path_dir_oracle_melo_harmo)
(print path_dir_oracle_voicings)
(print path_dir_live_oracles)


;(defun define_paths_save (s)
;(setf path_tunessavedir s
;      path_midibuff (format nil "~a/~a" s "midibuff.mid")
;      path_midibuff_melo-harmo (format nil "~a/~a" s "midibuff_melo-harmo.mid")
;      path_bufferAntescofo (format nil "~a/~a" s "buffer-for-antescofo.txt")));;

;(defun define_paths_corpus (s)
;  (setf path_dir_oracle_melo_harmo (format nil "~a/~a" s "Oracles_melo_harmo")
;        path_dir_oracle_voicings (format nil "~a/~a" s "Oracles_voicings")))



