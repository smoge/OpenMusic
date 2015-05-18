(in-package :om)


; IMPROVIZER.LISP
(defmethod set-start-region-oraclechan ((self Improvizer) begin end begin_slider end_slider)
  (set-start-region self (list (floor (* begin (/ (max begin_slider (1- (maxetat self))) end_slider))) 
                                 (floor (* end (/ (max begin_slider (1- (maxetat self))) end_slider))))))

; ANTESCOFO.LISP -> TUNE.LISP ?

(defmethod load-saved-oracle-in-oraclechan ((self tune) s num_oracle) 
  (if (probe-file s)
      (progn (setf (gethash num_oracle (oracletable self)) (load-improvizer s)))
        (print "Impro oracle not found")
))

