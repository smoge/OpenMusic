(in-package :om)

(setq *testout* nil)

(defun test-wait-for-relay ()
  (print "---------------------------------------------")
  (print "--------Starting Wait-for-relay Test---------")
  (print "---------------------------------------------")
  (let* ((improhandler (build-impro-handler :name "Hey"
                                            :expanded-scenario (NewHarmLabelList (expand_grid *scenario-degeu*))
                                            :db-path *db-path-solo1*
                                            :output-fun #'(lambda (a b c) (print `(,a ,b ,c)))))
         (query1 (query-alloc :handler improhandler :gen-start 12))
         (query2 (query-alloc :handler improhandler :gen-start 22)))

    (process-new-query query1)
    (process-new-query query2)
    (sleep 10)
    (print "---------------------------------------------")
    (print "--------Finished Wait-for-relay Test---------")
    (print "---------------------------------------------")
    (q-output query2)))



