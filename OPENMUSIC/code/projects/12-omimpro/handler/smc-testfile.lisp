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
         (query2 (query-alloc :handler improhandler :gen-start 22))
         (query3 (query-alloc :handler improhandler :gen-start 43))
         (query4 (query-alloc :handler improhandler :gen-start 72))
         (query5 (query-alloc :handler improhandler :gen-start 11))
         (query6 (query-alloc :handler improhandler :gen-start 122)))

    (process-new-query query1)
    (process-new-query query3)
    (process-new-query query2)
    (process-new-query query4)
    (process-new-query query5)
    ;(process-new-query query6)
    (sleep 10)
    (print "---------------------------------------------")
    (print "--------Finished Wait-for-relay Test---------")
    (print "---------------------------------------------")
    ;(q-output query2)
    ))



;(test-wait-for-relay)