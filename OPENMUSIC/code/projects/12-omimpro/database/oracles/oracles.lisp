(in-package :om)

(defvar *oracles* nil)

(setq *oracles*
      '("AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
        "AutumnleavesDoMin-solo2-Chan10-2014.3.1-13h40.or"
        "AutumnleavesDoMin-solo3-Chan11-2014.3.1-13h40.or"
        "AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or"))

(setq *db-path-solo1* (namestring (make-local-path *load-pathname* (nth 0 *oracles*)))
      *db-path-solo2* (namestring (make-local-path *load-pathname* (nth 1 *oracles*)))
      *db-path-solo3* (namestring (make-local-path *load-pathname* (nth 2 *oracles*)))
      *db-path-accomp1* (namestring (make-local-path *load-pathname* (nth 3 *oracles*))))