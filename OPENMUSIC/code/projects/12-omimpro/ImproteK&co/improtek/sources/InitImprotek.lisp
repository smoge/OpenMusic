(in-package :om)(defun improtek-init-IHM ()  (set-view-size *om-workSpace-win* 250  250)  (let ((listener (ccl::current-listener))        (listener1 (ccl::make-new-listener)))    (set-view-size  listener  400 300)    (set-view-position listener 0 320)    (window-select listener)    (set-view-size  listener1  400 150)    (set-view-position listener1 0 700)    (omax)))(setf *global-deltachords* 0) 