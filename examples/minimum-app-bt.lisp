(in-package :gficl-examples/minimum-app)

(defclass minimum-app-bt (minimum-app gficl-app:threaded-executor-mixin) ())

#||
(setq $up '(((0 0.9)) ((-0.9 -0.9)) ((0.9 -0.9))))
(setq $dn '(((0 -0.9)) ((0.9 0.9)) ((-0.9 0.9))))

(setq $t1 (make-instance 'minimum-app-bt))
(gficl-app:launch $t1)
(setf (gficl-app:process-pending-events-style $t1) :wait)

(gficl-app:in-thread
  (setf (slot-value $t1 'vertices) $dn)
  (signal 'gficl-app:restart-pipeline))

(gficl-app:apply-in-thread $t1 (lambda () (signal 'gficl-app:quit)))
(gficl-app:apply-in-thread $t1 (lambda ()
				 (setf (slot-value $t1 'vertices) $up)
				   (signal 'gficl-app:restart-pipeline)
				 (format t "WILLIE ~A~&" (bt:current-thread))))
||#

