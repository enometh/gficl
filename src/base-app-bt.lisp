;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri May 23 14:30:13 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; integrate GFICL-APP with Bordeaux Threads.

;;; mixin GFICL-APP:THREADED-EXECUTOR-MIXIN into your subclass of
;;; GFICL-APP:BASE-APP and you can use GFICL-APP:APPLY-IN-THREAD to
;;; schedule lisp thunks to run in the same thread that %glfw:init was
;;; called during the next GFICL-APP:UPDATE step.
;;;
;;; use GFICL-APP:LAUNCH to start the app in it's own thread and the
;;; macro GFICL-APP:IN-THREAD to schedule execution of forms within
;;; the current app's main thread., or use GFICL-APP:BASE-APP-BT which
;;; already does this for you.

(in-package "GFICL")

#+nil
(require 'bordeaux-threads)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar (lambda (x)
	    (export (intern x "GFICL-APP") "GFICL-APP"))
	  '("THREADED-EXECUTOR-MIXIN"
	    "APPLY-IN-THREAD"
	    "LAUNCH"
	    "IN-THREAD"
	    "RESTART-PIPELINE"
	    "SHUTDOWN"
	    "BASE-APP-BT")))

(defclass gficl-app:threaded-executor-mixin ()
  ((main-thread :initform nil)
   (task-queue :initform nil)
   (task-queue-lock :initform (bt:make-lock))))

(defmethod gficl-app:start :before ((app gficl-app:threaded-executor-mixin)
				    &key &allow-other-keys)
  (with-slots (main-thread) app
    (setq main-thread (bt:current-thread))))

(defmethod gficl-app:update-fn :before ((app gficl-app:threaded-executor-mixin))
  (with-slots (task-queue-lock task-queue) app
    (bt:with-lock-held (task-queue-lock)
      (loop for thunk = (pop task-queue)
	    while thunk do
	    (restart-case (funcall thunk)
	      (cont ()
		;; XXX if you ABORT and don't exit via this CONT
		;; RESTART I *guarantee* your lisp will be hosed.
		:report "ignore the error encountered when executing task")
	      (blow-queue ()
		:test (lambda (c) (declare (ignore c)) (> (length task-queue) 0))
		:report "ignore the error encountered when executing task and clear the current task queue"
		(setq task-queue nil)))))))

(defmethod gficl-app:apply-in-thread ((app gficl-app:threaded-executor-mixin) func &rest args)
  (with-slots (main-thread) app
    (assert (bt:thread-alive-p main-thread)))
  (with-slots (task-queue-lock task-queue) app
    (bt:with-lock-held (task-queue-lock)
      (push (lambda () (apply func args)) task-queue))
    :scheduled))

(defmethod gficl-app:launch ((app gficl-app:threaded-executor-mixin) &rest args &key &allow-other-keys)
  (with-slots (main-thread) app
    (when (bt:threadp main-thread)
      (assert (not (bt:thread-alive-p main-thread)) nil
	  "gficl main-runner is running")))
  (let ((bt:*default-special-bindings*
	 `((gficl::*state* . ,gficl::*state*)
	   (gficl::*active-objects* . ,gficl::*active-objects*)
	   (gficl::*shader-warnings* . ,gficl::*shader-warnings*)
	   (gficl::*vao* . ,gficl::*vao*)
	   (cl-glfw3:*window* . ,cl-glfw3:*window*)
	   (gficl::*max-msaa-samples* . ,gficl::*max-msaa-samples*))))
    (bt:make-thread (lambda ()
		      (apply #'gficl-app:start app args))
		    :name (format nil "gficl-main-runner for ~A" app))))

;; ;madhu 251214 changed in-thread signature to always use the app
(defmacro gficl-app:in-thread (app &body body)
  (let ((app-var (gensym)))
    `(let ((,app-var ,app))
       (check-type ,app-var gficl-app:threaded-executor-mixin)
       (with-slots (main-thread) ,app-var
	 (assert (bt:thread-alive-p main-thread)))
       (gficl-app:apply-in-thread ,app-var (lambda () ,@body)))))

(defun gficl-app:shutdown (app)
  (gficl-app:in-thread app  (signal 'gficl-app:quit)))

(defun gficl-app:restart-pipeline (app)
  (gficl-app:in-thread app (signal 'gficl-app:restart-pipeline)))


(defclass gficl-app:base-app-bt
    (gficl-app:base-app gficl-app:threaded-executor-mixin)
  ())