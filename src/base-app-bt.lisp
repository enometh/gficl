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
	    "BASE-APP-BT"
	    "REPLACE-AND-RELOAD"
	    "SKELETON")))

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

#+lem-mailbox
(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(gficl-app::call-in-thread-sync
	    gficl-app::in-thread-sync
	    gficl-app::*call-in-thread-sync-timeout*
	    gficl-app::*call-in-thread-sync-break-on-errors*)
	  "GFICL-APP"))

#+lem-mailbox
(progn
(defvar gficl-app:*call-in-thread-sync-break-on-errors* nil)
(defvar gficl-app:*call-in-thread-sync-timeout* 6)
(defun gficl-app:call-in-thread-sync (app func)
  (let ((mailbox (lem-mailbox:make-mailbox)))
    (gficl-app:in-thread app
      (let (result errorp error)
	 (unwind-protect
	      (prog nil
		 (handler-bind ((error (lambda (e)
					 (setq errorp t error e)
					 (if gficl-app:*call-in-thread-sync-break-on-errors*
					     nil
					     (go done)))))
		   (setq result (multiple-value-list (funcall func))))
		 done)
	   (lem-mailbox:send-message mailbox (list result errorp error)))))
    (multiple-value-bind (result-values successp)
	(lem-mailbox:receive-message mailbox :timeout gficl-app:*call-in-thread-sync-timeout*)
      (cond (successp (destructuring-bind (result-values errorp error) result-values
			(cond (errorp (error "CALL-IN-THREAD-SYNC failed with ~S ~A" error error))
			      (t (values-list result-values)))))
	    (t (error "CALL-IN-THREAD-SYNC timed out"))))))

(defmacro gficl-app:in-thread-sync (app &body body)
  `(gficl-app:call-in-thread-sync ,app (lambda () ,@body))))



;;; ----------------------------------------------------------------------
;;;
;;; EXTRAS
;;;

(defun gficl-app:replace-and-reload (app &rest slots)
  "Hack. after changing SLOTS in the class definition of APP's class,
push these into a running instance of APP."
  (flet ((replace-slots (app &rest slots)
	   (let ((new (make-instance (class-name (class-of app)))))
	     (dolist (slot slots)
	       (setf (slot-value app slot)
		     (slot-value new slot))))))
    (apply #'replace-slots app slots)
    (gficl-app:restart-pipeline app)))

(defpackage "GFICL-SKELETON" (:use "CL"))
(in-package "GFICL-SKELETON")

(defun gficl-app:skeleton (class-name file &key (pkg (concatenate 'string "GFICL-EXAMPLE-APP/" (string class-name))))
  "dump skeleton code for CLASS-NAME in FILE"
  (let* ((lname (string-downcase (string class-name)))
	 (forms
	  `(progn
	     ";;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-"
	     (defpackage ,pkg (:use "CL"))
	     (in-package ,pkg)
	     (defclass ,class-name (gficl-app:base-app-bt)
	       ((vs-source :initform "#version 330
layout (location = 0) in vec2 coord;
layout (location = 1) in vec2 texcoord;
void main () { gl_Position = vec4(coord.xy,0,1); }
")
		(fs-source :initform "#version 330
void main() { gl_FragColor = vec4(0, .7, .7, 1); }")
		(shader :initform nil)
		(vertex-data :initform nil))
	       (:default-initargs
		:title ,lname
		:opengl-debug-context t
		:context-version-major 4
		:context-version-minor 3))
	     (defmethod gficl-app:cleanup-fn ((app ,class-name))
	       (with-slots (shader vertex-data) app
		 (when vertex-data
		   (gficl:delete-gl vertex-data)
		   (setq vertex-data nil))
		 (when shader
		   (gficl:delete-gl shader)
		   (setq shader nil))))
	     (defmethod gficl-app:setup-fn ((app ,class-name))
	       (with-slots (shader vertex-data vs-source fs-source) app
		 (when (and vs-source fs-source)
		   (with-simple-restart (cont "Cont")
		     (setq shader (gficl:make-shader vs-source fs-source))))
		 (setq vertex-data
		       (gficl:make-vertex-data
			(gficl:make-vertex-form
			 (list (gficl:make-vertex-slot 2 :float :vertex-slot-index 0)
			       (gficl:make-vertex-slot 2 :float :vertex-slot-index 1)))
			`(((-1.0 -1.0) (0.0 0.0))
			  ((1.0 -1.0) (1.0 0.0))
			  ((1.0 1.0) (1.0 1.0))
			  ((-1.0 1.0) (0.0 1.0)))
			'(0 3 2 2 1 0)))))
	     (defmethod gficl-app:resize-fn ((app ,class-name) w h)
	       (with-slots (shader) app
		 (gficl:bind-gl shader))
	       (gl:viewport 0 0 w h))
	     (defmethod gficl-app:update-fn ((app ,class-name))
	       (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))
	     (defmethod gficl-app:draw-fn ((app ,class-name))
	       (with-slots (vertex-data shader) app
		 (gficl:bind-gl shader)
		 (gficl:draw-vertex-data vertex-data)))
	     ,(format nil "\#\|\|
(setq $t1 (make-instance '~A))
(gficl-app:launch $t1)
\|\|\#"
		      class-name))))
    (let ((*print-case* :downcase)
	  (*package* (find-package "GFICL-SKELETON")))
      (assert (eql (car forms) 'progn))
      (with-open-file (stream file :direction :output :if-exists :supersede)
	(loop for i from 0 for (form . rest) on (cdr forms)
	      do (if (stringp form)
		     (format stream "~A~&" form)
		     (format stream "~S~&" form))
	      unless (or (< i 2) (endp rest))
	      do (terpri stream))))))
(in-package "GFICL")

#||
(defpackage "GFICL-EXAMPLE-APP/WIKI-MODERN-GRAPH1" (:use "CL"))
(export 'GFICL-EXAMPLE-APP/WIKI-MODERN-GRAPH1::wiki-modern-graph1
	"GFICL-EXAMPLE-APP/WIKI-MODERN-GRAPH1")
(let ((*package* (find-package  "GFICL-EXAMPLE-APP/WIKI-MODERN-GRAPH1")))
  (gficl-app:skeleton 'GFICL-EXAMPLE-APP/WIKI-MODERN-GRAPH1::wiki-modern-graph1 "/dev/shm/1.l"))
(trivial-formatter:fmt-one-file "/dev/shm/1.l" "/dev/shm/2.l")
||#