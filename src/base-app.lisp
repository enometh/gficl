;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue May 20 15:32:33 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; The GFICL-APP package defines an abstraction over the gficl window
;;; lifecycle.  Subclass GFICL-APP:BASE-APP and define a bunch of
;;; lifecycle methods like GFICL-APP:SETUP-FN, and GFICL-APP:RENDER-FN
;;; on it.  Create an instance with MAKE-INSTANCE and call
;;; GFICL-APP:START on it.
;;;
;;; This is not necessarily a good idea as this exchanges fast
;;; function & macro calls for slow clos method dispatch, even in the
;;; render loop.  Other lisp opengl frameworks seem to go down this
;;; rabbithole with mixed results, so there is motivation to see how
;;; bad an implementation this results in.
;;;
;;; ;madhu 251214 Multiple Windows on GLFW3.  intro.dox states "The
;;; reference documentation for every GLFW function states whether it
;;; is limited to the main thread.  "Initialization, termination,
;;; event processing and the creation and destruction of windows,
;;; cursors and OpenGL and OpenGL ES contexts are all restricted to
;;; the main thread due to limitations of one or several platforms."
;;;
;;; Linux does not seem to be one of these platforms, and we can get
;;; away with running multiple windows each with its own gl context
;;; (made current with cl-glfw3:make-context-current) in separate
;;; threads.  However gficl still uses global state and the variables
;;; *state*, *active-windows*, and *shader-warnings* probably need be
;;; thread local. cl-glfw3:*window* which implicitly used has to be
;;; thread local.


(defpackage "GFICL-APP"
  (:use)
  (:export
   "BASE-APP"
   "SETUP-FN"
   "UPDATE-FN"
   "DRAW-FN"
   "PRE-WINDOW-FN"
   "RESIZE-FN"
   "CLEANUP-FN"
   "START"
   "RESTART-PIPELINE"
   "QUIT"
   "PROCESS-PENDING-EVENTS-STYLE"
   "DISABLE-DRAW-FN"
   "*APPS*"
   ))

(in-package "GFICL")

;; gficl:with-window params
(eval-when (:load-toplevel :compile-toplevel)
(defvar +gficl-with-window-options+
  '((title "window")
    (width 500)
    (height 300)
    (visible t)
    (cursor :normal (member :normal :hidden :disabled))
    (vsync t)
    (opengl-version-major 3)
    (opengl-version-minor 3)
    ;; end comapt options with gficl:start
    ;; beg hints for cl-glfw3:create-window
    (resizable t)
    (decorated t)
    (red-bits 8) (green-bits 8) (blue-bits 8) (alpha-bits 8)
    (depth-bits 24) (stencil-bits 8)
    (accum-red-bits 0) (accum-green-bits 0) (accum-blue-bits 0)
    (accum-alpha-bits 0)
    (aux-buffers 0)
    (samples 0)
    (refresh-rate 0)
    (stereo nil)
    (srgb-capable nil)
    (client-api :opengl-api)
    ;; conflicts with gficl:start :opengl-version-minor, :opengl-version-minor
    ;; context-version-* overrides opengl-version*.
    (context-version-major 3)
    (context-version-minor 3)
    (context-robustness :no-robustness)
    (opengl-forward-compat nil)
    (opengl-debug-context nil)
    (opengl-profile :opengl-any-profile))
  "Alist of gficl window options (name initform [type]")

(defmacro define-class-from-options-alist (class-name options-alist)
  "options-alist is evaluated with EVAL"
  `(defclass ,class-name ()
     ,(loop for (name initform type) in (eval options-alist)
	    for initarg = (intern (symbol-name name) "KEYWORD")
	    collect
	    `(,name :initform ,initform
		    :initarg ,initarg
		    ,@(and type `(:type ,type))))))

(define-class-from-options-alist gficl-window-options-mixin
    +gficl-with-window-options+))

(defclass idle-renderer-mixin ()
  ((disable-draw-fn :initform nil :accessor gficl-app:disable-draw-fn :initarg :disable-draw-fn :type boolean)
   (process-pending-events-style :initform :poll :accessor gficl-app:process-pending-events-style
				 :documentation
				 "One of :poll or :wait or an integer (number of seconds) to pass to glfw:wait-events-timeout."
				 :initarg :process-pending-events-style
				 :type (or (integer 1) (eql :poll) (eql :wait)))))

(defclass gficl-app:base-app (gficl-window-options-mixin
			      idle-renderer-mixin)
  ())

(defgeneric gficl-app:setup-fn (base-app))

(defgeneric gficl-app:update-fn (base-app)
  (:method :before ((base-app gficl-app:base-app))
   (declare (optimize (speed 3)))
   (update-render-state)
   (let ((style (gficl-app:process-pending-events-style base-app)))
     (etypecase style
       ((integer 0 100) (glfw:wait-events-timeout style))
       ((member :poll) (glfw:poll-events))
       ((member :wait) (glfw:wait-events))))))

(defgeneric gficl-app:draw-fn (base-app)
  (:method :after ((base-app gficl-app:base-app))
   (glfw:swap-buffers)))
(defgeneric gficl-app:pre-window-fn (base-app)
  (:method ((base-app gficl-app:base-app))))
(defgeneric gficl-app:resize-fn (base-app w h)
  (:method ((base-app gficl-app:base-app) w h)))
(defgeneric gficl-app:cleanup-fn (base-app)
  (:method ((base-app gficl-app:base-app))))

;; signaling gficl-app:restart-pipeline (in say gficl-app:draw-fn, or
;; gficl-app:upadte-fn) will cause the execution to break out of the
;; gficl-app:start main loop, and restart the pipeline. (by calling
;; gficl-app:setup-fn and re-entering the main loop).
(define-condition gficl-app:restart-pipeline (condition) ())

(define-condition gficl-app:quit (condition) ())

(defun frob-window-options (window-options-mixin keys)
  (when (getf keys :opengl-version-major)
    (setf (getf keys :context-version-major)
	  (getf keys :opengl-version-major)))
  (when (getf keys :opengl-version-minor)
    (setf (getf keys :context-version-minor)
	  (getf keys :opengl-version-minor)))
  ;; complicated defaulting behaviour: prefer supplied keys to slots,
  ;; prefer context-version-* to opengl-version-*
  (loop for (a b) in '((opengl-version-minor context-version-minor)
		       (opengl-version-major context-version-major))
	for x = (slot-value window-options-mixin a)
	for y = (slot-value window-options-mixin b)
	for k = (intern (symbol-name b) :keyword)
	do (unless (getf keys k)
	     (unless (= x y)
	       (warn "preferring :~A ~A instead of conflicting :~A ~A" b y a x))
	     (setf (getf keys k) y)))
  (loop for (indicator default) in +gficl-with-window-options+
	for key = (intern (symbol-name indicator) :keyword)
	for val = (or (getf keys key)
		      (slot-value window-options-mixin indicator))
	append (list key val)))

(defvar gficl-app:*apps* nil "List of running gficl-apps")

;; macroexpands gficl:with-window
(defmethod gficl-app:start ((base-app gficl-app:base-app)
			    &rest keys ;; keys are frobbed from the slots of gficl-window-options-mixin.
			    &key &allow-other-keys
			    &aux (winparams (frob-window-options base-app keys)))
  (with-simple-restart (cont "Cont")
    (assert (not (find base-app gficl-app:*apps*)) nil "already registered"))
  (flet ((pre-window-fn () (gficl-app:pre-window-fn base-app))
	 (resize-fn (w h) (gficl-app:resize-fn base-app w h)))
    (setq gficl::*state* (make-instance 'gficl::render-state
			   :height (getf winparams :height)
			   :width (getf winparams :width)
			   :resize-fn #'resize-fn))
    (setq gficl::*active-objects* 0)
    (setq gficl::*shader-warnings* nil)
    (let ((cl-glfw3::prev-error-fun
	   (cl-glfw3:set-error-callback 'cl-glfw3::default-error-fun)))
      (unless (cffi-sys:null-pointer-p cl-glfw3::prev-error-fun)
	(%cl-glfw3:set-error-callback cl-glfw3::prev-error-fun)))
    (cl-glfw3:initialize)
    (push base-app gficl-app:*apps*)
    (unwind-protect
	 (progn
	   (funcall #'pre-window-fn)
	   (unwind-protect
		(prog nil
		   (apply #'cl-glfw3:create-window
			  (plist-sans-keys winparams
			   :cursor :opengl-version-major :opengl-version-minor
			   :vsync))
		   (gficl::register-glfw-callbacks)
		   (cl-glfw3:set-input-mode :cursor (getf winparams :cursor))
		   (%cl-glfw3:swap-interval
		    (if (getf winparams :vsync) 1 0))
		 reset
		   (gficl-app:setup-fn base-app)
		   (unwind-protect
			(handler-case
			    (loop until (gficl:closedp)
				  do (gficl-app:update-fn base-app)
				  do (cond ((gficl-app:disable-draw-fn base-app)
					    (sleep 1))
					   (t (gficl-app:draw-fn base-app))))
			  (gficl-app:restart-pipeline (c)
			    (declare (ignore c))
			    (go reset))
			  (gficl-app:quit (c)
			    (declare (ignore c))
			    (go quit)))
		     (gficl-app:cleanup-fn base-app))
		 quit
		   (if (not (= 0 gficl::*active-objects*))
		       (format t
			       "~%warning: ~a gl object~:p ~:*~[ ~;was~:;were~] not freed~%"
			       gficl::*active-objects*)))
             (cl-glfw3:destroy-window)
	     (setq gficl-app:*apps* (delete base-app gficl-app:*apps*))))
      (setq gficl::*state* nil)
      (when (endp gficl-app:*apps*)
	(%cl-glfw3:terminate)))))


#+nil ;; if system somehow gets wedged with a stale *state* (unclean
      ;; thread exit)
(setq gficl::*state* nil gficl::*active-objects* 0)

#+nil
(%cl-glfw3:terminate)
