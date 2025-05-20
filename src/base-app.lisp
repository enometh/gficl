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
   "START"))

(in-package "GFICL")

(defclass gficl-app:base-app () ())

(defgeneric gficl-app:setup-fn (base-app))
(defgeneric gficl-app:update-fn (base-app)
  (:method :before ((base-app gficl-app:base-app))
   (update-render-state)
   (glfw:poll-events)))
(defgeneric gficl-app:draw-fn (base-app)
  (:method :after ((base-app gficl-app:base-app))
   (glfw:swap-buffers)))
(defgeneric gficl-app:pre-window-fn (base-app)
  (:method ((base-app gficl-app:base-app))))
(defgeneric gficl-app:resize-fn (base-app w h)
  (:method ((base-app gficl-app:base-app) w h)))
(defgeneric gficl-app:cleanup-fn (base-app)
  (:method ((base-app gficl-app:base-app))))

;; macroexpands gficl:with-window
(defmethod gficl-app:start ((base-app gficl-app:base-app)
			    &key
			    (title "window")
			    (width 500)
			    (height 300)
			    (visible t)
			    (cursor :normal) ;; normal, hidden, disabled
			    (vsync t)
			    (opengl-version-major 3)
			    (opengl-version-minor 3))
  (flet ((pre-window-fn () (gficl-app:pre-window-fn base-app))
	 (resize-fn (w h) (gficl-app:resize-fn base-app w h)))
    (setq gficl::*state* (make-instance 'gficl::render-state
			   :height height :width width
			   :resize-fn #'resize-fn))
    (setq gficl::*active-objects* 0)
    (setq gficl::*shader-warnings* nil)
    (let ((cl-glfw3::prev-error-fun
	   (cl-glfw3:set-error-callback 'cl-glfw3::default-error-fun)))
      (unless (cffi-sys:null-pointer-p cl-glfw3::prev-error-fun)
	(%cl-glfw3:set-error-callback cl-glfw3::prev-error-fun)))
    (cl-glfw3:initialize)
    (unwind-protect
	 (progn
	   (funcall #'pre-window-fn)
	   (unwind-protect
		(progn
		  (cl-glfw3:create-window :title title :width width :height height :visible visible :context-version-major opengl-version-major :context-version-minor opengl-version-minor)
		  (gficl::register-glfw-callbacks)
		  (cl-glfw3:set-input-mode :cursor cursor)
		  (%cl-glfw3:swap-interval
		   (if vsync 1 0))
		  (gficl-app:setup-fn base-app)
		  (loop until (gficl:closedp)
			do (gficl-app:update-fn base-app)
			do (gficl-app:draw-fn base-app))
		  (gficl-app:cleanup-fn base-app)
		  (if (not (= 0 gficl::*active-objects*))
		      (format t
			      "~%warning: ~a gl object~:p ~:*~[ ~;was~:;were~] not freed~%"
			      gficl::*active-objects*)))
             (cl-glfw3:destroy-window)))
      (%cl-glfw3:terminate))))


#+nil ;; if system somehow gets wedged with a stale *state* (unclean
      ;; thread exit)
(setq gficl::*state* nil gficl::*active-objects* 0)

#+nil
(%cl-glfw3:terminate)
