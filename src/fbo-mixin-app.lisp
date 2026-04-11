;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Apr 06 17:30:35 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;; mixin fbo-mixin into your subclass of gficl-app to render to an
;;; offloaded framebuffer texture target, via gficl-app:draw-fn. set
;;; slot use-fbo nil to skip this render path. the goal is to use this
;;; facility for rendering text, without rendering all fonts every
;;; time every single render loop..
;;;
;;; also expose fbo-screen-* functions which can be used without
;;; gficl-app

(in-package "GFICL")
(eval-when (load eval compile)
  (export '(gficl-app::fbo-mixin gficl-app::use-fbo) :gficl-app)
  (export '(gficl::fbo-screen-new
	    gficl::fbo-screen-setup
	    gficl::fbo-screen-cleanup
	    gficl::fbo-screen-maybe-init-fbo
	    gficl::fbo-screen-draw-pre
	    gficl::fbo-screen-draw-post)
	  :gficl))

(defclass gficl-app:fbo-mixin ()
  ;; shaders from https://github.com/JoeyDeVries/LearnOpenGL
  ;; /filler/f/build/LearnOpenGL/src/4.advanced_opengl/5.1.framebuffers/framebuffers.cpp
  ((screen-vs-source :initform "#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoords;

out vec2 TexCoords;

void main()
{
    TexCoords = aTexCoords;
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
}
")
   (screen-fs-source :initform "#version 330 core
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D screenTexture;

void main()
{
    vec3 col = texture(screenTexture, TexCoords).rgb;
    FragColor = vec4(col, 1.0);
}
")
   (screen-shader :initform nil)
   (screen-vertex-data :initform nil)
   (screen-fbo :initform nil)
   (screen-vertex-form
    :initform
    (gficl:make-vertex-form
     (list (gficl:make-vertex-slot 2 :float
				   :vertex-slot-index 0)
	   (gficl:make-vertex-slot 2 :float
				   :vertex-slot-index 1))))
   (screen-indices :initform '(0 3 2 2 1 0))
   (screen-vertices
    :initform (flet ((xcomp (a b)
		       (mapcar (lambda (a b)
				 (list (car a) (car b)))
			       a b)))
		(xcomp '(((-1.0 -1.0))
			 ((1.0 -1.0))
			 ((1.0 1.0))
			 ((-1.0 1.0)))
		       '(((0.0 0.0))	 ;0 bottom left
			 ((1.0 0.0))	 ;1 bottom right
			 ((1.0 1.0))	 ;2 top right
			 ((0.0 1.0)))))	 ;3 top-left
    )
   (fbo-initialized-p :initform nil)
   (gficl-app:use-fbo :initform t)
   (test-texture :initform nil :initarg :test-texture)))

;;; write the functionality so we can use this facility without using
;;; gficl-app

(defun fbo-screen-new ()
  (make-instance 'gficl-app:fbo-mixin))

(defun fbo-screen-setup (app)
  (check-type app gficl-app:fbo-mixin)
  (with-slots (screen-vertex-data screen-shader
	       screen-vs-source
	       screen-fs-source
	       screen-vertex-form screen-vertices screen-indices)
      app
    (setq screen-vertex-data
	  (gficl:make-vertex-data
	   screen-vertex-form screen-vertices screen-indices :static-draw))
    (with-simple-restart (cont "Cont")
      (setq screen-shader (gficl:make-shader screen-vs-source
					     screen-fs-source)))))

(defun fbo-screen-cleanup (app)
  (check-type app gficl-app:fbo-mixin)
  (with-slots (screen-shader screen-fbo screen-vertex-data test-texture)
      app
    (when screen-shader
      (gficl:delete-gl screen-shader)
      (setq screen-shader nil))
    (when screen-vertex-data
      (gficl:delete-gl screen-vertex-data)
      (setq screen-vertex-data nil))
    (when test-texture
      (gficl:delete-gl test-texture)
      (setq test-texture nil))
    (when screen-fbo
      (gficl:delete-gl screen-fbo)
      (setq screen-fbo nil))))

(defun fbo-screen-maybe-init-fbo (app w h)
  (check-type app gficl-app:fbo-mixin)
  (with-slots (screen-fbo fbo-initialized-p) app
    (unless (and screen-fbo
		 (equal w (car fbo-initialized-p))
		 (equal h (cdr fbo-initialized-p)))
      (when screen-fbo
	(gficl:delete-gl screen-fbo))
      (setq screen-fbo (gficl:make-framebuffer
			(list (gficl:make-attachment-description
			       :type :texture)
			      (gficl:make-attachment-description
			       :position :depth-stencil-attachment
			       ;; :type :renderbuffer
			       ))
			w h))
      (etypecase fbo-initialized-p
	(null (setq fbo-initialized-p (cons w h)))
	(cons (rplaca fbo-initialized-p w)
	      (rplacd fbo-initialized-p h)))
      ;; gficl:make-framebuffer calls gl:bind-framebuffer
      ;; but we want to do that on demand, so rebind this:
      (progn (gl:bind-framebuffer :framebuffer 0)
	     (gl:bind-renderbuffer :renderbuffer 0)
	     (gl:bind-texture :texture-2d 0)))))

(defun fbo-screen-draw-pre (app)
  "bind to framebuffer and draw scene as we normally would to color texture"
  (check-type app gficl-app:fbo-mixin)
  (with-slots (gficl-app::use-fbo screen-fbo) app
    (cond (gficl-app::use-fbo
	   (gficl:bind-gl screen-fbo)
	   ;; (gl:clear-color .1 .1 .1 1)
	   (gl:clear :color-buffer)
	   (gl:clear :color-buffer-bit :depth-buffer-bit)
	   ;; (gl:enable :depth-test)
	   )
	  (t (gl:bind-framebuffer :framebuffer 0)
	     (gl:bind-renderbuffer :renderbuffer 0)
	     (gl:bind-texture :texture-2d 0)))))

(defun fbo-screen-draw-post (app &key (draw t))
  "now bind back to default framebuffer and draw a quad plane with the attached framebuffer color texture"
  (check-type app gficl-app:fbo-mixin)
  (with-slots (gficl-app:use-fbo screen-fbo screen-vertex-data screen-shader
	       test-texture)
      app
    (when gficl-app:use-fbo
      (gl:bind-framebuffer :framebuffer 0)
      (gl:bind-renderbuffer :renderbuffer 0)
      (gl:bind-texture :texture-2d 0)
      (when draw
	;; (gl:disable :depth-test)
	(gl:clear :color-buffer)
	;; (gl:clear-color 1 1 1 1)
	(gl:clear :color-buffer-bit :depth-buffer-bit)
	(gficl:bind-gl screen-shader)
	(if test-texture
	    (gficl:bind-gl test-texture)
	    (gl:bind-texture :texture-2d
			     (gficl:framebuffer-texture-id screen-fbo 0)))
	(gficl:draw-vertex-data screen-vertex-data)))))

(defmethod gficl-app:cleanup-fn :after ((app gficl-app:fbo-mixin))
  (fbo-screen-cleanup app))

(defmethod gficl-app:resize-fn :around  ((app gficl-app:fbo-mixin) w h)
  (call-next-method)
  (fbo-screen-maybe-init-fbo app w h))

(defmethod gficl-app:setup-fn :after ((app gficl-app:fbo-mixin))
  (fbo-screen-setup app)
  ;; gficl-app:setup-fn generally calls gficl-app:resize-fn, and we
  ;; initialize the framebuffer object there. in case the primary
  ;; method neglects to call it we call it here, and try to be smart
  ;; about allocating a new framebuffer object.
  (gficl-app:resize-fn app
		       (gficl:window-width) (gficl:window-height)))

;; ;madhu 260407 it is retarded to introduce more method dispatch on
;; gficl-app:draw-fn.  we already call glfw:swap-buffers in an after
;; method on base-app (which really ought to be an around method with
;; most-specific last dispatch so it happens at the end). instead of
;; defining before/after or an around method on gficl-app:fbo-mixin,
;; to do the setup and teardown of the renderbuffer around the call on
;; the class we are wrapping, let's define a primary method which
;; calls the next method. this implies gficl-app:fbo-mixin should
;; never be subclassed. and it should appear first in the listing of
;; superclasses to defclass.

(defmethod gficl-app:draw-fn ((app gficl-app:fbo-mixin))
  (fbo-screen-draw-pre app)
  (when (next-method-p) (call-next-method))
  (fbo-screen-draw-post app))

#||
(require 'gficl-examples-bt)
(defclass test (gficl-app:fbo-mixin gficl-examples/minimum-app::minimum-app-bt)
  ())
(setq $a1 (make-instance 'test))
(gficl-app:launch $a1)
(gficl-app:in-thread-sync $a1
  (with-slots (test-texture) $a1
    (setq test-texture
	  nil #+nil
	  (gficl/load/image-imlib2:image-imlib2
	   "/home/madhu/inbox/images/965.jpg"))))
(trace gficl-app:draw-fn :methods t)
(untrace)
(setq $a2 (make-instance 'test :disable-draw-fn t))
(gficl-app:launch $a2)
(setf (slot-value $a2 'gficl-app:use-fbo) t)
(setf (slot-value $a2 'gficl-app:use-fbo) nil)
(gficl-app:in-thread $a2  (glfw:swap-buffers))
(gficl-app:in-thread-sync $a2
  (gl:clear-color 1 1 0  1)
  (gl:clear :color-buffer-bit)
  (glfw:swap-buffers))
(gficl-app:in-thread-sync $a2
  (gficl:bind-gl (slot-value $a2 'gficl-examples/minimum-app::shader-program))
  (gl:clear :color-buffer-bit)
  (gficl:draw-vertex-data
    (slot-value $a2 'gficl-examples/minimum-app::vertex-data))
    (glfw:swap-buffers))
||#