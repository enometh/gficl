;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue May 20 15:57:04 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; minimal example using gficl-app:base-app
;;;
(defpackage #:gficl-examples/minimum-app
  (:use :cl))
(in-package :gficl-examples/minimum-app)

(defclass minimum-app (gficl-app:base-app)
  ((vs-source
    :initform "#version 330
layout (location = 0) in vec2 vertex;

void main() {
  gl_Position = vec4(vertex, 0, 1);
}")
   (fs-source
    :initform "#version 330
out vec4 colour;

void main() {
  colour = vec4(gl_FragCoord.x / 500, gl_FragCoord.y / 300, 1, 1);
}")
   (vertices
    :initform '(((0 0.9)) ((-0.9 -0.9)) ((0.9 -0.9))))
   (shader-program
    :initform nil)
   (vertex-data
    :initform nil)))

(defmethod gficl-app:cleanup-fn ((app minimum-app))
  (with-slots (vertex-data shader-program) app
    (when vertex-data
      (gficl:delete-gl vertex-data)
      (setq vertex-data nil))
    (when shader-program
      (gficl:delete-gl shader-program)
      (setq shader-program nil))))

(defmethod gficl-app:setup-fn ((app minimum-app))
  (with-slots (vertex-data
	       vertex-data-form vertices shader-program
	       vs-source fs-source)
      app
    (gficl-app:cleanup-fn app)
    (setq vertex-data
	  (gficl:make-vertex-data
	   (gficl:make-vertex-form (list (gficl:make-vertex-slot 2 :float)))
	   vertices))
    (with-simple-restart (cont "Cont")
      (setq shader-program
	    (gficl:make-shader vs-source fs-source)))
    (gficl:bind-gl shader-program)))

(defmethod gficl-app:draw-fn ((app minimum-app))
  (with-slots (vertex-data shader-program) app
    (gficl:bind-gl shader-program)
    (gl:clear :color-buffer)
    (gficl:draw-vertex-data vertex-data)))

(defmethod gficl-app:update-fn ((app minimum-app))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defun run ()
  (gficl-app:start (make-instance 'minimum-app)))

#||
(run)
(setq $t1 (make-instance 'minimum-app))
(gficl-app:start $t1)
||#
