;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu May 15 08:20:20 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; - madhu 250514 - shaders from stacksmith/cepl-shadertoy
(defpackage #:gficl-examples/shadertoy
  (:use :cl)
  (:export #:run))

(in-package :gficl-examples/shadertoy)

(defparameter *vert* "// vertex-stage
#version 460

layout(location = 0)  in vec3 fk_vert_position;
layout(location = 1)  in vec2 fk_vert_texture;

out _FROM_VERTEX_STAGE_
{
     out vec2 _VERTEX_STAGE_OUT_1;
} v_out;

void main()
{
    vec4 g_PROG1_TMP4240 = vec4(fk_vert_position,1.0f);
    v_out._VERTEX_STAGE_OUT_1 = fk_vert_texture;
    vec4 g_GEXPR0_4241 = g_PROG1_TMP4240;
    gl_Position = g_GEXPR0_4241;
    return;
}

")

(defparameter *frag*  "// fragment-stage
#version 460

in _FROM_VERTEX_STAGE_
{
     in vec2 _VERTEX_STAGE_OUT_1;
} v_in;

layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

uniform vec4 IDATE;
uniform float IGLOBALTIME;
uniform vec3 IRESOLUTION;

void main()
{
    _FRAGMENT_STAGE_OUT_0 = sin((IDATE.wwzx + vec4(length((gl_FragCoord * 0.1f)))));
    return;
}

")

;; *quad* is a list compatible with g-pt format (vec-3 pos and a vec-2 tex)
(defvar $quad
  (list (list (list -1    1  0) (list 0.0 0.0))
	(list (list -1   -1  0) (list 0.0 1.0))
	(list (list  1   -1  0) (list 1.0 1.0))
	(list (list -1    1  0) (list 0.0 0.0))
	(list (list  1   -1  0) (list 1.0 1.0))
	(list (list  1    1  0) (list 1.0 0.0))))

(defvar $vertex-data-form
  (gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :int)
				(gficl:make-vertex-slot 2 :float))))

(defparameter *iDate* #(0 0 0 0))

(defun set-iDate ()
  "set *iDate* to a v4 containing year month day second"
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore minute hour day-of-week dst-p second tz))
    (replace *iDate* (list year month date (glfw:get-time)))))

(defparameter *iGlobalTime* 0.0)
(defparameter *iResolution* #(0.0 0.0 0.0))

(defvar *shader* nil)

(defun replace-shader (vert frag)
  (let (shader)
    (with-simple-restart (cont "Cont")
      (setq shader (gficl:make-shader vert frag)))
    (when shader
      (when *shader*
	(gficl:delete-gl *shader*))
      (setq *shader* shader))))

(defun resize (w h)
  (replace *iresolution* (list w h 0.0))
  (gficl:bind-gl *shader*)
  (gl:viewport 0 0 w h))

(defvar *recompile* nil
  "Set to T during the main loop to interrupt the main loop and recompile
the shader from current *vert* and *frag* strings. The code which processes
the interrupt should reset this to NIL.")

(defun run ()
  (gficl:with-window (:title "gficl shadertoy" :width 700 :height 394  :resize-callback #'resize)
    (setq *recompile* nil)
    (prog ((*data* nil))
       (declare (special *data*))
     setup
       (unless *shader* (replace-shader *vert* *frag*))
       (unless *data*
	 (setq *data* (gficl:make-vertex-data $vertex-data-form $quad)))
       (resize (gficl:window-width) (gficl:window-height))
       (unwind-protect
	    (loop until (or (gficl:closedp) *recompile*)
		  do (gficl:with-update (dt) ;; update
		       dt
		       (set-idate)
		       (setq *iglobaltime* (glfw:get-time))
		       (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))
		  do (gficl:with-render ;;draw
		       (gl:clear :color-buffer)
		       (gl:uniformfv (gficl:shader-loc *shader* "IDATE")
				     (map 'vector 'float *idate*))
		       (gl:uniformf (gficl:shader-loc *shader* "IGLOBALTIME")
				    *iglobaltime*)
		       (gl:uniformfv (gficl:shader-loc *shader* "IRESOLUTION")
				     *iresolution*)
		       (gficl:draw-vertex-data *data*)))
	 (when *recompile*
	   (format t "RECOMPILE SHADER~&")
	   (replace-shader *vert* *frag*)
	   (setq *recompile* nil)
	   (go setup))
	 (format t "CLEANUP~&")
	 (when *shader* (gficl:delete-gl *shader*) (setq *shader* nil))
	 (when *data* (gficl:delete-gl *data*) (setq *data* nil))))))

#+nil
(run)


;; redefine frag to a new fragment shader
#+nil
(defparameter *frag*  "// fragment-stage
#version 460

in _FROM_VERTEX_STAGE_
{
     in vec2 _VERTEX_STAGE_OUT_1;
} v_in;

layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

uniform vec4 IDATE;
uniform float IGLOBALTIME;
uniform vec3 IRESOLUTION;

void main()
{
    vec2 P = (((2.0f * gl_FragCoord).xy - IRESOLUTION.xy) / IRESOLUTION.y);
    float A = atan(P.x,P.y);
    float R = pow((pow((P.x * P.x),4.0f) + pow((P.y * P.y),4.0f)),(1.0f / 8.0f));
    vec2 UV = vec2(((1.0f / R) + (0.2f * IGLOBALTIME)), A);
    float F = (cos((12.0f * UV.x)) * cos((6.0f * UV.y)));
    vec3 COL = ((0.5f * sin((vec3((F * 3.1416f)) + vec3(0.0f, 0.5f, 1.0f)))) + vec3(0.5f));
    (COL = (COL * R));
    vec4 g_GEXPR0_1808 = vec4(COL.x, COL.y, COL.z, 1.0f);
    _FRAGMENT_STAGE_OUT_0 = g_GEXPR0_1808;
    return;
}

")

;; recompile shader with new *frag*
#+nil
(setq *recompile* t)