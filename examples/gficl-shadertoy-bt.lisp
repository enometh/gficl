;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu May 15 08:20:20 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; - madhu 250514 - shaders from stacksmith/cepl-shadertoy
;;; - madhu 250520 - rewritten to use gficl-app:base-app lifecycle
;;; - madhu 250811 - moved to gficl-app:base-app-bt lifecycle
;;;
(defpackage #:gficl-examples/shadertoy
  (:use :cl)
  (:export #:run))
(in-package :gficl-examples/shadertoy)

(defclass shadertoy-app (gficl-app:base-app-bt)
  ((vert :initarg :vert
	 :initform "// vertex-stage
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
   (frag :initarg :frag
	 :initform "// fragment-stage
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
   (quad
    :initform
    (list (list (list -1    1  0) (list 0.0 0.0)) ;top-left
	  (list (list -1   -1  0) (list 0.0 1.0)) ;bottom-left
	  (list (list  1   -1  0) (list 1.0 1.0)) ;bottom-right
	  (list (list -1    1  0) (list 0.0 0.0))
	  (list (list  1   -1  0) (list 1.0 1.0))
	  (list (list  1    1  0) (list 1.0 0.0)) ;top-right
	  ))
   (vertex-data-form
    :initform
    (gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :int)
				  (gficl:make-vertex-slot 2 :float))))
   (idate :initform #(0 0 0 0))
   (iglobaltime :initform 0.0)
   (iresolution :initform #(0.0 0.0 0.0))
   (data :initform nil)
   (shader :initform nil))
  (:default-initargs
   :title "gficl shadertoy"
   :width 700
   :height 394))

(defmethod set-idate ((app shadertoy-app))
  "set *iDate* to a v4 containing year month day second"
  (with-slots (idate) app
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time (get-universal-time))
      (declare (ignore minute hour day-of-week dst-p second tz))
      (replace idate (list year month date (glfw:get-time))))))

(defmethod replace-shader ((app shadertoy-app) vert frag)
  "Internal. if shader is successfully compiled from vert and frag
shader sources, the slots shader, vert and frag slots of APP are
updated."
  (with-slots ((main-shader shader) (main-vert vert) (main-frag frag))
      app
    (let (shader)
      (with-simple-restart (cont "Cont")
	(setq shader (gficl:make-shader vert frag)))
      (when shader
	(when main-shader
	  (gficl:delete-gl main-shader))
	(setq main-shader shader)
	(unless (eql main-vert vert)
	  (setq main-vert vert))
	(unless (eql main-frag frag)
	  (setq main-frag frag))))))

(defmethod gficl-app:resize-fn ((app shadertoy-app) w h)
  (with-slots (iresolution shader) app
    (replace iresolution (list w h 0.0))
    (gficl:bind-gl shader)
    (gl:viewport 0 0 w h)))

(defmethod gficl-app:cleanup-fn ((app shadertoy-app))
  (with-slots (data shader) app
    (when shader (gficl:delete-gl shader) (setq shader nil))
    (when data (gficl:delete-gl data) (setq data nil))))

(defmethod gficl-app:setup-fn ((app shadertoy-app))
  ;; (gficl-app:cleanup-fn app) ; NOTE cleanup is called automatically
  ;; via unwind-protect in the main loop via gficl-app:start.
  (with-slots (data shader vertex-data-form quad vert frag) app
    (assert (and (not data) (not shader)))
    (setq data (gficl:make-vertex-data vertex-data-form quad))
    (replace-shader app vert frag))
  ;; call gficl:bind-gl through the resize function
  (gficl-app:resize-fn app (gficl:window-width) (gficl:window-height)))

(defmethod replace-frag ((app shadertoy-app) fs-source)
  (with-slots (vert) app
    (gficl-app:apply-in-thread
     app
     (lambda (app vert frag)
       (replace-shader app vert frag)
       (signal 'gficl-app:restart-pipeline))
     app vert fs-source)))

(defmethod gficl-app:update-fn ((app shadertoy-app))
  (set-idate app)
  (with-slots (iglobaltime) app
    (setq iglobaltime (glfw:get-time)))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defmethod gficl-app:draw-fn ((app shadertoy-app))
  (with-slots (idate iglobaltime iresolution data shader) app
    (gl:clear :color-buffer)
    (gl:uniformfv (gficl:shader-loc shader "IDATE")
		  (map 'vector 'float idate))
    (gl:uniformf (gficl:shader-loc shader "IGLOBALTIME")
		 iglobaltime)
    (gl:uniformfv (gficl:shader-loc shader "IRESOLUTION")
		  iresolution)
    (gficl:draw-vertex-data data)))

(defun run ()
  (gficl-app:launch (make-instance 'shadertoy-app)))

#||
(run)
(setq $t (make-instance 'shadertoy-app))
(gficl-app:shutdown)
(gficl-app:launch $t)
(eq $t gficl-app:*app*)
||#

;; redefine frag to a new fragment shader
#+nil
(replace-frag gficl-app:*app* "// fragment-stage
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
