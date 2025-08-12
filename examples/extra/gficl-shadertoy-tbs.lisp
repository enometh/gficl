;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Aug 11 12:04:29 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; ;madhu 250811 - shaders from github.com patriciogonzalezvivo
;;; thebookofshaders, glslCanvas, glslviewer. incomplete (texture
;;; support TBD).

(in-package :gficl-examples/shadertoy)

(defclass tbs-shadertoy-app (shadertoy-app)
  ((texcoords :initform
	      '(0.0 0.0
		1.0 0.0
		0.0 1.0
		0.0 1.0
		1.0 0.0
		1.0 1.0))
   (vertcoords :initform
	       '(-1.0 -1.0
		 1.0 -1.0
		 -1.0 1.0
		 -1.0 1.0
		 1.0 -1.0
		 1.0 1.0))
   (animated :initform nil)
   (ndelta :initform nil)
   (ntime :initform nil)
   (ndate :initform nil)
   (ntextures :initform nil)
   (time-loc :initform nil)
   (resolution-loc :initform nil)
   (mouse-loc :initform nil)
   (mouse :initform #(0.0 0.0)))

  (:default-initargs
   :title "TheBookOfShaders APP"
   :vert "
#if __VERSION__ >= 130
   #define attribute in
   #define varying out
#endif

#ifdef GL_ES
precision mediump float;
#endif

attribute vec2 a_position;
attribute vec2 a_texcoord;

varying vec2 v_texcoord;

void main() {
    gl_Position = vec4(a_position, 0.0, 1.0);
    v_texcoord = a_texcoord;
}
"
   :frag "
#ifdef GL_ES
precision mediump float;
#endif

#if __VERSION__ >= 130
   out vec4 mgl_FragColor;
   #define varying in
 #else
   #define mgl_FragColor gl_FragColor
#endif

varying vec2 v_texcoord;

void main(){
    mgl_FragColor = vec4(0.0);
}
"))

(defmethod gficl-app:setup-fn ((app tbs-shadertoy-app))
  (with-slots (data
	       texcoords vertcoords
	       vertex-data-form
	       shader  vert frag)
      app
    (assert (and (not data) (not shader)))
    (prog nil
     again
       (restart-case
	   (return (replace-shader app vert frag))
	 (reset-frag-shader ()
	   :report "Reset frag shader to dummy"
	   (setq frag "void main(){gl_FragColor = vec4(1.0);}")
	   (go again))))
    (let ((texid (gl:get-attrib-location (gficl:id shader)
					 "a_texcoord"))
	  (vertid (gl:get-attrib-location (gficl:id shader)
					  "a_position")))
      (setq vertex-data-form
	    (gficl:make-vertex-form
	     (list
	      (gficl:make-vertex-slot 2 :float :vertex-slot-index vertid)
	      (gficl:make-vertex-slot 2 :float :vertex-slot-index texid))))
      (setq data
	    (gficl:make-vertex-data
	     vertex-data-form
	     (loop for (a b) on texcoords by #'cddr
		   for (c d) on vertcoords by #'cddr
		   collect (list (list c d) (list a b))))))
    (with-slots (time-loc resolution-loc mouse-loc) app
      (setq time-loc (gficl:shader-loc shader "u_time"))
      (setq resolution-loc (gficl:shader-loc shader "u_resolution"))
      (setq mouse-loc (gficl:shader-loc shader "u_mouse")))
    ;; call (gficl:bind-gl shader) via resize-fn on parent-class.
    (gficl-app:resize-fn app (gficl:window-width) (gficl:window-height))))

(defmethod gficl-app:update-fn ((app tbs-shadertoy-app))
  (with-slots (iglobaltime mouse) app
    (setq iglobaltime (glfw:get-time))
    (destructuring-bind (x y) (gficl:mouse-pos)
      (setf (elt mouse 0) x)
      (setf (elt mouse 1) y)))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defmethod gficl-app:draw-fn ((app tbs-shadertoy-app))
  (with-slots (data iglobaltime iresolution shader
		    mouse
		    time-loc resolution-loc mouse-loc)
      app
    (gl:clear :color-buffer)
    (unless (= time-loc -1) (gl:uniformf time-loc iglobaltime))
    (unless (= resolution-loc -1)
      (gl:uniformfv resolution-loc (subseq iresolution 0 2)))
    (unless (= mouse-loc -1)
      (gl:uniformfv mouse-loc mouse))
    (gficl:draw-vertex-data data)))

#||
(gficl-app:cleanup-fn $t1)
(gficl-app:shutdown)
(setq $t1 (make-instance 'tbs-shadertoy-app))
(eql $t1 gficl-app:*app*)
(gficl-app:launch $t1)
||#

#+nil
(replace-frag $t1
	      "
void main() {
	gl_FragColor = vec4(0.0,0.8,0.8,1.0);
}")

#+nil
(user:string->file (slot-value $t 'frag) "/dev/shm/1.fs")
;; glslViewer /dev/shm/1.fs