;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Aug 12 09:30:08 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; ;madhu 250812 - partial reimplementation of
;;; gitlab.gnome.org:gtk4/demos/gtk-demo/gtkshadertoy.c with verbatim
;;; copying of the shaders therein.  incomplete (e.g. no support for
;;; channels, samplerate).

(in-package :gficl-examples/shadertoy)

(defclass gtk4-demo-shadertoy-app (shadertoy-app)
  ((default-image-shader
       :initform "  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
      // Normalized pixel coordinates (from 0 to 1)
      vec2 uv = fragCoord/iResolution.xy;

     // Time varying pixel color
      vec3 col = 0.5 + 0.5*cos(iTime+uv.xyx+vec3(0,2,4));

      if (distance(iMouse.xy, fragCoord.xy) <= 10.0) {
          col = vec3(0.0);
      }

      // Output to screen
      fragColor = vec4(col,1.0);
  };
")
   (fragment-prefix :initform "  #version 150 core

  uniform vec3      iResolution;           // viewport resolution (in pixels)
  uniform float     iTime;                 // shader playback time (in seconds)
  uniform float     iTimeDelta;            // render time (in seconds)
  uniform int       iFrame;                // shader playback frame
  uniform float     iChannelTime[4];       // channel playback time (in seconds)
  uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
  uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
  uniform sampler2D iChannel0;
  uniform sampler2D iChannel1;
  uniform sampler2D iChannel2;
  uniform sampler2D iChannel3;
  uniform vec4      iDate;                 // (year, month, day, time in seconds)
  uniform float     iSampleRate;           // sound sample rate (i.e., 44100)

  in vec2 fragCoord;
  out vec4 vFragColor;;
")

   (fragment-suffix :initform "void main() {
          vec4 c;
          mainImage(c, fragCoord);
           vFragColor = c;
      };
")
   (image-shader :initform nil)
   ;; locations of uniforms for program
   (resolution-location :initform nil)
   (time-location :initform nil)
   (timedelta-location :initform nil)
   (mouse-location :initform nil)
   (frame-location :initform nil)
   ;; current uniform values
   ;;; (resolution :initform #(0.0 0.0 0.0)) - iresolution in parent class, width, height, 1.0 (screen aspect ration)
   ;;; (time :initform nil) - iglobaltime in parent class
   (timedelta :initform 0.0)
   (mouse :initform #(0.0 0.0 0.0 0.0))
   (frame :initform 0)
   ;; animation data
   (first-frame-time :initform 0)	;uint64
   (first-frame :initform 0)		;uint64, both unused.
   ;;
   (vertex-data :initform  ;; two triangles across whole screen
		'(-1.0 -1.0 0.0 1.0
		  -1.0 1.0 0.0 1.0
		  1.0 1.0 0.0 1.0
		  -1.0 -1.0 0.0 1.0
		  1.0 1.0 0.0 1.0
		  1.0 -1.0 0.0 1.0))
   )

  (:default-initargs
   :title "gtk4-demo-shadertoy-app"
   ;; other slots don't have initargs, and will have dud values
   ;; until replaced
   :vert "  #version 150 core

  uniform vec3 iResolution;

  in vec2 position;
  out vec2 fragCoord;

  void main() {
      gl_Position = vec4(position, 0.0, 1.0);

      // Convert from OpenGL coordinate system (with origin in center
      // of screen) to Shadertoy/texture coordinate system (with origin
      // in lower left corner)
      fragCoord = (gl_Position.xy + vec2(1.0)) / vec2(2.0) * iResolution.xy;
  };
"
   :frag nil))

(defmethod prepare-frag ((app gtk4-demo-shadertoy-app) new-image-shader)
  "For GTK4-DEMO-SHADER-TOY-app use `PREPARE-FRAG' instead of
   `REPLACE-SHADER', as it wraps up the behaviour of the latter.
   NEW-IMAGE-SHADER can be NIL."
  (with-slots (default-image-shader fragment-prefix fragment-suffix
		(main-image-shader image-shader) vert)
      app
    (prog (new-frag)
     again
       (setq new-frag (concatenate 'string fragment-prefix
				   (or new-image-shader default-image-shader)
				   fragment-suffix))
       (restart-case
	   (return (prog1 (replace-shader app vert new-frag)
		     (unless (eql main-image-shader new-image-shader)
		       (setq main-image-shader new-image-shader))))
	 (restore-default-image-shader ()
	   :report "restore default image shader"
	   (setq main-image-shader nil)
	   (go again))))))

(defmethod replace-frag ((app gtk4-demo-shadertoy-app) new-image-shader)
  ;; note: parameter doesn't conform to the generic function.
  (gficl-app:apply-in-thread
   app
   (lambda (app new-image-shader)
     (prepare-frag app new-image-shader)
     (signal 'gficl-app:restart-pipeline))
   app new-image-shader))

(defmethod gficl-app:setup-fn ((app gtk4-demo-shadertoy-app))
  (with-slots (data
	       vertex-data
	       vertex-data-form
	       image-shader
	       shader vert frag)
      app
    (assert (and (not data) (not shader)))
    (prepare-frag app image-shader)
    (setq vertex-data-form
	  (gficl:make-vertex-form (list (gficl:make-vertex-slot 4 :float))))
    (setq data
	  (gficl:make-vertex-data
	   vertex-data-form
	   (loop for (a b c d) on vertex-data by #'cddddr
		 collect (list (list a b c d))))))
  (with-slots (shader
	       resolution-location time-location timedelta-location
	       frame-location mouse-location)
      app
    (setq resolution-location (gficl:shader-loc shader "iResolution"))
    (setq time-location (gficl:shader-loc shader "iTime"))
    (setq timedelta-location (gficl:shader-loc shader "iTimeDelta"))
    (setq frame-location (gficl:shader-loc shader "iFrame"))
    (setq mouse-location (gficl:shader-loc shader "iMouse")))
  (with-slots (first-frame-time first-frame) app
    (setq first-frame-time 0)
    (setq first-frame 0))
  ;; call (gficl:bind-gl shader) via resize-fn (see parent class)
  (gficl-app:resize-fn app (gficl:window-width) (gficl:window-height)))

(defmethod gficl-app:update-fn ((app gtk4-demo-shadertoy-app))
  (with-slots (iglobaltime timedelta frame) app
    (setq iglobaltime (glfw:get-time))
    (setq timedelta (float (gficl::update-frame-time)))
    (incf frame)			;XXXWTF
    )
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defmethod gficl-app:draw-fn ((app gtk4-demo-shadertoy-app))
  (with-slots (data
	       resolution-location iresolution
	       time-location iglobaltime
	       timedelta-location timedelta
	       frame-location frame
	       mouse-location mouse)
      app
    (gl:clear :color-buffer)
    (unless (= resolution-location -1)
      (gl:uniformfv resolution-location (map 'vector 'float iresolution)))
    (unless (= time-location -1)
      (gl:uniformf time-location iglobaltime))
    (unless (= timedelta-location -1)
      (gl:uniformf timedelta-location timedelta))
    (unless (= frame-location -1)
      (gl:uniformi frame-location frame))
    (unless (= mouse-location -1)
      (gl:uniformfv mouse-location (map 'vector 'float mouse)))
    (gficl:draw-vertex-data data)))

;; the following methods on gficl::input-state are f ugly, should we
;; use gficl:mouse-pos in update-fn instead?

(defmethod gficl::update-mouse-pos :after ((state gficl::input-state) x y)
  (when (typep gficl-app::*app* 'gtk4-demo-shadertoy-app)
    (with-slots (mouse iresolution) gficl-app::*app*
      (setf (elt mouse 0) x)
      (setf (elt mouse 1) (-  (elt iresolution 1) y)))))

(defmethod gficl::update-mouse-buttons :after ((state gficl::input-state) button action)
  (when (and (typep gficl-app::*app* 'gtk4-demo-shadertoy-app)
	     (eql button :left))
    (with-slots (mouse iresolution) gficl-app::*app*
      (ecase action
	(:press
	 (setf (elt mouse 2) (elt mouse 0))
	 (setf (elt mouse 3) (elt mouse 1)))
	(:release
	 (setf (elt mouse 2) (- (elt mouse 2)))
	 (setf (elt mouse 3) (- (elt mouse 3))))))))

#||
(setq $t2 (make-instance 'gtk4-demo-shadertoy-app))
(gficl-app:launch $t2)
(gficl-app:cleanup-fn $t2)
(setf (slot-value $t2 'image-shader) (slot-value $t2 'default-image-shader))
(replace-frag $t2 "
void mainImage(out vec4 fragColor, in vec2 fragCoord) { fragColor = vec4(0.0,0.8,0.8,1.0);};
")
(replace-frag $t2 (user::slurp-file "/dev/shm/gtk4/demos/gtk-demo/alienplanet.glsl" nil :element-type 'character))
||#

