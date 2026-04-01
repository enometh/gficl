;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Mar 30 03:11:40 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
(defpackage "GFICL/LOAD/FT2"
  (:shadow "VECTOR")
  (:use "CL"))
(in-package "GFICL/LOAD/FT2")

#||
(load "/home/madhu/cl/extern/claw-cxx-ft/claw-cxx-ft.bindings.system")
(require 'claw-cxx-ft.bindings)
#+nil
(cffi:foreign-symbol-pointer "FT_Init_FreeType")
||#

(unless (cffi:find-foreign-library "freetype")
  (cffi:load-foreign-library "libfreetype.so"))

;; use global instance of FT library and faces do thread safety stuff
;; later.

(defvar $ft nil "FT_library CFFI Object")
(defvar $ft-face nil "FT_Face CFFI Object")
(defvar $g nil "FT_GlyphSlot Object: = face->glyph")

(defvar $font-file-ft-face-map (make-hash-table :test #'equal))

(eval-when (load eval compile)
  (user::package-add-nicknames "CLAW-CXX-FT" "FT2"))

(defun close-ft ()
  (when $ft-face
    (with-simple-restart (cont "Cont")
      (ft2:done-face $ft-face))
    (setq $ft-face nil))
  (when $ft
    (with-simple-restart (cont "Cont")
      (ft2:done-free-type $ft))
    (setq $ft nil)))

(defun init-ft (font)
  (declare (notinline ft2:new-face ft2:set-pixel-sizes))
  ;; cannot inline these functions because there no applicable methods
  ;; for cobj::funcall-dynamic-extent-form with args (cobj:wrap-lvalue
  ;; ft-face). also see load-char.
  (assert (not $ft) nil "please (close-ft) before init-ft")
  (setq $ft (cobj:cobject-new 'ft2:library))
  (unless (zerop (ft2:init-free-type $ft))
    (error "could not init freetype library"))
  (assert (not $ft-face))
  (setq $ft-face (cobj:cobject-new 'ft2:face))
  (unless (zerop (ft2:new-face
		  (cobj:wrap-lvalue $ft)
		  font
		  0
		  $ft-face))
    (error "could not open font ~A" font))
  (setq $g (ft2:face-rec-glyph
	    (cobj:wrap-lvalue $ft-face 'ft2:face-rec)))
  (ft2:set-pixel-sizes (cobj:wrap-lvalue $ft-face) 0 48))

(defun load-char (ft-face char)
  (declare (notinline ft2:load-char))
  (unless (zerop (ft2:load-char (cobj:wrap-lvalue ft-face)
				(etypecase char
				  (number char)
				  (character (char-code char)))
				ft2:+LOAD-RENDER+))
      (error "Could not load character ~C" char)))


(defun make-tex-for-char ()
  "make a texture for the currently loaded char"
  ;; (gl:pixel-store :unpack-alignment 1) set for format :red
  (gficl:make-texture (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap $g))
		      (ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap $g))
		      :format :red
		      :internal-format :red
		      :data
		      (cobj:cobject-pointer (ft2:bitmap-buffer
					     (ft2:glyph-slot-rec-bitmap $g)))
		      :filter :nearest
		      :wrap :clamp-to-edge
		      :format :red))

#+nil
(close-ft)

;;#+nil
(unless $ft
  (init-ft
   "/home/madhu/cl/extern/Github/gficl/examples/assets/Roboto-Regular.ttf"))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defclass ft01-app (gficl-app:base-app-bt)
  ((vs-source :initform "#version 330

layout (location = 0) in vec4 coord;
out vec2 texcoord;
uniform mat4 projection;

void main(void) {
    gl_Position = projection * vec4(coord.xy, 0, 1);
    texcoord = coord.zw;
}")
   (fs-source :initform "#version 150

in vec2 texcoord;
uniform sampler2D tex;
uniform vec4 color;

void main(void) {
  gl_FragColor =  // vec4(1, 1, 1, texture2D(tex, texcoord).r) * color;
   texture2D(tex, texcoord);
}")
   (tex :initform nil)
   (vertex-data :initform nil)
   (shader :initform nil)
   (fmap :initform (make-hash-table :test #'equal)))
  (:default-initargs
   :opengl-debug-context t
   :title "font rendering"))

(defun xcomp2 (a b)
  (mapcar (lambda (a b)
	    (list(append (car a) (car b))))
	  a b))

(defun make-quad-f1 ()
  (values
   (xcomp2 '(((1.0  1.0))
	     ((-1.0  1.0))
	     ((-1.0 -1.0))
	     ((1.0 -1.0)))
	   '(((1.0 0.0))
	     ((0.0 0.0))
	     ((0.0 1.0))
	     ((1.0 1.0))))
   '(0 3 2 2 1 0)))

#+nil
(make-quad-f1)

(defun make-vertex-data-for-char ()
  (apply #'gficl:make-vertex-data
	 (gficl:make-vertex-form
	  (list (gficl:make-vertex-slot 4 :float)))
	 (multiple-value-list
	  (apply #'make-quad-f1
		 nil))))

(defmethod gficl-app:cleanup-fn ((app ft01-app))
  (with-slots (tex vertex-data shader fmap) app
    (when tex
      (gficl:delete-gl tex)
      (setq tex nil))
    (when shader
      (gficl:delete-gl shader)
      (setq shader nil))
    (when vertex-data
      (gficl:delete-gl vertex-data)
      (setq vertex-data nil))
    (when fmap
      (clrhash fmap))))


(defmethod gficl-app:setup-fn ((app ft01-app))
  (gl:clear-color 0.5 0.7 0.8 0)
  ;;Enable blending, necessary for our alpha texture
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (with-slots (tex vertex-data shader vs-source fs-source)
      app
    (setq vertex-data (make-vertex-data-for-char))
    (setq tex(make-tex-for-char))
    (with-simple-restart (cont "Cont")
      (setq shader (gficl:make-shader vs-source fs-source))))
  (gficl-app:resize-fn app
		       (gficl:window-width) (gficl:window-height)))

(defmethod gficl-app:resize-fn ((app ft01-app) w h)
  (with-slots (shader) app
    (gficl:bind-gl shader)
    (gficl:bind-matrix shader "projection"
		       (gficl:orthographic-matrix -2 2 -2 2 0 1)))
  (let ((side (min w h)))
    (gl:viewport (+ 0 (/ (- w side) 2))
		 (+ 0 (/ (- h side) 2))
		 side side
		 )))

(defmethod gficl-app:update-fn ((app ft01-app))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defmethod gficl-app:draw-fn ((app ft01-app))
  (with-slots (vertex-data shader tex) app
    (gl:clear :color-buffer)
    (when vertex-data
      (gficl:draw-vertex-data vertex-data))))

#||
(setq $app (make-instance 'ft01-app))
(load-char $ft-face #\a)
(gficl-app:launch $app)
(load-char $ft-face #\b)
(gficl-app:restart-pipeline $app)
||#
