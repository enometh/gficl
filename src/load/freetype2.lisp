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
  (let ((w (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap $g)))
	(h (ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap $g))))
    (when (and (> w  0) (> h 0))
      (gficl:make-texture w
			  h
			  :format :red
			  :internal-format :red
			  :data
			  (cobj:cobject-pointer (ft2:bitmap-buffer
						 (ft2:glyph-slot-rec-bitmap $g)))
			  :filter :nearest
			  :wrap :clamp-to-edge
			  :format :red))))

;; turns out to be identical to fude-gl glyph
(defstruct char-rec
  texture-id
  char
  w h ;; size of glyph
  bearing-x bearing-y ;; offset from baseline to left/top of glyph
  advance-x
  advance-y
  ;; offset to advance to next glyph in (1/64th of a pixel)
  )

(defun intern-char-rec (c map &key force omit-texture)
  (multiple-value-bind (rec foundp)
      (gethash c map)
    (cond ((and foundp (not force)) rec)
	  (t (load-char $ft-face c)
	     (let ((rec
		    (make-char-rec
		     :char c
		     :texture-id (unless omit-texture
				   (make-tex-for-char))
		     :h (ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap $g))
		     :w (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap $g))
		     :bearing-x (ft2:glyph-slot-rec-bitmap-left $g)
		     :bearing-y (ft2:glyph-slot-rec-bitmap-top $g)
		     :advance-x
		     (ash (ft2:vector-x (ft2:glyph-slot-rec-advance $g)) -6)
		     :advance-y
		     (ash (ft2:vector-y (ft2:glyph-slot-rec-advance $g)) -6)
		     )))
	       (setf (gethash c map) rec)
	       rec)))))


#+nil
(close-ft)

;;#+nil
(unless $ft
  (init-ft
   "/home/madhu/cl/extern/Github/gficl/examples/assets/Roboto-Regular.ttf"))

(defun text-extent (text fmap scale)
  (let ((xpos 0) (max-h -1))
    (loop for char across text
	  for c = (intern-char-rec char fmap)
	  for h = (* (char-rec-h c) scale)
	  for w = (* (char-rec-w c) scale)
	  do (setq max-h (max h max-h))
	  (incf xpos (* scale (char-rec-advance-x c))))
    (values xpos max-h)))

#||
(setq $h (make-hash-table))
(text-extent "the quick brown fox" $h 1)
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defun xcomp2 (a b)
  (mapcar (lambda (a b)
	    (list(append (car a) (car b))))
	  a b))

#||
-w/2,h/2                w/2,h/2
-w/2-h/2                w/2,-h/2

-1,1                1,1
-1,-1               1,-1

0,1                1,1
0,0                1,0

||#

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
  gl_FragColor =    vec4(1, 1, 1, texture2D(tex, texcoord).r) * color;
  //  texture2D(tex, texcoord) * color;;
}")
   (tex :initform nil)
   (vertex-data :initform nil)
   (shader :initform nil)
   (fmap :initform (make-hash-table :test #'equal))
   (buff :initform nil)
   (buflen :initform nil)
   (vertices :initform (xcomp2 '(((-1.0 -1.0))
				 ((1.0 -1.0))
				 ((1.0 1.0))
				 ((-1.0 1.0)))
			       '(((0.0 0.0))
				 ((1.0 0.0))
				 ((1.0 1.0))
				 ((0.0 1.0)))))
   (indices :initform  '(0 3 2 2 1 0))
   (vertex-form :initform
		(gficl:make-vertex-form
		 (list (gficl:make-vertex-slot 4 :float)))))
  (:default-initargs
   :opengl-debug-context t
   :title "font rendering"))

(defmethod gficl-app:cleanup-fn ((app ft01-app))
  (with-slots (tex vertex-data shader fmap buff buflen)
      app
    (when tex
      (gficl:delete-gl tex)
      (setq tex nil))
    (when shader
      (gficl:delete-gl shader)
      (setq shader nil))
    (when vertex-data
      (gficl:delete-gl vertex-data)
      (setq vertex-data nil))
    (when buff
      (cffi:foreign-free buff)
      (setq buff nil))
    (when buflen
      (setq buflen nil))
    (when fmap
      (maphash (lambda (k v)
		 (declare (ignore k))
		 (when  (char-rec-texture-id v)
		   (gficl:delete-gl (char-rec-texture-id v))))
	       fmap)
      (clrhash fmap))))

(defmethod gficl-app:setup-fn ((app ft01-app))
  (gl:clear-color 0.5 0.7 0.8 0)
  ;;Enable blending, necessary for our alpha texture
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (with-slots (tex vertex-data shader vs-source fs-source
	       buff vertex-form vertices indices
	       buflen)
      app
    (setq vertex-data
	  (gficl:make-vertex-data vertex-form vertices indices :dynamic-draw))
    (with-simple-restart (cont "Cont")
      (setq shader (gficl:make-shader vs-source fs-source)))
    (setq buflen (* (length vertices)
		    (gficl::vertex-mem-size vertex-form)))
    (setq buff (gficl::vertex-list-to-array
		vertex-form
		vertices)))
  (gficl-app:resize-fn app
		       (gficl:window-width) (gficl:window-height)))

(defvar *drawing-mode* :text); or ;char

(defmethod gficl-app:resize-fn ((app ft01-app) w h)
  (with-slots (shader) app
    (gficl:bind-gl shader)
    (gficl:bind-matrix shader "projection"
		       (gficl:screen-orthographic-matrix w h))
    (gficl:bind-vec shader "color"
		    (gficl:make-vec '(1 1 1 1))))
    (gl:viewport 0 0 w h))

(defmethod gficl-app:update-fn ((app ft01-app))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defun render-char (app char &key (xpos 0) (ypos 0) (scale 1))
  (with-slots (buff vertex-data buflen vertex-form fmap) app
    (let ((c (intern-char-rec char fmap)))
      (assert c)
      (when (char-rec-texture-id c)
	(gl:bind-texture :texture-2d (gficl:id (char-rec-texture-id c))))
      (let ((h (* (char-rec-h c) scale))
	    (w (* (char-rec-w c) scale))
	    (x (+ xpos (* (char-rec-bearing-x c) scale)))
	    (y (-  ypos
		   (* (- (char-rec-h c) (char-rec-bearing-y c))
		      scale)
		   #+nil
		   (+ (char-rec-h c)))))
	(gficl::vertex-list-to-array
	 vertex-form
	 (xcomp2 `(
		   ;; bottom-left
		   ((,x ,y))
		   ;; bottom-right
		   ((,(+ x w) ,y))
		   ;; top right
		   ((,(+ x w) ,(+ y h)))
		   ;; top-left
		   ((,x ,(+ y h)))
		   )
		 '(((0.0 0.0))
		   ((1.0 0.0))
		   ((1.0 1.0))
		   ((0.0 1.0))))
	 buff)
	(gficl::send vertex-data buff 0 buflen)))))


(defun render-text (app text &key (xpos 0) (ypos 0) (scale 1))
  (with-slots (buff vertex-data buflen vertex-form fmap) app
    (loop for char across text
	  for c = (intern-char-rec char fmap)
	  for h = (* (char-rec-h c) scale)
	  for w = (* (char-rec-w c) scale)
	  for x = (+ xpos (* (char-rec-bearing-x c) scale))
	  for y = (- ypos (* #+nil(- (char-rec-h c) (char-rec-bearing-y c))
			   (char-rec-bearing-y c)
			   scale))
	  do
	  (when (char-rec-texture-id c)
	    (gl:bind-texture :texture-2d (gficl:id (char-rec-texture-id c))))
	  (gficl::vertex-list-to-array
	   vertex-form
	   (xcomp2 `(
		     ;; bottom-left
		     ((,x ,y))
		     ;; bottom-right
		     ((,(+ x w) ,y))
		     ;; top right
		     ((,(+ x w) ,(+ y h)))
		     ;; top-left
		     ((,x ,(+ y h)))
		     )
		   '(((0.0 0.0))
		     ((1.0 0.0))
		     ((1.0 1.0))
		     ((0.0 1.0))))
	   buff)
	  (gficl::send vertex-data buff 0 buflen)
	  (gficl:draw-vertex-data vertex-data)
	  (incf xpos (* scale (char-rec-advance-x c))))))

(defun draw-some-text (app)
  (gl:clear :color-buffer)
  (with-slots (vertex-data) app
    (ecase *drawing-mode*
      (:text(render-text app "the quick brown fox" :ypos 100 :xpos 30 :scale .5)
       (render-text app "jumped over the" :ypos 150 :xpos 30 :scale 1)
       (render-text app "lazy dog" :ypos 200 :xpos 30 :scale 1)
       (render-text app (format nil "location ~S" (gficl:mouse-pos))
		    :ypos 250 :xpos 30 :scale .5))
      (:char
       (when vertex-data
	 (gficl:draw-vertex-data vertex-data))))))

(defmethod gficl-app:draw-fn ((app ft01-app))
  (draw-some-text app))

#||
(setq $app (make-instance 'ft01-app))
(gficl-app:launch $app)
(setq *drawing-mode* :char)
(gficl-app:in-thread-sync $app
  (render-char $app #\h :xpos  0 :ypos  0))
(setq *drawing-mode* :text)
(gficl-app:in-thread-sync $app
  (render-text $app "foo bar 1234" :xpos  100 :ypos  200))
||#
