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
  (:use "CL")
  (:export
   "FT2-APP-SETUP" "FT2-APP-CLEANUP" "FT2-APP-INIT" "FT2-APP-RENDER-CHAR"
   "FT2-APP-RENDER-TEXT"
   "FONT-MAN" "INIT-FONT-MAN" "$FM" "CLOSE-FONT-MAN"
   "FIND-CREATE-FACE"
))
(in-package "GFICL/LOAD/FT2")

#||
(load "/home/madhu/cl/extern/claw-cxx-ft/claw-cxx-ft.bindings.system")
(mk:oos :cffi-object.ops :load)
(mk:oos :claw-cxx-ft.bindings :load)
(mk:oos :claw-cxx-fc.bindings :load)
#+nil
(cffi:foreign-symbol-pointer "FT_Init_FreeType")
||#

(unless (cffi:find-foreign-library "freetype")
  (cffi:load-foreign-library "libfreetype.so"))

(unless (cffi:find-foreign-library "fontconfig")
  (cffi:load-foreign-library "libfontconfig.so"))

(eval-when (load eval compile)
  (user::package-add-nicknames "CLAW-CXX-FT" "FT2"))

(defclass face-rec ()
  ((face :initform nil :documentation "FT_Face CFFI Object")
   (g :initform nil :documentation "FT_Glyph CFFI Object" )
   (size :initform nil)))

(defun close-face-rec (face-rec)
  (with-slots (face g) face-rec
    (when face
      (with-simple-restart (cont "Cont")
	(ft2:done-face face)
	(setq face nil)))
    (when g
      (setq g nil))))

(defun init-face-rec (ft face-rec size truename)
  ;;(declare (notinline ft2:new-face))
  (with-slots (face g (sz size)) face-rec
    (assert (every #'null (list face g)))
    (setq face (cobj:cobject-new 'ft2:face))
    (unless (zerop (ft2:new-face
		    (cobj:wrap-lvalue ft)
		    (namestring truename)
		    0
			  face))
      (error "could not open font ~A" truename))
    (setq g (ft2:face-rec-glyph
	     (cobj:wrap-lvalue face 'ft2:face-rec)))
    (setq sz size)
    (ft2:set-pixel-sizes (cobj:wrap-lvalue face) 0 size)))

(defclass font-man ()
  ((ft :initform nil :documentation "FT_library CFFI Object" :allocation :class)
   (path-to-face-map :initform     ;; actually path-to-faces map
		     (make-hash-table :test #'equal) :allocation :class)))

(defun close-font-man (font-man)
  (with-slots (ft path-to-face-map) font-man
    (maphash (lambda (path face-recs)
	       (declare (ignore path))
	       (dolist (face-rec face-recs)
		 (close-face-rec face-rec)))
	     path-to-face-map)
    (clrhash path-to-face-map)
    (when ft
      (with-simple-restart (cont "Cont")
	(ft2:done-free-type ft))
      (setq ft nil))))

(defun init-font-man (font-man)
  (with-slots (ft) font-man
    (unless ft
      (setq ft (cobj:cobject-new 'ft2:library))
      (unless (zerop (ft2:init-free-type ft))
	(error "could not init freetype library")))))

(defun find-create-face (font-man font-file size)
  "Returns a face-rec object"
  ;;(declare (notinline ft2:new-face))
  (with-slots (ft path-to-face-map) font-man
    (let* ((truename (truename font-file))
	   (elts (gethash truename path-to-face-map)))
      (when elts
	(loop for elt in elts
	      if (= (slot-value elt 'size) size)
	      do (return-from find-create-face elt)))
      (let ((rec (make-instance 'face-rec)))
	(init-face-rec ft rec size truename)
	(push rec elts)
	(setf (gethash truename path-to-face-map) elts)
	rec))))

(defun face-load-char (face-rec char)
  ;;(declare (notinline ft2:load-char))
  (with-slots (face) face-rec
    (unless (zerop (ft2:load-char (cobj:wrap-lvalue face)
				  (etypecase char
				    (number char)
				    (character (char-code char)))
				  ft2:+LOAD-RENDER+))
      (error "Could not load character ~C" char))))

(defun face-get-metrics (face-rec)
  (with-slots (g) face-rec
    (list :h (ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap g))
	  :w (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap g))
	  :bearing-x (ft2:glyph-slot-rec-bitmap-left g)
	  :bearing-y (ft2:glyph-slot-rec-bitmap-top g)
	  :advance-x
	  (ash (ft2:vector-x (ft2:glyph-slot-rec-advance g)) -6)
	  :advance-y
	  (ash (ft2:vector-y (ft2:glyph-slot-rec-advance g)) -6)
	  )))

(defvar $fm (let ((fm (make-instance 'font-man)))
	      (init-font-man fm)
	      fm))
#||
(close-font-man $fm);segfaults ccl
(setq $face (find-create-face
	     $fm
	     "/home/madhu/cl/extern/Github/gficl/examples/assets/Roboto-Regular.ttf"
	     48))
(face-load-char $face #\x)
(face-get-metrics $face)
(with-slots (g) $face
  (list (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap g))
	(ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap g))))
(face-load-char $face #\h)
(face-get-metrics $face)
||#

(defvar *fconfig* nil)

(defun finish-config ()
  (when *fconfig*
    (claw-cxx-fc:%fc-config-destroy *fconfig*)
    (setq *fconfig* nil)))

(defun find-config ()
  (or *fconfig*
      (setq *fconfig* (claw-cxx-fc:%fc-init-load-config-and-fonts))))

(defun match-description (desc)
  (cffi:with-foreign-string (string desc :encoding :utf-8)
    (cffi:with-foreign-objects ((result 'claw-cxx-fc:fc-result)
				(value 'claw-cxx-fc:fc-value))
      (let ((pattern (claw-cxx-fc:%fc-name-parse string))
	    (config (find-config)))
	(claw-cxx-fc:%fc-config-substitute config pattern :fc-match-pattern)
	(claw-cxx-fc:%fc-default-substitute pattern)
	(let ((match (claw-cxx-fc:%fc-font-match config pattern result)))
	  (unless (eql (cffi:mem-ref result 'claw-cxx-fc:fc-result) :fc-result-match)
	    (error "fontconfig failed to find match for ~S" string))
	  (unless (eql (claw-cxx-fc:%fc-pattern-get match
						    claw-cxx-fc:+FC-FILE+ 0 value)
		       :fc-result-match)
	    (error "fontconfig failed to find match for ~S" string))))
      (cffi:foreign-string-to-lisp
       (cffi:foreign-slot-value
	(cffi:foreign-slot-pointer value 'claw-cxx-fc:fc-value 'claw-cxx-fc:u)
	'(:union claw-cxx-fc:c\:@s@-fc-value@u@fontconfig.h@10286)
	'claw-cxx-fc:s)
       :encoding :utf-8))))

#+nil
(time (match-description "Noto Serif Devanagari"))

(defun make-tex-for-char (face-rec)
  (with-slots (g) face-rec
    "make a texture for the currently loaded char"
    ;; (gl:pixel-store :unpack-alignment 1) set for format :red
    (let ((w (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap g)))
	  (h (ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap g))))
      (when (and (> w  0) (> h 0))
	(gficl:make-texture w
			    h
			    :format :red
			    :internal-format :red
			    :data
			    (cobj:cobject-pointer (ft2:bitmap-buffer
						   (ft2:glyph-slot-rec-bitmap g)))
			    :filter :nearest
			    :wrap :clamp-to-edge
			    :format :red)))))

;; turns out to be identical to fude-gl glyph
(defstruct char-rec
  texture-id
  char
  w h ;; size of glyph
  bearing-x bearing-y ;; offset from baseline to left/top of glyph
  advance-x
  advance-y
  ;; offset to advance to next glyph in (1/64th of a pixel)
  size
  )

(defun intern-char-rec (face-rec c map &key force omit-texture &aux rec)
  (with-slots (g size) face-rec
    (multiple-value-bind (recs foundp)
	(gethash c map)
      (when foundp
	(loop for elt in recs
	      if (= (slot-value elt 'size)
		    (slot-value face-rec 'size))
	      return (setq rec elt)))
      (cond ((and rec (not force)) rec)
	    (t (when (and rec force)
		 (let ((id (char-rec-texture-id rec)))
		   (when id (gficl:delete-gl id))))
	       (face-load-char face-rec c)
	       (setq rec
		     (make-char-rec
		      :char c
		      :texture-id (unless omit-texture
				    (make-tex-for-char face-rec))
		      :h (ft2:bitmap-rows (ft2:glyph-slot-rec-bitmap g))
		      :w (ft2:bitmap-width (ft2:glyph-slot-rec-bitmap g))
		      :bearing-x (ft2:glyph-slot-rec-bitmap-left g)
		      :bearing-y (ft2:glyph-slot-rec-bitmap-top g)
		      :advance-x
		      (ash (ft2:vector-x (ft2:glyph-slot-rec-advance g)) -6)
		      :advance-y
		      (ash (ft2:vector-y (ft2:glyph-slot-rec-advance g)) -6)
		      :size size
		      ))
	       (push rec recs)
	       (setf (gethash c map) recs)
	       rec)))))

(defun text-extent (face-rec text fmap scale)
  (let ((xpos 0) (max-h -1))
    (loop for char across text
	  for c = (intern-char-rec face-rec char fmap)
	  for h = (* (char-rec-h c) scale)
	  for w = (* (char-rec-w c) scale)
	  do (setq max-h (max h max-h))
	  (incf xpos (* scale (char-rec-advance-x c))))
    (values xpos max-h)))

#||
(setq $h (make-hash-table))
(text-extent $face "the quick brown fox" $h 1)
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

(eval-when (load eval compile)
  (export '(gficl-app::ft2-mixin-app) :gficl-app))

(defclass  gficl-app:ft2-mixin-app ()
  ((vs-source :initform "#version 330

layout (location = 0) in vec4 coord;
out vec2 texcoord;
uniform mat4 projection;

void main(void) {
    gl_Position = projection * vec4(coord.xy, 0, 1);
    texcoord = coord.zw;
}")
   (fs-source :initform "#version 330

in vec2 texcoord;
uniform sampler2D tex;
uniform vec4 color;

void main(void) {
  gl_FragColor =    vec4(1, 1, 1, texture2D(tex, texcoord).r);// * color;
  //  texture2D(tex, texcoord) * color;;
}")
   (vertex-data :initform nil)
   (shader :initform nil)
   (fmap :initform (make-hash-table :test #'equal))
   (buff :initform nil)
   (buflen :initform nil)
   (vertices :initform (flet ((xcomp2 (a b)
				(mapcar (lambda (a b)
					  (list(append (car a) (car b))))
					a b)))
			 (xcomp2 '(((-1.0 -1.0))
				   ((1.0 -1.0))
				   ((1.0 1.0))
				   ((-1.0 1.0)))
				 '(((0.0 0.0))
				   ((1.0 0.0))
				   ((1.0 1.0))
				   ((0.0 1.0))))))
   (indices :initform  '(0 3 2 2 1 0))
   (vertex-form :initform
		(gficl:make-vertex-form
		 (list (gficl:make-vertex-slot 4 :float))))))

(defun ft2-app-cleanup (app)
  (check-type app gficl-app:ft2-mixin-app)
  (with-slots (vertex-data shader fmap buff buflen)
      app
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
      (maphash (lambda (k vs)
		 (declare (ignore k))
		 (dolist (v vs)
		   (when  (char-rec-texture-id v)
		     (gficl:delete-gl (char-rec-texture-id v)))))
	       fmap)
      (clrhash fmap))))

(defun ft2-app-setup (app)
  (check-type app gficl-app:ft2-mixin-app)
  ;;(gl:clear :color-buffer-bit)
  ;;(gl:clear-color 0.5 0.7 0.8 0)
  ;;(gl:clear-color 1 1 1 0)
  ;;Enable blending, necessary for our alpha texture
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (with-slots (vertex-data shader vs-source fs-source
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
		vertices))
    (gficl:bind-gl shader)
    (let ((loc (gficl:shader-loc shader "tex")))
      (when (/= loc -1)
	(gl:active-texture :texture0)
	(gl:uniformi loc 0)))))

(defun ft2-app-init (app w h)
  (check-type app gficl-app:ft2-mixin-app)
  (with-slots (shader) app
    (gficl:bind-gl shader)
    (gficl:bind-matrix shader "projection"
		       (gficl:screen-orthographic-matrix w h))
    #+nil
    (gficl:bind-vec shader "color"
		    (gficl:make-vec '(1 1 1 1)))))

(defun ft2-app-render-char (app face-rec char &key (xpos 0) (ypos 0) (scale 1))
  (check-type app gficl-app:ft2-mixin-app)
  (with-slots (buff vertex-data buflen vertex-form fmap shader) app
    (let* ((c (intern-char-rec face-rec char fmap)))
      (assert c)
      (gficl:bind-gl shader)
      (gl:active-texture :texture0)
      (when (char-rec-texture-id c)
	(gl:bind-texture :texture-2d (gficl:id (char-rec-texture-id c))))
      (let ((h (char-rec-h c))
	    (w (char-rec-w c))
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

(defun ft2-app-render-text (app face-rec text &key (xpos 0) (ypos 0) (scale 1))
  (check-type app gficl-app:ft2-mixin-app)
  (with-slots (buff vertex-data buflen vertex-form fmap shader) app
    (gficl:bind-gl shader)
    (loop for char across text
	  for c = (intern-char-rec face-rec char fmap)
	  for h = (* (char-rec-h c) scale)
	  for w = (* (char-rec-w c) scale)
	  for x = (+ xpos (* (char-rec-bearing-x c) scale))
	  for y = (- ypos (* #+nil(- (char-rec-h c) (char-rec-bearing-y c))
			   (char-rec-bearing-y c)
			   scale))
	  do
	  (gl:active-texture :texture0)
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



;;; ----------------------------------------------------------------------
;;;
;;;
;;; Example

(defclass ft01-app (gficl-app:ft2-mixin-app gficl-app:base-app-bt)
  ()
  (:default-initargs
   :opengl-debug-context t
   :title "font rendering"))

(defmethod gficl-app:cleanup-fn ((app ft01-app))
  (ft2-app-cleanup app))

(defmethod gficl-app:setup-fn ((app ft01-app))
  (ft2-app-setup app)
  (gficl-app:resize-fn app
		       (gficl:window-width) (gficl:window-height)))

(defmethod gficl-app:resize-fn ((app ft01-app) w h)
  (with-slots (shader) app
    (gficl:bind-gl shader)
    (ft2-app-init app w h)
    (gl:viewport 0 0 w h)))

(defmethod gficl-app:update-fn ((app ft01-app))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))


(defvar *drawing-mode* :text); or ;char

(defun draw-some-text (app face-rec)
  (gl:clear :color-buffer)
  (with-slots (vertex-data) app
    (ecase *drawing-mode*
      (:text (ft2-app-render-text app face-rec "the quick brown fox" :ypos 100 :xpos 30 :scale .5)
       (ft2-app-render-text app face-rec "jumped over the" :ypos 150 :xpos 30 :scale 1)
       (ft2-app-render-text app face-rec "lazy dog" :ypos 200 :xpos 30 :scale 1)
       (ft2-app-render-text app face-rec (format nil "location ~S" (gficl:mouse-pos))
		    :ypos 250 :xpos 30 :scale .5))
      (:char
       (when vertex-data
	 (gficl:draw-vertex-data vertex-data))))))

(defvar $face-rec nil)

(defmethod gficl-app:draw-fn ((app ft01-app))
  (when $face-rec
    (draw-some-text app $face-rec)))

#||
(setq $app (make-instance 'ft01-app))
(gficl-app:launch $app)

(setq $face-rec (find-create-face $fm "/home/madhu/cl/extern/Github/gficl/examples/assets/Roboto-Regular.ttf" 48))
(setq *drawing-mode* :char)
(setq *drawing-mode* :text)
(gficl-app:in-thread-sync $app
  (ft2-app-render-char $app $face-rec #\h :xpos  0 :ypos  0))

(setq $face-rec (find-create-face $fm "/usr/local/share/fonts/local/IBMPlex/IBM-Plex-Sans-Hebrew/IBMPlexSansHebrew-Text.otf" 64))

(gficl-app:in-thread-sync $app
  (ft2-app-render-char $app $face-rec #\א :xpos  0 :ypos  0))

(setq $face-rec (find-create-face $fm "/home/madhu/.fonts/shobhika/Shobhika-Regular.otf" 64))
(gficl-app:in-thread-sync $app
  (ft2-app-render-char $app $face-rec #\ख :xpos  0 :ypos  0))
||#
