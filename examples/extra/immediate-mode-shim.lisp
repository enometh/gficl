;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Dec 11 12:27:32 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; An immediate mode shim which implements a few immediate mode calls
;;; (gl:begin, gl:vertex etc.) under the core profile.  implemented as
;;; an experiment to run "Kaveh's Common Lisp Lessons" which presently
;;; target the older opengl api.
;;;
;;; OPENGL functions are exported in a new "GL" package. Functions are
;;; provided to switch the immediate mode functions (listed via
;;; (glvnd-list)) in this "GL" package to either their original
;;; "CL-OPENGL" implementations or to our new ad-hoc implementation.
;;; Unfortunately switching the symbols requires recompilation/reload
;;; of the code which uses these symbols
;;;
;;; the naming of the packages, classes, and function names needs
;;; rethink

(defpackage "GFICL-CORE-PROFILE-SHIM"
  (:use "CL")
  (:export
   "CORE-APP" "*APP*" "W"    "SHADER-PROGRAM"
   "MUNGE-GL-PACKAGE" "RESTORE-GL-PACKAGE" "SWITCH-TO-GL2.1" "SWITCH-TO-SHIM"
))
(in-package "GFICL-CORE-PROFILE-SHIM")

;; glvnd
(defpackage "GFICL-CORE-PROFILE-SHIM-SYMBOLS"
  (:export
   "BEGIN"
   "END"
   "VERTEX"
   "COLOR"))

(defclass core-app (gficl-app:base-app-bt)
  ((vs-source
    :initarg :vs-source
    :initform "#version 330 core
layout (location = 0) in vec3 aPos;
void main()
{
gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
}")
   (fs-source
    :initarg :fs-source
    :initform "#version 330 core
uniform vec4 u_color;
out vec4 FragColor;
void main()
{
FragColor = u_color;
}")
   (shader-program :initform nil)
   (vertex-data :initform nil)
   (mode :initform :triangles)
   (vertices :initform nil))
  (:default-initargs
   :context-version-major 4     ;; core profile ensured through default-initargs
   :context-version-minor 3
   :disable-draw-fn t
   :opengl-profile :opengl-core-profile))

(defmethod gficl-app:cleanup-fn ((self core-app))
  (with-slots (vertex-data shader-program) self
    (when shader-program
      (gficl:delete-gl shader-program)
      (setq shader-program nil))
    (when vertex-data
      (gficl:delete-gl vertex-data)
      (setq vertex-data nil))))

(defmethod gficl-app:setup-fn ((self core-app))
  (with-slots (shader-program
	       vs-source fs-source)
      self
      (setq shader-program
	    (gficl:make-shader vs-source fs-source))))

(defmethod gficl-app:draw-fn ((self core-app))
  (error "use :disable-draw-fn t")
  (with-slots (vertex-data shader-program) self
    (when vertex-data
      (gficl:draw-vertex-data vertex-data))))

(defvar *app* nil)

;;;
;;; Default Impl!
;;;
(defmethod gficl-app:update-fn ((app core-app))
  (gficl:map-keys-pressed (:escape (glfw:set-window-should-close))))

(defmethod gficl-app:resize-fn ((app core-app) new-width new-height)
  (let ((min (min new-height new-width)))
    (cl-opengl:viewport 0 0 min min)))

;;;
;;; Default Util
;;;
(defun launch ()
  (if (not *app*)
      (setq *app* (make-instance 'core-app)))
  (gficl-app:launch *app*))

(defun shutdown ()
  (when *app*
    (gficl-app:shutdown *app*)))

;;; use only with disable-draw-fn t
(defmacro w (&body body)
  `(gficl-app:in-thread *app*
     (prog1 (progn ,@body)
       (glfw:swap-buffers))))


;;; ----------------------------------------------------------------------
;;;
;;; IMMEDIATE MODE IMPLEMENTATION
;;;
(defmacro defun-core (fun-name args &body body)
  `(defun ,(find-symbol (symbol-name fun-name) "GFICL-CORE-PROFILE-SHIM-SYMBOLS")
       ,args ,@body))

(defun-core color (r g b &optional (a 1.0d0))
  (with-slots (shader-program) *app*
    (gficl:bind-vec shader-program "u_color"
		    (gficl:make-vec
		     (list r g b a)))))

(defun-core begin (mode)
  (with-slots (vertices (draw-mode mode)) *app*
    (if vertices
	(error "Invalid Operation"))
    (setq draw-mode mode)))

(defun-core vertex (a b c)
  (with-slots (vertices (draw-mode mode)) *app*
    (push (list (list a b c)) vertices)))

(defun-core end ()
  (with-slots (vertex-data vertices (draw-mode mode)) *app*
    (when vertex-data
      (gficl:delete-gl vertex-data)
      (setq vertex-data nil))
    (setq vertex-data (gficl:make-vertex-data
		       (gficl:make-vertex-form
			(list
			 (gficl:make-vertex-slot 3 :float)))
		       vertices))
    (setf (gficl:draw-mode vertex-data) draw-mode)
    (gficl:draw-vertex-data vertex-data)
    (setq vertices nil)))



;;; ----------------------------------------------------------------------
;;;
;;; PACKAGE MUNGING

;;; the GL package. after switching the syms to either OPENGL or
;;; GFICL-CORE-PROFILE-SHIM-SYMBOLS, all the sources that use the
;;; immediate mode functions must be reloaded

;; ;madhu 260407 package munging does not work in lispworks. the
;; assertion of (not (find-package "GL")) fails after deleting the
;; nickname "GL" from "OPENGL".
;;
;; in clozure munge-gl-package fails during the compilation phase:
;; after the rename package form removes the nickname "GL" from the
;; "CL-OPENGL" package, (funcall (compile nil (lambda () (find-package
;; "GL")))) returns the CL-OPENGL package.
;; there a workaround is possible:
;;    #+clozure
;;    (when (find-package "GL")
;;      (ccl:with-lock-grabbed (ccl::*package-refs-lock*)
;;	(remhash "GL"  ccl::*package-refs*)))

(defun munge-gl-package ()
  "initial setup. remove GL as a nickname for CL-OPENGL and make it a new
package which exports all of CL-OPENGL symbols"
  (assert (equal (package-name (find-package "OPENGL")) "CL-OPENGL"))
  (when (equal (find-package "GL") (find-package "CL-OPENGL"))
    (rename-package "CL-OPENGL" "CL-OPENGL"
		    (delete "GL" (package-nicknames "CL-OPENGL") :test #'equal))
    (assert (not (find-package "GL")))
    (make-package "GL" :use nil)
    (loop for sym being each external-symbols of "CL-OPENGL"
	  do (import sym "GL") (export sym "GL"))))

;; munge packages when we load this file
(munge-gl-package)

(defun restore-gl-package ()
  (let ((gl (find-package "GL"))
	(opengl (find-package "CL-OPENGL")))
    (unless (eql gl opengl)
      (delete-package "GL")
      (rename-package "CL-OPENGL" "CL-OPENGL"
		      (adjoin "GL" (package-nicknames "CL-OPENGL")
			      :test #'equal)))))

#||
(loop for sym being each external-symbols of "CL-OPENGL" count 1)
(package-nicknames "CL-OPENGL")
(find-package "GL")
(loop for sym being each external-symbols of "GL" count 1)
(list 'gl:begin)
(restore-gl-package)
(mk:oos :cl-opengl :load)
||#

(defun switch-sym (sym target-pkg)
  "Ensure that GL:SYM is identical to TARGET-PKG:SYM"
  (assert (setq target-pkg (find-package target-pkg)))
  (assert (not (eql target-pkg (find-package "GL"))))
  (let ((symbol-name (if (stringp sym) sym (symbol-name sym))))
    (multiple-value-bind (sym1 status1) (find-symbol symbol-name "GL")
      (when sym1
	(assert (eql status1 :external)))
      (multiple-value-bind (sym2 status2) (find-symbol symbol-name target-pkg)
	(assert sym2)
	(assert (eql status2 :external))
	(unless (eql sym2 sym1)
	  (when sym1
	    (unintern sym1 "GL"))
	  (import sym2 "GL")
	  (export sym2 "GL"))))))

(defun glvnd-list ()
  (loop for s being each external-symbol of "GFICL-CORE-PROFILE-SHIM-SYMBOLS"
	collect (symbol-name s)))

(defun switch-to-gl2.1 ()
  (map nil (lambda (sym) (switch-sym sym "OPENGL")) (glvnd-list)))

(defun switch-to-shim ()
  (map nil (lambda (sym) (switch-sym sym "GFICL-CORE-PROFILE-SHIM-SYMBOLS"))
       (glvnd-list)))

#+nil
(switch-to-shim)

#+nil
(list 'gl:begin)

#+nil
(switch-to-gl2.1)


;;; ----------------------------------------------------------------------
;;;
;;; Lesson-00
;;; from  Kaveh-Common-Lisp-Lessons/lesson-00.lisp (byulparan's clone)
;;; draw a square outline in OpenGL
(defpackage "KAVEH-LESSON-00"
  (:use "CL" "GFICL-CORE-PROFILE-SHIM"))
(in-package "KAVEH-LESSON-00")

(defun draw-square ()
  (gl:color 1.0 1.0 1.0)
  (gl:line-width 3.0)
  (gl:begin :line-loop)
  (gl:vertex  0.5  0.5 0.0)
  (gl:vertex  0.5 -0.5 0.0)
  (gl:vertex -0.5 -0.5 0.0)
  (gl:vertex -0.5  0.5 0.0)
  (gl:end))

(defclass my-opengl-view (core-app)
  ())

(defun run ()
  (unless *app*
    (setq *app* (make-instance 'my-opengl-view :disable-draw-fn nil)))
  (gficl-app:launch *app*))


(defmethod gficl-app:setup-fn :after ((app my-opengl-view))
  (with-slots (shader-program) app
    (gficl:bind-gl shader-program)))

(defmethod gficl-app:draw-fn ((app my-opengl-view))
  (draw-square))

#||
(setq *app* nil)
(munge-gl-package) ; already done when loading
(switch-to-gl2.1)
(list 'gl:begin) ;; => (CL-OPENGL-BINDINGS:BEGIN)
(switch-to-shim)
(list 'gl:begin) ;; => (GFICL-CORE-PROFILE-SHIM-SYMBOLS:BEGIN)
;; re-evaluate draw-square
(run)
(gficl-app:shutdown *app*)
||#



;;; ----------------------------------------------------------------------
;;;
;;; Lesson-01
;;; adapted from  Kaveh-Common-Lisp-Lessons/lesson-00.lisp (byulparan's clone)
;;; draw a square outline in OpenGL
;;;
(defpackage "KAVEH-LESSON-01" (:use "CL" "GFICL-CORE-PROFILE-SHIM"))
(in-package "KAVEH-LESSON-01")

(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())))

(defclass shape () ())

(defgeneric draw (shape))

(defmethod add-shape ((self scene) (s shape))
  (push s (shapes self))
  s)

(defmethod clear-shapes ((self scene))
  (setf (shapes self) '()))

(defmethod draw ((self scene))
  (dolist (s (shapes self))
    (draw s)))

(defclass point ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)))

(defmethod (setf x) (val (self point))
  (setf (slot-value self 'x) (coerce val 'single-float)))

(defmethod (setf y) (val (self point))
  (setf (slot-value self 'y) (coerce val 'single-float)))

(defun p! (x y)
  (make-instance 'point :x (coerce x 'single-float)
			:y (coerce y 'single-float)))

(defmethod p+ ((p1 point) (p2 point))
  (p! (+ (x p1) (x p2))
      (+ (y p1) (y p2))))

(defclass polygon-shape (shape)
  ((is-closed-shape? :accessor is-closed-shape? :initarg :is-closed-shape? :initform t)
   (points :accessor points :initarg :points :initform '())))

(defmethod add-point ((self polygon-shape) (p point))
  (push p (points self)))


(defun make-square-shape (length)
  (let ((v (/ length 2.0)))
    (make-instance 'polygon-shape
		   :points (list (p!    v     v )
				 (p!    v  (- v))
				 (p! (- v) (- v))
				 (p! (- v)    v )))))


;; from lesson-02
(defun make-circle-shape (diameter &optional (num-points 64))
  (let ((radius (/ diameter 2.0))
	(angle-delta (/ (* 2 pi) num-points))
	(shape (make-instance 'polygon-shape)))
    (dotimes (i num-points)
      (let ((angle (* i angle-delta)))
	(add-point shape (p! (* (sin angle) radius) (* (cos angle) radius)))))
    shape))

;; use the immediate mode impl
(defmethod draw ((self polygon-shape))
  (gl:color 1.0 1.0 1.0)
  (gl:line-width 3.0)
  (if (is-closed-shape? self)
      (gl:begin :line-loop)
      (gl:begin :line-strip))
  (dolist (p (points self))
    (gl:vertex (x p) (y p) 0.0))
  (gl:end))

(defun redraw ()
  (gl:flush))

(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

;;
;;
(defclass scene-view (core-app)
  ((scene :accessor scene :initarg :scene :initform nil)))

(defmethod gficl-app:draw-fn ((self scene-view))
  (with-slots ((shader-program shader-program)) self
    (gficl:bind-gl shader-program))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)
  (when (scene self)
    (draw (scene self)))
  (gl:flush))

(defparameter *scene* (make-instance 'scene))

(defun run ()
  (unless *app*
    (setq *app* (make-instance 'scene-view
		  :disable-draw-fn nil
		  :scene *scene*)))
  (gficl-app:launch *app*))

#||
(setq *app* nil)
(run)
(scene *app*)
(defparameter *sq* (make-square-shape 1.0))
(with-redraw
  (clear-shapes *scene*))
  (add-shape *scene*  (make-square-shape 0.8))
  (add-shape *scene* (make-circle-shape 1.0)))
  (redraw))
||#
