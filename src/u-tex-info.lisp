(defpackage "GFICL-U-TEX-INFO"
  (:use "CL")
  (:export "U-TEX-INFO" "U-TEX-NAME" "U-TEX" "U-TEX-LOC"
   "U-TEX-RESOLUTION" "U-TEX-RESOLUTION-NAME"
   "U-TEX-RESOLUTION-LOC"
   "U-TEX-INFO-CLEAR" "U-TEX-INFO-INIT"
   "U-TEX-INFO-GL-UNIFORM"))
(in-package "GFICL-U-TEX-INFO")


;; (in-package :gficl-examples/shadertoy)
;; (mapcar 'ccl:slot-definition-name (ccl:class-slots (find-class 'u-tex-info)))

(defclass u-tex-info ()
  ((u-tex-name :initform nil :initarg :name)
   (u-tex :initform nil)
   (u-tex-loc :initform nil)
   (u-tex-resolution :initform #(0.0 0.0))
   (u-tex-resolution-name :initform nil)
   (u-tex-resolution-loc :initform nil)
   (u-tex-path :initform nil :initarg :path)
   (u-tex-info-modified :initform t)))

#+nil
(defun make-clear-form (obj class-name)
  (let ((slot-names (mapcar 'closer-mop:slot-definition-name (closer-mop:class-slots (find-class class-name)))))
    `(with-slots (,slot-names) ,obj
       (setq ,@(loop for s in slot-names append `(,s nil))))))

#+nil
(make-clear-form 'obj 'u-tex-info)

(defun u-tex-info-clear (u)
  (with-slots (u-tex-name u-tex u-tex-loc u-tex-resolution-name
	       u-tex-resolution-loc u-tex-path)
      u
    (when u-tex
      (gficl:delete-gl u-tex))
    (setq u-tex-name nil
	  u-tex nil
	  u-tex-loc nil
	  u-tex-resolution-name nil
	  u-tex-resolution-loc nil
	  u-tex-path nil)))

(defun u-tex-info-init (obj shader-program name path resolution-name)
  (with-slots (u-tex
	       u-tex-name
	       u-tex-resolution
	       u-tex-loc u-tex-resolution-loc u-tex-resolution-name
	       u-tex-path)
      obj
    (setq u-tex-name name u-tex-path path)
    (when u-tex-name
      (setq u-tex-loc (gficl:shader-loc shader-program u-tex-name)))
    (setq u-tex-resolution-name resolution-name)
    (when u-tex-resolution-name
      (setq u-tex-resolution-loc
	    (gficl:shader-loc shader-program u-tex-resolution-name)))
    (when (and u-tex-loc (/= u-tex-loc -1) u-tex-path)
      (multiple-value-bind (tex ww hh)
	  (gficl/load/image-imlib2:image-imlib2 u-tex-path
						:mipmapping nil
						:wrap :clamp-to-border ;:repeat
						:filter :linear	;:nearest
						)
	(setq u-tex tex)
	(replace u-tex-resolution (list ww hh))
	(gficl:bind-gl u-tex)))))

(defun u-tex-info-gl-uniform (u)
  (with-slots (u-tex-loc u-tex-resolution-loc u-tex-resolution u-tex-info-modified) u
    (when u-tex-info-modified
      (when (and u-tex-loc (/= u-tex-loc -1) (/= u-tex-resolution-loc -1))
	(gl:uniformfv u-tex-resolution-loc (subseq u-tex-resolution 0 2)))
      (setq u-tex-info-modified nil))))
