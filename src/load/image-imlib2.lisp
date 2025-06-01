;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon May 26 11:49:24 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(defpackage "GFICL/LOAD/IMAGE-IMLIB2"
  (:export "IMAGE-IMLIB2")
  (:use "CL"))
(in-package "GFICL/LOAD/IMAGE-IMLIB2")

#||
(load "~/cl/extern/cl-claw-imlib2/claw-cxx-imlib2.system")
(require 'claw-cxx-imlib2.bindings)
(cl-user::package-add-nicknames "CLAW-CXX-IMLIB2" "IMLIB")
(unless (cffi:find-foreign-library "Imlib2")
  (cffi:load-foreign-library "libImlib2.so"))
||#


;; "The image data is returned in the format of a uint32_t (32 bits) per pixel in a linear array ordered from the top left of the image to the bottom right going from left to right each line. Each pixel has the upper 8 bits as the alpha channel and the lower 8 bits are the blue channel - so a pixel's bits are ARGB (from most to least significant,8 bits per channel).,"

(defun vertical-flip (ptr w h)
  "flip rows:
before: top-left = (0 0), bottom-right = (1 1).
after: bottom-left = (0 0), top-right = (1 1)."
  (if (cffi:pointerp ptr)
      (cffi:with-foreign-object (tmp :uint32 w)
	(flet ((memcpy (dst src)
		 (cffi:foreign-funcall
		     "memcpy" :pointer dst
		     :pointer src
		     :int (* w #.(cffi:foreign-type-size :uint32)))))
	  (loop for row below (ash h -1)
		for row0 = (cffi:mem-aptr ptr :uint32 (* row w))
		for row1 = (cffi:mem-aptr ptr :uint32 (* (- h row 1) w))  do
		(memcpy tmp row0)
		(memcpy row0 row1)
		(memcpy row1 tmp))))
        (loop with w = (* w 4)
	      for row below (ash h -1)
	      for row0 =  (* row w)
	      for row1 =  (* (- h row 1) w)
	      do
	      (rotatef (subseq ptr row0 (+ row0 w))
		       (subseq ptr row1 (+ row1 w))))))

(defun argb->rgba (elt)
  (etypecase elt
    (integer
     (let ((a (ldb (byte 8 24) elt))
	   (r (ldb (byte 8 16) elt))
	   (g (ldb (byte 8 8) elt))
	   (b (ldb (byte 8 0) elt))
	   (new-elt 0))
       (setf (ldb (byte 8 0) new-elt) r)
       (setf (ldb (byte 8 8) new-elt) g)
       (setf (ldb (byte 8 16) new-elt) b)
       (setf (ldb (byte 8 24) new-elt) a)
       new-elt))
    (sequence
     (loop for i below (length elt) by 4
	   do (let ((b (elt elt (+ i 0)))
		    (g (elt elt (+ i 1)))
		    (r (elt elt (+ i 2)))
		    (a (elt elt (+ i 3))))
		(setf (elt elt (+ i 0)) r)
		(setf (elt elt (+ i 1)) g)
		(setf (elt elt (+ i 2)) b)
		(setf (elt elt (+ i 3)) a)))
     elt)))

(defun image-imlib2 (path &rest texture-key-args)
  (cffi:with-foreign-object (err :int)
    (let ((image (imlib:load-image-with-errno-return
		  path err)))
      (unless (zerop (cffi:mem-ref err :int))
	(error "Imlib2: failed to load image: load-error: ~A"
	       (imlib:strerror (cffi:mem-ref err :int))))
      (imlib:context-set-image image)
      (unwind-protect
	   (let* ((w (imlib:image-get-width))
		  (h (imlib:image-get-height))
		  (nchan 4)
		  (ptr (imlib:image-get-data)))
	     (loop for i below (* w h)
		   for elt = (cffi:mem-aref ptr :uint32 i)
		   do (setf (cffi:mem-aref ptr :uint32 i)
			    (argb->rgba elt)))
	     (vertical-flip ptr w h)
	     (values (apply #'gficl:make-texture
			    w h
			    :format (gficl:get-image-format nchan)
			    :data ptr
			    texture-key-args)
		     w h))
	(imlib:context-free (imlib:context-get))
	(imlib:free-image)))))
