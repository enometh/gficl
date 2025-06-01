(in-package :gficl/load)

(defun text (font-path text &key
		       (font-size 100)
		       (font-dpi 500))
  "Generate a texture containing the supplied string rendered with
the given font. returns values 'texture width height min-x max-y'"
  (multiple-value-bind (pixel-data min-x max-y w h)
		       (ttf:text-pixarray font-path text font-size font-dpi font-dpi)
   (if (not pixel-data) nil
     (values (make-texture-from-pixarray pixel-data)
	     w h min-x max-y))))

;;; Helpers

(defun make-texture-from-pixarray (data)
  (destructuring-bind (h w) (array-dimensions data)
   (cffi:with-foreign-pointer
    (ptr (* w h))
    (loop for x from 0 below w do
	  (loop for y from 0 below h do
		(setf (cffi:mem-aref ptr :unsigned-char
				     (+ (* y w) x))
		      (aref data y x))))
    (gficl:make-texture w h :data ptr :wrap :clamp-to-edge :format :red))))
