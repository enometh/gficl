(in-package :gficl-examples/font)

(defparameter *tex* nil)
(defparameter *quad* nil)
(defparameter *shader* nil)

(defparameter *vert-shader*
	      "#version 330
layout (location = 0) in vec2 vertex;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 projection;

void main() {
    TexCoords = vertex;
    gl_Position = projection * model * vec4(vertex, 0, 1);
}")
(defparameter *frag-shader*
  "#version 330

in vec2 TexCoords;
out vec4 colour;

uniform sampler2D tex;

void main() {
  float text_alpha = texture(tex, TexCoords).r;
  if(text_alpha == 0)
    discard;
  vec3 text_colour = vec3(1);
  colour = vec4(text_colour, text_alpha);
}")

(defparameter *font-path* #p"examples/assets/Roboto-Regular.ttf")
(defparameter *character-range* '(32 126))
(defparameter *font-size* 100)
(defparameter *font-dpi* 500)

(defclass text ()
  ((texture :initarg :texture :accessor texture :type gficl:texture)
   (w :initarg :w :type integer)
   (h :initarg :h :type integer)
   (min-x :initarg :min-x :type integer)
   (max-y :initarg :max-y :type integer)))

(defun make-text (texture w h min-x max-y)
  (make-instance 'text :texture texture :w w :h h :min-x min-x :max-y max-y))

(defun make-texture-from-pixarray (data)
  (destructuring-bind
   (h w) (print (array-dimensions data))
   (cffi:with-foreign-pointer
    (ptr (* w h))
    (loop for x from 0 below w do
	  (loop for y from 0 below h do
		(setf (cffi:mem-aref ptr :unsigned-char
				     (+ (* y w) x))
		      (aref data y x))))
    (gficl:make-texture w h :data ptr :wrap :clamp-to-edge :format :red))))

(defun load-text (font-path text &key
		       (font-size 100)
		       (font-dpi 500))
  (multiple-value-bind
   (pixel-data min-x max-y w h)
   (truetype-clx:text-pixarray font-path text font-size font-dpi font-dpi)
   (if (not pixel-data) nil
     (make-text (make-texture-from-pixarray pixel-data) w h min-x max-y))))

(defun setup ()
  (gl:clear-color 0 1 0 0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (setf *shader* (gficl:make-shader *vert-shader* *frag-shader*))
  
  (setf *quad* (gficl:make-vertex-data
		(gficl:make-vertex-form
		 (list (gficl:make-vertex-slot 2 :float)))
		'(((0 0)) ((1 0)) ((1 1)) ((0 1))) '(0 3 2 2 1 0)))
  (resize (gficl:window-width) (gficl:window-height))

  (let ((text (load-text *font-path* "Apples")))
    (setf *tex* (texture text))
    (with-slots (w h) text
      (gficl:bind-matrix
       *shader* "model"
       (gficl:*mat
	(gficl:translation-matrix '(0 0 0))
	(gficl:scale-matrix (list (/ w 4) (/ h 4) 1)))))))

(defun resize (w h)
  (gficl:bind-gl *shader*)
   (gficl:bind-matrix *shader* "projection"
		      (gficl:screen-orthographic-matrix w h)))

(defun cleanup ()
  (gficl:delete-gl *shader*)
  (gficl:delete-gl *tex*)
  (gficl:delete-gl *quad*))

(defun update ()
  (gficl:with-update ()
    (gficl:map-keys-down (:escape (glfw:set-window-should-close)))))

(defun render ()
  (gficl:with-render
   (gl:clear :color-buffer)
   (gficl:bind-gl *shader*)
   (gl:active-texture :texture0)
   (gficl:bind-gl *tex*)
   (gficl:draw-vertex-data *quad*)))

(defun run ()
  (gficl:with-window
   (:title "font rendering" :width 600 :height 400 :resize-callback 'resize)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (render))
   (cleanup)))
