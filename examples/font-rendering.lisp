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
  vec3 text_colour = vec3(0.7, 0.95, 0.9);
  colour = vec4(text_colour, text_alpha);
}")

(defparameter *font-path*(GFICL/LOAD::RESOLVE-PATH #p"examples/assets/Roboto-Regular.ttf"))

(defun setup ()
  (gl:clear-color 0.5 0.7 0.8 0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (setf *shader* (gficl:make-shader *vert-shader* *frag-shader*))
  
  (setf *quad* (gficl:make-vertex-data
		(gficl:make-vertex-form
		 (list (gficl:make-vertex-slot 2 :float)))
		'(((0 0)) ((1 0)) ((1 1)) ((0 1))) '(0 3 2 2 1 0)))
  (resize (gficl:window-width) (gficl:window-height))

  (multiple-value-bind
   (tex w h) (gficl/load:text *font-path* "gficl!" :font-size 55 :font-dpi 200)
   (setf *tex* tex)
   (gficl:bind-matrix
    *shader* "model"
    (gficl:*mat
     (gficl:translation-matrix '(140 120 0))
     (gficl:scale-matrix (list w h 1))))))

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
   (:title "font rendering" :width 600 :height 400 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (render))
   (cleanup)))
