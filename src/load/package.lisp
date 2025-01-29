(defpackage :gficl/load
  (:use :cl)
  (:local-nicknames (#:obj #:org.shirakumo.fraf.wavefront)
		    (#:gltf #:org.shirakumo.fraf.gltf)
		    (#:ttf #:truetype-clx))
  (:export
   :model
   :image
   :text
   :shader
   :compute-shader
   #+mk-defsystem *default-system*
   :resolve-path))
