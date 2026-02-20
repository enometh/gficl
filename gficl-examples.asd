(require 'asdf)
(in-package :asdf-user)
(defsystem :gficl-examples
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "shadows"
  :entry-point "gficl-examples/shadows:run"
  :depends-on (:gficl
	       :gficl/load)
  :components ((:module "examples"
		:components
		((:file "package")
		 (:file "minimum")
		 (:file "quad-spin")
		 (:file "cube-wave")
		 (:file "post-processing")
		 (:file "model-loading")
		 (:file "font-rendering")
		 (:file "shadows")
		 (:file "pbr")))))
