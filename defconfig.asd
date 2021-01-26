;;;; defconfig.asd

(asdf:defsystem #:defconfig
  :description "A configuration system for user exposed variables"
  :author "Your Name <your.name@example.com>"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivial-cltl2)
  :components ((:file "package")
               (:file "defconfig")
	       (:file "setv")
	       (:file "access")))

(asdf:defsystem #:defconfig-gui
  :description "A CLIM gui for defconfig"
  :author "Your Name <your.name@example.com>"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:defconfig #:clim #:clim-lisp #:mcclim #:slim)
  :components ((:file "gui/package")
	       (:file "gui/defconfig-gui")))
