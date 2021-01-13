;;;; defconfig.asd

(asdf:defsystem #:defconfig
  :description "Describe defconfig here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivial-cltl2)
  :components ((:file "package")
               (:file "defconfig")
	       (:file "setv")
	       (:file "access")))


