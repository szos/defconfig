;;;; defconfig.asd

(asdf:defsystem #:defconfig
  :description "A configuration system for user exposed variables"
  :author "Your Name <your.name@example.com>"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:trivial-cltl2)
  :components ((:file "package")
	       (:file "classes")
	       (:file "conditions")
	       (:file "database")
               (:file "defconfig")
               (:file "setv")
               (:file "access")))

(asdf:defsystem #:defconfig.test
  :description "test suite for defconfig"
  :author "Your Name <your.name@example.com>"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:defconfig)
  :components ((:file "tests/defconfig-tests")))
