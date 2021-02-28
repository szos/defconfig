;;;; defconfig.asd

(asdf:defsystem #:defconfig
  :description "A configuration system for user exposed variables"
  :author "szos at posteo dot net"
  :license "GPLv3"
  :version "1.1.2"
  :serial t
  :depends-on (#:alexandria
               #-clisp
               #:trivial-cltl2)
  :components ((:file "package")
	       (:file "macros")
	       (:file "database")
	       (:file "classes")
	       (:file "conditions")
	       (:file "defconfig")
               (:file "setv")
               (:file "access")))

#-clisp
(asdf:defsystem #:defconfig/tests
  :description "test suite for defconfig"
  :author "Your Name <your.name@example.com>"
  :license "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:defconfig)
  :components ((:file "tests/defconfig-tests")))
