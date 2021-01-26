;;;; package.lisp

(defpackage #:defconfig-gui
  (:use #:clim #:clim-lisp #:clim-extensions)
  (:import-from #:defconfig
		#:defconfig
		#:define-defconfig-db
		#:get-db
		#:get-db-var
		#:list-dbs
		#:delete-db

		#:config-info-default-value
		#:config-info-prev-value
		#:config-info-valid-values-description
		#:config-info-docstring)
  (:import-from #:climi #:named-color))
