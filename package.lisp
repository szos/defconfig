;;;; package.lisp

(defpackage #:defconfig
  (:use #:cl)
  (:export defconfig
	   setv
           ;; place->config-info
	   config-info-search
	   ;; *default-db*
	   invalid-datum-error
	   invalid-coerced-datum-error
	   no-config-found-error
	   make-config-database))
