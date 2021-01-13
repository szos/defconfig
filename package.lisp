;;;; package.lisp

(defpackage #:defconfig
  (:use #:cl)
  (:import-from #:trivial-cltl2 #:compiler-let)
  (:export #:defconfig
	   #:setv
           ;; place->config-info
	   #:config-info-search
	   ;; *default-db*
	   #:invalid-datum-error
	   #:invalid-coerced-datum-error
	   #:no-config-found-error
	   #:make-config-database))
