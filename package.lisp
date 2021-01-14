;;;; package.lisp

(defpackage #:defconfig
  (:use #:cl)
  (:import-from #:trivial-cltl2 #:compiler-let)
  (:export #:defconfig
	   #:setv
	   #:setv-atomic
	   #:with-atomic-setv
	   #:config-info-search
	   #:invalid-datum-error
	   #:invalid-coerced-datum-error
	   #:no-config-found-error
	   #:make-config-database
	   #:reset-place))
