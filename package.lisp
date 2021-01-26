;;;; package.lisp

(defpackage #:defconfig
  (:use #:cl)
  (:import-from #:trivial-cltl2 #:compiler-let)
  (:export #:defconfig
	   
	   ;; setters and access functions/macros
	   #:setv
	   #:setv-atomic
	   #:with-atomic-setv
	   #:reset-place
	   #:config-info-search

	   ;; errors
	   #:config-error
	   #:invalid-datum-error
	   #:invalid-coerced-datum-error
	   #:no-config-found-error
	   #:database-already-exists-error

	   ;; database creation and lookup
	   #:define-defconfig-db
	   #:get-db
	   #:get-db-var
	   #:list-dbs
	   #:delete-db))
