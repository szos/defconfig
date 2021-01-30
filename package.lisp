;;;; package.lisp

(defpackage #:defconfig
  (:use #:cl)
  (:import-from #:trivial-cltl2 #:compiler-let)
  (:export #:defconfig ; main macro
	   #:*setv-permissiveness*
	   
	   ;; setters and searchers
	   #:setv
	   #:setv-atomic
	   #:with-atomic-setv
	   #:with-atomic-setv*
	   #:reset-place
	   #:reset-computed-place
	   #:clean-previous-value 
	   #:config-info-search
	   #:tag-configurable-place

	   ;; accessors
	   #:config-info-default-value
	   #:config-info-previous-value
           #:config-info-valid-values-description
	   #:config-info-documentation
	   #:config-info-place
	   #:config-info-predicate
	   #:config-info-coercer
           
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
