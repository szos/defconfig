(defpackage #:defconfig.test
  (:use #:cl)
  (:local-nicknames (#:am #:fiveam))
  (:import-from #:defconfig #:defconfig #:setv #:with-atomic-setv
		#:config-info-search #:make-config-database #:reset-place)
  (:import-from #:fiveam #:is #:signals))

(in-package :defconfig.test)

(unless (find-package :fiveam)
  (error "Please load fiveam to run the test suite"))

(am:test makedb
  (defvar *testing-db* (make-config-database))
  (am:is (listp *testing-db*))
  (am:is (hash-table-p (car *testing-db*)))
  (am:is (hash-table-p (cdr *testing-db*))))

(am:test test-defconfig
  (defconfig *light-dark* 'dark :typespec '(member dark light)
    :reinitialize t :regen-config t :db *testing-db*)
  (am:is (eql 'dark *light-dark*))
  (am:is (typep (defconfig::place->config-info '*light-dark* :db *testing-db*)
		'defconfig::config-info)))

(am:test test-setv
  (am:is (eql 'dark *light-dark*))
  (am:signals defconfig::no-config-found-error
    (setv *light-dark* 'light))
  (am:is (eql (setv *light-dark* 'light
		 :db *testing-db*)
	   'light))
  (am:signals defconfig::invalid-datum-error
    (setv *light-dark* 'neither-light-nor-dark
	  :db *testing-db*))
  (signals defconfig:no-config-found-error
    (setv *light-dark* 'dark)))

(am:test test-with-atomic-setv
  (defconfig *bounded-number* 0 :typespec '(integer 0 10)
    :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
    :documentation "A number with the bounds 0 to 10 inclusive"
    :tags '("bounded number" "integer") :db *testing-db*
    :reinitialize t :regen-config t)
  (is (= *bounded-number* 0))
  (signals defconfig::setv-wrapped-error
    (with-atomic-setv ()
      (setv *bounded-number* 1
  	    :db *testing-db*)
       (error "testing-error")))
  (is (= *bounded-number* 0))
  (is (= 1
	 (with-atomic-setv ()
	   (setv *bounded-number* 1
		 :db *testing-db*))))
  (signals defconfig:invalid-datum-error
    (handler-case (with-atomic-setv ()
		    (setv *bounded-number* 20
			  :db *testing-db*))
      (defconfig::setv-wrapped-error (c)
	(error (defconfig::setv-wrapped-error-condition c))))))
