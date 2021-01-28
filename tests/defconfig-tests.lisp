(unless (find-package :fiveam)
  (error "Please load fiveam to run the test suite"))

(defpackage #:defconfig.test
  (:use #:cl)
  (:local-nicknames (#:am #:fiveam))
  (:import-from #:defconfig #:defconfig #:setv #:with-atomic-setv #:setv-atomic
		#:config-info-search #:make-config-database #:reset-place
		#:define-defconfig-db #:get-db #:delete-db)
  (:import-from #:fiveam #:is #:signals))

(in-package :defconfig.test)

(am:test clean
  (am:is (or (and (boundp '*testing-db*)
		  (get-db :testing)
		  (delete-db :testing t))
	     t)))

(am:test makedb
  (define-defconfig-db *testing-db* :testing)
  ;; (defvar *testing-db* (make-config-database))
  (am:is (consp *testing-db*))
  (am:is (hash-table-p (car *testing-db*)))
  (am:is (hash-table-p (cdr *testing-db*)))
  (am:is (eq (get-db :testing) *testing-db*)))

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

(am:test fine-grained-w-a-s-signal-setv-wrapped-error
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
      (error "testing-error"))))

(am:test fine-grained-w-a-s-invalid-datum-error
  (defconfig *bounded-number* 0 :typespec '(integer 0 10)
    :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
    :documentation "A number with the bounds 0 to 10 inclusive"
    :tags '("bounded number" "integer") :db *testing-db*
    :reinitialize t :regen-config t)
  (is (= *bounded-number* 0))
  (signals defconfig:invalid-datum-error
    (handler-case (with-atomic-setv ()
		    (setv *bounded-number* 20
			  :db *testing-db*))
      (defconfig::setv-wrapped-error (c)
	(error (defconfig::setv-wrapped-error-condition c))))))

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

(am:test test-w-a-s-specific-errors
  (defconfig *bounded-number* 0 :typespec '(integer 0 10)
    :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
    :documentation "A number with the bounds 0 to 10 inclusive"
    :tags '("bounded number" "integer") :db *testing-db*
    :reinitialize t :regen-config t)
  (defconfig *other-bounded-number* 0 :typespec '(integer 0 10)
    :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
    :documentation "A number with the bounds 0 to 10 inclusive"
    :tags '("bounded number" "integer") :db *testing-db*
    :reinitialize t :regen-config t)
  (with-atomic-setv (:handle-errors (defconfig::config-error))
    (setv *other-bounded-number* 1
	  *bounded-number* 1
	  :db *testing-db*))
  (is (and (equal *bounded-number* 1)
	   (equal *other-bounded-number* 1)))
  (signals simple-error
    (with-atomic-setv (:handle-errors (defconfig::config-error))
      (setv *other-bounded-number* 2
	    *bounded-number* 2
	    :db *testing-db*)
      (error "ahhhh!!!")))
  (is (and (equal *bounded-number* 2)
	   (equal *other-bounded-number* 2))))

(am:test test-setv-atomic
  (is (eql (setv *light-dark* 'dark
		 :db *testing-db*)
	   'dark))
  (signals defconfig:invalid-datum-error
    (setv *light-dark* 'invalid
	  :db *testing-db*)))

(am:test test-coercion
  (is (= (setv *bounded-number* 0
	       :db *testing-db*)
	 0))
  (is (= *bounded-number* 0))
  (is (= (setv *bounded-number* "1"
	       :db *testing-db*)
	 1))
  (is (= *bounded-number* 1))
  (signals defconfig:invalid-coerced-datum-error
    (setv *bounded-number* "20"
	  :db *testing-db*))
  (is (= *bounded-number* 1)))
