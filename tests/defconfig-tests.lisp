(unless (find-package :fiveam)
  (error "Please load fiveam to run the test suite"))

(defpackage #:defconfig.test
  (:use #:cl)
  (:local-nicknames (#:am #:fiveam))
  (:import-from #:defconfig #:defconfig #:setv #:with-atomic-setv #:setv-atomic
		#:config-info-search #:make-config-database #:reset-place
		#:define-defconfig-db #:get-db #:delete-db #:*setv-permissiveness*
		#:defaccessor-config)
  (:import-from #:fiveam #:is #:signals))

(in-package :defconfig.test)

(am:test clean
  (am:is (or (and (boundp '*testing-db*)
		  (get-db :testing)
		  (delete-db :testing t))
	     t))
  (am:is (eql (setv *setv-permissiveness* :strict) :strict)))

(am:test makedb
  (define-defconfig-db *testing-db* :testing)
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
  (let ((defconfig::*setv-permissiveness* :strict))
    (am:signals defconfig::no-config-found-error
      (setv *light-dark* 'light)))
  (am:is (eql (setv *light-dark* 'light
		 :db *testing-db*)
	   'light))
  (am:signals defconfig::invalid-datum-error
    (setv *light-dark* 'neither-light-nor-dark
	  :db *testing-db*))
  (is (eql *light-dark* 'light))
  ;; why does (signals warning ...) not allow us to continue? i think we need
  ;; another macro here specifically for warnings - signals expands into a
  ;; handler-bind on the condition and returns from the block prematurely, which
  ;; is fine and dandy if were dealing with errors, but the warning here is meant
  ;; to warn the user but still process the setv, not abort early...
  (signals warning
    (let ((defconfig::*setv-permissiveness* :greedy))
      (setv *light-dark* 'dark)))
  (is (eql *light-dark* 'light))
  (is (eql (let ((defconfig::*setv-permissiveness* :greedy))
	     (setv *light-dark* 'dark))
	   'dark))
  (is (eql *light-dark* 'dark)))

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

(am:test test-for-prev-value-with-atomic-setv
  (defconfig *bounded-number* 0
    :typespec '(integer 0 10)
    :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
    :documentation "A number with the bounds 0 to 10 inclusive"
    :tags '("bounded number" "integer")
    :db *testing-db*
    :reinitialize t :regen-config t)
  (signals defconfig:config-error
    (with-atomic-setv ()
      (setv *bounded-number* 1
	    :db *testing-db*)
      (setv *bounded-number* 2
	    :db *testing-db*)
      (setv *bounded-number* 50
	    :db *testing-db*)))
  (is (= *bounded-number* 0))
  (is (= (setv *bounded-number* 5
	       :db *testing-db*)
	 5))
  (signals defconfig:config-error
    (with-atomic-setv ()
      (setv *bounded-number* 1
	    :db *testing-db*)
      (setv *bounded-number* 2
	    :db *testing-db*)
      (setv *bounded-number* 50
	    :db *testing-db*)))
  (is (= *bounded-number* 5)))

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

(am:test test-accessor-validation
  (defclass testing-class ()
    ((slot1 :initarg :1 :accessor testing-class-slot-1)
     (slot2 :initarg :2 :accessor testing-class-slot-2)))
  (defparameter *testing-class* (make-instance 'testing-class :1 1 :2 -2))
  (defconfig (testing-class-slot-1) 1 :typespec '(integer -10 10)
    :regen-config t)
  (setv (testing-class-slot-1 *testing-class*) 2)
  (is (= (testing-class-slot-1 *testing-class*) 2))
  (signals defconfig:invalid-datum-error
    (setv (testing-class-slot-1 *testing-class*) 20))

  (defaccessor-config (testing-class-slot-2)
    :typespec `(integer ,most-negative-fixnum 0)
    :coercer (lambda (x)
	       (if (numberp x)
		   (- x)
		   x))
    :regen-config t)
  (is (= (setv (testing-class-slot-2 *testing-class*) 0) 0))
  (signals defconfig:invalid-datum-error
    (setv (testing-class-slot-2 *testing-class*) "hi"))
  (is (= (setv (testing-class-slot-2 *testing-class*) 8) -8)))
