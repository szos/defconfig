(defpackage #:defconfig/tests
  (:use #:cl)
  ;; (:local-nicknames (#:am #:fiveam))
  (:import-from #:defconfig #:defconfig #:setv #:with-atomic-setv #:setv-atomic
		#:make-config-database #:reset-place #:with-atomic-setv*
		#:define-defconfig-db #:get-db #:delete-db #:*setv-permissiveness*
		#:define-accessor-config #:define-minimal-config
		#:defconfig-minimal #:psetv)
  (:import-from #:fiveam #:is #:signals))

(in-package :defconfig/tests)

(defparameter *counter* 0)

(defconfig:delete-db :testing t)

(define-defconfig-db *testing-db* :testing)

(defconfig *light-dark* 'dark
  :typespec '(member dark light)
  :db *testing-db*
  :reinitialize t
  :regen-config t)

(defconfig *bounded-number* 0
  :typespec '(integer 0 10)
  :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
  :documentation "A number with the bounds 0 to 10 inclusive"
  :tags '("bounded number" "integer")
  :db *testing-db*
  :reinitialize t
  :regen-config t)

(defconfig *other-bounded-number* 0
  :typespec '(integer 0 10)
  :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
  :documentation "A number with the bounds 0 to 10 inclusive"
  :tags '("bounded number" "integer")
  :db *testing-db*
  :reinitialize t
  :regen-config t)

(defclass testing-class ()
    ((slot1 :initarg :1 :accessor testing-class-slot-1)
     (slot2 :initarg :2 :accessor testing-class-slot-2)))

(defparameter *testing-class* (make-instance 'testing-class :1 1 :2 -2))

(deftype non-positive-integer ()
  '(integer * 0))

(defconfig (testing-class-slot-1)
  :typespec '(integer -10 10)
  :regen-config t)

(define-accessor-config testing-class-slot-2
  :typespec 'non-positive-integer
  :coercer (lambda (x)
	     (if (numberp x)
		 (- x)
		 x))
  :regen-config t)

(defparameter *testing-class* (make-instance 'testing-class :1 0 :2 0))

(fiveam:test makedb
  (fiveam:is (consp *testing-db*))
  (fiveam:is (hash-table-p (car *testing-db*)))
  (fiveam:is (hash-table-p (cdr *testing-db*)))
  (fiveam:is (eq (get-db :testing) *testing-db*)))

(fiveam:test test-defconfig
  (fiveam:is (eql 'dark *light-dark*))
  (fiveam:is (typep (defconfig::place->config-info '*light-dark* :db *testing-db*)
		'defconfig::config-info)))

(fiveam:test test-setv
  (fiveam:is (eql 'dark *light-dark*))
  (let ((defconfig::*setv-permissiveness* :strict))
    (fiveam:signals defconfig::no-config-found-error
      (setv *light-dark* 'light)))
  (fiveam:is (eql (setv *light-dark* 'light
		 :db *testing-db*)
	   'light))
  (fiveam:signals defconfig::invalid-datum-error
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
  (is (eql *light-dark* 'dark))
  (let ((*setv-permissiveness* :permissive))
    (setv *light-dark* "invalid value"))
  (is (string= *light-dark* "invalid value"))
  (setf *light-dark* 'dark)
  (signals defconfig:config-error
    (let ((*setv-permissiveness* :greedy+permissive))
      (setv *light-dark* "invalid value")))
  (is (eql *light-dark* 'dark)))

(fiveam:test fine-grained-w-a-s-signal-setv-wrapped-error
  (unless (= *bounded-number* 0)
    (setv *bounded-number* 0
	  :db *testing-db*))
  (is (= *bounded-number* 0))
  (signals defconfig::setv-wrapped-error
    (with-atomic-setv ()
      (setv *bounded-number* 1
  	    :db *testing-db*)
      (error "testing-error")))
  (is (= *bounded-number* 0))
  (signals defconfig::setv-wrapped-error
    (with-atomic-setv* ()
      (setv *bounded-number* 1
  	    :db *testing-db*)
      (error "testing-error")))
  (is (= *bounded-number* 0)))

(fiveam:test fine-grained-w-a-s-invalid-datum-error
  (setv *bounded-number* 0
	:db *testing-db*)
  (is (= *bounded-number* 0))
  (signals defconfig:invalid-datum-error
    (handler-case (with-atomic-setv ()
		    (setv *bounded-number* 20
			  :db *testing-db*))
      (defconfig::setv-wrapped-error (c)
	(error (defconfig::setv-wrapped-error-condition c)))))
  (is (= *bounded-number* 0))
  (signals defconfig:invalid-datum-error
    (handler-case (with-atomic-setv* ()
		    (setv *bounded-number* 20
			  :db *testing-db*))
      (defconfig::setv-wrapped-error (c)
	(error (defconfig::setv-wrapped-error-condition c)))))
  (is (= *bounded-number* 0)))

(fiveam:test test-with-atomic-setv
  (setv *bounded-number* 0 :db *testing-db*)
  (is (= *bounded-number* 0))
  (signals defconfig::setv-wrapped-error
    (with-atomic-setv ()
      (setv *bounded-number* 1
  	    :db *testing-db*)
      (error "testing-error")))
  (signals defconfig::setv-wrapped-error
    (with-atomic-setv* ()
      (setv *bounded-number* 1
  	    :db *testing-db*)
      (error "testing-error")))
  (is (= *bounded-number* 0))
  (is (= 3
	 (with-atomic-setv ()
	   (setv *bounded-number* 3
		 :db *testing-db*))))
  (is (= 1
	 (with-atomic-setv* ()
	   (setv *bounded-number* 1
		 :db *testing-db*))))
  (signals defconfig:invalid-datum-error
    (handler-case (with-atomic-setv ()
		    (setv *bounded-number* 20
			  :db *testing-db*))
      (defconfig::setv-wrapped-error (c)
	(error (defconfig::setv-wrapped-error-condition c)))))
  (signals defconfig:invalid-datum-error
    (handler-case (with-atomic-setv* ()
		    (setv *bounded-number* 20
			  :db *testing-db*))
      (defconfig::setv-wrapped-error (c)
	(error (defconfig::setv-wrapped-error-condition c))))))

(fiveam:test test-w-a-s-specific-errors
  (with-atomic-setv (:handle-conditions defconfig::config-error)
    (setv *other-bounded-number* 1
	  *bounded-number* 1
	  :db *testing-db*))
  (is (and (equal *bounded-number* 1)
	   (equal *other-bounded-number* 1)))
  (with-atomic-setv* (:handle-conditions defconfig::config-error)
    (setv *other-bounded-number* 2
	  *bounded-number* 2
	  :db *testing-db*))
  (is (and (equal *bounded-number* 2)
	   (equal *other-bounded-number* 2)))
  (setf *bounded-number* 0
	*other-bounded-number* 0)
  (signals simple-error
    (with-atomic-setv (:handle-conditions defconfig::config-error)
      (setv *other-bounded-number* 2
	    *bounded-number* 2
	    :db *testing-db*)
      (error "ahhhh!!!")))
  (is (and (equal *bounded-number* 2)
	   (equal *other-bounded-number* 2)))
  (signals simple-error
    (with-atomic-setv* (:handle-conditions defconfig::config-error)
      (setv *other-bounded-number* 3
	    *bounded-number* 3
	    :db *testing-db*)
      (error "ahhhh!!!")))
  (is (and (equal *bounded-number* 3)
	   (equal *other-bounded-number* 3))))

(fiveam:test test-for-prev-value-with-atomic-setv
  (setv *bounded-number* 0 :db *testing-db*)
  (signals defconfig:config-error
    (with-atomic-setv ()
      (setv *bounded-number* 1
	    :db *testing-db*)
      (setv *bounded-number* 2
	    :db *testing-db*)
      (setv *bounded-number* 50
	    :db *testing-db*)))
  (is (= *bounded-number* 0))
  (signals defconfig:config-error
    (with-atomic-setv* ()
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
  (is (= *bounded-number* 5))
  
  (signals simple-error
    (with-atomic-setv (:handle-conditions defconfig:config-error)
      (setv *bounded-number* 1
	    :db *testing-db*)
      (setv *bounded-number* 2
	    :db *testing-db*)
      (error "simple error")
      (setv *bounded-number* 3
	    :db *testing-db*)
      (setv *bounded-number* 4
	    :db *testing-db*)))
  (is (= *bounded-number* 2))
  (setf *bounded-number* 0)
  (signals simple-error
    (with-atomic-setv* (:handle-conditions defconfig:config-error)
      (setv *bounded-number* 1
	    :db *testing-db*)
      (setv *bounded-number* 2
	    :db *testing-db*)
      (error "simple error")
      (setv *bounded-number* 3
	    :db *testing-db*)
      (setv *bounded-number* 4
	    :db *testing-db*)))
  (is (= *bounded-number* 2)))

(fiveam:test test-setv-atomic
  (is (eql (setv *light-dark* 'dark
		 :db *testing-db*)
	   'dark))
  (signals defconfig:invalid-datum-error
    (setv *light-dark* 'invalid
	  :db *testing-db*)))

(fiveam:test test-coercion
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

(fiveam:test test-accessor-validation
  (setv (testing-class-slot-1 *testing-class*) 2)
  (is (= (testing-class-slot-1 *testing-class*) 2))
  (signals defconfig:invalid-datum-error
    (setv (testing-class-slot-1 *testing-class*) 20))
  (is (= (setv (testing-class-slot-2 *testing-class*) 0) 0))
  (signals defconfig:invalid-datum-error
    (setv (testing-class-slot-2 *testing-class*) "hi"))
  (is (= (setv (testing-class-slot-2 *testing-class*) 8) -8)))

(fiveam:test test-with-atomic-setv-on-accessors
  (with-atomic-setv ()
    (setv (testing-class-slot-1 *testing-class*) 4))
  (is (= (testing-class-slot-1 *testing-class*) 4))

  ;; this will not work with the pure runtime version of w-a-s.
  ;; below this we use w-a-s* which does its computations jointly at runtime
  ;; and macroexpansion time via compiler-let. 
  (signals error
    (with-atomic-setv ()
      (setv (testing-class-slot-1 *testing-class*) 8)
      (error "foo")))
  (is (= (testing-class-slot-1 *testing-class*) 4))
  ;; this was the test that is failing... 

  ;; ;; test with compiler-let w-a-s*
  (setf (testing-class-slot-1 *testing-class*) 0)
  (signals defconfig:config-error
    (with-atomic-setv* ()
      (setv (testing-class-slot-1 *testing-class*) 1)
      (error "foo")))
  (is (= (testing-class-slot-1 *testing-class*) 0)))

(fiveam:test test-w-a-s*
  (let ((*setv-permissiveness* :greedy))
    (with-atomic-setv* ()
      (setv *bounded-number* 1)))
  (is (= *bounded-number* 1)))

(defconfig *var1* 0
  :typespec 'integer)

(defmacro var1 () '*var1*)

(defconfig *var2* 0
  :typespec 'integer)

(defconfig *var3* 0
  :typespec 'integer)

(defconfig *var4* 0
  :typespec 'integer)

(defconfig *var5* 0
  :typespec 'integer)

(fiveam:test test-many-setvs*
  (setf *var1* 0)
  (signals error 
    (with-atomic-setv* ()
      (setv *var1* 1)
      (setv *var2* 1)
      (setv *var3* 1)
      (setv *var4* 1)
      (setv *var5* 1)
      (error "foo")))
  (is (= *var1* 0))
  (is (= *var2* 0))
  (is (= *var3* 0))
  (is (= *var4* 0))
  (is (= *var5* 0)))

(fiveam:test test-side-effects-within-accessor-w-a-s
  (setf *counter* 0)
  (setf (testing-class-slot-1 *testing-class*) 0)
  (with-atomic-setv ()
    (setv (testing-class-slot-1 (progn (incf *counter*)
				       *testing-class*))
	  -9)
    (setv (testing-class-slot-1 (progn (incf *counter*)
				       *testing-class*))
	  -8)
    (setv (testing-class-slot-1 (progn (incf *counter*)
				       *testing-class*))
	  -7))
  (is (= *counter* 3))
  (is (= (testing-class-slot-1 *testing-class*) -7))

  (setf *counter* 0)
  (setv (testing-class-slot-1 *testing-class*) 0)
  (signals defconfig:setv-wrapped-error
    (with-atomic-setv ()
      (setv (testing-class-slot-1 (progn (incf *counter*)
					 *testing-class*))
	    -9)
      (setv (testing-class-slot-1 (progn (incf *counter*)
					 *testing-class*))
	    -8)
      (setv (testing-class-slot-1 (progn (incf *counter*)
					 *testing-class*))
	    -7)
      (error "ohnoerror")))
  (is (= *counter* 3))
  (is (= (testing-class-slot-1 *testing-class*) 0)))

(fiveam:test test-setv-with-macro
  (setf *var1* 0)
  (setf *setv-permissiveness* :strict)
  (signals defconfig:no-config-found-error
    (setv (var1) 1))
  (is (= *var1* 0))
  (setf *setv-permissiveness* :permissive)
  (setv (var1) 1)
  (is (= *var1* 1))
  (setf *var1* 0))

(defconfig-minimal *min* 0
  :typespec '(integer 0 10)
  :coercer (lambda (n)
	     (handler-case (abs n)
	       (error () n)))
  :reinitialize t
  :regen-config t)

(fiveam:test minimal-configs
  (signals defconfig:config-error
    (setv *min* 300))
  (is (= *min* 0)))

(defconfig-minimal *a* 'a
  :typespec 'symbol)

(defconfig-minimal *b* "b"
  :typespec 'string)

(defconfig-minimal *c* 'c
  :typespec 'symbol)

(fiveam:test test-psetv
  (is (and (eql *a* 'a)
           (eql *c* 'c)))
  (psetv *a* *c*
         *c* *a*)
  (is (and (eql *a* 'c)
           (eql *c* 'a)))
  (signals defconfig:invalid-datum-error
    (psetv *a* *c*
           *b* *a*
           *c* *a*))
  (is (and (eql *a* 'a)
           (string= *b* "b")
           (eql *c* 'a)))
  (psetv *a* 'a
         *c* 'c
         *b* "b")
  (is (and (eql *a* 'a)
           (string= *b* "b")
           (eql *c* 'c))))

(fiveam:test test-predicateless
  (defconfig *mytester* 'mytester))
