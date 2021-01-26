(in-package :defconfig)

(define-condition setv-wrapped-error (error)
  ((condition :initarg :error :accessor setv-wrapped-error-condition))
  (:report
   (lambda (c s)
     (with-slots (condition) c
       (format s "WITH-ATOMIC-SETV encountered the error ~S and reset."
	       (type-of condition))))))

(defun remove-keys (list keys)
  "returns two values - accumulated non-keys and keys "
  (labels ((wanted-key-p (thing)
	     (loop for el in keys
		   for keys-key = (if (listp el) (car el) el)
		   when (eql keys-key thing)
		     do (return-from wanted-key-p t)))
	   (churn (list accum-rest accum-keys)
	     (cond ((not list)
		    (values (reverse accum-rest)
			    (reverse accum-keys)))
		   ((wanted-key-p (car list))
		    (churn (cddr list) accum-rest
			   (cons (cadr list)
				 (cons (car list)
				       accum-keys))))
		   (t (churn (cdr list)
			     (cons (car list)
				   accum-rest)
			     accum-keys)))))
    (churn list nil nil)))

(defmacro destructuring-keys ((var &rest keys) list &body body)
  "separates keys from list, storing the remainder in var, and making each key a
variable via destructuring-bind."
  (alexandria:with-gensyms (key-hold)
    `(multiple-value-bind (,var ,key-hold) (remove-keys ,list ',keys)
       (destructuring-bind (&key ,@(loop for k in keys
					 if (listp k)
					   collect (cons (read-from-string
							  (symbol-name (car k)))
							 (cdr k))
					 else 
					   collect (read-from-string
						    (symbol-name k))))
	   ,key-hold
	 ,@body))))

(defmacro %setv-coerced (validity config-info-object place original-value
			 coerced-value)
  `(restart-case
       (if ,validity
	   (progn (psetf (config-info-prev-value ,config-info-object) ,place
			 ,place ,coerced-value)
		  ,coerced-value)
	   (error 'invalid-coerced-datum-error :place ',place
					       :value ,original-value
					       :coerced-value ,coerced-value))
     (set-place-to-coerced-value ()
       :report (lambda (stream)
		 (format stream "Set ~S to ~S" ',place ,coerced-value))
       (psetf (config-info-prev-value ,config-info-object) ,place
	      ,place ,coerced-value)
       ,coerced-value)))

(defmacro %setv-original (validity config-info-object place value)
  (alexandria:with-gensyms (coer-hold coer-valid?)
    `(restart-case
	 (cond (,validity
		(psetf (config-info-prev-value ,config-info-object) ,place
		       ,place ,value)
		,value)
	       ((config-info-coercer ,config-info-object)
		(let* ((,coer-hold
			 (funcall (config-info-coercer ,config-info-object)
				  ,value))
		       (,coer-valid?
			 (funcall (config-info-predicate ,config-info-object)
				  ,coer-hold)))
		  (%setv-coerced ,coer-valid? ,config-info-object
				 ,place ,value ,coer-hold)))
	       (t (error 'invalid-datum-error :place ',place :value ,value)))
       (set-place-to-value ()
	 :report (lambda (s)
		   (format s "Set ~S to ~S" ',place ,value))
	 (psetf (config-info-prev-value ,config-info-object) ,place
		,place ,value)
	 ,value))))

(defmacro %setv (place value db)
  "Validates value against the config-info object looked up in db using place. 
value will be evaluated first, then the config-info object will be looked up.
After that the value will be checked against the registered predicate. If value
is valid, place is set to it, and the config info object will update its 
previous value slot. otherwise coercion is attempted and, if successful, place
gets set to the coerced value. Restarts are put into place "
  (alexandria:with-gensyms (hold hash config-info-object valid?)
    `(let* ((,hold ,value)
	    (,hash ,(if (listp place) `(car ,db) `(cdr ,db)))
	    (,config-info-object
	      ,(if (listp place)
		   `(or (gethash ',place ,hash)
			(gethash ',(car place) ,hash)
			(error 'no-config-found-error :place ',place :db ',db))
		   `(or (gethash ',place ,hash)
			(error 'no-config-found-error :place ',place :db ',db))))
	    (,valid? (funcall (config-info-predicate ,config-info-object) ,hold)))
       (%setv-original ,valid? ,config-info-object ,place ,hold))))

(defmacro setv (&rest args)
  "Setv must get an even number of args - every place must have a value. Setv
can also take the key argument :db, to specify which database to look up config
objects in. "
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn ,@(loop for (p v) on pairs by 'cddr
		    collect `(%setv ,p ,v ,db)))))

(defmacro setv-atomic (&rest args)
  "this version of setv saves the original value of the places being set, and 
resets all to their original value if an error is encountered. the error is then
resignalled"
  (alexandria:with-gensyms (c)
    (multiple-value-bind (pairs db) (remove-keys args '(:db))
      (let ((syms (loop for (p v) on pairs by 'cddr collect (gensym))))
	`(let ,(loop for (place value) on pairs by 'cddr
		     for gensym in syms
		     collect `(,gensym ,place))
	   (handler-case
	       (progn ,@(loop for (place value) on pairs by 'cddr
			      collect `(%setv ,place ,value
					      ,@(if db
						    (cdr db)
						    '(*default-db*)))))
	     (error (,c)
	       ,@(loop for (place value) on pairs by 'cddr
		       for gensym in syms
		       collect `(setf ,place ,gensym))
	       (error ,c))))))))

(defmacro %atomic-setv-reset (&key (pop t))
  "this macro resets all encountered places within a call to with-atomic-setv."
  (declare (special *setv-place-accumulator*))
  (let ((place-list (if pop
			(cdr *setv-place-accumulator*)
			*setv-place-accumulator*)))
    `(progn ,@(loop for (db place) in place-list
		    collect `(reset-place ,place :db ,db :previous-value t)))))

(defmacro %setv-with-reset (block-name place value db)
  "Wrap setv in a handler to catch all errors, which will reset all encountered
 places, after which it returns the condition from the named block."
  (declare (special *setv-place-accumulator*))
  (push (list db place) *setv-place-accumulator*)
  (alexandria:with-gensyms (c)
    `(handler-case (%setv ,place ,value ,db)
       (error (,c)
	 (%atomic-setv-reset)
	 (return-from ,block-name ,c)))))

(defmacro %atomic-setv (block-name &rest args)
  "generates a set of calls to %setv-with-reset."
  (declare (special *setv-place-accumulator*))
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn
       ,@(loop for (p v) on pairs by 'cddr
	       collect `(%setv-with-reset ,block-name ,p ,v ,db)))))

(defmacro with-atomic-setv ((&key (re-error t)) &body body)
  "This macro causes every call to setv to, on an invalid value, reset the place
in question to its previous value."
  (alexandria:with-gensyms (args block-name c inner-c)
    `(compiler-let ((*setv-place-accumulator* nil))
       (let ((,c (block ,block-name
		   (macrolet ((setv (&rest ,args)
				`(%atomic-setv ,',block-name ,@,args)))
		     (handler-case (progn ,@body)
		       (error (,inner-c)
			 (%atomic-setv-reset :pop nil)
			 (return-from ,block-name ,inner-c)))))))
	 (if (and (typep ,c 'error) ,re-error)
	     (error 'setv-wrapped-error :error ,c)
	     ,c)))))
