(in-package :defconfig)

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
				 (cons (car list) accum-keys))))
		   (t (churn (cdr list) (cons (car list) accum-rest) accum-keys)))))
    (churn list nil nil)))

(defmacro destructuring-keys ((var &rest keys) list &body body)
  "separates keys from list, storing the remainder in var, and making each key a variable via
destructuring-bind."
  (alexandria:with-gensyms (key-hold)
    `(multiple-value-bind (,var ,key-hold) (remove-keys ,list ',keys)
       (destructuring-bind (&key ,@(loop for k in keys
					 if (listp k)
					   collect (cons (read-from-string
							  (symbol-name (car k)))
							 (cdr k))
					 else 
					   collect (read-from-string (symbol-name k))))
	   ,key-hold
	 ,@body))))

(defmacro %setv (place value db)
  (alexandria:with-gensyms (hold hash config-info valid? coer-hold coer-valid?)
    `(let* ((,hold ,value)
	    (,hash ,(if (listp place) `(car ,db) `(cdr ,db)))
	    (,config-info
	      ,(if (listp place)
		   `(or (gethash ',place ,hash)
			(gethash ',(car place) ,hash)
			(error 'no-config-found-error :place ',place :db ',db))
		   `(or (gethash ',place ,hash)
			(error 'no-config-found-error :place ',place :db ',db))))
	    (,valid? (funcall (config-info-predicate ,config-info) ,hold)))
       (restart-case
	   (cond (,valid? (psetf (config-info-prev-value ,config-info) ,place
				 ,place ,hold))
		 ((config-info-coercer ,config-info)
		  (let* ((,coer-hold (funcall (config-info-coercer ,config-info) ,hold))
			 (,coer-valid? (funcall (config-info-predicate ,config-info) ,coer-hold)))
		    (restart-case 
			(if ,coer-valid?
			    (psetf (config-info-prev-value ,config-info) ,place
				   ,place ,coer-hold)
			    (error 'invalid-coerced-datum-error
				   :place ',place :value ,hold :coerced-value ,coer-hold))
		      (set-place-to-coerced-value ()
			:report (lambda (stream)
				  (format stream "Set ~S to ~S" ',place ,coer-hold))
			(psetf (config-info-prev-value ,config-info) ,place
			       ,place ,coer-hold)))))
		 (t (error 'invalid-datum-error :place ',place :value ,hold)))
	 (set-place-to-value ()
	   :report (lambda (stream)
		     (format stream "Set ~S to ~S" ',place ,hold))
	   (psetf (config-info-prev-value ,config-info) ,place
		  ,place ,hold))))))

(defmacro setv (&rest args)
  "Setv must get an even number of args - every place must have a value"
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn ,@(loop for (p v) on pairs by 'cddr
		    collect `(%setv ,p ,v ,db)))))

(defmacro setv-atomic (&rest args)
  "this version of setv saves the original value of the places being set, and resets all to their original 
value if an error is encountered."
  (multiple-value-bind (pairs db) (remove-keys args '(:db))
    (let ((syms (loop for (p v) on pairs by 'cddr collect (gensym))))
      `(let ,(loop for (place value) on pairs by 'cddr
		   for gensym in syms
		   collect `(,gensym ,place))
	 (handler-case
	     (progn ,@(loop for (place value) on pairs by 'cddr
			    collect `(%setv ,place ,value ,@(if db (cdr db) '(*default-db*)))))
	   (error (c)
	     ,@(loop for (place value) on pairs by 'cddr
		     for gensym in syms
		     collect `(setf ,place ,gensym))
	     c))))))

(defmacro %atomic-setv-reset (db)
  (declare (special *setv-place-accumulator*))
  (let ((place-list *setv-place-accumulator*))
    `(progn ,@(loop for place in (cdr place-list)
		    collect `(reset-place ,place :db ,db :previous-value t)))))

(defmacro %setv-with-reset (block-name place value db)
  (declare (special *setv-place-accumulator*))
  (push place *setv-place-accumulator*)
  `(handler-case `(%setv ,place ,value ,db)
     (error ()
       (%atomic-setv-reset ,db)
       (return-from ,block-name))))

(defmacro %atomic-setv (block-name &rest args)
  (declare (special *setv-place-accumulator*))
  (print *setv-place-accumulator*)
  (loop for (place value) on args by #'cddr
	unless (eql place :db)
	  collect (push place *setv-place-accumulator*))
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn ,@(loop for (p v) on pairs by 'cddr
		    collect `(%setv-with-reset ,block-name ,p ,v ,db)))))

(defmacro with-atomic-setv (&body body)
  (alexandria:with-gensyms (args block-name)
    `(compiler-let ((*setv-place-accumulator* nil))
       (block ;; with-atomic-setv-block
	   ,block-name
	 (macrolet ((setv (&rest ,args)
		      `(%atomic-setv ;; with-atomic-setv-block
			,',block-name
			,@,args)))
	   ,@body)))))

;; (with-atomic-setv
;;   (format t "light~%")
;;   (setv *varname* 'light)
;;   (format t "goodbye~%")
;;   (setv *varname* 'goodbye))
