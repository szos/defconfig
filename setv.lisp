(in-package :defconfig)

(defun remove-keys (list keys-to-remove)
  "list must be an even length"
  (let ((return-list nil)
	(removed-keys nil))
    (loop for (k v) on list by 'cddr
	  if (member k keys-to-remove)
	    do (push k removed-keys)
	       (push v removed-keys)
	  else do (push k return-list)
		  (push v return-list))
    (values (reverse return-list)
	    (reverse removed-keys))))

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
	   (cond (,valid? (setf ,place ,hold))
		 ((config-info-coercer ,config-info)
		  (let* ((,coer-hold (funcall (config-info-coercer ,config-info) ,hold))
			 (,coer-valid? (funcall (config-info-predicate ,config-info) ,coer-hold)))
		    (restart-case 
			(if ,coer-valid?
			    (setf ,place ,coer-hold)
			    (error 'invalid-coerced-datum-error
				   :place ',place :value ,hold :coerced-value ,coer-hold))
		      (set-place-to-coerced-value ()
			:report (lambda (stream)
				  (format stream "Set ~S to ~S" ',place ,coer-hold))
			(setf ,place ,coer-hold)))))
		 (t (error 'invalid-datum-error :place ',place :value ,hold)))
	 (set-place-to-value ()
	   :report (lambda (stream)
		     (format stream "Set ~S to ~S" ',place ,hold))
	   (setf ,place ,hold))))))

(defmacro setv (&rest args)
  "Setv must get an even number of args - every place must have a value"
  (multiple-value-bind (pairs database) (remove-keys args '(:db))
    `(progn ,@(loop for (place value) on pairs by 'cddr
		    collect `(%setv ,place ,value ,@(if database (cdr database) '(*default-db*)))))))

(defmacro setv2 (&rest args)
  (let ((realargs (if (equal (car args) :atomic)
		      ())
		  )
	))
  (multiple-value-bind (pairs database) (remove-keys args '(:db))
    `(progn ,@(loop for (place value) on pairs by 'cddr
		    collect `(%setv ,place ,value ,@(if database (cdr database) '(*default-db*)))))))

;; (defmacro with-atomic-setv (&body body)
;;   (alexandria:with-gensyms (settings args pairs db place value))
;;   `(let ((,settings nil))
;;      (macrolet ((setv (&rest ,args)
;; 		  (multiple-value-bind (,pairs ,db) (remove-keys ,args '(:db))
;; 		    `(progn ,@(loop for (,place ,value) on ,pairs by 'cddr
;; 				    collect `(progn ,(push (list 'place place) ,settings)
;; 						    (%setv ,place ,value ,@(if db (cdr db) '(*default-db*))))))
;; 		    )))
;;        (restart-case
;; 	   (handler-case ())
;; 	 ))))

(defmacro with-atomic-setv (&body body)
  `(progn ,@body))

;;; here is what we want out of with-atomic-setv
(with-atomic-setv
  (let ((x 1)
	(sym (gensym)))
    (setv *x* x
	  *sym* sym)
    (print 'hi)
    (setv *x* "string"
	  *sym* "string")))

;;; should expand to something like this:
(let ((#:*x*123 *x*)
      (#:*sym*123 *sym*))
  (handler-case
      (let ((x 1)
	    (sym (gensym)))
	(setv *x* x
	      *sym* sym)
	(print 'hi)
	(setv *x* "string"
	      *sym* "string"))
    (invalid-datum-error (c)
      (setf *x* #:*x*123
	    *sym* #:*sym*123))))
