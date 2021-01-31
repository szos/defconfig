(in-package :defconfig)

(defconfig *setv-permissiveness* :strict
  :typespec '(member :strict :greedy :permissive :greedy+permissive)
  :tags '("setv" "permissiveness" "allow other values")
  :documentation "Determines how setv will act when no config-info object is found.
:strict means to error out. :greedy means to search through all registered 
databases for a config-info object and use the first one that is found, or if none
is found error out. :permissive means to setf when a config-info object isnt found.
:greedy+permissive means to search through all registered databases and use the 
first object found, but if one isnt found to setf regardless.")

(define-condition setv-wrapped-error (config-error)
  ((condition :initarg :error :accessor setv-wrapped-error-condition))
  (:report
   (lambda (c s)
     (with-slots (condition) c
       (format s "WITH-ATOMIC-SETV encountered the error ~S and reset."
	       (type-of condition)))))
  (:documentation
   "This condition is only ever signalled from within WITH-ATOMIC-SETV, and 
indicates that an error was caught which caused WITH-ATOMIC-SETV to reset all
places found within its body. It has one slot, CONDITION, which contains the 
condition that was caught."))

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
  "separates keys from list, storing the remainder in VAR, and making each key a
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

(defmacro %setv-ensure-setf (place value config-info-object)
  `(progn ,(if (listp place)
	       `(setf ,place ,value)
	       `(psetf ,place ,value
		       (config-info-prev-value ,config-info-object) ,place))
	  ,value))

(defun %fsetv-ensure-validity (config-info-object value invalid-symbol
			       &optional errorp place error extra-args)
  "check VALUE against CONFIG-INFO-OBJECTs predicate. return VALUE if it passes.
If ERRORP is true error if VALUE doesnt pass. If ERRORP is nil and VALUE doesnt 
pass, return INVALID-SYMBOL. Restarts are put in place to provide a value or set
regardless."
  (let ((real-place (or place (config-info-place config-info-object))))
    (restart-case 
	(let ((valid? (funcall (config-info-predicate config-info-object)
			       value)))
	  (cond (valid? value)
		(errorp (if error
			    (apply 'error error
				   (append extra-args
					   (list :config-object config-info-object
						 :place real-place
						 :value value)))
			    (error 'invalid-datum-error
				   :config-object config-info-object
				   :place real-place
				   :value value)))
		(t invalid-symbol)))
      (use-value (provided)
	:test (lambda (c) (typep c 'invalid-datum-error))
	:report (lambda (s)
		  (format s "Supply a new value for ~S" real-place))
	:interactive (lambda ()
		       (format *query-io* "Enter Value:  ")
		       (force-output *query-io*)
		       (list (read *query-io*)))
	(return-from %fsetv-ensure-validity provided))
      (use-validated-value (provided)
	:test (lambda (c) (typep c 'invalid-datum-error))
	:report (lambda (s)
		  (format s "Supply a new value to be validated for ~S"
			  real-place))
	:interactive (lambda ()
		       (format *query-io* "Enter Value:  ")
		       (force-output *query-io*)
		       (list (read *query-io*)))
	(%fsetv-ensure-validity config-info-object provided invalid-symbol
				errorp real-place))
      (set-regardless ()
	:test (lambda (c) (typep c 'invalid-datum-error))
	:report (lambda (s) (format s "Regardless of validity, set ~S to ~S"
				    real-place value))
	(return-from %fsetv-ensure-validity value)))))

(defmacro %%setv-coerced (place value config-object coercer)
  (alexandria:with-gensyms (coer-hold validated-value invalid-sym)
    `(if ,coercer 
	 (let* ((,coer-hold (funcall ,coercer ,value))
		(,validated-value
		  (%fsetv-ensure-validity ,config-object ,coer-hold ',invalid-sym
					  t ',place 'invalid-coerced-datum-error
					  (list :coerced-value ,coer-hold))))
	   ;; we dont need to check ,validated-values here as we will ALWAYS error
	   ;; if we get a invalid coerced data. 
	   (%setv-ensure-setf ,place ,validated-value ,config-object))
	 (case *setv-permissiveness*
	   ((:permissive :greedy+permissive)
	    (%setv-ensure-setf ,place ,value ,config-object))
	   (otherwise
	    (error 'invalid-datum-error :config-object ,config-object
					:place ',place
					:value ,value))))))

(defun %fsetv-get-config-info-object (place hash db &optional setf-symbol)
  "return setf symbol if we want the caller to setf place. setf-symbol only needs
to be provided if were calling this in a setv expansion."
  (handler-case 
      (or (gethash (if (listp place)
		       (car place)
		       place)
		   hash)
	  (error 'no-config-found-error :place place :db db))
    (no-config-found-error (c)
      (case *setv-permissiveness*
	(:strict (error c))
	(:permissive (return-from %fsetv-get-config-info-object setf-symbol))
	((:greedy :greedy+permissive)
	 (let* ((config-obj
		  (loop for (key db) on *db-plist* by 'cddr
			for obj = (place->config-info place :db (cdr db))
			when obj return (cons obj db))))
	   (cond (config-obj
		  (warn "*SETV-PERMISSIVENESS* is ~S, using configuration object from database ~S rather than ~S" *setv-permissiveness* (cadr config-obj) db)
		  (return-from %fsetv-get-config-info-object (car config-obj)))
		 ((eql *setv-permissiveness* :greedy+permissive)
		  (return-from %fsetv-get-config-info-object setf-symbol))
		 (t (error 'no-config-found-error
			   :place place :db 'all-registered-databases)))))))))

(defmacro %%setv (place value db)
  (alexandria:with-gensyms (hold hash config-info-object invalid-sym
				 validated-value coer setf?-sym)
    `(let ((,hold ,value))
       (let* ((,hash ,(if (listp place) `(car ,db) `(cdr ,db)))
	      (,config-info-object
		(let ((obj (%fsetv-get-config-info-object ',place ,hash ',db
							  ',setf?-sym)))
		  (if (eql obj ',setf?-sym)
		      (setf ,place ,hold)
		      obj)))
	      (,coer (config-info-coercer ,config-info-object))
	      (,validated-value
		;; get a validated value - we use this instead of hold because
		;; if there is a coercer for the place we will return invalid-sym
		;; when ,hold is invalid, and if not we will error out with
		;; restarts in place to provide a value or set regardless
		(%fsetv-ensure-validity ,config-info-object ,hold ',invalid-sym
					(not ,coer) ',place)))
	 (if (eql ,validated-value ',invalid-sym)
	     (%%setv-coerced ,place ,hold ,config-info-object ,coer)
	     (%setv-ensure-setf ,place ,validated-value ,config-info-object))))))

(defmacro setv (&rest args)
  "Setv must get an even number of ARGS - every place must have a value. Setv
can also take the key argument :db, to specify which database to look up config
objects in. "
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn ,@(loop for (p v) on pairs by 'cddr
		    collect `(%%setv ,p ,v ,db)))))

(defmacro setv-atomic (&rest args)
  "this version of setv saves the original value of the places being set, and 
resets all to their original value if an error is encountered. the error is then
resignalled. It is generally advisable to use WITH-ATOMIC-SETV instead."
  (alexandria:with-gensyms (c)
    (multiple-value-bind (pairs db) (remove-keys args '(:db))
      (let ((syms (loop for (p v) on pairs by 'cddr collect (gensym))))
	`(let ,(loop for (place value) on pairs by 'cddr
		     for gensym in syms
		     collect `(,gensym ,place))
	   (handler-case
	       (progn ,@(loop for (place value) on pairs by 'cddr
			      collect `(%%setv ,place ,value
					      ,@(if db
						    (cdr db)
						    '(*default-db*)))))
	     (error (,c)
	       ,@(loop for (place value) on pairs by 'cddr
		       for gensym in syms
		       collect `(setf ,place ,gensym))
	       (error ,c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile-time with-atomic-setv ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %atomic-setv-reset (&key (pop t))
  "this macro resets all encountered places within a call to with-atomic-setv."
  (declare (special *setv-place-accumulator*))
  (let ((place-list (if pop
			(cdr *setv-place-accumulator*)
			*setv-place-accumulator*)))
    `(progn ,@(loop for (db place) in place-list
		    collect `(reset-place ,place :db ,db :previous-value t)))))

(defmacro %setv-with-reset (block-name reset-on place value db)
  "Wrap setv in a handler to catch all errors, which will reset all encountered
 places, after which it returns the condition from the named block."
  (declare (special *setv-place-accumulator*))
  (push (list db place) *setv-place-accumulator*)
  (alexandria:with-gensyms (c)
    `(handler-case (%%setv ,place ,value ,db)
       ((or ,@reset-on) (,c)
	 (%atomic-setv-reset)
	 (return-from ,block-name ,c)))))

(defmacro %atomic-setv (block-name reset-on-errors &rest args)
  "generates a set of calls to %setv-with-reset."
  (declare (special *setv-place-accumulator*))
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn
       ,@(loop for (p v) on pairs by 'cddr
	       collect `(%setv-with-reset ,block-name ,reset-on-errors
					  ,p ,v ,db)))))

(defmacro with-atomic-setv*
    ((&key (re-error t) (handle-errors '(error))) &body body)
  "This macro functions the same as WITH-ATOMIC-SETV, but tracks value via the 
previous-value slot instead of in a list. As such any given place may be setv-ed
once within BODY."
  (alexandria:with-gensyms (args block-name c inner-c)
    `(compiler-let ((*setv-place-accumulator* nil))
       (let ((,c (block ,block-name
		   (macrolet ((setv (&rest ,args)
				`(%atomic-setv ,',block-name ,',handle-errors
					       ,@,args)))
		     (handler-case (progn ,@body)
		       ((or ,@handle-errors) (,inner-c)
			 (%atomic-setv-reset :pop nil)
			 (return-from ,block-name ,inner-c)))))))
	 (if (and ,re-error (typep ,c '(or ,@handle-errors)))
	     (error 'setv-wrapped-error :error ,c)
	     ,c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime with-atomic-setv ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %runtime-atomic-setv-reset (&key pop)
  `(let ((already-reset nil))
     (loop for (var val) in (reverse
			     ,(if pop
				  '(cdr with-atomic-setv-accumulator)
				  'with-atomic-setv-accumulator))
	   unless (member var already-reset)
	     do (setf (symbol-value var) val)
		(push var already-reset))))

(defmacro %runtime-setv-with-reset (block-name reset-errors place value db)
  (alexandria:with-gensyms (c)
    `(progn
       (push (list ',place ,place)
	     with-atomic-setv-accumulator)
       (handler-case (%%setv ,place ,value ,db)
	 ((or ,@reset-errors) (,c)
	   (%runtime-atomic-setv-reset :pop t)
	   (return-from ,block-name ,c))))))

(defmacro %runtime-atomic-setv (block-name reset-errors &rest args)
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(progn
       ,@(loop for (p v) on pairs by 'cddr
	       collect `(%runtime-setv-with-reset ,block-name ,reset-errors
					      ,p ,v ,db)))))

(defmacro with-runtime-atomic-setv ((&key (re-error t) (handle-errors '(error)))
				    &body body)
  (alexandria:with-gensyms (block-name args c inner-c)
    `(let ((,c (block ,block-name
		 (let ((with-atomic-setv-accumulator nil))
		   (macrolet ((setv (&rest ,args)
				`(%runtime-atomic-setv ,',block-name
						       ,',handle-errors
						       ,@,args)))
		     (handler-case (progn ,@body)
		       ((or ,@handle-errors) (,inner-c)
			 (%runtime-atomic-setv-reset)
			 (return-from ,block-name ,inner-c))))))))
       (if (and ,re-error (typep ,c '(or ,@handle-errors)))
	   (error 'setv-wrapped-error :error ,c)
	   (progn (warn "WITH-ATOMIC-SETV encountered the error~%~S~%and reset."
			,c)
		  ,c)))))

(defmacro with-atomic-setv ((&key (re-error t) (handle-errors '(error)))
			    &body body)
  "This macro causes every call to setv to, on an invalid value, reset the place
in question to its previous value, as well as any previously encountered places.
When supplied, HANDLE-ERRORS should be an unquoted list of errors to handle. In 
this context, handling an error means catching it, resetting all values changed
via setv, and if RE-ERROR is t, signal an error of type setv-wrapped-error. Any 
error types not present in HANDLE-ERRORS will not cause a reset and will not 
be caught - they will propogate up out of with-atomic-setv."
  `(with-runtime-atomic-setv (:re-error ,re-error :handle-errors ,handle-errors)
     ,@body))
