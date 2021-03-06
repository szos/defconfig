(in-package :defconfig)

(defconfig *setv-permissiveness* :STRICT
  :typespec '(member :STRICT :GREEDY :PERMISSIVE :GREEDY+PERMISSIVE)
  :tags '("setv" "permissiveness" "allow other values")
  :documentation "Determines how setv will act when no config-info object is 
found. :STRICT means to error out. :GREEDY means to search through all registered 
databases for a config-info object and use the first one that is found, or if none
is found error out. :PERMISSIVE means to setf when a config-info object isnt 
found. :GREEDY+PERMISSIVE means to search through all registered databases and 
use the first object found, but if one isnt found to setf regardless.")

(defun remove-keys (list keys)
  "returns two values - accumulated non-keys and keys "
  (declare (type list list)
           (type list keys))
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
  (alexandria:with-gensyms (holdover)
    `(progn ,(if (listp place)
                 `(setf ,place ,value)
                 `(let ((,holdover ,place))
                    (setf ,place ,value)
                    (when (typep ,config-info-object 'config-info)
                      (setf (config-info-prev-value ,config-info-object) ,holdover))))
            ,value)))

(defun %fsetv-ensure-validity (throwtag config-info-object value invalid-symbol
			       &optional errorp place error extra-args)
  "check VALUE against CONFIG-INFO-OBJECTs predicate. return VALUE if it passes.
If ERRORP is true error if VALUE doesnt pass. If ERRORP is nil and VALUE doesnt 
pass, return INVALID-SYMBOL. Restarts are put in place to provide a value or set
regardless."
  (declare (type symbol throwtag)
           (type (or config-info accessor-config-info minimal-config-info)
                 config-info-object)
           (type (or symbol null) invalid-symbol)
           (type boolean errorp)
           (type (or symbol list) place)
           (type (or symbol null) error)
           (type list extra-args))
  (let ((real-place (or place (config-info-place config-info-object))))
    (declare (type (or symbol list) real-place))
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
      (continue ()
        :report (lambda (s)
                  (format s "Return NIL without setting ~A" real-place))
        (throw throwtag nil))
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
	(%fsetv-ensure-validity throwtag config-info-object provided
                                invalid-symbol errorp real-place))
      (set-regardless ()
	:test (lambda (c) (typep c 'invalid-datum-error))
	:report (lambda (s) (format s "Regardless of validity, set ~S to ~S"
				    real-place value))
	(return-from %fsetv-ensure-validity value)))))

(defmacro %%setv-coerced (place value config-object coercer throwtag)
  (alexandria:with-gensyms (coer-hold validated-value invalid-sym)
    `(if ,coercer 
	 (let* ((,coer-hold (funcall ,coercer ,value))
		(,validated-value
		  (%fsetv-ensure-validity ,throwtag ,config-object ,coer-hold
                                          ',invalid-sym t ',place
                                          'invalid-coerced-datum-error
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
  (declare (type (or symbol list) place)
           (type hash-table hash)
           (type symbol db)
           (type (or symbol null) setf-symbol))
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
				 validated-value coer setf?-sym block)
    `(let ((,hold ,value))
       (catch ',block
         (let* ((,hash ,(if (listp place) `(car ,db) `(cdr ,db)))
		(,config-info-object
		  (let ((obj (%fsetv-get-config-info-object ',place ,hash ',db
							    ',setf?-sym)))
		    (if (eql obj ',setf?-sym)
			(progn (setf ,place ,hold)
			       (throw ',block ,hold))
			obj)))
		(,coer (config-info-coercer ,config-info-object))
		(,validated-value
		  ;; get a validated value - we use this instead of hold because
		  ;; if there is a coercer for the place we will return invalid-sym
		  ;; when ,hold is invalid, and if not we will error out with
		  ;; restarts in place to provide a value or set regardless
		  (%fsetv-ensure-validity ',block ,config-info-object ,hold
                                          ',invalid-sym (not ,coer) ',place)))
           (declare (type hash-table ,hash)
                    (type (or minimal-config-info accessor-config-info
                              config-info null)
                          ,config-info-object)
                    (type (or function null) ,coer))
	   (if (eql ,validated-value ',invalid-sym)
	       (%%setv-coerced ,place ,hold ,config-info-object ,coer ',block)
	       (%setv-ensure-setf ,place ,validated-value ,config-info-object)))))))

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

(defmacro %psetv (db pairs)
  (let ((gensyms (loop for x in pairs collect (gensym "NEW"))))
    `(let* ,(loop for (p v) on pairs by 'cddr
                  for gensym in gensyms
                  collect `(,gensym ,v))
       ,@(loop for (p v) on pairs by 'cddr
               for gensym in gensyms
               collect `(%%setv ,p ,gensym ,db))
       nil)))

(defmacro psetv (&rest args)
  "The setv equivalent of psetf - set all places in parallel"
  (destructuring-keys (pairs (:db '*default-db*)) args
    `(%psetv ,db ,pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compile-time with-atomic-setv ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %atomic-setv-reset (accumulator &key (pop t))
  "this macro resets all encountered places within a call to with-atomic-setv*."
  (declare (special *setv-place-accumulator*))
  (let ((place-list (if pop
			(cdr *setv-place-accumulator*)
			*setv-place-accumulator*)))
    `(progn
       ,@(when pop `((pop ,accumulator)))
       ,@(loop for (db place) in place-list
	       collect `(setf ,place (pop ,accumulator))))))

(defmacro %setv-with-reset (block-name reset-on accumulator place value db)
  "Wrap setv in a handler to catch all errors, which will reset all encountered
 places, after which it returns the condition from the named block."
  (declare (special *setv-place-accumulator*))
  (push (list db place) *setv-place-accumulator*)
  (alexandria:with-gensyms (c)
    `(progn
       (push ,place ,accumulator)
       (handler-case (%%setv ,place ,value ,db)
	 (,reset-on (,c)
	   (%atomic-setv-reset ,accumulator)
	   (return-from ,block-name ,c))))))

(defmacro %atomic-setv (block-name reset-on-errors accumulator database &rest args)
  "generates a set of calls to %setv-with-reset."
  (declare (special *setv-place-accumulator*))
  (destructuring-keys (pairs (:db database)) args
    `(progn
       ,@(loop for (p v) on pairs by 'cddr
	       collect `(%setv-with-reset ,block-name ,reset-on-errors ,accumulator
					  ,p ,v ,db)))))

(defmacro %with-atomic-setv* ((&key (re-error t) handle-conditions db) &body body)
  "This macro utilizes compiler-let to allow rollbacks of accessor stuff. "
  (alexandria:with-gensyms (args block-name c inner-c accumulator)
    `(compiler-let ((*setv-place-accumulator* nil))
       (let ((,c (block ,block-name
		   (let ((,accumulator '()))
		     (macrolet ((setv (&rest ,args)
				  `(%atomic-setv ,',block-name
						 ,',handle-conditions
						 ,',accumulator
                                                 ,',db
						 ,@,args)))
		       (handler-case
                           (restart-case (progn ,@body)
                             (abort-and-reset ()
                               :report "Exit WITH-ATOMIC-SETV* and reset"
                               (error 'with-atomic-setv-internal-error))
                             (abort-without-resetting ()
                               :report "Exit WITH-ATOMIC-SETV* without resetting"
                               (return-from ,block-name nil)))
                         ((or with-atomic-setv-internal-error ,handle-conditions)
                           (,inner-c)
			   (%atomic-setv-reset ,accumulator :pop nil)
			   (return-from ,block-name ,inner-c))))))))
	 (if (and ,re-error (typep ,c ',handle-conditions))
	     (error 'setv-wrapped-error :error ,c)
	     ,c)))))

(defmacro with-atomic-setv* ((&key (re-error t) handle-conditions db) &body body)
  `(%with-atomic-setv* (:re-error ,re-error
			:handle-conditions ,(or handle-conditions 'error)
                        :db ,(or db '*default-db*))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime with-atomic-setv ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %runtime-atomic-setv-reset (accumulator &key pop)
  `(let ((already-reset nil))
     (loop for (var val varplace) in (reverse (,(if pop 'cdr 'identity)
					       ,accumulator))
	   unless (member var already-reset :test 'equalp)
	     do (if (symbolp var)
		    (setf (symbol-value var) val)
		    (funcall (fdefinition `(setf ,(car var))) val varplace))
		(push var already-reset))))

(defmacro %runtime-setv-with-reset (ac block-name reset-errors place value db)
  (alexandria:with-gensyms (c acplace)
    `(let ((,acplace ,(if (listp place)
			(cadr place)
			place)))
       (push (list ',place
		   (funcall ',(if (listp place) 
				  (car place)
				  'identity)
			    ,acplace)
		   ,acplace)
	     ,ac)
       (handler-case (%%setv ,(if (listp place)
				  `(,(car place) ,acplace)
				  place)
			     ,value ,db)
	 (,reset-errors (,c) 
	   (%runtime-atomic-setv-reset ,ac :pop t)
	   (return-from ,block-name ,c))))))

(defmacro %runtime-atomic-setv (accumulator block-name reset-errors database
                                &rest args)
  (destructuring-keys (pairs (:db database)) args
    `(progn
       ,@(loop for (p v) on pairs by 'cddr
	       collect `(%runtime-setv-with-reset ,accumulator
						  ,block-name
						  ,reset-errors
						  ,p ,v ,db)))))

(defmacro with-runtime-atomic-setv ((&key (re-error t) handle-conditions db)
				    &body body)
  (alexandria:with-gensyms (block-name args c inner-c outer-block accumulator)
    `(block ,outer-block
       (let ((,c (block ,block-name
		   (let ((,accumulator nil))
		     (macrolet ((setv (&rest ,args)
				  `(%runtime-atomic-setv ,',accumulator
							 ,',block-name
							 ,',handle-conditions
                                                         ,',db
							 ,@,args)))
		       (handler-case
			   (restart-case (progn ,@body)
			     (abort-and-reset ()
			       :report "Exit WITH-ATOMIC-SETV and reset everything"
			       ;; (%runtime-atomic-setv-reset ,accumulator)
			       ;; (return-from ,outer-block nil)
                               (error 'with-atomic-setv-internal-error))
			     (abort-without-resetting ()
			       :report "Exit WITH-ATOMIC-SETV without resetting"
			       (return-from ,outer-block nil)))
			 ((or with-atomic-setv-internal-error ,handle-conditions)
                           (,inner-c)
			   (%runtime-atomic-setv-reset ,accumulator)
			   (return-from ,block-name ,inner-c))))))))
	 (if (and ,re-error (typep ,c ',handle-conditions))
	     (error 'setv-wrapped-error :error ,c)
	     (progn
	       (typecase ,c
		 (error
		  (warn "WITH-ATOMIC-SETV encountered the error~%~S~%and reset."
			,c))
		 (warning
		  (warn ,c)))
	       ,c))))))

(defmacro with-atomic-setv ((&key (errorp t) handle-conditions db) &body body)
  "This macro, upon encountering an error, resets all places encountered within 
calls to setv to be reset to the value they held before the call to 
with-atomic-setv. Which errors to reset upon can be controlled with 
HANDLE-CONDITIONS. If it is nil, it defaults to 'error. If a handled condition is 
encountered, it will be wrapped in SETV-WRAPPED-ERROR, unless RE-ERROR is nil, in 
which case a warning will be generated and the condition will be returned. "
  `(with-runtime-atomic-setv (:re-error ,errorp
			      :handle-conditions ,(or handle-conditions 'error)
                              :db ,(or db '*default-db*))
     ,@body))
