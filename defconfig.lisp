(in-package :defconfig)

(defclass config-info-functions ()
  ((predicate :initarg :predicate :initform #'identity :type (function (*) boolean)
              :accessor config-info-predicate
	      :documentation "The predicate against which valid values are checked")
   (coercer :initarg :coercer :initform nil :accessor config-info-coercer
	    :documentation "The function by which invalid datum will attempt to be coerced")))

(defclass config-info-direct-info ()
  ((place :initarg :place :accessor config-info-place
	  :documentation "The place which this config info governs.")
   (default-value :initarg :default :accessor config-info-default-value
		  :documentation "The default value of this config-info object")))

(defclass config-info-metadata ()
  ((name :initarg :name :initform "Unnamed config-info object" :accessor config-info-name
	 :documentation "The formal name by which this config-info object can be searched for")
   (tags :initarg :tags :initform '() :accessor config-info-tags
	 :documentation "Tags which can be used for finding a config-info object")
   (docstring :initarg :documentation :initform nil :accessor config-info-documentation
	      :documentation "The docstring for the place being governed. if a variable it is the same as the variables docstring")
   (valid-values :initarg :valid-values :initform :unset :accessor config-info-valid-values-description
		 :documentation "An explanation of the valid values and predicate function")))

(defclass config-info (config-info-metadata config-info-functions config-info-direct-info) ())

;;; turn valid-values into a string, unless a custom string is provided. this should be moved into the generation fn
(defmethod initialize-instance :after ((obj config-info) &key)
  (with-slots (valid-values predicate coercer) obj
    (unless (stringp valid-values)
      (cond ((eql :unset valid-values)
	     (setf valid-values 
		   (concatenate 'string
				(format nil "Valid values are tested using ~S"
					(cond ((functionp predicate)
					       (let ((fname (nth-value 2 (function-lambda-expression
									  predicate))))
						 (cond ((symbolp fname) fname)
						       ((listp fname) (list (car fname) (cadr fname)))
						       (t 'unknown))))
					      ((symbolp predicate) predicate)
					      (t 'unknown-predicate)))
				(if coercer
				    (format nil ". Coercion is attempted on invalid values using ~S."
					    (cond ((functionp coercer)
						   (let ((fname (nth-value 2 (function-lambda-expression
									      coercer))))
						     (cond ((symbolp fname) fname)
							   ((listp fname) (list (car fname) (cadr fname)))
							   (t 'unknown))))
						  ((symbolp coercer) coercer)
						  (t 'unknown-coercer)))
				    "."))))
	    (t
	     (setf valid-values
		   (concatenate 'string
				(format nil "Valid values must conform to type specifier ~S" valid-values)
				(if coercer
				    (format nil ". Coercion is attempted on invalid values using ~S."
					    (cond ((functionp coercer)
						   (let ((fname (nth-value 2 (function-lambda-expression
									      coercer))))
						     (cond ((symbolp fname) fname)
							   ((listp fname) (list (car fname) (cadr fname)))
							   (t 'unknown))))
						  ((symbolp coercer) coercer)
						  (t 'unknown-coercer)))
				    "."))))))))

;;; Set up conditions

(define-condition config-error (error) ())

(define-condition invalid-datum-error (config-error)
  ((place-form :initarg :place :reader invalid-datum-error-place :initform nil)
   (value :initarg :value :reader invalid-datum-error-value :initform nil))
  (:report
   (lambda (c s)
     (with-slots (place-form value) c
       (format s "The value ~S is invalid for place ~S." value place-form)))))

(define-condition invalid-coerced-datum-error (invalid-datum-error)
  ((coerced-value :initarg :coerced-value :initform nil
		  :reader invalid-coerced-datum-error-value))
  (:report
   (lambda (c s)
     (with-slots (place-form value) c
       (format s "The value ~S is invalid for place ~S, and was unable to be coerced to a valid value"
	       value place-form)))))

(define-condition no-config-found-error (config-error)
  ((place-form :initarg :place :reader no-config-found-error-place :initform nil)
   (database :initarg :db :reader no-config-found-error-db :initform nil))
  (:report
   (lambda (c s)
     (with-slots (place-form) c
       (format s "Unable to find config-info for place ~S" place-form)))))

;;; database

(defun make-customizer-database ()
  "creates a cons of two hash tables, the car for function lookup and the cdr for variable lookup."
  (cons (make-hash-table :test 'equalp) (make-hash-table :test 'equalp)))

(defparameter *default-db* (make-customizer-database))

;;; actual defconfig workers and macros. 

(defun %defconfig (place default &key (predicate 'cl::identity predicate-provided-p)
				   coercer reinitialize documentation tags regen-config
				   (db '*default-db*) valid-values-list)
  (alexandria:with-gensyms (hold hash validated obj pred)
    `(let* ((,pred ,@(if predicate-provided-p
			 `(,predicate)
			 `(',predicate)))
	    (,hold ,default)
	    (,hash ,(if (listp place) `(car ,db) `(cdr ,db))) ; if place is a list its an accessor
	    (,validated (funcall ,pred ,hold))
	    (,obj ,(if (listp place)
		       `(gethash ',(if (= (length place) 1) (car place) place) ,hash)
		       `(gethash ',place ,hash))))
       (if ,validated
	   ,@(cond ((and reinitialize (listp place))
		    `((setf ,place ,hold)))
		   (reinitialize
		    `((defparameter ,place ,hold ,@(when documentation (list documentation)))))
		   (t (if (listp place)
			  `(nil)
			  `((defvar ,place ,hold ,@(when documentation (list documentation)))))))
	   (error 'invalid-datum-error :place ',place :value ,hold))
       (when (or (not ,obj) (and ,obj ,regen-config))
	 (setf ,(if (listp place)
		    `(gethash ',(if (= (length place) 1) (car place) place) ,hash)
		    `(gethash ',place ,hash))
	       (make-instance 'config-info
			      ,@(when predicate `(:predicate ,pred))
			      ,@(when coercer `(:coercer ,coercer))
			      ,@(when documentation `(:documentation ,documentation))
			      ,@(when tags `(:tags ,tags))
			      :place ',place
			      :default ,hold
			      ,@(when valid-values-list `(:valid-values ,valid-values-list))))))))

(defmacro %defconf-vv-intermediary (place default &key predicate coercer reinitialize regen-config
						    documentation tags valid-values-list)
  (%defconfig place default :predicate predicate :reinitialize reinitialize :tags tags :regen-config regen-config
			    :coercer coercer :documentation documentation :valid-values-list valid-values-list))

(defmacro defconfig (place default-value
		     &key validator typespec coercer reinitialize regen-config documentation tags)
  (when (and validator typespec)
    (error "A validator and typespec keyargs cannot both be provided to defconfig"))
  (cond (validator
	 (%defconfig place default-value :predicate validator :coercer coercer :reinitialize reinitialize
					 :documentation documentation :tags tags :regen-config regen-config))
	(typespec
	 `(%defconf-vv-intermediary ,place ,default-value
				    :predicate (lambda (x) (typep x ,typespec))
				    :coercer ,coercer :reinitialize ,reinitialize :documentation ,documentation
				    :tags ,tags :regen-config ,regen-config :valid-values-list ,typespec))
	
	(t
	 (%defconfig place default-value :coercer coercer :reinitialize reinitialize
					 :documentation documentation :tags tags :regen-config regen-config))))
