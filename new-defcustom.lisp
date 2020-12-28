(in-package :defcustom)

;;; Set up class hierarchy for ease of grokking

(defclass config-info-functions ()
  ((predicate :initarg :predicate :initform #'identity :type (function (*) boolean)
	      :documentation "The predicate against which valid values are checked")
   (coercer :initarg :coercer :initform nil
	    :documentation "The function by which invalid datum will attempt to be coerced")))

(defclass config-info-direct-info ()
  ((symbol :initarg :symbol :accessor config-info-symbol
	   :documentation "The symbol which this config info governs. no package information is included")
   (symbol-package :initarg :package :accessor config-info-symbol-package
		   :documentation "The package within which the config-info symbol resides")
   (default-value :initarg :default :accessor config-info-default-value
		  :documentation "The default value of this config-info object")))

(defclass config-info-metadata ()
  ((name :initarg :name :initform "Unnamed config-info object" :accessor config-info-name
	 :documentation "The formal name by which this config-info object can be searched for")
   (tags :initarg :tags :initform '() :accessor config-info-tags
	 :documentation "Tags which can be used for finding a config-info object")
   (valid-values :initarg :valid-values :initform :unset :accessor config-info-valid-values-description
		 :documentation "An explanation of the valid values and predicate function")
   (valid-values-test :initarg :valid-values-test :initform nil
		      :documentation "A symbol denoting the :test key variable passed to member when valid-values are passed")))

(defclass config-info (config-info-metadata config-info-functions config-info-direct-info) ())

;;; turn valid-values into a string, unless a custom string is provided.
(defmethod initialize-instance :after ((obj config-info) &key)
  (with-slots (valid-values predicate coercer) obj
    (unless (stringp valid-values)
      (setf valid-values (concatenate 'string
				      (format nil "Valid values are tested using ~S"
					      (cond ((functionp predicate)
						     (let ((fname (nth-value 2 (function-lambda-expression
										predicate))))
						       (cond ((symbolp fname) fname)
							     ((listp fname) (list (car fname) (cadr fname)))
							     (t 'unknown))))
						    ((symbolp predicate) predicate)
						    (t 'unknown)))
				      (if (eql valid-values :unset) ""
					  (case (length valid-values)
					    ((1) (format nil ", and are limited to ~{~S~^, ~}" valid-values))
					    ((2) (format nil ", and are limited to ~S and ~S"
							 (car valid-values) (cadr valid-values)))
					    (otherwise (format nil ", and are limited to ~{~S~^, ~}, and ~S"
							       (butlast valid-values) (car (last valid-values))))))
				      (if coercer
					  (format nil ". Coercion is attempted on invalid values using ~S"
						  (cond ((functionp coercer)
							 (let ((fname (nth-value 2 (function-lambda-expression
										    coercer))))
							   (cond ((symbolp fname) fname)
								 ((listp fname) (list (car fname) (cadr fname)))
								 (t 'unknown))))
							((symbolp coercer) coercer)
							(t 'unknown)))
					  ""))))))

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
     (with-slots (place-form value coerced-value) c
       (format s "The value ~S is invalid for place ~S, and was coerced to ~S which is also invalid"
	       value place-form coerced-value)))))

;;; Set up default database 

(defun make-customizer-database ()
  "creates a cons of two hash tables, the car for function lookup and the cdr for variable lookup."
  (cons (make-hash-table :test 'equalp) (make-hash-table :test 'equalp)))

(defparameter *config-info-db* (make-customizer-database))

(defun %db-type (place)
  (if (symbolp place)
      :variable
      :function))

(defun %config-info-dispatch-id (symbol-or-list)
  (let ((sym (if (symbolp symbol-or-list)
		 symbol-or-list
		 (car symbol-or-list))))
    (cons (symbol-package sym) (read-from-string (symbol-name sym)))))

(defun %read-new-db ()
  (format *query-io* "Enter a new database: ")
  (force-output *query-io*)
  (list (read)))

(defmacro %set-valid (place value db)
  (alexandria:with-gensyms (namespace value-holder real-db dispatch-id obj pred coer
				      valid? coerced-hold valid-coercion? new-db)
    `(let* ((,namespace ,(%db-type place))
	    (,value-holder ,value)
	    (,real-db (if (eql ,namespace :function)
			  (car ,db)
			  (cdr ,db)))
	    (,dispatch-id ,(%config-info-dispatch-id place))
	    (,obj (or (gethash ,dispatch-id ,real-db)
		      (restart-case (error 'config-not-found-error :place (quote ,place) :db-type ,namespace)
			(use-db (,new-db)
			  :report "supply a new database to query"
			  :interactive (lambda () (%read-new-db))
			  (gethash ,dispatch-id ,new-db)))))
	    (,pred (slot-value ,obj 'predicate))
	    (,coer (slot-value ,obj 'coercer))
	    (,valid? (funcall ,pred ,value-holder)))
       (restart-case
	   (cond (,valid?
		  (setf ,place ,value-holder))
		 (,coer
		  (let* ((,coerced-hold (funcall ,coer ,value-holder))
			 (,valid-coercion? (funcall ,pred ,coerced-hold)))
		    (if ,valid-coercion?
			(setf ,place ,coerced-hold)
			(error 'invalid-datum-error :place (quote ,place)
						    :value ,value-holder
						    :coerced-value ,coerced-hold))))
		 (t (error 'invalid-datum-error :place (quote ,place)) :value ,value-holder))
	 (continue ()
	   :report "continue without setf-ing"
	   nil)
	 (ignore-invalidity ()
	   :report "Ignore invalid value and set anyway"
	   (setf ,place ,value-holder))))))

(defun add-config-to-db (id instance db)
  (setf (gethash id db) instance))

(defun %define-config-info
    (symbol-or-accessor default-value predicate coercer doc db name tags valid-values &optional (type 'defparameter))
  "type should be either 'defvar or 'defparameter"
  (let ((dispatch-id (%config-info-dispatch-id symbol-or-accessor))
	(variable (symbolp symbol-or-accessor)))
    (alexandria:with-gensyms (pred val valid-def config)
      `(let* ((,pred ,predicate)
	      (,val ,default-value)
	      (,valid-def (funcall ,pred ,val)))
	 (if ,valid-def
	     (progn ,(if variable
			 `(,type ,symbol-or-accessor ,val ,@(when doc `(,doc)))
			 `(setf ,symbol-or-accessor ,val))
		    (let* ((,config (make-instance 'config-info
						   :predicate ,predicate
						   :coercer ,coercer
						   :symbol ',(cdr dispatch-id)
						   :package ,(car dispatch-id)
						   :default ,val
						   :tags ,tags
						   :name ,(or name
							      (concatenate 'string
									   (package-name (car dispatch-id)) "::"
									   (symbol-name (cdr dispatch-id))))
						   ,@(when valid-values `(:valid-values ,valid-values)))))
		      (add-config-to-db ',dispatch-id ,config (cdr ,db))))
	     (error 'invalid-datum-error :place ',symbol-or-accessor :value ,val))))))

(defmacro defconf-var (var default predicate &key coercer doc db tags name valid-values)
  "do not set valid-values. it is only here to be used by defconf-enum-*"
  (%define-config-info var default predicate coercer doc (or db '*config-info-db*) name tags valid-values 'defvar)
  ;; (define-dynamic-variable-config-info 'defvar var default predicate coercer doc (or db '*config-info-db*) name tags
  ;;   valid-values)
  )

(defmacro defconf-param (var default predicate &key coercer doc db tags name valid-values)
  (%define-config-info var default predicate coercer doc db name tags valid-values)
  ;; (define-dynamic-variable-config-info 'defparameter var default predicate coercer doc db name tags valid-values)
  )

(defmacro defconf-enum-var (var default valid-values &key test coercer doc db tags name)
  `(defconf-var ,var ,default (lambda (arg) (member arg ,valid-values :test ,(or test ''equal)))
     :coercer ,coercer :doc ,doc :db ,db :tags ,tags :name ,name :valid-values ,valid-values))

(defmacro defconf-enum-var (var default valid-values &key test coercer doc db tags name)
  `(defconf-parameter ,var ,default (lambda (arg) (member arg ,valid-values :test ,(or test ''equal)))
     :coercer ,coercer :doc ,doc :db ,db :tags ,tags :name ,name :valid-values ,valid-values))

(defmacro %defconf (place default-value predicate coercer valid-values name tags doc db)
  (%define-config-info place default-value predicate coercer doc db name tags valid-values))

(defmacro define-config-info (place default-value &key predicate coercer valid-values test name tags doc db)
  "If valid-values is provided a suitable predicate is constructed. if predicate is provided it overrides the valid-values"
  (cond (predicate 
	 `(%defconf ,place ,default-value ,predicate ,coercer nil ,name ,tags ,doc ,db))
	(valid-values
	 `(%defconf ,place ,default-value (lambda (x) (member x ,valid-values :test ,(or test ''equal)))
		    ,coercer ,valid-values ,name ,tags ,doc ,db))))

(defmacro define-config-info-2 (type symbol default-value &key predicate coercer valid-values test name tags doc db)
  "If valid-values is provided a suitable predicate is constructed. if predicate is provided it overrides the valid-values"
  (case type
    (:function
     )
    (:variable
     ))
  (cond (predicate 
	 `(%defconf ,place ,default-value ,predicate ,coercer nil ,name ,tags ,doc ,db))
	(valid-values
	 `(%defconf ,place ,default-value (lambda (x) (member x ,valid-values :test ,(or test ''equal)))
		    ,coercer ,valid-values ,name ,tags ,doc ,db))))

(define-config-info-2 :function stumpwm:window-x 0 'numberp )

(define-config-info-2-enum :function stumpwm:window-hints nil '(:window-hint-1 :window-hint-2))

(define-config-info (cl-user::identity) :hi :valid-values '(:hi :ho))


