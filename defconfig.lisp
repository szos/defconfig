(in-package :defcustom)

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
		 :documentation "An explanation of the valid values and predicate function")))

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

;;; database

(defun make-customizer-database ()
  "creates a cons of two hash tables, the car for function lookup and the cdr for variable lookup."
  (cons (make-hash-table :test 'equalp) (make-hash-table :test 'equalp)))

(defparameter *default-db* (make-customizer-database))

;;; actual defconfig workers and macros. 

(defun %defconfig (place default &key predicate coercer reinitialize test documentation tags
				   (db *default-db*))
  (alexandria:with-gensyms (hold hash validated)
    `(let* ((,hold ,default)
	    (,hash ,(if (listp place) `(car ,db) `(cdr ,db))) ; if place is a list its an accessor
	    (,validated (funcall ,predicate ,hold))
	    )
       (if ,validated
	   ,(cond ((and reinitialize (listp place))
		   `(setf ,place ,hold))
		  (reinitialize
		   `(defparameter ,place ,hold ,@(when documentation (list documentation))))
		  (t `(defvar ,place ,hold ,@(when documentation (list documentation)))))
	   (error 'invalid-datum-error :place ',place :value ,hold))
       ,(cond ((listp place) ; we have an accessor 
	       `(setf (gethash ,(if (= (length place) 1)
				    (car place)
				    place)
			       ,db)
		      ))))))

(defmacro %defconf-vv-intermediary (place default &key predicate coercer reinitialize documentation tags)
  (%defconfig place default :predicate predicate :coercer coercer :reinitialize reinitialize
			    :documentation documentation :tags tags))

(defmacro defconfig (place default-value &key validator valid-values coercer reinitialize (test 'eql)
					   documentation tags)
  (when (and predicate valid-values)
    (error "A predicate and valid-values keyargs cannot both be provided to defconfig"))
  (cond (predicate
	 (%defconfig place default-value :predicate validator :coercer coercer :reinitialize reinitialize
					 :documentation documentation :tags tags))
	(valid-values
	 `(%defconf-vv-intermediary ,place ,default-value
				    :predicate (lambda (x) (member x ,valid-values :test ,test))
				    :coercer coercer :reinitialize reinitialize :documentation documentation
				    :tags tags))
	
	(t
	 (%defconfig place default-value :predicate 'cl::identity :coercer coercer :reinitialize reinitialize
					 :documentation documentation :tags tags))))
