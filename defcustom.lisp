;;;; defcustom.lisp

(in-package #:defcustom)

(defmacro idif (if-then else)
  (alexandria:with-gensyms (hold)
    `(let ((,hold ,if-then))
       (if ,hold ,hold ,else))))

(defun any (thing matches &key (test 'equal))
  (let ((hit (loop for match in matches
		   if (funcall test thing match)
		     return match)))
    (if hit hit
	(error 'internal-invalid-value-error))))

(defclass customizable-setting () 
  ((name :initarg :name :accessor customizable-setting-name
	 :documentation "a pretty printable name for the setting")
   (symbol :initarg :symbol :accessor customizable-setting-symbol
	   :documentation "the symbol without any package information attached")
   (package :initarg :package :accessor customizable-setting-package
	    :documentation "the package the symbol lives in")
   (tags :initarg :tags :accessor customizable-setting-tags)
   (default-value :initarg :default-value :accessor customizable-setting-default-value)
   (symbol-documentation :initarg :symbol-doc)
   (predicate :initarg :predicate :accessor customizable-setting-predicate)
   (coercer :initarg :coercer :accessor customizable-setting-coercer
	    :initform nil)
   (valid-values-fn
    :initarg :validator-fn :accessor customizable-setting-validator-fn
    :documentation "a function to validate values for the symbol symbol in package package")
   (valid-values-rules
    ;; we might not need this, and instead will generate a valid-values-description from
    ;; whatever we pass in to the function to make an object - we want this as a keyarg
    ;; in said function, but not in the class itself. 
    :documentation "the body of the function which will be used to validate the value.")
   (valid-values-description
    :initform "Description not provided, please see the valid-values-rules function."
    :initarg :valid-values-description
    :accessor customizable-setting-validity-description
    :documentation "A description of the rules used to validate the value. Defaults to instructions to see the lambda itself.")))


(defvar *customizable-settings-hash-table* (make-hash-table :test 'equalp))

;;; how to dispatch:
;;; We want to dispatch not only on the symbol but on its package. Our customize
;;; function/macro should take a symbol and generate the following cons:
;;; (package-object . symbol-name-symbol)
;;; which we can get via (symbol-package symbol) and (read-from-string (symbol-name symbol))
;;; This will allow us to at least attempt to be package-agnostic.

(defun make-setting-id-from-symbol (symbol)
  (let ((pkg (symbol-package symbol))
	(clean-symbol (read-from-string (symbol-name symbol))))
    (cons pkg clean-symbol)))

(defun make-colons-for-dispatch-id (dispatch-id)
  (multiple-value-bind (sym in-or-ex)
      (find-symbol (symbol-name (cdr dispatch-id)) (car dispatch-id))
    (declare (ignore sym))
    (case in-or-ex
      (:internal "::")
      (:external ":"))))

(defun fn-name (symbol-or-function)
  (cond ((and (symbolp symbol-or-function) (fboundp symbol-or-function))
	 symbol-or-function)
	((functionp symbol-or-function)
	 (let ((fn-name (nth-value 2 (function-lambda-expression symbol-or-function))))
	   (cond ((symbolp fn-name) fn-name)
		 (t ;; "anonymous function"
		    nil))))))

(defun limited-number-predicate-curry (min max &optional inclusive)
  (lambda (n)
    (if inclusive
	(and (<= min n) (>= max n))
	(and (< min n) (> max n)))))

(defun %defcustom-2 (symbol default-value name tags predicate coercer database &key valid-values valid-values-test)
  (let* ((dispatch-id (make-setting-id-from-symbol symbol))
	 (colon-string (make-colons-for-dispatch-id dispatch-id))
	 (real-name (or name (format nil "~A~A~A"
				     (package-name (car dispatch-id))
				     colon-string
				     (symbol-name symbol))))
	 (rules-description
	   (if valid-values
	       (format nil "Valid values are checked with ~S and limited to ~{~S,^ ~}"
		       valid-values-test valid-values)
	       (let ((name (fn-name predicate))
		     (coerceing-name (or )))
		 (if (stringp name)
		     (format nil "Valid values are checked with an anonymous function~S")
		     (format nil "Valid values are checked with the predicate ~S~S"
			     ))))))))

(defmacro defcustom-2 (var val doc &key name tags (predicate 'cl:identity) coercer valid-values valid-values-test
				     (custom-database '*customizable-settings-hash-table*))
  "if valid-values is provided then predicate is ignored, even if predicate is also provided."
  (alexandria:with-gensyms (real-predicate arg value-hold)
    `(let ((,real-predicate ,(if (and valid-values (listp valid-values))
				 `(lambda (,arg)
				    (defcustom::any ,arg ,valid-values
						    :test ,(if valid-values-test valid-values-test
							       'identity)))
				 predicate))
	   (,value-hold ,val))
       (defparameter ,var ,value-hold ,doc)
       (%defcustom-2 ))))

(defcustom-2 *hi* 'hi
  "hi"
  :valid-values '('hi 'ho)
  :valid-values-test 'equalp)

(defun %defcustom (symbol default-value doc name function database &key rules-description gen-rules comparator tags)
  (let* ((dispatch-id (make-setting-id-from-symbol symbol))
	 (colon-string (make-colons-for-dispatch-id dispatch-id))
	 (real-name (or name (format nil "~A~A~A"
				     (package-name (car dispatch-id))
				     colon-string
				     (symbol-name symbol))))
	 (rules-pprint-description
	   (or rules-description 
	       (cond (gen-rules
		      (format nil "Valid values are computed with ~S and are limited to ~{~S~^, ~}"
			      comparator gen-rules))
		     ((and (symbolp function) (fboundp function))
		      (format nil "Valid values are computed using ~S" function))
		     ((functionp function)
		      ;; This is mildly subpar - ideally we could get the function argument types and return types
		      ;; in order to generate documentation about possible applicable types
		      (let ((fn-name (nth-value 2 (function-lambda-expression function))))
			(format nil (if (symbolp fn-name)
					"Valid values are computed using ~S"
					"Valid values are computed using an anonymous function with signature ~S")
				fn-name)))
		     (t (error "An non-function was passed to %defcustom as the validator"))))))
    (add-customizable-setting dispatch-id
			      (make-instance 'customizable-setting
					     :package (car dispatch-id)
					     :symbol (cdr dispatch-id)
					     :tags (cons real-name
							 (cons (symbol-name symbol)
							       tags))
					     :name real-name
					     :default-value default-value
					     :validator-fn function
					     :valid-values-description rules-pprint-description
					     :symbol-doc doc)
			      database)))

(defmacro defcustom (symbol value doc &key (validity-rules 'cl:identity)
					name tags validity-description test-when-rules-are-list
					(defcustom-expansion 'defparameter)
					(custom-database '*customizable-settings-hash-table*))
  (alexandria:with-gensyms (validator-function arg holder-for-fn holder-for-val)
    (declare (ignorable arg))
    `(let ((,validator-function ,(if (listp validity-rules)
				     `(lambda (,arg)
					(let ((,holder-for-fn
						(defcustom::any ,arg ,validity-rules
								:test ,(if test-when-rules-are-list
									   test-when-rules-are-list
									   ''equal))))
					  (if ,holder-for-fn ,holder-for-fn
					      (error 'internal-invalid-value-error))))
				     validity-rules))
	   (,holder-for-val ,value))
       (,defcustom-expansion ,symbol ,holder-for-val)
       (%defcustom ',symbol ,holder-for-val ,doc ,name ,validator-function ,custom-database
		   ,@(when validity-description `(:rules-description ,validity-description))
		   ,@(when (listp validity-rules)
		       `(:gen-rules ,validity-rules
			 :comparator ,(or test-when-rules-are-list ''equal)))
		   :tags ,tags))))

(defcustom *test* 1
  "testing. should only accept numbers 1 2 3 and 4"
  :validity-rules identity ;; '(1 2 3 4)
  ;; :test-when-rules-are-list 'equalp
  )

;; (defmacro defcustom (symbol value doc valid-values-rules
;; 		     &key name tags
;; 		       valid-values-description
;; 		       ;; used iff valid-values-rules isnt a function
;; 		       (valid-values-rules-test ''equal)
;; 		       (defcustom-expansion-type 'defparameter)
;; 		       (custom-database '*customizable-settings-hash-table*))
;;   (alexandria:with-gensyms (dispatch-id colons validator-fn rules-description lambda-arg nm
;; 					hold)
;;     `(let* ((,dispatch-id (make-setting-id-from-symbol ',symbol))
;; 	    (,colons (make-colons-for-dispatch-id ,dispatch-id))
;; 	    (,nm (or ,name (format nil "~A~A~A"
;; 				   (package-name (car ,dispatch-id))
;; 				   ,colons
;; 				   (symbol-name ',symbol))))
;; 	    (,validator-fn
;; 	      (if (listp ,valid-values-rules)
;; 		  (lambda (,lambda-arg)
;; 		    (defcustom::any ,lambda-arg ,valid-values-rules
;; 				    :test ,valid-values-rules-test))
;; 		  `(if (or (functionp ,valid-values-rules)
;; 			   (and (symbolp ,valid-values-rules)
;; 				(fboundp ,valid-values-rules)))
;; 		       ,valid-values-rules
;; 		       (error "no valid rules were passed to defcustom"))))
;; 	    (,rules-description
;; 	      (or ,valid-values-description
;; 		  ,(if (listp valid-values-rules)
;; 		       `(format nil "Valid values are computed with ~S and are limited to ~{~S~^, ~}"
;; 				,valid-values-rules-test ,valid-values-rules)
;; 		       `(format nil "Valid values are computed using ~S"
;; 				,valid-values-rules))))
;; 	    (,hold ,value))
;;        (declare (string ,nm))
;;        (,defcustom-expansion-type ,symbol ,hold ,doc)
;;        (add-customizable-setting ,dispatch-id
;; 				 (make-instance 'customizable-setting
;; 						:package (car ,dispatch-id)
;; 						:symbol (cdr ,dispatch-id)
;; 						:tags ',tags
;; 						:name (or ,nm)
;; 						:default-value ,hold
;; 						:validator-fn ,validator-fn
;; 						:valid-values-description ,rules-description
;; 						:symbol-doc ,doc)
;; 				 ,custom-database))))

(defun add-customizable-setting (id obj db)
  (setf (gethash id db) obj))

(define-condition no-valid-customizable-setting (error)
  ((id :initarg :id :reader no-valid-customizable-setting-id)))

(define-condition invalid-value-error (error)
  ((package :initarg :package :reader invalid-value-error-package)
   (symbol :initarg :symbol :reader invalid-value-error-symbol)
   (value :initarg :value :reader invalid-value-error-value)
   (customizable-setting :initarg :setting :reader invalid-value-error-customizable-setting))
  (:report
   (lambda (c s)
     (with-slots (value symbol package) c
       (format s "The value ~S is invalid for the symbol ~S in package ~S"
	       value symbol package)))))

(define-condition internal-invalid-value-error (error) ())

(defmacro %customize (symbol value db)
  (alexandria:with-gensyms (hold dispatch-id setting validated)
    `(let* ((,hold ,value)
	    (,dispatch-id (make-setting-id-from-symbol ',symbol))
	    (,setting (gethash ,dispatch-id ,db)))
       (restart-case 
	   (if ,setting
	       (handler-case
		   (let ((,validated (funcall (customizable-setting-validator-fn ,setting)
					      ,hold)))
		     (setf ,symbol ,validated))
		 (internal-invalid-value-error ()
		   (error 'invalid-value-error
			  :package (car ,dispatch-id)
			  :symbol (cdr ,dispatch-id)
			  :value ,hold
			  :original-expression ',value
			  :setting ,setting)))
	       (error 'no-valid-customizable-setting :id ,dispatch-id))
	 (set-regardless ()
	   :report (lambda (stream)
		     (format stream "Set ~S to ~S" ',symbol ,hold))
	   (setf ,symbol ,hold))))))

(defmacro customize (symbol value &rest rest)
  (let ((db (or (loop for (s v) on rest by #'cddr
		      when (equal :db s)
			do (return v))
		'*customizable-settings-hash-table*))
	(others (loop for x in rest
		      if (equal :db x)
			do (return ac)
		      else collect x into ac)))
    `(progn ,@(loop for (s v) on (cons symbol (cons value others)) by #'cddr
		    collect `(%customize ,s ,v ,db)))))
