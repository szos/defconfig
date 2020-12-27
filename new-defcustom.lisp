(in-package :defcustom)

(defclass config-info-functions ()
  ((predicate :initarg :predicate :initform #'identity
	      :documentation "The predicate against which valid values are checked")
   (coercer :initarg :coercer :initform nil
	    :documentation "The function by which invalid datum will attempt to be coerced")))

(defclass config-info-direct-info ()
  ((symbol :initarg :symbol :accessor config-info-symbol
	   :documentation "The symbol which this config info governs. no package information is included")
   (symbol-package :initarg :package :accessor config-info-symbol-package
		   :documentation "The package within which the config-info symbol resides")))

(defclass config-info-metadata ()
  ((name :initarg :name :initform "Unnamed config-info object" :accessor config-info-name
	 :documentation "The formal name by which this config-info object can be searched for")
   (tags :initarg :tags :initform '() :accessor config-info-tags
	 :documentation "Tags which can be used for ")))

(defclass config-info (config-info-metadata config-info-functions config-info-direct-info) ())

(defun make-customizer-database ()
  "creates a cons of two hash tables, the car for function lookup and the cdr for variable lookup."
  (cons (make-hash-table :test 'equalp) (make-hash-table :test 'equalp)))

(defparameter *customizer-db* (make-customizer-database))

(defun %db-type (place)
  (if (symbolp place)
      :variable
      :function))

(defun %customizer-dispatch-id (symbol-or-list)
  (let ((sym (if (symbolp symbol-or-list)
		 symbol-or-list
		 (car symbol-or-list))))
    (cons (read-from-string (package-name (symbol-package sym))) (read-from-string (symbol-name sym)))))

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
	    (,dispatch-id ,(%customizer-dispatch-id place))
	    (,obj (or (gethash ,dispatch-id ,real-db)
		      (restart-case (error 'config-not-found-error :place (quote ,place) :db-type ,namespace)
			(use-db (,new-db)
			  :report "supply a new database to query"
			  :interactive (lambda () (%read-new-db))
			  (gethash ,dispatch-id ,new-db)))))
	    (,pred (slot-value ,obj 'predicate))
	    (,coer (slot-value ,obj 'coercer))
	    (,valid? (funcall ,pred ,value-holder)))
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
	     (t (error 'invalid-datum-error :place (quote ,place)) :value ,value-holder)))))

(defun lookup-customizer (symbol-or-list &key (db *customizer-db*))
  (let* ((dispatch-symbol (if (symbolp symbol-or-list) symbol-or-list (car symbol-or-list)))
	 (dispatch-id (make-setting-id-from-symbol dispatch-symbol)))
    (lookup-customizer-object dispatch-id (if (symbolp symbol-or-list)
					      (car db)
					      (cdr db)))))

(macrolet ((window-x (window)
	     `(let ((customizer-object (lookup-function-customizer 'stumpwm::window-x)))
		))))
