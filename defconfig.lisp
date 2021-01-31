
(in-package :defconfig)

(defclass config-info-functions ()
  ((predicate :initarg :predicate :initform #'identity
              :type (function (*) boolean)
              :reader config-info-predicate
              :documentation "The predicate against which valid values are checked")
   (coercer :initarg :coercer :initform nil :reader config-info-coercer
            :documentation "The function by which invalid datum will attempt to be coerced")))

(defclass config-info-direct-info ()
  ((db :initarg :db :reader config-info-db
       :documentation "the database that the config info object is housed in.")
   (place :initarg :place :reader config-info-place
          :documentation "The place which this config info governs.")))

(defclass config-info-values ()
  ((default-value :initarg :default :reader config-info-default-value
                  :documentation "The default value of this config-info object")
   (prev-value :initarg :previous :reader config-info-previous-value
	       :accessor config-info-prev-value
	       :documentation "holds the value previously assigned to the config-info object. initially the same as default-value")))

(defclass config-info-metadata ()
  ((name :initarg :name :initform "Unnamed config-info object"
         :reader config-info-name
         :documentation "The formal name by which this config-info object can be searched for"
         ;; one cant yet search by name - searching needs to be reworked/rethought
         )
   (tags :initarg :tags :initform '() :reader config-info-tags
	 :accessor config-info-tag-list
         :documentation "Tags which can be used for finding a config-info object")
   (docstring :initarg :documentation :initform nil
              :reader config-info-documentation
              :documentation "The docstring for the place being governed. if a variable it is the same as the variables docstring")
   (valid-values :initarg :valid-values :initform :unset
                 :reader config-info-valid-values-description
                 :documentation "An explanation of the valid values and predicate function")))

(defclass config-info (config-info-metadata config-info-functions
                       config-info-direct-info config-info-values)
  ())

(defclass accessor-config-info (config-info-metadata config-info-functions
				config-info-direct-info)
  ())

(defmethod print-object ((object config-info) stream)
  (print-unreadable-object (object stream)
    (format stream "CONFIG-INFO ~A" (config-info-place object))))

(defmethod print-object ((object accessor-config-info) stream)
  (print-unreadable-object (object stream)
    (format stream "ACCESSOR-CONFIG-INFO ~A" (config-info-place object))))

;;; turn valid-values into a string, unless a custom string is provided. this should be moved into the generation fn
(defmethod initialize-instance :after ((obj config-info) &key)
  (with-slots (valid-values predicate coercer) obj
    (unless (slot-boundp obj 'prev-value)
      (setf (slot-value obj 'prev-value) (slot-value obj 'default-value)))
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

(define-condition config-error (error) ()
  (:documentation
   "This condition is the root condition for all defconfig conditions. If one wants
to catch all defconfig conditions (such as with handler-case) then config-error 
should be used."))

(define-condition invalid-datum-error (config-error)
  ((place-form :initarg :place :reader invalid-datum-error-place :initform nil)
   (value :initarg :value :reader invalid-datum-error-value :initform nil)
   (config-object :initarg :config-object :initform nil
		  :reader invalid-datum-error-config-object))
  (:report
   (lambda (c s)
     (with-slots (place-form value) c
       (format s "The value ~S is invalid for place ~S." value place-form))))
  (:documentation
   "This condition indicates that VALUE is invalid for PLACE-FORM"))

(define-condition invalid-coerced-datum-error (invalid-datum-error)
  ((coerced-value :initarg :coerced-value :initform nil
                  :reader invalid-coerced-datum-error-value))
  (:report
   (lambda (c s)
     (with-slots (place-form value coerced-value) c
       (format s "The value ~S is invalid for place ~S.~%Coercion produced the value ~S, which is also invalid"
               value place-form coerced-value))))
  (:documentation
   "This condition indicates that coercion was attempted on VALUE, producing COERCED-VALUE, and that COERCED-VALUE is invalid for PLACE-FORM"))

(define-condition no-config-found-error (config-error)
  ((place-form :initarg :place :reader no-config-found-error-place :initform nil)
   (database :initarg :db :reader no-config-found-error-db :initform nil))
  (:report
   (lambda (c s)
     (with-slots (place-form database) c
       (format s "Unable to find config-info for place ~S in database ~S"
	       place-form database))))
  (:documentation
   "This condition indicates that PLACE-FORM does not denote a config-info object in DATABASE"))

(define-condition database-already-exists-error (config-error)
  ((key :initarg :key :reader database-already-exists-error-key))
  (:report
   (lambda (condition stream)
     (format stream "The key ~S already denotes a defconfig database"
             (database-already-exists-error-key condition))))
  (:documentation
   "This condition indicates KEY already denotes a database in *db-plist*"))

(define-condition untrackable-place-error (config-error)
  ((object :initarg :object :reader untrackable-place-error-object)
   (place :initarg :place :reader untrackable-place-error-place))
  (:report
   (lambda (condition stream)
     (format stream "The place ~S does not track default or previous values"
	     (untrackable-place-error-place condition)))))

(define-condition no-bound-default-value-error (untrackable-place-error) ()
  (:report
   (lambda (condition stream)
     (format stream
	     "No default value bound when trying to reset ~S"
	     (untrackable-place-error-place condition))))
  (:documentation
   "This condition indicates that the default-value slot of OBJECT is unbound. 
This will only be signalled when trying to reset a place to its default value."))

(define-condition not-resettable-place-error (untrackable-place-error) ()
  (:report
   (lambda (condition stream)
     (format stream "Place ~S is an accessor and is not resettable"
	     (untrackable-place-error-place condition))))
  (:documentation
   "This condition indicates that a reset was attempted on an accessor place."))

;;; database

(defparameter *db-plist* nil
  "a plist holding all databases for defconfig")

(defun make-config-database ()
  "creates a cons of two hash tables, the car for function lookup and the cdr for
variable lookup. car must be equalp as we compare lists. for internal use only."
  (cons (make-hash-table :test 'equalp) 
        (make-hash-table :test 'eql)))

(defun add-db-to-plist (key varname)
  "add a database to *db-plist* in the correct format of (KEY (VARNAME value)).
VARNAME is the quoted variable referencing the database, while value is the
symbol-value of VARNAME. for internal use only"
  (setf *db-plist* (cons key
			 (cons (cons varname
				     (symbol-value varname))
			       *db-plist*))))

(defun db-key-exists-p (key)
  "return t/nil if KEY denotes a pre-existing db"
  (if (getf *db-plist* key) t nil))

(defun getdb (key)
  "used internally by defconfig to wrap around getf - think of it as currying getf
with *db-plist* as the place"
  (getf *db-plist* key))

(defun get-db (key)
  "get the database associated with KEY"
  (cdr (getdb key)))

(defun get-db-var (key)
  "get the variable holding the database associated with KEY"
  (car (getdb key)))

(defun list-dbs ()
  "list all defconfig databases"
  (loop for (key db) on *db-plist* by 'cddr
        collect key))

(defun delete-db (key &optional makunbound)
  "delete the database associated with KEY. if MAKUNBOUND is T, then unbind the 
symbol holding the database associated with KEY"
  (let ((dbvar (get-db-var key)))
    (when (and makunbound dbvar)
      (makunbound dbvar)))
  (remf *db-plist* key))

(defun def-defconfig-db-error-check (key var)
  "Check that KEY is a keyword and doesnt already denotes a database in
*db-plist*. If it does signal an error within the context of a use-value restart
to allow the user to provide a new value to use instead of KEY"
  (restart-bind ((use-value
		   (lambda (new-key)
		     (return-from def-defconfig-db-error-check new-key))
		   :interactive-function (lambda () (list (read *query-io*)))
		   :report-function (lambda (stream)
				      (format stream "Supply a new key to use"))
		   :test-function (lambda (condition)
				    (typecase condition
				      (type-error t)
				      (database-already-exists-error t)
				      (t nil)))))
    (cond ((not (keywordp key))
	   (error 'type-error
		  :expected-type 'keyword
		  :datum key
		  :context (format nil "when defining defconfig database ~S"
				   var)))
	  ((db-key-exists-p key)
	   (error 'database-already-exists-error :key key))
	  (t key))))

(defmacro define-defconfig-db (var key &key (parameter t)
					 (doc "A defconfig database"))
  "define a dynamic variable name VAR to be a defconfig database accessible by
passing KEY to the function get-db. If PARAMETER is true, create this var with 
a defparameter form, otherwise use defvar. DOC is the documentation to pass to 
the def(parameter|var) form."
  (alexandria:with-gensyms (realkey)
    `(locally (declare (special ,var))
       (let ((,realkey (def-defconfig-db-error-check ,key ',var)))
	 (declare (ignorable ,realkey))
	 (,(if parameter 'defparameter 'defvar) ,var (make-config-database) ,doc)
	 (add-db-to-plist ,realkey ',var)))))

(define-defconfig-db *default-db* :default
  :doc "The default database for defconfig")

;;; actual defconfig workers and macros.

(defun %defconfig-accessor (place &key coercer tags name regen-config
				    (db '*default-db*) valid-values-list
				    documentation
				    (predicate 'cl::identity predicate-provided-p))
  (alexandria:with-gensyms (hash obj pred)
    `(let* ((,pred ,@(if predicate-provided-p
                         `(,predicate)
                         `(',predicate)))
	    (,hash (car ,db))
	    (,obj (gethash ',(car place) ,hash)))
       (if (or (not ,obj) (and ,obj ,regen-config))
	   (setf (gethash ',(car place) ,hash)
		 (make-instance 'accessor-config-info
				:predicate ,pred
				,@(when coercer
				    `(:coercer ,coercer))
				,@(when documentation
				    `(:documentation ,documentation))
				:name ,(if name
					   name
					   (format nil "config-info object for ~A"
						   place))
				,@(when tags
				    `(:tags ,tags))
				:place ',place
				,@(when valid-values-list
				    `(:valid-values ,valid-values-list))
				:db ',db))
	   ,obj))))

(defun %defconfig-parameter (place default &key coercer reinitialize tags name
					     regen-config (db '*default-db*)
					     valid-values-list documentation
					     (predicate 'cl::identity
							predicate-provided-p))
  "The worker function for defconfig. This does the following
1) validating the default value
2) validating the default value
3) gathering the any preexisting object

After gathering the the needed variables, it determines whether or not to set to
set the place to the default value. After that, it checks whether or not a new 
config-info object should be created and if so creates it and places it in the 
database"
  (alexandria:with-gensyms (hold hash validated obj pred)
    `(let* ((,pred ,@(if predicate-provided-p
                         `(,predicate)
                         `(',predicate)))
            (,hold ,default)
            (,hash (cdr ,db))
	    (,validated (funcall ,pred ,hold))
            (,obj (gethash ',place ,hash)))
       (if ,validated
	   (,(if reinitialize 'defparameter 'defvar)
	    ,place ,hold ,@(when documentation (list documentation)))
	   (error 'invalid-datum-error :place ',place :value ,hold))
       (if (or (not ,obj) (and ,obj ,regen-config))
	   (setf (gethash ',place ,hash)
		 (make-instance 'config-info
				,@(when predicate
				    `(:predicate ,pred))
				,@(when coercer
				    `(:coercer ,coercer))
				,@(when documentation
				    `(:documentation ,documentation))
				:name ,(if name
					   name
					   (format nil "config-info-~A" place))
				,@(when tags
				    `(:tags ,tags))
				:place ',place
				:default ,hold
				,@(when valid-values-list
				    `(:valid-values ,valid-values-list))
				:db ',db))
	   ,obj))))

(defun %defconfig (place default &key predicate coercer reinitialize 
				      regen-config documentation tags name db
				      valid-values-list)
  (if (listp place) ; if place is a list its an accessor
      (%defconfig-accessor place :predicate predicate
				 :coercer coercer
				 :regen-config regen-config
				 :name name
				 :tags tags
				 :db db
				 :valid-values-list valid-values-list
				 :documentation documentation)
      (%defconfig-parameter place default :predicate predicate
					  :coercer coercer
					  :reinitialize reinitialize
					  :regen-config regen-config
					  :name name
					  :tags tags
					  :db db
					  :valid-values-list valid-values-list
					  :documentation documentation)))

(defmacro %defconf-vv-intermediary (place default &key predicate coercer name
                                                    reinitialize regen-config
                                                    documentation tags db
                                                    valid-values-list)
  "wrap around a call to %defconfig - for use by defconfig when generating a 
lambda out of a typespec "
  (%defconfig place default :predicate predicate :reinitialize reinitialize
                            :tags tags :regen-config regen-config :name name
                            :coercer coercer :documentation documentation
                            :valid-values-list valid-values-list :db db))

(defmacro defconfig (place default-value &key validator typespec coercer
                                           reinitialize regen-config name
                                           documentation tags (db '*default-db*))
  "Defconfig defines a config-info object and potentially a dynamic variable. 

PLACE can be a symbol or a list. If it is a symbol it is assumed to be a dynamic
variable. If a config-info object has already been generated and placed in DB, 
another one is not generated unless REGEN-CONFIG is true. Similarly, if 
REINITIALIZE is false we generate a defvar call of: 
; (defvar PLACE DEFAULT-VALUE DOCUMENTATION)
wheras if it is true we generate
; (defparameter PLACE DEFAULT-VALUE DOCUMENTATION)

If PLACE is a list it is assumed to be an accessor or other setf-able function
call (ie something defined with defsetf). It must be a list of one element, the 
accessor or setf-able function to dispatch upon. When PLACE is a list DEFAULT-VALUE
is ignored - accessor-config-info objects do not track previous or default values.
If REINITIALIZE is true and the length of PLACE is 2 then PLACE is set to the 
default value. REGEN-CONFIG is as above.  

VALIDATOR and TYPESPEC may not coexist in a single defconfig call. VALIDATOR is
for providing a function to validate values. It must take a single value, the 
value to validate. TYPESPEC takes a type specification and generates a
validation function from it. 

If provided, COERCER must be a function taking a single argument: the value to
coerce. It is called iff an invalid value is passed to setv, and it is called on
the invalid value in an attempt to generate a valid one. The return value of 
COERCER is checked against the VALIDATOR (or the function generated with
TYPESPEC) and if it is valid it is used in place of the original value.

DOCUMENTATION is the documentation of PLACE and is used in the
defvar/defparameter form when PLACE is a symbol and is placed in the config-info
object regardless of whether PLACE is a symbol or a list.

DB is the database to place the generated config-info object into, and defaults
to *default-db*. Defconfig does not check if DB is in the plist of databases
before placing the config-info object into DB. It is assumed that if a DB has been
removed from the database plist the user has a good understanding of what they
are doing and is managing the database themselves. (databases must be manually
removed from the plist). 

NAME is a string naming the generated config-info object. it is currently
unused.

TAGS are strings that can be used to search for a config-info object. The search 
functionality is currently only partially implemented. "
  (when (and validator typespec)
    (error "A validator and typespec keyargs cannot both be provided to defconfig"))
  (cond (typespec
         `(%defconf-vv-intermediary ,place ,default-value
                                    :predicate (lambda (x)
                                                 ,(format nil "check if X is of type ~A" typespec)
                                                 (typep x ,typespec))
                                    :coercer ,coercer :reinitialize ,reinitialize
                                    :documentation ,documentation :tags ,tags
                                    :regen-config ,regen-config :db ,db :name ,name
                                    :valid-values-list ,typespec))
	(validator
         (%defconfig place default-value :predicate validator :coercer coercer
                                         :reinitialize reinitialize :db db
                                         :documentation documentation :name name
                                         :tags tags :regen-config regen-config))
        
        (t
         (%defconfig place default-value
                     :coercer coercer :reinitialize reinitialize :db db
                     :documentation documentation :tags tags
                     :regen-config regen-config :name name))))

(defmacro defconfig-accessor (place &key validator typespec coercer documentation 
				      tags name regen-config (db '*default-db*))
  "A version of defconfig for use explicitly with defining accessors. arguments are
as in defconfig."
  (cond
    (typespec
     `(%defconf-vv-intermediary ,place nil
				:predicate
				(lambda (x)
				  ,(format nil "check if X is of type ~A" typespec)
				  (typep x ,typespec))
				:coercer ,coercer
				:documentation ,documentation :tags ,tags
				:regen-config ,regen-config :db ,db :name ,name
				:valid-values-list ,typespec))
    (validator
     (%defconfig-accessor place :predicate validator
				:coercer coercer
				:regen-config regen-config
				:name name
				:tags tags
				:db db
				:documentation documentation))
    (t 
     (%defconfig-accessor place :coercer coercer
				:regen-config regen-config
				:name name
				:tags tags
				:db db
				:documentation documentation))))
