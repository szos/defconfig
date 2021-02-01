
(in-package :defconfig)

(defmacro defconf-a (place &key predicate coercer db tags documentation regen)
  (alexandria:with-gensyms (hash obj pred)
    `(let* ((,pred ,(if predicate predicate 'cl::identity))
            (,hash (car ,(if db db '*default-db*)))
            (,obj (gethash ',place ,hash)))
       (if (or (not ,obj) ,regen)
           (setf (gethash ',place ,hash)
                 (make-instance 'accessor-config-info
                                :name ,(format nil "config-info-~A" place)
                                :predicate ,pred
                                :place ',place
                                :db ,(if db db '*default-db*)
                                ,@(when documentation
                                    (list :documentation documentation))
                                ,@(when coercer
                                    (list :coercer coercer))
                                ,@(when tags
                                    (list :tags tags))))
           ,obj))))

(defmacro define-accessor-config (accessor &key validator typespec coercer db tags
					     regen-config documentation)
  "Define an accessor config object and place it in DB with the key ACCESSOR"
  (when (and validator typespec)
    (error "The arguments :VALIDATOR and :TYPESPEC cannot both be supplied"))
  `(defconf-a ,accessor
       ,@(cond (typespec `(:predicate (lambda (x) (typep x ,typespec))))
               (validator `(:predicate ,validator)))
     :coercer ,coercer :db ,db :tags ,tags :documentation ,documentation
     :regen ,regen-config))

(defmacro defconf-v (place default &key predicate coercer db tags documentation
                                     regen)
  (alexandria:with-gensyms (hold hash validated obj pred)
    `(let* ((,pred ,(if predicate predicate 'cl::identity))
            (,hold ,default)
            (,hash (cdr ,(if db db '*default-db*)))
            (,validated (funcall ,pred ,hold))
            (,obj (gethash ',place ,hash)))
       (unless ,validated (error 'invalid-datum-error :place ',place :value ,hold))
       (if (or (not ,obj) ,regen)
           (setf (gethash ',place ,hash)
                 (make-instance 'config-info
                                :name ,(format nil "config-info-~A" place)
                                :predicate ,pred
                                ,@(when coercer
                                    `(:coercer ,coercer))
                                ,@(when documentation
                                    `(:documentation ,documentation ))
                                ,@(when tags
                                    `(:tags ,tags))
                                :place ',place
                                :default ,hold
                                :db ,(if db db '*default-db*)))
           ,obj))))

(defmacro define-variable-config (place default-value
				  &key validator typespec coercer db tags
                                    documentation regen-config)
  "Define a variable config object and place it in DB with the key PLACE."
  (when (and validator typespec)
    (error "The arguments :VALIDATOR and :TYPESPEC cannot both be supplied"))
  `(defconf-v ,place ,default-value
     ,@(cond (typespec `(:predicate (lambda (x) (typep x ,typespec))))
             (validator `(:predicate ,validator)))
     :coercer ,coercer :db ,db :tags ,tags
     :documentation ,documentation :regen ,regen-config))

(defmacro defconfig (place &rest args)
  "Defconfig defines a config-info object and potentially a dynamic variable. 

PLACE may be either a symbol or a list of length 1. If PLACE is a list, defconfig
functions as a wrapper around define-accessor-config. If it is a symbol, defconfig 
defines a variable config as well as a dynamic variable. Additionally, if the first
element of ARGS is a keyword and the second element of ARGS is not a keyword, the
default value will be the value of PLACE. 

The following keys are acceptable in ARGS: VALIDATOR, TYPESPEC, COERCER, 
DOCUMENTATION, DB, TAGS, and REGEN-CONFIG. REINITIALIZE is also acceptable if 
PLACE is a symbol. 

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

TAGS are strings that can be used to search for a config-info object. The search 
functionality is currently only partially implemented."
  (cond ((consp place)
	 `(define-accessor-config ,(car place) ,@args))
	((and (keywordp (car args)) (not (keywordp (cadr args)))) 
	 ;; then we have no default value and we want to use the current value of
	 ;; place. 
	 (destructuring-bind (&key validator typespec coercer db tags
				reinitialize documentation regen-config)
	     args
	   (declare (ignore reinitialize))
	   `(define-variable-config ,place ,place
	      :validator ,validator :typespec ,typespec :coercer ,coercer
	      :db ,db :tags ,tags :documentation ,documentation
	      :regen-config ,regen-config)))
	(t
	 (destructuring-bind (default &key validator typespec coercer db tags
					reinitialize documentation regen-config)
	     args
	   `(prog1
		(restart-case
		    (define-variable-config ,place ,default
		      :validator ,validator :typespec ,typespec :coercer ,coercer
		      :db ,db :tags ,tags :documentation ,documentation
		      :regen-config ,regen-config)
		  (define-variable-regardless () nil))
	      (,(if reinitialize 'defparameter 'defvar)
	       ,place ,default ,@(when documentation (list documentation))))))))
