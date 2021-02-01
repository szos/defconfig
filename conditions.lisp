
(in-package :defconfig)

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
