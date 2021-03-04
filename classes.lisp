(in-package :defconfig)

(defun generate-valid-values-predicate-string (&key typespec predicate)
  (cond (typespec 
         (format nil "Valid values must conform to the typespec ~S"
                 typespec))
        (t
         (format nil "Valid values must are tested using ~S"
                 (cond ((symbolp predicate) predicate)
                       ((functionp predicate)
                        (let ((fname (nth-value 2 (function-lambda-expression
                                                   predicate))))
                          (cond ((symbolp fname) fname)
                                ((listp fname) (list (car fname)
                                                     (cadr fname)))
                                (t 'unknown-function)))))))))

(defun generate-valid-values-coercer-string (&optional coercer)
  (if coercer
      (format nil ". Coercion is attempted with the function ~S."
              (cond ((symbolp coercer) coercer)
                    ((functionp coercer)
                     (let ((fname (nth-value 2 (function-lambda-expression
                                                coercer))))
                       (cond ((symbolp fname) fname)
                             ((listp fname) (list (car fname)
                                                  (cadr fname)))
                             (t 'unknown-function))))))
      "."))

(defun generate-vv-string (spec obj)
  (destructuring-bind (typespec-or-fn-indicator typespec-or-fn)
      spec
    (concatenate
     'string
     (if (eql typespec-or-fn-indicator 'typespec)
         (generate-valid-values-predicate-string :typespec typespec-or-fn)
         (generate-valid-values-predicate-string :predicate typespec-or-fn))
     (generate-valid-values-coercer-string (config-info-coercer obj)))))

(defclass config-info-functions ()
  ((predicate :initarg :predicate
              :initform #'identity
              :type (function (*) boolean)
              :reader config-info-predicate
              :documentation "The predicate against which valid values are checked")
   (coercer :initarg :coercer
            :initform nil
            :type (or function null)
            :reader config-info-coercer
            :documentation "The function by which invalid datum will attempt to be coerced")))

(defclass config-info-direct-info ()
  ((db :initarg :db
       :type (or symbol cons)
       :reader config-info-db
       :documentation "the database that the config info object is housed in.")
   (place :initarg :place
          :type symbol
          :reader config-info-place
          :documentation "The place which this config info governs.")))

(defclass config-info-values ()
  ((default-value :initarg :default
                  :reader config-info-default-value
                  :documentation "The default value of this config-info object")
   (prev-value :initarg :previous
               :reader config-info-previous-value
               :accessor config-info-prev-value
               :documentation "holds the value previously assigned to the config-info object. initially the same as default-value")))

(defclass config-info-metadata ()
  ((name :initarg :name
         :initform "Unnamed config-info object"
         :reader config-info-name
         :documentation "The formal name by which this config-info object can be searched for"
         ;; one cant yet search by name - searching needs to be reworked/rethought
         )
   (tags :initarg :tags
         :initform '()
         :reader config-info-tags
         :accessor config-info-tag-list
         :documentation "Tags which can be used for finding a config-info object")
   (docstring :initarg :documentation
              :initform nil
              :type (or string null)
              :reader config-info-documentation
              :documentation "The docstring for the place being governed. if a variable it is the same as the variables docstring")
   (valid-values :initarg :valid-values
                 :reader config-info-valid-values-description
                 :documentation "An explanation of the valid values and predicate function")))

(defclass minimal-config-info (config-info-functions config-info-direct-info) ())

(defclass accessor-config-info (config-info-metadata minimal-config-info) ())

(defclass config-info (accessor-config-info config-info-values) ())


(defmethod print-object ((object minimal-config-info) stream)
  (print-unreadable-object (object stream)
    (format stream "MINIMAL-CONFIG-INFO ~A" (config-info-place object))))

(defmethod print-object ((object config-info) stream)
  (print-unreadable-object (object stream)
    (format stream "CONFIG-INFO ~A" (config-info-place object))))

(defmethod print-object ((object accessor-config-info) stream)
  (print-unreadable-object (object stream)
    (format stream "ACCESSOR-CONFIG-INFO ~A" (config-info-place object))))

(defun slot-bound-p (obj slot)
  (slot-boundp obj slot))

(defmethod initialize-instance :after ((obj accessor-config-info) &key)
  (cond ((slot-bound-p obj 'valid-values)
         (unless (typep (slot-value obj 'valid-values) 'string)
           (setf (slot-value obj 'valid-values)
                 (generate-vv-string (slot-value obj 'valid-values) obj))))
        (t (setf (slot-value obj 'valid-values)
                 "Unspecified"))))

(defmethod initialize-instance :after ((obj config-info) &key)
  (unless (slot-boundp obj 'prev-value)
    (setf (slot-value obj 'prev-value) (slot-value obj 'default-value))))

