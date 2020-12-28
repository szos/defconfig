(in-package :defcustom)

(defconfig *varname* 'dark :valid-values '(dark light) :test 'eql :reinitialize t)
;; the above would expand into a defparameter form and config-info which validates 'dark against the list
;; '(dark light) using comparison-fn.
(defconfig *varname* 'dark :predicate 'symbolp)
;; the above would expand into a defvar form which validates 'dark using 'symbolp
(defconfig *varname* :top :valid-values '(member :top :bottom :left :right) :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("varname" "*varname*" "location")
  :reinitialize t)
;; the above would expand into a defparamter form which validates against the list '(:top :bottom :left :right) and
;; if validation fails coercion is attempted on the value and the result validated again. It would have 
;; documentation for the variable and a list of tags for searching for the object.

;;; the following examples would register an object WITHOUT creating a def* form

(defconfig (input-bar-color *default-input-bar*) 'dark :valid-values '(dark light) :test 'eql :reinitialize t)
;; The above would expand into a config-info instance which validates against the list '(dark light) using 'eql.
;; because reinitialize is provided, it will also include the form (setf (input-bar-color *default-input-bar*) 'dark)
;; this will only validate when the accessor input-bar-color is called on the argument *default-input-bar*
(defconfig (input-bar-color *default-input-bar*) 'dark :predicate 'symbolp)
;; the above would expand into a config-info instance which validates using 'symbolp, and doesnt include a setf form
;; this will only validate when the accessor input-bar-color is called on the argument *default-input-bar*
(defconfig (input-bar-location) :top :valid-values
  '(member :top :bottom :left :right) :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("input-bar" "bar" "input" "location"))
;; the above will expand into a config-info instance which validates based on the list '(:top :bottom :left :right)
;; it will attempt to coerce invalid values using 'location-sym-coercer
;; it will be valid for ALL forms beginning (setf (input-bar-location ...) ...), as opposed to the previous examples
;; which are only valid when called with the form (setf (input-bar-color *default-input-bar*) ...)






;;; the below examples are not propper, and should be ignored. they are left here as a not to self. 

(defcustom *example* :testing
  "a basic example to test the customizablilty of *example*"
  '(:testing :hi :this :is :a :test))

;;; now, this will work:

(customize *example* :hi)

;;; but this will not:

(customize *example* :nonworking)

;;; lets try another example

(defcustom *another-example* :keyword
  "another example, which accepts any keywords"
  'keywordp)
