(in-package :defconfig)

;; the simplest example:
(defconfig *varname* 'dark)
;; the above would expand into a defvar form and a config-info instance, which validates using 'identity making
;; every value valid.

;; more complexe examples. 
(defconfig *varname* 'dark :typespec '(member dark light) :reinitialize t :regen-config t)
;; the above would expand into a defparameter form and config-info which validates 'dark against the list
;; '(dark light) using comparison-fn.
(defconfig *varname* 'dark :validator 'symbolp)
;; the above would expand into a defvar form which validates 'dark using 'symbolp
(defconfig *varname* :top :typespec '(member :top :bottom :left :right) :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("varname" "*varname*" "location")
  :reinitialize t :regen-config t)
;; the above would expand into a defparamter form which validates against the list '(:top :bottom :left :right) and
;; if validation fails coercion is attempted on the value and the result validated again. It would have 
;; documentation for the variable and a list of tags for searching for the object.

;;; the following examples would register an object WITHOUT creating a def* form

(defconfig (input-bar-color *default-input-bar*) 'dark :typespec '(member dark light) :reinitialize t)
;; The above would expand into a config-info instance which validates against the list '(dark light) using 'eql.
;; because reinitialize is provided, it will also include the form (setf (input-bar-color *default-input-bar*) 'dark)
;; this will only validate when the accessor input-bar-color is called on the argument *default-input-bar*
(defconfig (input-bar-color) 'dark :validator 'symbolp)
;; the above would expand into a config-info instance which validates using 'symbolp, and doesnt include a setf form
;; this will only validate when the accessor input-bar-color is called on an argument that isnt *default-input-bar*
;; assuming that no other defconfig forms have been defined. 
(defconfig (input-bar-location) :top :typespec '(member :top :bottom :left :right)
  :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("input-bar" "bar" "input" "location")
  :regen-config t)
(defconfig (input-bar-location *default-bar*) :top :typespec '(member :top :bottom :left :right :center)
  :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("input-bar" "bar" "input" "location")
  :regen-config t)
;; the above will expand into a config-info instance which validates based on the list '(:top :bottom :left :right)
;; it will attempt to coerce invalid values using 'location-sym-coercer
;; it will be valid for ALL forms beginning (setf (input-bar-location ...) ...), as opposed to the previous examples
;; which are only valid when called with the form (setf (input-bar-color *default-input-bar*) ...)

;;; When determining which object to use to validate when setting using an accessor we want to first check if the
;;; specific form has a validator. using the above as an example, if we call
;;; (set-valid (input-bar-color *mybar*) 'red), we will first look to see if '(input-bar-color *mybar*) is a key
;;; in the accessors hash table, and if so use it. if not, we then look to see if 'input-bar-color is a key in the
;;; same table (or perhaps a different table? db representation still needs to be decided upon) and if so use that.
;;; so for the above examples here is the behavioure:
;;; (set-valid (input-bar-color *default-input-bar*) 'red) - this will be invalid
;;; (set-valid (input-bar-color *mybar*) 'red) - this will be valid. 

;;; Regarding documentation:
;;; Should documentation be provided in a parameter/var producing defconfig form then the documentation should be
;;; both spliced in to the def* form AND added to the config-info object.
;;; should it be provided in an accessor defconfig, it will only be added to the config-info object.


;;;; Some Common Use Cases
;;;; If you have a use case you feel is common, feel free to add it here

;;; Bounded Numbers
;; sometimes you might want to have a bounded number - for example a counter that shouldnt exceed a certain value.
;; we can use defconfig and setv to ensure that it isnt exceeded in two ways - typespec or a predicate.
;; for this use case, typespec is the easiest. We will also supply a coercer, on the off chance someone tries to set
;; this to a string.
(defconfig *bounded-number* 0 :typespec '(integer 0 10)
  :coercer (lambda (x) (if (stringp x) (parse-integer x) x))
  :documentation "A number with the bounds 0 to 10 inclusive"
  :tags '("bounded number" "integer")
  :reinitialize t :regen-config t)

;;; Matching Specific Strings
;; You might want to check if a string is formatted propperly - here typespec is insufficient.
;; one example of this is to make sure that StumpWM mode line strings only contain valid formatters
(defun valid-formatter? (string) t) ; lets ignore this for now
(defun validate-stump-mode-line (string)
  (loop for (c1 c2) on (coerce string 'list)
	if (and (char= c1 #\%)
		(not (valid-formatter? (coerce (list c1 c2) 'string))))
	  do (return-from validate-stump-mode-line nil))
  t)
(defconfig *screen-mode-line-format* "[^B%n^b] %W" :validator 'validate-stump-mode-line
  :tags '("stumpwm" "mode-line" "mode-line-format"))



;;; WITH-ATOMIC-SETV

;; this macro resets all setv'd places on encountering any error
;; all examples use the following defconfig definitions
(defconfig *varname* :top :typespec '(member :top :bottom :left :right) ;; :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("varname" "*varname*" "location")
  :reinitialize t :regen-config t)
(defconfig *varname2* :top :typespec '(member :top :bottom :left :right) ;; :coercer 'location-sym-coercer
  :documentation "a variable for locations on screen" :tags '("varname" "*varname*" "location")
  :reinitialize t :regen-config t)

;; for example, the following wont modify *varname* or *varname2*, as :center is forbidden
;; however, it will technically modify *varname*, it just resets it upon encountering the
;; error with *varname2*.
(with-atomic-setv ()
  (setv *varname* :bottom)
  (setv *varname2* :center))

;; but this example will modify both. 
(with-atomic-setv ()
  (setv *varname* :bottom)
  (setv *varname2* :right))

;; errors signalled outside of setv forms wont reset variables. thusly this example leaves
;; *varname2* bound to :right (assuming your evaluating these examples one after the other)
;; while *varname* gets bound to :left, unless a continue restart is put in place  around error
(with-atomic-setv ()
  (setv *varname* :left)
  (error "some error!")
  (setv *varname2* :bottom))


