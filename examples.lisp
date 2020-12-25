(in-package :defcustom)

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
