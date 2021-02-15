(in-package :defconfig)

(defmacro atypecase (thing &body cases)
  `(let ((it ,thing))
     (typecase it
       ,@cases)))
