(in-package :defconfig)

(defun place->config-info (place &key (db *default-db*))
  (if (listp place)
      (or (gethash place (car db))
	  (gethash (car place) (car db)))
      (gethash place (cdr db))))

(defun config-info-search (tag/s &key (namespace :both) (db *default-db*))
  "Takes a string or list of strings, as well as a namespace and a database, and searches for objects with the 
provided tags in the namespace of the provided database. Returns a list whose car is the list of objects in 
accessor namespace and whose cadr is the list of objects in variable namespace. "
  (let ((tags (if (listp tag/s) tag/s (list tag/s))))
    (flet ((fmap ()
	     (let (fobjs)
	       (maphash (lambda (k v)
			  (declare (ignore k))
			  (loop for tag in (config-info-tags v)
				do (loop for el in tags
					 if (string= el tag)
					   do (push v fobjs))))
			(car db))
	       fobjs))
	   (vmap ()
	     (let (vobjs)
	       (maphash (lambda (k v)
			  (declare (ignore k))
			  (block tagblock
			    (loop for tag in (config-info-tags v)
				  do (loop for el in tags
					   if (string= el tag)
					     do (push v vobjs)))))
			(cdr db))
	       vobjs)))
      (case namespace
	((:accessors) (list (fmap) nil))
	((:variables) (list nil (vmap)))
	(otherwise (list (fmap) (vmap)))))))

(defmacro reset-place (place &key (db '*default-db*))
  "looks up a place "
  (alexandria:with-gensyms (obj)
    `(let ((,obj (place->config-info ',place :db ,db)))
       (if ,obj
	   (setf ,place (config-info-default-value ,obj))
	   (error 'no-config-found-error :place ',place :db ',db)))))


