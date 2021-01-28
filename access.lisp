(in-package :defconfig)

(defun list-of-strings-p (thing)
  (when (listp thing)
    (every 'stringp thing)))

(defun place->config-info (place &key (db *default-db*))
  (if (listp place)
      (or (gethash place (car db))
	  (gethash (car place) (car db)))
      (gethash place (cdr db))))

(defmacro with-config-info ((var place &key (db '*default-db*)) &body body)
  `(let ((,var (place->config-info ,place :db ,db)))
     (if ,var
	 (progn ,@body)
	 (error 'no-config-found-error :place ',place :db ',db))))

(defun config-info-search-tags (tags &key (namespace :both) (db *default-db*))
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
      (otherwise (list (fmap) (vmap))))))

(defun config-info-search-in-db (term &key (namespace :both) (db *default-db*))
  "takes a term, as well as a namespace and a database. the :db keyarg should be a
database as returned by make-config-database. the :namespace keyarg should be one
of :both :accessor or :variable. Note that the namespace keyarg isnt used if term
is a symbol. Term should be either a string, a list of strings, a symbol, or a
list of symbols representing an accessor and a place."
  (cond ((stringp term)
	 (config-info-search-tags (list term) :namespace namespace :db db))
	((list-of-strings-p term)
	 (config-info-search-tags term :namespace namespace :db db))
	((or (symbolp term) (listp term))
	 (place->config-info term :db db))))

(defun search-configurable-objects (term &optional database-key)
  "Returns a list of all configurable objects matching TERM. "
  (let ((dbs (if database-key
		 (list (get-db-var database-key))
		 (loop for (key (dbsym ahash . vhash)) on *db-plist* by 'cddr
		       collect dbsym))))
    (alexandria:flatten
     (loop for db in dbs
	   collect (config-info-search-in-db term :db (symbol-value db))))))

(defun reset-computed-place (place &key (db *default-db*) previous-value
				     (already-reset-test 'eql))
  "A function version of reset-place - ie it evaluates its arguments. "
  (let* ((obj (or (place->config-info place :db db)
		  (error 'no-config-found-error :place place :db db)))
	 (curval (symbol-value (config-info-place obj)))
	 (newval (if previous-value
		     (config-info-prev-value obj)
		     (config-info-default-value obj))))
    (unless (funcall already-reset-test curval newval)
      (setf (symbol-value (config-info-place obj)) newval
	    (config-info-prev-value obj) curval)
      newval)))

(defmacro reset-place (place &key (db '*default-db*) previous-value)
  "looks up a place and set it to its default or previous value."
  `(reset-computed-place ',place :db ,db :previous-value ,previous-value))

(defun clean-previous-value (place &key (db *default-db*))
  "use to set the previous value of place to the default value. This is useful for
places that may hold a large object which you want gc'd"
  (with-config-info (obj place :db db)
    (setf (config-info-prev-value obj) (config-info-default-value obj))))
