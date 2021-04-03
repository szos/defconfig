(in-package :defconfig)

(defun list-of-strings-p (thing)
  (when (listp thing)
    (every 'stringp thing)))

(defun place->config-info (place &key (db *default-db*))
  (declare (type (or symbol cons) db)
           (type (or symbol cons) place))
  (let ((rdb (if (symbolp db) (symbol-value db) db)))
    (declare (type cons rdb))
    (if (listp place)
	(gethash (car place) (car rdb))
	(gethash place (cdr rdb)))))

(defun %%with-config-info (fn place database policy)
  (declare (type (member :strict :greedy) policy)
           (type function fn)
           (type (or symbol list) place)
           (type (or keyword symbol cons) database))
  (check-type policy (member :strict :greedy))
  (let ((*setv-permissiveness* policy)
	(db (cond ((keywordp database) (get-db database))
		  ((symbolp database) (symbol-value database))
		  (t database))))
    (declare (type cons db))
    (funcall fn (restart-case (%fsetv-get-config-info-object
			       place
                               (if (listp place) (car db) (cdr db))
                               database)
		  (use-value (new-database)
		    :test (lambda (c)
			    (and (typep c 'no-config-found-error)
				 (not (eql (no-config-found-error-db c)
					   'all-registered-databases))))
		    :report "Supply a new database"
		    :interactive (lambda ()
				   (format *query-io*
					   "Enter database key or variable: ")
				   (force-output *query-io*)
				   (list (read *query-io*)))
		    (%%with-config-info fn place new-database policy))))))

(defmacro with-config-info ((var place &key (db '*default-db*)
					 (unfound-policy :strict))
			    &body body)
  `(%%with-config-info (lambda (,var) ,@body) ,place ,db ,unfound-policy))

(defun tag-configurable-place (tag place &key (db *default-db*) reclass)
  "Push TAG onto the list of tags for the config-info object associated with 
PLACE in DB."
  (declare (type (or string symbol) tag)
           (type (or symbol list) place)
           (type (or symbol cons) db))
  (atypecase (place->config-info place :db db)
    (minimal-config-info
     (if reclass
	 (progn (change-class it 'accessor-config-info)
		(push tag (config-info-tag-list it)))
	 (error 'type-error :expected-type 'config-info-metadata
			    :datum it
			    :context (format nil "when pushing tagging ~S" it))))
    ((or accessor-config-info config-info)
     (push tag (config-info-tag-list it)))))

(defun config-info-search-tags (tags &key (namespace :both) (db *default-db*))
  (declare (type list tags)
           (type keyword namespace)
           (type cons db))
  (flet ((fmap ()
	   (let (fobjs)
	     (maphash (lambda (k v)
			(declare (ignore k))
			(loop for tag in (when (typep v 'config-info-metadata)
					   (config-info-tags v))
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
			  (loop for tag in (when (typep v 'config-info-metadata)
					     (config-info-tags v))
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
  (declare (type (or string list symbol) term)
           (type keyword namespace)
           (type cons db))
  (cond ((stringp term)
	 (config-info-search-tags (list term) :namespace namespace :db db))
	((list-of-strings-p term)
	 (config-info-search-tags term :namespace namespace :db db))
	((or (symbolp term) (listp term))
	 (place->config-info term :db db))))

(defun search-configurable-objects (term &optional database-key)
  "Returns a list of all configurable objects matching TERM. If DATABASE-KEY is 
provided, search only in that database."
  (declare (type (or keyword null) database-key))
  (let ((dbs (if database-key
		 (list (get-db-var database-key))
		 (loop for (key (dbsym ahash . vhash)) on *db-plist* by 'cddr
		       collect dbsym))))
    (alexandria:flatten
     (loop for db in dbs
	   collect (config-info-search-in-db term :db (symbol-value db))))))

(defgeneric reset-computed-place (place &key db previous-value test)
  (:documentation
   "Reset PLACE to its default value, unless PREVIOUS-VALUE is true, then reset to 
the previous value. TEST is used to check if a reset is needed.")

  (:method ((place symbol) &key (db *default-db*) (test 'eql) previous-value)
    (with-config-info (obj place :db db)
      (reset-computed-place obj :db db :test test :previous-value previous-value)))

  (:method ((object config-info) &key db (test 'eql) previous-value)
    (declare (ignore db))
    (let ((curval (symbol-value (config-info-place object)))
	  (newval (if previous-value
		      (config-info-previous-value object)
		      (config-info-default-value object))))
      (unless (funcall test curval newval)
	(setf (symbol-value (config-info-place object)) newval
	      (config-info-prev-value object) curval)
        newval)))

  (:method ((place list) &key db test previous-value)
    (declare (ignore test previous-value))
    (with-config-info (obj place :db db)
      (error 'not-resettable-place-error :place place :object obj)))

  (:method ((object accessor-config-info) &key db test previous-value)
    (declare (ignore db test previous-value))
    (error 'not-resettable-place-error :place (config-info-place object)
				       :object object))
  (:method ((object minimal-config-info) &key db test previous-value)
    (declare (ignore db test previous-value))
    (error 'not-resettable-place-error :place (config-info-place object)
				       :object object)))

(defmacro reset-place (place &key (db '*default-db*) previous-value)
  "looks up PLACE in DB and set it to its default or previous value."
  `(reset-computed-place ',place :db ,db :previous-value ,previous-value))

(defgeneric clean-previous-value (place &key db)
  (:documentation
   "use to set the previous value of PLACE to the default value. This is useful for
places that may hold a large object which you want gc'd")
  
  (:method ((place symbol) &key (db *default-db*))
    (with-config-info (object place :db db)
      (setf (config-info-prev-value object) (config-info-default-value object))))
  
  (:method ((object config-info) &key db)
    (declare (ignore db))
    (setf (config-info-prev-value object) (config-info-default-value object)))
  
  (:method ((place list) &key db)
    (with-config-info (object place :db db)
      (error 'untrackable-place-error :place place :object object)))
  
  (:method ((object accessor-config-info) &key db)
    (declare (ignore db))
    (error 'untrackable-place-error :place (config-info-place object)
				    :object object)))
