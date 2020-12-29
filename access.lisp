(in-package :defconfig)

(defun place->config-info (place &optional (db *default-db*))
  (if (listp place)
      (or (gethash place (car db))
	  (gethash (car place) (car db)))
      (gethash place (cdr db))))

(defun config-info-search (tag/s &optional (db *default-db*))
  (let ((tags (if (listp tag/s) tag/s (list tag/s)))
	(fobjs nil)
	(vobjs nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (block tagblock
		 (loop for tag in (config-info-tags v)
		       do (loop for el in tags
				if (string= el tag)
				  do (push v fobjs)
				     (return-from tagblock)))))
	     (car db))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (block tagblock
		 (loop for tag in (config-info-tags v)
		       do (loop for el in tags
				if (string= el tag)
				  do (push v vobjs)
				     (return-from tagblock)))))
	     (cdr db))
    (list fobjs vobjs)))




