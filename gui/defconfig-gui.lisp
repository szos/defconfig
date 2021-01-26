;;;; defconfig-gui.lisp

(in-package :defconfig-gui)

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro format-color (stream ink control-string &rest args)
  `(with-drawing-options (,stream :ink ,ink)
     (format ,stream ,control-string ,@args)))

(define-defconfig-db *gui-db* :defconfig-gui
  :parameter nil
  :doc "a defconfig database for the defconfig gui's settings")

(defconfig *database-name-color* +orange-red+
  :typespec 'climi::standard-color
  :db *gui-db*
  :tags '("database-color" "database-display-color" "color")
  :documentation "the color with which to display database names")

(defconfig *config-symbol-color* +green+ :typespec 'climi::standard-color
  :db *gui-db*
  :tags '("config" "symbol" "color")
  :documentation "the color with which to display config symbols")

(define-application-frame defconfig-configurator () ()
  (:panes
   (database-list :application
		  :display-function 'display-database-list
		  :scroll-bars nil
		  :width 600)
   (database-view :application
		  :display-function 'display-database
		  :scroll-bars nil
		  :width 600)
   (interactor :interactor))
  (:layouts
   (default
    (vertically ()
      (20 database-list)
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (:fill (scrolling () database-view))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (1/4 interactor)))))

(define-defconfig-configurator-command (com-refresh-frame :name t) () ())
(define-defconfig-configurator-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-type database-list-presentation ())

(define-defconfig-configurator-command (com-inspect-database :name t)
    ((database keyword))
  (let* ((dbp (defconfig::getdb database))
	 (name (car dbp))
	 (db (cdr dbp)))
    (set-display-db database name db)))

(define-presentation-to-command-translator inspect-database
    (database-list-presentation com-inspect-database defconfig-configurator
     :gesture :select
     :documentation "Inspect a database")
    (db-key)
  (list db-key))

(defun display-database-list (frame pane)
  (declare (ignore frame))
  (with-end-of-line-action (pane :wrap*)
    (bold (pane) (format pane "DATABASES:  "))
    (let ((dbs defconfig::*db-plist*))
      (loop for (key db) on dbs by 'cddr
	    do (with-output-as-presentation (pane key 'database-list-presentation)
		 (format pane "~S (~S), " key (car db)))))))

(defun write-database-to-stream (pane db)
  (slim:with-table (pane)
    (slim:row
      (slim:cell
	(format pane "Config Variable"))
      (slim:cell
	(format pane "Current Value"))
      (slim:cell
	(format pane "Previous Value"))
      (slim:cell
	(format pane "Default Value"))
      (slim:cell
	(format pane "Valid Values")))
    (terpri pane)
    (if (hash-table-p db)
	(maphash
	 (lambda (k v)
	   (slim:row
	     (slim:cell 
	       (format-color pane *config-symbol-color* "~S" k)
	       (format pane ": "))
	     (slim:cell
	       (format pane "~S" (symbol-value k)))
	     (slim:cell
	       (format pane "~S" (config-info-prev-value v)))
	     (slim:cell
	       (format pane "~S" (config-info-default-value v)))
	     (slim:cell
	       (format pane "~S" (config-info-valid-values-description v))))
	   (terpri pane))
	 db)
	(format pane "~S" db))))

(let (key dbname db)
  (defun display-database (frame pane)
    (declare (ignore frame))
    (format pane "Inspecting Database ")
    (bold (pane) (format-color pane *database-name-color* "~S" key))
    (format pane ", also referred to as ")
    (bold (pane) (format-color pane *database-name-color* "~S~%~%" dbname))
    (format pane "~%Accessor Database~%")
    (with-end-of-line-action (pane :allow)
      (write-database-to-stream pane (car db)))
    (fresh-line pane)
    (fresh-line pane)
    (format pane "~%Variable Database~%")
    (with-end-of-line-action (pane :allow)
      (write-database-to-stream pane (cdr db))))
  (defun set-display-db (db-key db-name database)
    (setf key db-key
	  dbname db-name
	  db database)))
