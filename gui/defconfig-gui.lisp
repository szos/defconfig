;;;; defconfig-gui.lisp

(in-package :defconfig-gui)

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro monospaced ((stream) &body body)
  `(with-text-family (,stream :fix)
     ,@body))

(defmacro format-color (stream ink control-string &rest args)
  `(with-drawing-options (,stream :ink ,ink)
     (format ,stream ,control-string ,@args)))

(defmacro change-layout (frame layout)
  `(unless (equal (frame-current-layout ,frame) ,layout)
     (setf (frame-current-layout ,frame) ,layout)))

(define-defconfig-db *gui-db* :defconfig-gui
  :parameter nil
  :doc "a defconfig database for the defconfig gui's settings")

(defconfig *database-name-color* +dark-green+
  :typespec 'climi::standard-color
  :db *gui-db*
  :tags '("database-color" "database-display-color" "color")
  :documentation "the color with which to display database names"
  :reinitialize t :regen-config t)

(defconfig *config-symbol-color* +dark-sea-green+ :typespec 'climi::standard-color
  :db *gui-db*
  :tags '("config" "symbol" "color")
  :documentation "the color with which to display config symbols"
  :reinitialize t :regen-config t)

(define-application-frame defconfig-configurator ()
  ((current-config-info :initform nil
			:accessor configurator-current-config-info))
  (:panes
   (database-list :application
		  :display-function 'display-database-list
		  :scroll-bars nil
		  :width :compute
		  :height :compute)
   (database-view :application
		  :display-function 'display-database
		  :scroll-bars nil
		  :width :compute
		  :height :compute)
   (config-info-view :application
		     :display-function 'display-config-info
		     :scroll-bars nil
		     :width :compute
		     :height :compute)
   (interactor :interactor))
  (:layouts
   (default
    (vertically ()
      (20 database-list)
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (:fill (scrolling () database-view))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (1/4 interactor)))
   (inspect-config-info
    (vertically ()
      (20 database-list)
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (:fill (scrolling () config-info-view))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (1/4 interactor)))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'defconfig-configurator)))

(define-defconfig-configurator-command (com-refresh-frame :name "Refresh")  () ())

(define-defconfig-configurator-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-type database-list-presentation ())

(define-defconfig-configurator-command (com-inspect-database :name t)
    ((database keyword))
  (let* ((dbp (defconfig::getdb database))
	 (name (car dbp))
	 (db (cdr dbp)))
    (set-display-db database name db))
  (change-layout *application-frame* 'default))

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
		 (format pane "~S (" key )
		 (italic (pane)
		   (format pane "~S" (car db)))
		 (format pane ")"))
	       (format pane ", ")))))

(define-presentation-type inpect-config-info-presentation ())
(define-presentation-to-command-translator inspect-config-info
    (inpect-config-info-presentation com-inspect-config-info
     defconfig-configurator :gesture :select
			    :documentation "Inspect the configuration object")
    (obj)
  (list obj))

(define-defconfig-configurator-command (com-inspect-config-info :name t)
    ((config-info defconfig::config-info))
  (setf (configurator-current-config-info *application-frame*) config-info)
  (unless (equal (frame-current-layout *application-frame*) 'inspect-config-info)
    (setf (frame-current-layout *application-frame*) 'inspect-config-info)))

(defun hash-table-empty-p (hash-table)
  (case (hash-table-count hash-table)
    (0 t)
    (otherwise nil)))

(defun write-database-to-stream (pane db &optional accessor)
  (slim:with-table (pane)
    (bold (pane)
      (slim:row
	(slim:cell
	  (format pane "Package"))
	(slim:cell
	  (format pane (if accessor "Accessor" "Variable")))
	(slim:cell
	  (format pane "Value"))))
    (cond ((not (hash-table-p db))
	   (slim:row (slim:cell (format pane "Unknown Database Type ~A" db))))
	  ((hash-table-empty-p db)
	   (slim:row (slim:cell (format pane "No Entries"))))
	  (t (maphash
	      (lambda (k v)
		(with-output-as-presentation
		    (pane v 'inpect-config-info-presentation)
		  (slim:row
		    (slim:cell
		      (format pane "~A" (package-name (symbol-package k))))
		    (slim:cell
		      (format-color pane *config-symbol-color* "~A"
				    (symbol-name k)))
		    (slim:cell
		      (format pane "~S" (symbol-value k))))))
	      db)))))

(let (key dbname db)
  (defun display-database (frame pane)
    (declare (ignore frame))
    (format pane "Inspecting Database ")
    (bold (pane) (format-color pane *database-name-color* "~S" key))
    (format pane ", also referred to as ")
    (bold (pane) (format-color pane *database-name-color* "~S~%~%" dbname))
    (format pane "~%Accessor Database~%")
    (with-end-of-line-action (pane :allow)
      (write-database-to-stream pane (car db) t))
    (format pane "~%Variable Database~%")
    (with-end-of-line-action (pane :allow)
      (write-database-to-stream pane (cdr db))))
  (defun set-display-db (db-key db-name database)
    (setf key db-key
	  dbname db-name
	  db database)))

(defun display-db-entry-values-column (pane config-info)
  (slim:row 
    (slim:cell
      (italic (pane)
	(format pane "Default Value:  ")))
    (slim:cell
      (monospaced (pane)
	(format pane "~A" (config-info-default-value config-info))))
    (slim:cell
      (with-output-as-gadget (pane)
	(make-pane 'push-button
		   :label (format nil "RESET ~A" (config-info-place config-info))
		   :activate-callback
		   (lambda (gadget)
		     (declare (ignore gadget))
		     (defconfig:reset-computed-place
		      (config-info-place config-info)
		      :db (symbol-value
			   (defconfig::config-info-db
			    config-info)))
		     (com-refresh-frame))))))
  (slim:row
    (slim:cell
      (italic (pane)
	(format pane "Previous Value:  ")))
    (slim:cell
      (monospaced (pane)
	(format pane "~A" (config-info-prev-value config-info))))
    (slim:cell
      (with-output-as-gadget (pane)
	(make-pane 'push-button
		   :label (format nil "RESET ~A" (config-info-place config-info))
		   :activate-callback
		   (lambda (gadget)
		     (declare (ignore gadget))
		     (defconfig::reset-computed-place
		      (config-info-place config-info)
		      :db (symbol-value
			   (defconfig::config-info-db
			    config-info))
		      :previous-value t)
		     (com-refresh-frame)))))))

(defun display-db-entry-fn (pane label name doc)
  (slim:row
    (slim:cell
      (italic (pane)
	(format pane "~A:  " label)))
    (slim:cell
      (monospaced (pane)
	(format pane "~A" name))))
  (slim:row
    (slim:cell
      (italic (pane)
	(format pane "Documentation:  ")))
    (slim:cell
      (monospaced (pane)
	(format pane "~A" doc)))))

(defun display-db-entry-functions-column (pane config-info)
  (flet ((get-fn-name (fnct)
	   (multiple-value-bind (ig1 ig2 lamb)
	       (function-lambda-expression fnct)
	     (declare (ignore ig1 ig2))
	     (if (listp lamb)
		 (list (car lamb) (cadr lamb))
		 lamb))))
    (let* ((cfn (config-info-coercer config-info))
	   (fn (config-info-predicate config-info))
	   (doc (documentation fn 'function))
	   (name (get-fn-name fn)))
      (display-db-entry-fn pane "Predicate" name doc)
      (when cfn
	(let ((cdoc (documentation cfn 'function))
	      (cname (get-fn-name cfn)))
	  (display-db-entry-fn pane "Coercer" cname cdoc))))))

(defun display-db-entry (pane config-info)
  (slim:with-table (pane)
    ;; one column for values, one for metadata, one for functions
    (display-db-entry-values-column pane config-info)
    ;; (display-db-entry-functions-column pane config-info)
    ))

(defun display-config-info (frame pane)
  (let* ((info-obj (configurator-current-config-info frame)))
    (format pane " ~%")
    (slim:with-table (pane)
      (slim:row
	(slim:cell
	  (format-color pane *config-symbol-color*
			"~0,4T~A" (config-info-place info-obj))
	  (format pane "  holds the value  ")
	  (format-color pane *config-symbol-color*
			"~A" (symbol-value (config-info-place info-obj))))))
    ;; (with-output-as-gadget (pane)
    ;;   (make-pane 'push-button-pane
    ;; 		 :label "test button"
    ;; 		 :activate-callback
    ;; 		 (lambda (g)
    ;; 		   (declare (ignore g))
    ;; 		   (notify-user *application-frame* "Hi there!"))))
    (format pane "~%~0,4T~A~%" (config-info-documentation info-obj))
    (display-db-entry pane info-obj)))
