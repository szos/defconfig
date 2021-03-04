(in-package :defconfig)

(defun defconfig-database-p (db)
  (and (consp db)
       (hash-table-p (car db))
       (hash-table-p (cdr db))
       (eql (hash-table-test (car db)) 'eql)
       (eql (hash-table-test (cdr db)) 'eql)))

(deftype defconfig-database ()
  `(satisfies defconfig-database-p))

(defvar *db-plist* '()
  "A plist holding all databases for defconfig")

(defun make-config-database ()
  "Return a cons of two hash tables"
  (cons (make-hash-table) (make-hash-table)))

(defun add-db-to-plist (key varname)
  "add a database to *db-plist* in the correct format of (KEY (VARNAME value)).
VARNAME is the quoted variable referencing the database, while value is the
symbol-value of VARNAME. for internal use only"
  (setf *db-plist* (cons key
                         (cons (cons varname
                                     (symbol-value varname))
                               *db-plist*))))

(defun db-key-exists-p (key)
  "return t/nil if KEY denotes a pre-existing db"
  (if (getf *db-plist* key) t nil))

(defun getdb (key)
  "used internally by defconfig to wrap around getf - think of it as currying getf
with *db-plist* as the place"
  (getf *db-plist* key))

(defun get-db (key)
  "get the database associated with KEY"
  (cdr (getdb key)))

(defun get-db-var (key)
  "get the variable holding the database associated with KEY"
  (car (getdb key)))

(defun list-dbs ()
  "list all defconfig databases"
  (loop for (key db) on *db-plist* by 'cddr
        collect key))

(defun delete-db (key &optional makunbound)
  "delete the database associated with KEY. if MAKUNBOUND is T, then unbind the 
symbol holding the database associated with KEY"
  (let ((dbvar (get-db-var key)))
    (when (and makunbound dbvar)
      (makunbound dbvar)))
  (remf *db-plist* key))

(defun def-defconfig-db-error-check (key var)
  "Check that KEY is a keyword and doesnt already denotes a database in
*db-plist*. If it does signal an error within the context of a use-value restart
to allow the user to provide a new value to use instead of KEY"
  (restart-bind ((use-value
                   (lambda (new-key)
                     (return-from def-defconfig-db-error-check new-key))
                   :interactive-function (lambda () (list (read *query-io*)))
                   :report-function (lambda (stream)
                                      (format stream "Supply a new key to use"))
                   :test-function (lambda (condition)
                                    (typecase condition
                                      (type-error t)
                                      (database-already-exists-error t)
                                      (t nil))))
                 (continue
                   (lambda ()
                     (return-from def-defconfig-db-error-check nil))
                   :report-function
                   (lambda (s)
                     (format s "Continue using existing database"))
                   :test-function (lambda (c)
                                    (typep c 'database-already-exists-error)))
                 (redefine
                   (lambda ()
                     (return-from def-defconfig-db-error-check key))
                   :report-function
                   (lambda (stream)
                     (format stream "Reinitialize the database denoted by ~A" key))
                   :test-function (lambda (c)
                                    (typep c 'database-already-exists-error))))
    (cond ((not (keywordp key))
           (error 'type-error
                  :expected-type 'keyword
                  :datum key
                  :context (format nil "when defining defconfig database ~S"
                                   var)))
          ((db-key-exists-p key)
           (error 'database-already-exists-error :key key))
          (t key))))

(defmacro define-defconfig-db (var key &key (parameter t) if-exists
                                         (doc "A defconfig database"))
  "define a dynamic variable name VAR to be a defconfig database accessible by
passing KEY to the function get-db. If PARAMETER is true, create this var with 
a defparameter form, otherwise use defvar. DOC is the documentation to pass to 
the def(parameter|var) form."
  (alexandria:with-gensyms (realkey)
    `(let ((,realkey (,@(case if-exists
                          (:use '(handler-bind
                                  ((database-already-exists-error
                                    (lambda (c)
                                      (let ((r (find-restart 'continue c)))
                                        (when r (invoke-restart r))))))))
                          (:redefine '(handler-bind
                                       ((database-already-exists-error
                                         (lambda (c)
                                           (let ((r (find-restart 'redefine c)))
                                             (when r (invoke-restart r))))))))
                          (otherwise '(progn)))
                      (def-defconfig-db-error-check ,key ',var))))
       (declare (special ,var))
       (when ,realkey
         (,(if parameter 'defparameter 'defvar) ,var (make-config-database) ,doc)
         (add-db-to-plist ,realkey ',var)))))

(define-defconfig-db *default-db* :default
  :doc "The default database for defconfig"
  :if-exists :use)
