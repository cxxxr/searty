(in-package :searty)

(defparameter *sqlite3-index-directory*
  (asdf:system-relative-pathname :searty "index/"))

(defparameter *sqlite3-database-file*
  (namestring (merge-pathnames "searty.db" *sqlite3-index-directory*)))
(defparameter *sqlite3-schema-file*
  (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defun sqlite3-init-database ()
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,*sqlite3-database-file*)))

(defclass sqlite3-database (database)
  ((index-directory :initarg :index-directory
                    :initform *sqlite3-index-directory*
                    :reader sqlite3-database-index-directory))
  (:default-initargs
   :connection (dbi:connect :sqlite3 :database-name *sqlite3-database-file*)))

(defun make-sqlite3-database (index-directory)
  (let* ((database-file (merge-pathnames "searty.db" index-directory))
         (connection (dbi:connect :sqlite3 :database-name database-file)))
    (make-instance 'sqlite3-database
                   :connection connection
                   :index-directory index-directory)))

(defmethod resolve-locations ((database sqlite3-database) token-id)
  (let ((filepath (merge-pathnames token-id (sqlite3-database-index-directory database))))
    (when (uiop:file-exists-p filepath)
      (with-open-file (in filepath :element-type '(unsigned-byte 8))
        (decode-locations in)))))

(defmethod upsert-inverted-index ((database sqlite3-database) token-id locations)
  (let* ((encoded-locations (encode-locations-to-vector locations))
         (filepath (merge-pathnames token-id (sqlite3-database-index-directory database))))
    (write-byte-vector-into-file encoded-locations filepath :if-exists :supersede)
    (execute-sql (database-connection database)
                 "INSERT INTO inverted_index (token_id, locations_data_file) VALUES (?, ?)
ON CONFLICT(token_id) DO NOTHING"
                 (list token-id (namestring filepath)))))
