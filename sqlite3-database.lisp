(in-package :searty)

(defparameter *index-directory*
  (asdf:system-relative-pathname :searty "index/"))

(defparameter *sqlite3-database-file*
  (namestring (asdf:system-relative-pathname :searty (merge-pathnames "searty.db" *index-directory*))))
(defparameter *sqlite3-schema-file*
  (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defun sqlite3-init-database ()
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,*sqlite3-database-file*)))

(defclass sqlite3-database (database)
  ()
  (:default-initargs
   :connection (dbi:connect :sqlite3 :database-name *sqlite3-database-file*)))

(defmethod resolve-locations ((database sqlite3-database) token-id)
  (let ((filepath (merge-pathnames token-id *index-directory*)))
    (when (uiop:file-exists-p filepath)
      (with-open-file (in filepath :element-type '(unsigned-byte 8))
        (decode-locations in)))))

(defmethod upsert-inverted-index ((database sqlite3-database) token-id locations)
  (let* ((encoded-locations (encode-locations-to-vector locations))
         (filepath (merge-pathnames token-id *index-directory*)))
    (write-byte-vector-into-file encoded-locations filepath :if-exists :supersede)
    (execute-sql (database-connection database)
                 "INSERT INTO inverted_index (token_id, locations_data_file) VALUES (?, ?)
ON CONFLICT(token_id) DO NOTHING"
                 (list token-id (namestring filepath)))))
