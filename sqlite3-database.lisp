(in-package :searty)

(defparameter *sqlite3-database-file* "/tmp/searty.sqlite3")
(defparameter *sqlite3-schema-file* (namestring (asdf:system-relative-pathname :searty "schema.sqlite3")))

(defun sqlite3-init-database ()
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,*sqlite3-database-file*)))

(defclass sqlite3-database (database)
  ()
  (:default-initargs
   :connection (dbi:connect :sqlite3 :database-name *sqlite3-database-file*)))

(defmethod upsert-inverted-index ((database sqlite3-database) token-id locations)
  (let ((encoded-locations (encode-locations-to-vector locations)))
    (execute-sql (database-connection database)
                 "INSERT INTO inverted_index (token_id, encoded_values) VALUES (?, ?)
ON CONFLICT(token_id) DO UPDATE SET encoded_values = ?"
                 (list token-id
                       encoded-locations
                       encoded-locations))))
