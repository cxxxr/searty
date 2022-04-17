(in-package :searty)

(defparameter *sqlite3-schema-file* (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defvar *database*)

(defgeneric insert-document (database document))
(defgeneric resolve-document-by-id (database id))
(defgeneric resolve-documents-by-ids (database ids))
(defgeneric resolve-whole-documents (database))
(defgeneric resolve-document-id-by-pathname (database pathname))
(defgeneric resolve-documents-by-pathnames (database pathnames))
(defgeneric insert-token (database token))
(defgeneric resolve-token (database token))
(defgeneric resolve-token-by-id (database id))
(defgeneric resolve-tokens-by-ids (database ids))
(defgeneric resolve-whole-tokens (database))
(defgeneric resolve-inverted-index-by-token-ids (database token-ids))
(defgeneric resolve-whole-inverted-index (database))
(defgeneric resolve-locations (database token-id))
(defgeneric upsert-inverted-index (database token-id locations))
(defgeneric resolve-symbol-id (database symbol))
(defgeneric insert-symbol (database symbol))
(defgeneric copy-symbol-table (dst-database src-database))
(defgeneric insert-symbol-definition (database symbol-id filename position))
(defgeneric copy-symbol-definition-table (dst-database src-database))

(defclass database ()
  ((connection :initarg :connection
               :reader database-connection)))

(defclass sqlite3-database (database)
  ())

(defun sqlite3-init-database (database-file)
  (ensure-directories-exist database-file)
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,database-file)))

(defun make-sqlite3-database (database-file)
  (let* ((connection (dbi:connect :sqlite3 :database-name database-file)))
    (make-instance 'sqlite3-database :connection connection)))

(defun call-with-database (database-file initialize without-disconnect function)
  (when initialize
    (sqlite3-init-database database-file))
  (let ((database (make-sqlite3-database database-file)))
    (unwind-protect (dbi:with-transaction (database-connection database)
                      (funcall function database))
      (unless without-disconnect
        (dbi:disconnect (database-connection database))))))

(defmacro with-database ((var database-file &key initialize without-disconnect) &body body)
  `(call-with-database ,database-file ,initialize ,without-disconnect (lambda (,var) ,@body)))

(defmethod insert-document ((database sqlite3-database) document)
  (execute-sxql (database-connection database)
                (sxql:insert-into :document
                  (sxql:set= :pathname (namestring (document-pathname document))
                             :external_format (string-upcase (document-external-format document))
                             :body (document-body document))))
  document)

(defun make-documents-from-records (records)
  (mapcar (lambda (record)
            (let ((id (getf record :|id|))
                  (pathname (getf record :|pathname|))
                  (external-format (getf record :|external_format|))
                  (body (getf record :|body|)))
              (make-document :id id :pathname pathname :external-format external-format :body body)))
          records))

(defmethod resolve-document-by-id ((database sqlite3-database) id)
  (when-let (document
             (make-documents-from-records
              (resolve-sxql (database-connection database)
                            (sxql:select (:id :pathname :external_format :body)
                              (sxql:from :document)
                              (sxql:where (:= :id id))
                              (sxql:limit 1)))))
    (first document)))

(defmethod resolve-documents-by-ids ((database sqlite3-database) ids)
  (make-documents-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :pathname :external_format :body)
                   (sxql:from :document)
                   (sxql:where (:in :id ids))))))

(defmethod resolve-whole-documents ((database sqlite3-database))
  (make-documents-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :pathname :external_format :body)
                   (sxql:from :document)))))

(defmethod resolve-document-id-by-pathname ((database sqlite3-database) pathname)
  (when-let ((records
              (resolve-sxql (database-connection database)
                            (sxql:select :id
                              (sxql:from :document)
                              (sxql:where (:= :pathname (namestring pathname)))
                              (sxql:limit 1)))))
    (getf (first records) :|id|)))

(defmethod resolve-documents-by-pathnames ((database sqlite3-database) pathnames)
  (make-documents-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :pathname :external_format)
                   (sxql:from :document)
                   (sxql:where (:in :pathname (mapcar #'namestring pathnames)))))))

(defmethod insert-token ((database sqlite3-database) token)
  (unless (token-id token)
    (setf (token-id token) (random-uuid)))
  (execute-sxql (database-connection database)
                (sxql:insert-into :token
                  (sxql:set= :id (token-id token)
                             :term (babel:string-to-octets (token-term token))
                             :kind (encode-token-kind (token-kind token)))))
  token)

(defmethod resolve-token ((database sqlite3-database) token)
  (when-let* ((records
               (resolve-sxql
                (database-connection database)
                (sxql:select :id
                  (sxql:from :token)
                  (sxql:where (:and (:= :term (babel:string-to-octets (token-term token)))
                                    (:= :kind (encode-token-kind (token-kind token)))))
                  (sxql:limit 1)))))
    (let* ((record (first records))
           (id (getf record :|id|)))
      (setf (token-id token) id))
    token))

(defun make-tokens-from-records (records)
  (loop :for record :in records
        :collect (let ((id (getf record :|id|))
                       (term (babel:octets-to-string (getf record :|term|)))
                       (kind (decode-token-kind (getf record :|kind|))))
                   (make-token :id id :term term :kind kind))))

(defmethod resolve-token-by-id ((database sqlite3-database) id)
  (when-let ((tokens (make-tokens-from-records
                      (resolve-sxql (database-connection database)
                                    (sxql:select (:id :term :kind)
                                      (sxql:from :token)
                                      (sxql:where (:= :id id))
                                      (sxql:limit 1))))))
    (first tokens)))

(defmethod resolve-tokens-by-ids ((database sqlite3-database) ids)
  (make-tokens-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :term :kind)
                   (sxql:from :token)
                   (sxql:where (:in :id ids))))))

(defmethod resolve-whole-tokens ((database sqlite3-database))
  (make-tokens-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :term :kind)
                   (sxql:from :token)))))

(defun decode-inverted-index-records (records)
  (let ((inverted-index (make-inverted-index)))
    (dolist (record records)
      (let ((token-id (getf record :|token_id|))
            (locations (getf record :|locations|)))
        (setf (inverted-index-get inverted-index token-id)
              (decode-doc-locations-from-vector locations))))
    inverted-index))

(defmethod resolve-inverted-index-by-token-ids ((database sqlite3-database) token-ids)
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:token_id :locations)
                   (sxql:from :inverted_index)
                   (sxql:where (:in :token_id token-ids))))))

(defmethod resolve-whole-inverted-index ((database sqlite3-database))
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:token_id :locations)
                   (sxql:from :inverted_index)))))

(defmethod resolve-locations ((database sqlite3-database) token-id)
  (when-let ((record (resolve-sxql (database-connection database)
                                   (sxql:select (:locations)
                                     (sxql:from :inverted_index)
                                     (sxql:where (:= :token_id token-id))))))
    (decode-doc-locations-from-vector (getf record :|locations|))))

(defmethod upsert-inverted-index ((database sqlite3-database) token-id locations)
  (let* ((encoded-locations (encode-locations-to-vector locations)))
    (execute-sql (database-connection database)
                 "INSERT INTO inverted_index (token_id, locations) VALUES (?, ?)
ON CONFLICT(token_id) DO NOTHING"
                 (list token-id encoded-locations))))

(defmethod resolve-symbol-id ((database sqlite3-database) symbol)
  (when-let ((records (resolve-sxql (database-connection database)
                                    (sxql:select :id
                                      (sxql:from :symbol)
                                      (sxql:where (:and (:= :package (symbol-package symbol))
                                                   (:= :name (symbol-name symbol))))))))
    (when records
      (getf (first records) :|id|))))

(defmethod insert-symbol ((database sqlite3-database) symbol)
  (let ((id (random-uuid)))
    (execute-sxql (database-connection database)
                  (sxql:insert-into :symbol
                    (sxql:set= :id id
                               :name (symbol-name symbol)
                               :package (package-name (symbol-package symbol)))))
    id))

(defmethod copy-symbol-table ((dst-database sqlite3-database) (src-database sqlite3-database))
  (dolist (record (resolve-sxql (database-connection src-database)
                                (sxql:select (:id :name :package)
                                  (sxql:from :symbol))))
    (execute-sxql (database-connection dst-database)
                  (sxql:insert-into :symbol
                    (sxql:set= :id (getf record :|id|)
                               :name (getf record :|name|)
                               :package (getf record :|package|))))))

(defmethod insert-symbol-definition ((database sqlite3-database) symbol-id filename position)
  (execute-sxql (database-connection database)
                (sxql:insert-into :symbol_definition
                  (sxql:set= :symbol_id symbol-id
                             :filename filename
                             :position position))))

(defmethod copy-symbol-definition-table ((dst-database sqlite3-database) (src-database sqlite3-database))
  (dolist (record (resolve-sxql (database-connection src-database)
                                (sxql:select (:symbol_id :filename :position)
                                  (sxql:from :symbol_definition))))
    (execute-sxql (database-connection dst-database)
                  (sxql:insert-into :symbol_definition
                    (sxql:set= :symbol_id (getf record :|symbol_id|)
                               :filename (getf record :|filename|)
                               :position (getf record :|position|))))))
