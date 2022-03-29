(in-package :searty)

(defparameter *sqlite3-database-file* "/tmp/searty.sqlite3")
(defparameter *sqlite3-schema-file* (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defun sqlite3-init-database ()
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,*sqlite3-database-file*)))

(defvar *database*)

(defgeneric insert-document (database document))
(defgeneric resolve-document-by-id (database id))
(defgeneric resolve-documents-by-ids (database ids))
(defgeneric insert-token (database token))
(defgeneric resolve-token (database token))
(defgeneric resolve-token-by-id (database id))
(defgeneric resolve-tokens-by-ids (database ids))
(defgeneric resolve-inverted-index-by-token-ids (database token-ids))
(defgeneric resolve-whole-inverted-index (database))
(defgeneric upsert-inverted-index (database token-id locations))

(defclass database ()
  ((connection :initarg :connection
               :reader database-connection)))

(defclass sqlite3-database (database)
  ()
  (:default-initargs
   :connection (dbi:connect :sqlite3 :database-name *sqlite3-database-file*)))

(defmethod insert-document ((database database) document)
  (execute-sxql (database-connection database)
                (sxql:insert-into :document
                  (sxql:set= :id (document-id document)
                             :pathname (namestring (document-pathname document))
                             :body (document-body document))))
  document)

(defun make-documents-from-records (records)
  (mapcar (lambda (record)
            (let ((id (getf record :|id|))
                  (pathname (getf record :|pathname|))
                  (body (getf record :|body|)))
              (make-document :id id :pathname pathname :body body)))
          records))

(defmethod resolve-document-by-id ((database database) id)
  (when-let (document
             (make-documents-from-records
              (resolve-sxql (database-connection database)
                            (sxql:select (:id :pathname :body)
                              (sxql:from :document)
                              (sxql:where (:= :id id))
                              (sxql:limit 1)))))
    (first document)))

(defmethod resolve-documents-by-ids ((database database) ids)
  (make-documents-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :pathname :body)
                   (sxql:from :document)
                   (sxql:where (:in :id ids))))))

(defmethod insert-token ((database database) token)
  (unless (token-id token)
    (setf (token-id token) (random-uuid)))
  (execute-sxql (database-connection database)
                (sxql:insert-into :token
                  (sxql:set= :id (token-id token)
                             :term (babel:string-to-octets (token-term token))
                             :kind (encode-token-kind (token-kind token)))))
  token)

(defmethod resolve-token ((database database) token)
  (when-let* ((records
               (resolve-sxql (database-connection database)
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
                       (kind (getf record :|kind|)))
                   (make-token :id id :term term :kind kind))))

(defmethod resolve-token-by-id ((database database) id)
  (when-let ((tokens (make-tokens-from-records
                      (resolve-sxql (database-connection database)
                                    (sxql:select (:id :term :kind)
                                      (sxql:from :token)
                                      (sxql:where (:= :id id))
                                      (sxql:limit 1))))))
    (first tokens)))

(defmethod resolve-tokens-by-ids ((database database) ids)
  (make-tokens-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :term :kind)
                   (sxql:from :token)
                   (sxql:where (:in :id ids))))))

(defun decode-inverted-index-records (records)
  (let ((inverted-index (make-inverted-index)))
    (dolist (record records)
      (let ((token-id (getf record :|token_id|))
            (encoded-values (getf record :|encoded_values|)))
        (setf (inverted-index-get inverted-index token-id)
              (decode-doc-locations-from-vector encoded-values))))
    inverted-index))

(defmethod resolve-inverted-index-by-token-ids ((database database) token-ids)
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:token_id :encoded_values)
                   (sxql:from :inverted_index)
                   (sxql:where (:in :token_id token-ids))))))

(defmethod resolve-whole-inverted-index ((database database))
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:token_id :encoded_values)
                   (sxql:from :inverted_index)))))

(defmethod upsert-inverted-index ((database sqlite3-database) token-id locations)
  (let ((encoded-locations (encode-locations-to-vector locations)))
    (execute-sql (database-connection database)
                 "INSERT INTO inverted_index (token_id, encoded_values) VALUES (?, ?)
ON CONFLICT(token_id) DO UPDATE SET encoded_values = ?"
                 (list token-id
                       encoded-locations
                       encoded-locations))))
