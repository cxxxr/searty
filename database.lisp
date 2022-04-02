(in-package :searty)

(defvar *database*)

(defgeneric insert-document (database document))
(defgeneric resolve-document-by-id (database id))
(defgeneric resolve-documents-by-ids (database ids))
(defgeneric resolve-document-id-by-pathname (database pathname))
(defgeneric insert-token (database token))
(defgeneric resolve-token (database token))
(defgeneric resolve-token-by-id (database id))
(defgeneric resolve-tokens-by-ids (database ids))
(defgeneric resolve-inverted-index-by-token-ids (database token-ids))
(defgeneric resolve-whole-inverted-index (database))
(defgeneric resolve-locations (database token-id))
(defgeneric upsert-inverted-index (database token-id locations))

(defclass database ()
  ((connection :initarg :connection
               :reader database-connection)))

(defmethod insert-document ((database database) document)
  (execute-sxql (database-connection database)
                (sxql:insert-into :document
                  (sxql:set= :pathname (namestring (document-pathname document))
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

(defmethod resolve-document-id-by-pathname ((database database) pathname)
  (when-let ((records
              (resolve-sxql (database-connection database)
                            (sxql:select :id
                              (sxql:from :document)
                              (sxql:where (:= :pathname (namestring pathname)))
                              (sxql:limit 1)))))
    (getf (first records) :|id|)))

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
            (locations-data-filepath (getf record :|locations_data_file|)))
        (setf (inverted-index-get inverted-index token-id)
              (decode-doc-locations-from-vector
               (read-file-into-byte-vector locations-data-filepath)))))
    inverted-index))

(defmethod resolve-inverted-index-by-token-ids ((database database) token-ids)
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:token_id :locations_data_file)
                   (sxql:from :inverted_index)
                   (sxql:where (:in :token_id token-ids))))))

(defmethod resolve-whole-inverted-index ((database database))
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:token_id :locations_data_file)
                   (sxql:from :inverted_index)))))

(defmethod resolve-locations ((database database) token-id)
  (error "unimplemented"))

(defmethod upsert-inverted-index ((database database) token-id locations)
  (error "unimplemented ~S" '(upsert-inverted-index (database) t t)))
