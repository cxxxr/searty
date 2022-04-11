(in-package :searty)

(defvar *database*)

(defgeneric insert-document (database document))
(defgeneric resolve-document-by-id (database id))
(defgeneric resolve-documents-by-ids (database ids))
(defgeneric resolve-whole-documents (database))
(defgeneric resolve-document-id-by-pathname (database pathname))
(defgeneric insert-token (database token))
(defgeneric resolve-token (database token))
(defgeneric resolve-token-by-id (database id))
(defgeneric resolve-tokens-by-ids (database ids))
(defgeneric resolve-whole-tokens (database))
(defgeneric resolve-inverted-index-by-token-ids (database token-ids))

(defclass database ()
  ((connection :initarg :connection
               :reader database-connection)))

(defun make-database ()
  (let ((connection (dbi:connect :postgres :database-name "searty" :username "searty_user" :password "searty")))
    (make-instance 'database
                   :connection connection)))

(defun connect-database ()
  (setq *database* (make-database)))

(defun delete-all-records (&optional (database *database*))
  (execute-sxql (database-connection database) (sxql:delete-from :document))
  (execute-sxql (database-connection database) (sxql:delete-from :token))
  (execute-sxql (database-connection database) (sxql:delete-from :inverted_index)))

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

(defmethod resolve-whole-documents ((database database))
  (make-documents-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :pathname :body)
                   (sxql:from :document)))))

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
                       (kind (decode-token-kind (getf record :|kind|))))
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

(defmethod resolve-whole-tokens ((database database))
  (make-tokens-from-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:id :term :kind)
                   (sxql:from :token)))))

(defun decode-inverted-index-records (records)
  (let ((inverted-index (make-inverted-index))
        (prev-token-id nil)
        (prev-document-id nil)
        (positions '())
        (locations '()))
    (dolist (record records)
      (let ((token-id (getf record :|token_id|))
            (document-id (getf record :|document_id|))
            (position (getf record :|position|)))
        (cond ((or (and (equal prev-token-id token-id)
                        (equal prev-document-id document-id))
                   (and (null prev-token-id)
                        (null prev-document-id)))
               (push position positions))
              ((equal prev-token-id token-id)
               (push (make-location :document-id prev-document-id
                                    :positions (nreverse positions))
                     locations)
               (setf positions (list position)))
              (t
               (push (make-location :document-id prev-document-id
                                    :positions (nreverse positions))
                     locations)
               (setf (inverted-index-get inverted-index prev-token-id)
                     (nreverse locations))
               (setf locations '())
               (setf positions (list position))))
        (setf prev-token-id token-id
              prev-document-id document-id)))
    (push (make-location :document-id prev-document-id
                         :positions (nreverse positions))
          locations)
    (setf (inverted-index-get inverted-index prev-token-id)
          (nreverse locations))
    inverted-index))

(defmethod resolve-inverted-index-by-token-ids ((database database) token-ids)
  (let ((records
          (resolve-sxql (database-connection database)
                        (sxql:select (:token_id :document_id :position)
                          (sxql:from :inverted_index)
                          (sxql:where (:in :token_id token-ids))
                          (sxql:order-by :token_id :document_id :position)))))
    (decode-inverted-index-records records)))

(defmethod insert-posting ((database database) token-id document-id position)
  (execute-sxql (database-connection database)
                (sxql:insert-into :inverted_index
                  (sxql:set= :token_id token-id
                             :document_id document-id
                             :position position))))
