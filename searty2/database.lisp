(in-package :searty2)

(defparameter *sqlite3-database-file* "/tmp/searty.sqlite3")

(defvar *database*)

(defclass database ()
  ((connection :initarg :connection
               :initform (dbi:connect :sqlite3 :database-name *sqlite3-database-file*)
               :reader database-connection)))

(defun insert-document (database document)
  (execute-sxql (database-connection database)
                (sxql:insert-into :document
                  (sxql:set= :id (document-id document)
                             :pathname (namestring (document-pathname document))
                             :body (document-body document))))
  (values))

(defun decode-inverted-index-records (records tokens)
  (let ((inverted-index (make-inverted-index))
        (kind-set (delete-duplicates (mapcar (compose #'encode-token-kind #'token-kind) tokens))))
    (dolist (record records)
      (let ((term (getf record :|term|))
            (kind (getf record :|kind|))
            (encoded-values (getf record :|encoded_values|)))
        (when (find kind kind-set)
          (setf (inverted-index-get inverted-index
                                    (make-token :term term :kind kind))
                (decode-doc-locations-from-vector encoded-values)))))
    inverted-index))

(defun resolve-inverted-index (database tokens)
  (decode-inverted-index-records
   (resolve-sxql (database-connection database)
                 (sxql:select (:term :kind :encoded_values)
                   (sxql:from :inverted_index)
                   (sxql:where (:in :term (mapcar #'token-term tokens)))))
   tokens))

(defun upsert-inverted-index (database token locations)
  (let ((encoded-locations (encode-locations-to-vector locations)))
    (execute-sql (database-connection database)
                 "INSERT INTO inverted_index (term, kind, encoded_values) VALUES (?, ?, ?)
ON CONFLICT(token_id) DO UPDATE SET encoded_values = ?"
                 (list (token-term token)
                       (token-kind token)
                       encoded-locations
                       encoded-locations))))
