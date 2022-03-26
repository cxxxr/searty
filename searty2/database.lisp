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
  document)

(defun resolve-inverted-index (database tokens kind)
  (resolve-sxql (database-connection database)
                (sxql:select (:token :kind :encoded_values)
                  (sxql:from :inverted_index)
                  (sxql:where (:and (:in :token (mapcar #'token-term tokens))
                                    (:= :kind kind))))))
