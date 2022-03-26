(in-package :searty2)

(defparameter *sqlite3-database-file* "/tmp/searty.sqlite3")

(defvar *database*)

(defclass database ()
  ((connection :initarg :connection
               :initform (dbi:connect :sqlite3 :database-name *sqlite3-database-file*)
               :reader database-connection)))

(defstruct (document (:constructor make-document (pathname body)))
  (id (random-uuid))
  pathname
  body)

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t)
    (princ (document-pathname document) stream)))

(defun document= (document1 document2)
  (equal (document-id document1) (document-id document2)))

(defun document< (document1 document2)
  (equal (document-id document1) (document-id document2)))

(defun insert-document (database document)
  (execute-sxql (database-connection database)
                (sxql:insert-into :document
                  (sxql:set= :id (document-id document)
                             :pathname (namestring (document-pathname document))
                             :body (document-body document))))
  document)
