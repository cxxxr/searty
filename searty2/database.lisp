(in-package :searty2)

(defvar *database*)

(defclass database ()
  ((connection :initarg :connection
               :reader database-connection)))

(defstruct (document (:constructor make-document (pathname body)))
  (id (random-uuid))
  pathname
  body)

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t)
    (princ (document-pathname document) stream)))

(defun insert-document (database pathname)
  (let ((document (%make-document pathname)))
    (execute-sxql (database-connection database)
                  (sxql:insert-into :document
                    (sxql:set= :id (document-id document)
                               :pathname (namestring pathname)
                               :body (document-body document))))
    document))
