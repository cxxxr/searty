(in-package :searty)

(defun uuid= (id1 id2)
  (string= id1 id2))

(defun document-id= (id1 id2)
  (= id1 id2))

(defun document-id< (id1 id2)
  (< id1 id2))

(defstruct document
  id
  pathname
  external-format
  body)

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t)
    (princ (document-pathname document) stream)))

(defun document= (document1 document2)
  (equal (document-id document1) (document-id document2)))

(defun document< (document1 document2)
  (equal (document-id document1) (document-id document2)))
