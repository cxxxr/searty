(in-package :searty)

(defstruct location document-id positions)

(defstruct inverted-index
  (table (make-hash-table :test 'equal)))

(defun inverted-index-get (inverted-index token-id)
  (gethash token-id (inverted-index-table inverted-index)))

(defun (setf inverted-index-get) (value inverted-index token-id)
  (setf (gethash token-id (inverted-index-table inverted-index))
        value))
