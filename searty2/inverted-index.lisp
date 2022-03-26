(in-package :searty2)

(defstruct inverted-value kind locations)
(defstruct location document-id positions)

(defstruct inverted-index
  (table (make-hash-table :test 'equal)))

(defun inverted-index-get (inverted-index term)
  (gethash term (inverted-index-table inverted-index)))

(defun (setf inverted-index-get) (value inverted-index term)
  (setf (gethash term (inverted-index-table inverted-index))
        value))

(defun inverted-index-insert (inverted-index document-id token)
  (let* ((inverted-values (inverted-index-get inverted-index (token-term token)))
         (inverted-value (find (token-kind token) inverted-values :key #'inverted-value-kind)))
    (if (null inverted-value)
        (push (make-inverted-value
               :kind (token-kind token)
               :locations (list (make-location :document-id document-id
                                               :positions (list (token-position token)))))
              (inverted-index-get inverted-index (token-term token)))
        (let ((loc (find document-id
                         (inverted-value-locations inverted-value)
                         :key (lambda (loc) (location-document-id loc))
                         :test #'id=)))
          (if (null loc)
              (setf (inverted-value-locations inverted-value)
                    (insert-sort (make-location :document-id document-id
                                                :positions (list (token-position token)))
                                 (inverted-value-locations inverted-value)
                                 #'id<
                                 :key #'location-document-id))
              (setf (location-positions loc)
                    (insert-sort (token-position token) (location-positions loc) #'<)))))))
