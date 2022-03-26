(in-package :searty2)

(defstruct inverted-value kind locations)
(defstruct location document positions)

(defstruct inverted-index
  (table (make-hash-table :test 'equal)))

(defun inverted-index-get (inverted-index term)
  (gethash term (inverted-index-table inverted-index)))

(defun (setf inverted-index-get) (value inverted-index term)
  (setf (gethash term (inverted-index-table inverted-index))
        value))

(defun inverted-index-insert (inverted-index document token)
  (let* ((inverted-values (inverted-index-get inverted-index (token-term token)))
         (inverted-value (find (token-kind token) inverted-values :key #'trigram-value-kind)))
    (if (null inverted-value)
        (push (make-trigram-value
               :kind (token-kind token)
               :locations (list (make-location :document document
                                               :positions (list (token-position token)))))
              (inverted-index-get inverted-index (token-term token)))
        (let ((loc (find document
                         (trigram-value-locations inverted-value)
                         :key (lambda (loc) (location-document loc))
                         :test #'document=)))
          (if (null loc)
              (setf (trigram-value-locations inverted-value)
                    (insert-sort (make-location :document document
                                                :positions (list (token-position token)))
                                 (trigram-value-locations inverted-value)
                                 #'document<
                                 :key #'location-document))
              (setf (location-positions loc)
                    (insert-sort (token-position token) (location-positions loc) #'<)))))))
