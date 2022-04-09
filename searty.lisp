(in-package :searty)

;; for debug
(defun dump-inverted-index (inverted-index)
  (let ((table (make-hash-table :test 'equal))
        (tokens (resolve-tokens-by-ids *database* (inverted-index-token-ids inverted-index)))
        (documents (resolve-documents-by-ids *database* (collect-all-document-ids inverted-index))))
    (inverted-index-foreach inverted-index
                            (lambda (token-id locations)
                              (let ((token (find token-id tokens :test #'uuid= :key #'token-id)))
                                (setf (gethash (token-term token) table)
                                      (mapcar (lambda (loc)
                                                (let ((document (find (location-document-id loc)
                                                                      documents
                                                                      :test #'document-id=
                                                                      :key #'document-id)))
                                                  (cons document
                                                        (location-positions loc))))
                                              locations)))))
    (hash-table-alist table)))
