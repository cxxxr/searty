(in-package :searty)

;;; entities
(defstruct document id pathname text)
(defstruct token id term)


;;; database
(defgeneric create-document (database pathname text))
(defgeneric create-token (database term))
(defgeneric find-token (database term))
(defgeneric select-inverted-index (database token-ids))
(defgeneric upsert-inverted-index (database token-id encoded-inverted-values))

(defclass database ()
  ((connection :initarg :connection
               :initform (required-argument :connection)
               :reader database-connection)))

(defmethod create-document ((database database) pathname text)
  (let ((pathname (convert-document-cache-pathname pathname))
        (id (random-uuid)))
    (dbi:do-sql (database-connection database)
      "INSERT INTO document (id, pathname) values (?, ?)"
      (list id (namestring pathname)))
    (make-document :id id :pathname pathname :text text)))

(defmethod create-token ((database database) term)
  (let ((id (random-uuid)))
    (dbi:do-sql (database-connection database)
      "INSERT INTO token (id, term) values (?, ?)"
      (list id term))
    (make-token :id id :term term)))

(defmethod find-token ((database database) term)
  (when-let ((records
              (dbi:fetch-all
               (dbi:execute (dbi:prepare (database-connection database)
                                         "SELECT term, id FROM token WHERE term = ? LIMIT 1")
                            (list term)))))
    (destructuring-bind (&key ((:|id| id))
                              ((:|term| term)))
        (first records)
      (make-token :id id :term term))))

(defmethod select-inverted-index ((database database) token-ids)
  (multiple-value-bind (sql params)
      (sxql:yield
       (sxql:select (:token_id :encoded_values)
         (sxql:from :inverted_index)
         (sxql:where (:in :token_id token-ids))))
    (dbi:fetch-all
     (dbi:execute (dbi:prepare (database-connection database)
                               sql)
                  params))))

(defmethod upsert-inverted-index ((database database) token-id encoded-inverted-values)
  (dbi:do-sql (database-connection database)
    "INSERT INTO inverted_index (token_id, encoded_values) VALUES (?, ?)
ON CONFLICT(token_id) DO UPDATE SET encoded_values = ?"
    (list token-id
          encoded-inverted-values
          encoded-inverted-values)))


;;; character filter
(defgeneric filter-character (character-filter text))
(defclass character-filter () ())

;;; tokenizer
(defgeneric tokenize (tokenizer text))

(defclass tokenizer () ())

(defclass word-tokenizer () ())

(defmethod tokenize ((tokenizer word-tokenizer) text)
  (word-tokenize text))

;;; token filter
(defgeneric filter-token (token-filter tokens))
(defclass token-filter () ())

;;; analyzer
(defgeneric analyze (analyzer text))

(defclass analyzer ()
  ((character-filters :initarg :character-filters
                      :initform '()
                      :reader analyzer-character-filters)
   (tokenizer :initarg :tokenizer
              :initform (required-argument :tokenizer)
              :reader analyzer-tokenizer)
   (token-filters :initarg :token-filters
                  :initform '()
                  :reader analyzer-token-filters)))

(defmethod analyze ((analyzer analyzer) text)
  (dolist (character-filter (analyzer-character-filters analyzer))
    (setf text (filter-character character-filter text)))
  (let ((tokens (tokenize (analyzer-tokenizer analyzer) text)))
    (dolist (token-filter (analyzer-token-filters analyzer))
      (setf tokens (filter-token token-filter tokens)))
    tokens))

;;; simple analyzer
(defclass simple-analyzer (analyzer)
  ()
  (:default-initargs :tokenizer (make-instance 'word-tokenizer)))

;;; indexer
(defclass indexer ()
  ((analyzer :initarg :analyzer
              :initform (required-argument :analyzer)
              :reader indexer-analyzer)
   (inverted-index :initform (make-inverted-index)
                   :reader indexer-inverted-index)
   (database :initarg :database
             :initform (required-argument :database)
             :reader indexer-database)))

(defun convert-document-cache-pathname (pathname)
  (pathname (format nil "~A~A"
                    (merge-pathnames ".searty/cache/" (user-homedir-pathname))
                    pathname)))

(defun cache-file (pathname text)
  (let ((pathname (convert-document-cache-pathname pathname)))
    (ensure-directories-exist pathname)
    (write-string-into-file text pathname
                            :if-exists :supersede
                            :if-does-not-exist :create)))

(defmethod add-document ((indexer indexer) pathname)
  (let* ((text (read-file-into-string pathname))
         (tokens (analyze (indexer-analyzer indexer) text))
         (document (create-document (indexer-database indexer) pathname text)))
    (cache-file pathname text)
    (loop :for pos :from 0
          :for token-term :in tokens
          :do (add-token indexer token-term document pos))
    (flush-inverted-index indexer)))

(defmethod add-token ((indexer indexer) token-term document pos)
  (let ((token (or (find-token (indexer-database indexer) token-term)
                   (create-token (indexer-database indexer) token-term))))
    (insert-inverted-value (indexer-inverted-index indexer)
                           (token-id token)
                           (document-id document)
                           pos)
    (values)))

(defmethod save-inverted-index ((indexer indexer) inverted-index)
  (let ((database (indexer-database indexer)))
    (do-inverted-index ((token-id inverted-values) inverted-index)
      (upsert-inverted-index database
                             token-id
                             (couerce-unsigned-byte-vector
                              (encode-inverted-values-to-vector inverted-values))))))

(defmethod resolve-inverted-index ((indexer indexer) token-ids)
  (let ((records (select-inverted-index (indexer-database indexer) token-ids))
        (inverted-index (make-inverted-index)))
    (dolist (record records)
      (destructuring-bind (&key ((:|token_id| token-id))
                                ((:|values| values)))
          record
        (setf (inverted-index-get inverted-index token-id)
              (decode-inverted-values-from-vector values))))
    inverted-index))

(defmethod flush-inverted-index ((indexer indexer))
  (let ((storage-inverted-index
          (resolve-inverted-index indexer
                                  (inverted-index-keys (indexer-inverted-index indexer)))))
    (save-inverted-index indexer
                         (merge-inverted-index (indexer-inverted-index indexer)
                                               storage-inverted-index)))
  (clear-inverted-index (indexer-inverted-index indexer)))

;;;
(defun example-index ()
  (let* ((analyzer (make-instance 'simple-analyzer))
         (connection (dbi:connect :sqlite3 :database-name "/tmp/searty.sqlite3"))
         (database (make-instance 'database :connection connection))
         (indexer (make-instance 'indexer
                                 :analyzer analyzer
                                 :database database)))
    (add-document indexer "searty.lisp")
    indexer))

#+(or)
(defun example-search (indexer query)
  )
