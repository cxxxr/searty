(in-package :searty)

;;; entities
(defstruct document id pathname body)
(defstruct token id term)


(defparameter *sqlite3-schema-file* (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defun sqlite3-init-database (database-file)
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,database-file)))


;;; database
(defgeneric create-document (database pathname text))
(defgeneric resolve-document-by-pathname (database pathname))
(defgeneric create-token (database term))
(defgeneric resolve-token (database term))
(defgeneric resolve-inverted-index (database token-ids))
(defgeneric resolve-whole-inverted-index (database))
(defgeneric upsert-inverted-index (database token-id encoded-doc-locations))

(defclass database ()
  ((connection :initarg :connection
               :initform (required-argument :connection)
               :reader database-connection)))

(defmethod create-document ((database database) pathname body)
  (let ((id (random-uuid)))
    (dbi:do-sql (database-connection database)
      "INSERT INTO document (id, pathname, body) values (?, ?, ?)"
      (list id (namestring pathname) body))
    (make-document :id id :pathname pathname :body body)))

(defmethod resolve-document-by-pathname ((database database) pathname)
  (when-let ((record
              (dbi:fetch-all
               (dbi:execute (dbi:prepare (database-connection database)
                                         "SELECT id, pathname, body FROM document WHERE pathname = ? LIMIT 1")
                            (list (princ-to-string pathname))))))
    (destructuring-bind (&key ((:|id| id))
                              ((:|pathname| pathname))
                              ((:|body| body)))
        (first record)
      (make-document :id id :pathname pathname :body body))))

(defmethod create-token ((database database) term)
  (let ((id (random-uuid)))
    (dbi:do-sql (database-connection database)
      "INSERT INTO token (id, term) values (?, ?)"
      (list id term))
    (make-token :id id :term term)))

(defun make-token-from-record (record)
  (destructuring-bind (&key ((:|id| id))
                            ((:|term| term)))
      record
    (make-token :id id :term term)))

(defmethod resolve-token ((database database) term)
  (when-let ((records
              (dbi:fetch-all
               (dbi:execute (dbi:prepare (database-connection database)
                                         "SELECT term, id FROM token WHERE term = ? LIMIT 1")
                            (list term)))))
    (make-token-from-record (first records))))

(defmethod resolve-tokens ((database database) terms)
  (multiple-value-bind (sql params)
      (sxql:yield
       (sxql:select (:term :id)
         (sxql:from :token)
         (sxql:where (:in :term terms))))
    (let ((records
            (dbi:fetch-all
             (dbi:execute (dbi:prepare (database-connection database) sql)
                          params))))
      (mapcar #'make-token-from-record records))))

(defun decode-inverted-index (records)
  (let ((inverted-index (make-inverted-index)))
    (dolist (record records)
      (destructuring-bind (&key ((:|token_id| token-id))
                                ((:|encoded_values| values)))
          record
        (setf (get-doc-locations inverted-index token-id)
              (decode-doc-locations-from-vector values))))
    inverted-index))

(defun resolve-inverted-index-aux (database sxql)
  (decode-inverted-index
   (multiple-value-bind (sql params) (sxql:yield sxql)
     (dbi:fetch-all
      (dbi:execute (dbi:prepare (database-connection database) sql)
                   params)))))

(defmethod resolve-inverted-index ((database database) token-ids)
  (resolve-inverted-index-aux
   database
   (sxql:select (:token_id :encoded_values)
     (sxql:from :inverted_index)
     (sxql:where (:in :token_id token-ids)))))

(defmethod resolve-whole-inverted-index ((database database))
  (resolve-inverted-index-aux
   database
   (sxql:select (:token_id :encoded_values)
     (sxql:from :inverted_index))))

(defmethod upsert-inverted-index ((database database) token-id encoded-doc-locations)
  (dbi:do-sql (database-connection database)
    "INSERT INTO inverted_index (token_id, encoded_values) VALUES (?, ?)
ON CONFLICT(token_id) DO UPDATE SET encoded_values = ?"
    (list token-id
          encoded-doc-locations
          encoded-doc-locations)))


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

(defmethod add-document ((indexer indexer) pathname)
  (let* ((text (read-file-into-string pathname))
         (tokens (analyze (indexer-analyzer indexer) text))
         (document (create-document (indexer-database indexer) pathname text)))
    (loop :for pos :from 0
          :for token-term :in tokens
          :do (add-token indexer token-term document pos))
    (flush-inverted-index indexer)))

(defmethod add-token ((indexer indexer) token-term document pos)
  (let ((token (or (resolve-token (indexer-database indexer) token-term)
                   (create-token (indexer-database indexer) token-term))))
    (insert-doc-location (indexer-inverted-index indexer)
                         (token-id token)
                         (document-id document)
                         pos)
    (values)))

(defmethod save-inverted-index ((indexer indexer) inverted-index)
  (let ((database (indexer-database indexer)))
    (do-inverted-index ((token-id doc-locations) inverted-index)
      (upsert-inverted-index database
                             token-id
                             (coerce-unsigned-byte-vector
                              (encode-doc-locations-to-vector doc-locations))))))

(defmethod flush-inverted-index ((indexer indexer))
  (let ((storage-inverted-index
          (resolve-inverted-index (indexer-database indexer)
                                  (inverted-index-tokens (indexer-inverted-index indexer)))))
    (save-inverted-index indexer
                         (merge-inverted-index (indexer-inverted-index indexer)
                                               storage-inverted-index)))
  (clear-inverted-index (indexer-inverted-index indexer)))

;;;
(defclass searcher ()
  ((analyzer :initarg :analyzer
             :initform (required-argument :analyzer)
             :reader searcher-analyzer)
   (database :initarg :database
             :initform (required-argument :database)
             :reader searcher-database)))

(defgeneric match (query tokens inverted-index))

(defclass query ()
  ((text :initarg :text
         :initform (required-argument :text)
         :reader query-text)))
(defclass and-matcher (query) ())
(defclass or-matcher (query) ())
(defclass phrase-matcher (query) ())

(defmethod match ((query and-matcher) tokens inverted-index)
  )

(defmethod match ((query or-matcher) tokens inverted-index)
  )

(defmethod match ((query phrase-matcher) tokens inverted-index)
  )

(defmethod do-search ((searcher searcher) query)
  (let* ((tokens
           (resolve-tokens (searcher-database searcher)
                           (analyze (searcher-analyzer searcher)
                                    (query-text query))))
         (inverted-index
           (resolve-inverted-index (searcher-database searcher)
                                   (mapcar #'token-id tokens))))
    (match query tokens inverted-index)
    inverted-index))

;;;
(defun example-index ()
  (let* ((analyzer (make-instance 'simple-analyzer))
         (connection (dbi:connect :sqlite3 :database-name "/tmp/searty.sqlite3"))
         (database (make-instance 'database :connection connection))
         (indexer (make-instance 'indexer
                                 :analyzer analyzer
                                 :database database)))
    (add-document indexer "package.lisp")
    (add-document indexer "utils.lisp")
    (add-document indexer "inverted-index.lisp")
    (add-document indexer "searty.lisp")
    indexer))

(eval-when ()
  (defparameter $analyzer (make-instance 'simple-analyzer))
  (defparameter $database (make-instance 'database :connection (dbi:connect :sqlite3 :database-name "/tmp/searty.sqlite3")))
  (defparameter $indexer (make-instance 'indexer
                                        :analyzer analyzer
                                        :database database))
  (defparameter $searcher (make-instance 'searcher
                                         :analyzer $analyzer
                                         :database $database)))
