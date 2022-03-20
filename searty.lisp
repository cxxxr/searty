(in-package :searty)

;;; entities
(defstruct document id pathname body)
(defstruct token id term)

(defmethod document-equal ((doc-1 document) (doc-2 document))
  (and (equal (document-id doc-1) (document-id doc-2))
       (uiop:pathname-equal (document-pathname doc-1) (document-pathname doc-2))
       (string= (document-body doc-1) (document-body doc-2))))


(defparameter *sqlite3-schema-file* (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defun sqlite3-init-database (database-file)
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,database-file)))


;;; database
(defgeneric create-document (database pathname text))
(defgeneric resolve-document-by-pathname (database pathname))
(defgeneric resolve-document-by-ids (database id))
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
    (execute-sxql (database-connection database)
                  (sxql:insert-into :document
                    (sxql:set= :id id
                               :pathname (namestring pathname)
                               :body body)))
    (make-document :id id :pathname pathname :body body)))

(defun make-document-from-record (record)
  (destructuring-bind (&key ((:|id| id))
                            ((:|pathname| pathname))
                            ((:|body| body)))
      record
    (make-document :id id :pathname pathname :body body)))

(defmethod resolve-document-by-pathname ((database database) pathname)
  (when-let ((records
              (resolve-sxql (database-connection database)
                            (sxql:select (:id :pathname :body)
                              (sxql:from :document)
                              (sxql:where (:= :pathname (namestring pathname)))
                              (sxql:limit 1)))))
    (make-document-from-record (first records))))

(defmethod resolve-document-by-ids ((database database) ids)
  (mapcar #'make-document-from-record
          (resolve-sxql (database-connection database)
                        (sxql:select (:id :pathname :body)
                          (sxql:from :document)
                          (sxql:where (:in :id ids))))))

(defmethod create-token ((database database) term)
  (let ((id (random-uuid)))
    (execute-sxql (database-connection database)
                  (sxql:insert-into :token
                    (sxql:set= :id id :term term)))
    (make-token :id id :term term)))

(defun make-token-from-record (record)
  (destructuring-bind (&key ((:|id| id))
                            ((:|term| term)))
      record
    (make-token :id id :term term)))

(defmethod resolve-token ((database database) term)
  (when-let ((records
              (resolve-sxql (database-connection database)
                            (sxql:select (:term :id)
                              (sxql:from :token)
                              (sxql:where (:= :term term))
                              (sxql:limit 1)))))
    (make-token-from-record (first records))))

(defmethod resolve-tokens ((database database) terms)
  (mapcar #'make-token-from-record
          (resolve-sxql (database-connection database)
                        (sxql:select (:term :id)
                          (sxql:from :token)
                          (sxql:where (:in :term terms))))))

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
   (resolve-sxql (database-connection database) sxql)))

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
  (execute-sql (database-connection database)
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
    (flush-inverted-index indexer)
    document))

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
(defgeneric execute-search (searcher query))

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

(defstruct matched
  token-id
  doc-locations)

(defstruct (posting (:constructor make-posting (token-id doc-locations)))
  token-id
  doc-locations)

(defun posting-head-doc-location (posting)
  (first (posting-doc-locations posting)))

(defun posting-head-doc-id (posting)
  (doc-location-document-id (posting-head-doc-location posting)))

(defun posting-null-p (posting)
  (null (posting-doc-locations posting)))

(defun same-document-id-p (postings)
  (let ((doc-id (posting-head-doc-id (first postings))))
    (loop :for posting :in (rest postings)
          :always (equal doc-id (posting-head-doc-id posting)))))

(defun posting-next (posting)
  (setf (posting-doc-locations posting)
        (rest (posting-doc-locations posting)))
  posting)

(defun posting-next-all (postings)
  (dolist (posting postings)
    (posting-next posting)))

(defun minimize-posting (postings)
  (let ((min (first postings)))
    (dolist (posting (rest postings))
      (when (id< (posting-head-doc-id posting) (posting-head-doc-id min))
        (setf min posting)))
    min))

(defun next-minimum-id-posting (postings)
  (let ((posting (minimize-posting postings)))
    (posting-next posting)))

(defun convert-matchies (token-locations-map tokens)
  (loop :for token :in tokens
        :for doc-locations := (gethash (token-id token) token-locations-map)
        :collect (make-matched :token-id (token-id token)
                               :doc-locations doc-locations)))

(defmethod match ((query and-matcher) tokens inverted-index)
  (let ((postings (map-inverted-index-values #'make-posting inverted-index))
        (token-locations-map (make-hash-table :test 'equal)))
    (when postings
      (loop :until (some #'posting-null-p postings)
            :do (cond ((same-document-id-p postings)
                       (dolist (posting postings)
                         (push (posting-head-doc-location posting)
                               (gethash (posting-token-id posting) token-locations-map)))
                       (posting-next-all postings))
                      (t
                       (next-minimum-id-posting postings))))
      (convert-matchies token-locations-map tokens))))

(defmethod match ((query or-matcher) tokens inverted-index)
  (error "unimplemented"))

(defmethod match ((query phrase-matcher) tokens inverted-index)
  (error "unimplemented"))

(defmethod execute-search ((searcher searcher) query)
  (let* ((tokens
           (resolve-tokens (searcher-database searcher)
                           (analyze (searcher-analyzer searcher)
                                    (query-text query))))
         (inverted-index
           (resolve-inverted-index (searcher-database searcher)
                                   (mapcar #'token-id tokens))))
    (match query tokens inverted-index)))
