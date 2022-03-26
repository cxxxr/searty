(in-package :searty)

;;; entities
(defstruct document id pathname body)
(defstruct token id term kind position)


(defparameter *sqlite3-schema-file* (namestring (asdf:system-relative-pathname :searty "schema.sql")))

(defun sqlite3-init-database (database-file)
  (uiop:run-program `("sqlite3" "-init" ,*sqlite3-schema-file* ,database-file)))


;;; database
(defgeneric create-document (database pathname text))
(defgeneric resolve-document-by-pathname (database pathname))
(defgeneric resolve-document-by-ids (database id))
(defgeneric create-token (database token))
(defgeneric resolve-token (database token))
(defgeneric resolve-token-by-term (database term))
(defgeneric resolve-tokens (database terms))
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

(defmethod create-token ((database database) token)
  (let ((id (random-uuid)))
    (execute-sxql (database-connection database)
                  (sxql:insert-into :token
                    (sxql:set= :id id :term (token-term token))))
    (make-token :id id :term (token-term token) :kind (token-kind token))))

(defun make-token-from-record (record position kind)
  (destructuring-bind (&key ((:|id| id))
                            ((:|term| term)))
      record
    (make-token :id id :term term :position position :kind kind)))

(defmethod resolve-token ((database database) token)
  (when-let ((records
              (resolve-sxql (database-connection database)
                            (sxql:select (:term :id)
                              (sxql:from :token)
                              (sxql:where (:= :term (token-term token)))
                              (sxql:limit 1)))))
    (make-token-from-record (first records)
                            (token-position token)
                            (token-kind token))))

(defmethod resolve-token-by-term ((database database) term)
  (when-let ((records
              (resolve-sxql (database-connection database)
                            (sxql:select (:term :id)
                              (sxql:from :token)
                              (sxql:where (:= :term term))
                              (sxql:limit 1)))))
    (make-token-from-record (first records)
                            nil
                            nil)))

(defmethod resolve-tokens ((database database) terms)
  (mapcar (lambda (record) (make-token-from-record record nil nil))
          (resolve-sxql (database-connection database)
                        (sxql:select (:term :id)
                          (sxql:from :token)
                          (sxql:where (:in :term terms))))))

(defmethod resolve-tokens-with-submatch ((database database) term)
  (mapcar (lambda (record) (make-token-from-record record nil nil))
          (resolve-sxql (database-connection database)
                        (sxql:select (:term :id)
                          (sxql:from :token)
                          (sxql:where (:like :term (format nil "%~A%" term)))))))

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


;;; tokenizer
(defgeneric tokenize (tokenizer text))

(defclass tokenizer () ())

(defclass word-tokenizer () ())

(defmethod tokenize ((tokenizer word-tokenizer) text)
  (loop :for term :in (word-tokenize text)
        :for pos :from 0
        :collect (make-token :term term :position pos)))

;;; indexer
(defclass indexer ()
  ((tokenizer :initarg :tokenizer
              :initform (required-argument :tokenizer)
              :reader indexer-tokenizer)
   (inverted-index :initform (make-inverted-index)
                   :reader indexer-inverted-index)
   (database :initarg :database
             :initform (required-argument :database)
             :reader indexer-database)))

(defmethod add-document ((indexer indexer) pathname)
  (dbi:with-transaction (database-connection (indexer-database indexer))
    (let* ((text (read-file-into-string pathname))
           (tokens (tokenize (indexer-tokenizer indexer) text))
           (document (create-document (indexer-database indexer) pathname text)))
      (loop :for pos :from 0
            :for token :in tokens
            :do (add-token indexer token document))
      (flush-inverted-index indexer)
      document)))

(defmethod add-token ((indexer indexer) token document)
  (let ((storage-token (or (resolve-token (indexer-database indexer) token)
                           (create-token (indexer-database indexer) token))))
    (insert-doc-location (indexer-inverted-index indexer)
                         (token-id storage-token)
                         (document-id document)
                         (token-position token))
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
  ((tokenizer :initarg :tokenizer
              :initform (required-argument :tokenizer)
              :reader searcher-tokenizer)
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
  token
  document-id
  positions)

(defun matched-equal (matched-1 matched-2)
  (and (equal (token-id (matched-token matched-1))
              (token-id (matched-token matched-2)))
       (equal (token-term (matched-token matched-1))
              (token-term (matched-token matched-2)))
       (equal (matched-document-id matched-1)
              (matched-document-id matched-2))
       (equal (matched-positions matched-1)
              (matched-positions matched-2))))

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
  (let ((acc '()))
    (dolist (token tokens)
      (dolist (loc (gethash (token-id token) token-locations-map))
        (push (make-matched :token token
                            :document-id (doc-location-document-id loc)
                            :positions (doc-location-positions loc))
              acc)))
    acc))

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
                           (mapcar #'token-term
                                   (tokenize (searcher-tokenizer searcher)
                                             (query-text query)))))
         (inverted-index
           (resolve-inverted-index (searcher-database searcher)
                                   (mapcar #'token-id tokens))))
    (match query tokens inverted-index)))


(defclass lisp-tokenizer () ())

(defmethod tokenize ((tokenizer lisp-tokenizer) text)
  (tokenize-lisp text))

(defclass lisp-searcher (searcher) ())

(defmethod execute-search ((searcher lisp-searcher) query)
  (let* ((tokens
           (loop :for token :in (tokenize (searcher-tokenizer searcher)
                                          (query-text query))
                 :append (resolve-tokens-with-submatch (searcher-database searcher) (token-term token)))))
    (loop :for token :in tokens
          :append (let ((inverted-index
                          (resolve-inverted-index (searcher-database searcher)
                                                  (list (token-id token)))))
                    (match query (list token) inverted-index)))))
