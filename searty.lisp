(in-package :searty)

;;; document
(defstruct document id name text)


;;; database
(defgeneric insert-document (database name text))

(defclass onmemory-database ()
  ((documents :initform '()
              :accessor onmemory-database-documents)))

(defmethod insert-document ((database onmemory-database) name text)
  (let* ((len (length (onmemory-database-documents database)))
         (document (make-document :id len :name name :text text)))
    (push document (onmemory-database-documents database))
    document))


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
(defgeneric add-document (indexer pathname))
(defgeneric add-token (indexer token document pos))

(defclass indexer ()
  ((analyzer :initarg :analyzer
              :initform (required-argument :analyzer)
              :reader indexer-analyzer)
   (inverted-index :initform (make-hash-table :test 'equal)
                   :reader indexer-inverted-index)
   (database :initform (make-instance 'onmemory-database)
             :reader indexer-database)))

(defmethod add-document ((indexer indexer) pathname)
  (let* ((text (read-file-into-string pathname))
         (tokens (analyze (indexer-analyzer indexer) text))
         (document (insert-document (indexer-database indexer) pathname text)))
    (loop :for pos :from 0
          :for token :in tokens
          :do (add-token indexer token document pos))))

(defmethod add-token ((indexer indexer) token document pos)
  (push (list document pos) (gethash token (indexer-inverted-index indexer))))

;;;
(defun example-index ()
  (let* ((analyzer (make-instance 'simple-analyzer))
         (indexer (make-instance 'indexer :analyzer analyzer)))
    (add-document indexer "searty.lisp")
    indexer))

#+(or)
(defun example-search (indexer query)
  )
