(defpackage :searty
  (:use :cl :alexandria)
  (:export
   :sqlite3-init-database
   ;; entities
   :document-id
   :document-pathname
   :document-body
   :token-id
   :token-term
   ;; analyzer
   :simple-analyzer
   :analyze
   ;; database
   :database
   :create-document
   :resolve-document-by-pathname
   :create-token
   :resolve-token
   :resolve-inverted-index
   :upsert-inverted-index
   ;; indexer
   :indexer
   :add-document
   ;; inverted-index
   :inverted-index-get
   :inverted-value-document-id
   :inverted-value-positions
   ))
