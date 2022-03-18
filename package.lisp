(defpackage :searty
  (:use :cl :alexandria)
  (:export
   :sqlite3-init-database
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
   :add-document))
