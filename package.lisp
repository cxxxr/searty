(defpackage :searty
  (:use :cl :alexandria)
  (:export
   ;; utils
   :ngram
   :random-uuid
   :word-tokenize
   :coerce-unsigned-byte-vector
   ;; sqlite
   :sqlite3-init-database
   ;; entities
   :document-equal
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
   ;; searcher
   :searcher
   :execute-search
   :and-matcher
   ;; inverted-index
   :doc-location-document-id
   :doc-location-positions
   :doc-locations-equal
   :make-inverted-index
   :inverted-index-equal
   :insert-doc-location
   :get-doc-locations
   :do-inverted-index
   :merge-inverted-index
   :encode-doc-locations-to-vector
   :decode-doc-locations-from-vector))
