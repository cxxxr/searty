(defsystem "searty"
  :depends-on ("alexandria"
               "uuid"
               "cl-ansi-text"
               "sxql"
               "flexi-streams"
               "cl-dbi"
               "dbd-sqlite3"
               "babel"
               "split-sequence")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "sql")
               (:file "token")
               (:file "document")
               (:file "inverted-index")
               (:file "database")
               (:file "lisp-tokenizer")
               (:file "tokenizer")
               (:file "indexer")
               (:file "searcher")
               (:file "searty")))
