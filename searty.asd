(defsystem "searty"
  :depends-on ("alexandria"
               "uuid"
               "cl-ansi-text"
               "sxql"
               "flexi-streams"
               "cl-dbi"
               "dbd-sqlite3"
               "babel"
               "split-sequence"
               "swank")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "sql")
               (:file "definition")
               (:file "token")
               (:file "document")
               (:file "inverted-index")
               (:file "database")
               (:file "lisp-tokenizer")
               (:file "tokenizer")
               (:file "spec")
               (:file "indexer")
               (:file "searcher")
               (:file "searty")))
