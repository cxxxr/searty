(defsystem "searty"
  :depends-on ("alexandria"
               "uuid"
               "cl-ansi-text"
               "sxql"
               "flexi-streams"
               "cl-dbi"
               "dbd-sqlite3"
               "babel")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "sql")
               (:file "token")
               (:file "document")
               (:file "inverted-index")
               (:file "database")
               (:file "sqlite3-database")
               (:file "lisp-tokenizer")
               (:file "searty")))
