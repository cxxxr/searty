(defsystem "searty2"
  :depends-on ("alexandria"
               "uuid"
               "cl-ansi-text"
               "sxql"
               "flexi-streams"
               "cl-dbi"
               "babel")
  :serial t
  :pathname "searty2"
  :components ((:file "package")
               (:file "utils")
               (:file "sql")
               (:file "token")
               (:file "document")
               (:file "inverted-index")
               (:file "database")
               (:file "lisp-tokenizer")
               (:file "searty")))
