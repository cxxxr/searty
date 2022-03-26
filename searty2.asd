(defsystem "searty2"
  :depends-on ("alexandria"
               "uuid"
               "cl-ansi-text"
               "sxql"
               "cl-dbi")
  :serial t
  :pathname "searty2"
  :components ((:file "package")
               (:file "utils")
               (:file "sql")

               (:file "token")
               (:file "document")
               (:file "inverted-index")

               (:file "lisp-tokenizer")
               (:file "database")
               (:file "searty")))
