(defsystem "searty"
  :depends-on ("alexandria"
               "cl-ppcre"
               "cl-dbi"
               "sxql"
               "flexi-streams"
               "uuid"
               "cl-ansi-text")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "inverted-index")
               (:file "sql")
               (:file "searty")
               (:file "main")

               (:file "searty2/lisp-tokenizer")
               (:file "searty2/searty2"))
  :in-order-to ((test-op (test-op searty-tests))))
