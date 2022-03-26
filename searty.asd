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
               (:file "lisp-tokenizer")
               (:file "inverted-index")
               (:file "sql")
               (:file "searty")
               (:file "main"))
  :in-order-to ((test-op (test-op searty-tests))))
