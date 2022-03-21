(defsystem "searty"
  :depends-on ("alexandria"
               "cl-ppcre"
               "cl-dbi"
               "sxql"
               "flexi-streams"
               "uuid")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "inverted-index")
               (:file "sql")
               (:file "searty")
               (:file "lisp-analyzer")
               (:file "main"))
  :in-order-to ((test-op (test-op searty-tests))))
