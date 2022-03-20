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
               (:file "searty")
               (:file "example"))
  :in-order-to ((test-op (test-op searty-tests))))
