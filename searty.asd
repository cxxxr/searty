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
               (:file "searty"))
  :in-order-to ((test-op (test-op searty-tests))))

(defsystem "searty-tests"
  :depends-on ("rove"
               "alexandria"
               "searty")
  :pathname "tests"
  :components ((:file "package")
               (:file "analyzer"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
