(defsystem "searty-tests"
  :depends-on ("rove"
               "alexandria"
               "searty")
  :pathname "tests"
  :components ((:file "package")
               (:file "inverted-index")
               (:file "analyzer")
               (:file "indexer"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
