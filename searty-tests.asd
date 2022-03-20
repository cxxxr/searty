(defsystem "searty-tests"
  :depends-on ("rove"
               "alexandria"
               "searty")
  :pathname "tests"
  :components ((:file "package")
               (:file "utils")
               (:file "inverted-index")
               (:file "analyzer")
               (:file "indexer")
               (:file "searcher"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
