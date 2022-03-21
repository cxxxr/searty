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
               (:file "searcher")
               (:file "lisp-tokenizer"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
