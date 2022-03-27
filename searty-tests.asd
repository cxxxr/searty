(defsystem "searty-tests"
  :depends-on ("rove"
               "alexandria"
               "searty")
  :pathname "tests"
  :components ((:file "package")
               (:file "lisp-tokenizer"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
