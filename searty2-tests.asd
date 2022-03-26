(defsystem "searty2-tests"
  :depends-on ("rove"
               "alexandria"
               "searty2")
  :pathname "searty2/tests"
  :components ((:file "package")
               (:file "lisp-tokenizer"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
