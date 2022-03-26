(defsystem "searty2"
  :depends-on ("alexandria"
               "uuid"
               "cl-ansi-text")
  :serial t
  :pathname "searty2"
  :components ((:file "package")
               (:file "utils")
               (:file "lisp-tokenizer")
               (:file "searty")))
