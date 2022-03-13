(defsystem "searty"
  :depends-on ("alexandria" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "searty")))
