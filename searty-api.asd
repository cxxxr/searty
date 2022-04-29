(defsystem "searty-api"
  :depends-on ("searty"
               "st-json"
               "ningle"
               "clack")
  :serial t
  :pathname "api"
  :components ((:file "app")))
