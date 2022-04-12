(defpackage :searty
  (:use :cl :alexandria)
  (:export :connect-database
           :disconnect-database
           :with-database
           :delete-all-records
           :index-system))
