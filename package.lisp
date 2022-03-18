(defpackage :searty
  (:use :cl :alexandria)
  (:export :sqlite3-init-database
           :simple-analyzer
           :analyze
           :database
           :indexer
           :add-document))
