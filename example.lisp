(in-package :searty)

(defun search-phrase-example (&rest search-phrase-arguments)
  (let ((*database* (make-instance 'sqlite3-database)))
    (write-line "------------------------------ INDEX ------------------------------")
    (sqlite3-init-database)
    (index-lisp-system :searty)
    (terpri)
    (write-line "------------------------------ RESULT ------------------------------")
    (pretty-print-matched (apply #'search-phrase search-phrase-arguments))))

(eval-when ()
  (search-phrase-example "defun"))
