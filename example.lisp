(in-package :searty)

(defun !index-system (system-name dist-dir)
  (with-asdf (dist-dir)
    (index-lisp-system system-name)))

(defun test ()
  (let ((*sqlite3-database-file* (format nil "/tmp/searty.db")))
    (ensure-directories-exist *sqlite3-index-directory*)
    (sqlite3-init-database)
    (!index-system "cl-ppcre" "/home/user/quicklisp-dist/2022-04-01/")
    (!index-system "1am" "/home/user/quicklisp-dist/2022-04-01/"))
  (let ((*database* (make-sqlite3-database "/tmp/")))
    (format t "search: ~D ms~%"
            (measure-time (pretty-print-matched (search-phrase "defun"))))))
