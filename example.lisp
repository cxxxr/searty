(in-package :searty)

(defun test ()
  (let ((*database* (make-database)))
    (index-system "cl-ppcre" "/home/user/quicklisp-dist/2022-04-01/")
    (index-system "1am" "/home/user/quicklisp-dist/2022-04-01/")
    (format t "search: ~D ms~%"
            (measure-time (pretty-print-matched (search-phrase "defun"))))))
