(in-package :searty)

(defun test ()
  (uiop:run-program '("rm" "-rf" "/tmp/searty/"))
  (format t "index: ~D ms~%"
          (measure-time (index-system "cl-ppcre" "/home/user/quicklisp-dist/2022-04-01/cl-ppcre-20220220-git/" "/tmp/searty/")))
  (let ((*database* (make-sqlite3-database "/tmp/searty/cl-ppcre//")))
    (format t "search: ~D ms~%"
            (measure-time (pretty-print-matched (search-phrase "defun"))))))
