(in-package :searty)

(defparameter *quicklisp-database* (asdf:system-relative-pathname :searty "db/quicklisp.db"))

(defun search-quicklisp (query &key start-bounding end-bounding)
  (with-database (*database* *quicklisp-database*)
    (pretty-print-matched (search-phrase query
                                         :start-bounding start-bounding
                                         :end-bounding end-bounding))))
