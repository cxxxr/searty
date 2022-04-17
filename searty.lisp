(in-package :searty)

(defparameter *quicklisp-database* (asdf:system-relative-pathname :searty "db/quicklisp.db"))

(defun search-quicklisp (query &key start-boundary end-boundary)
  (with-database (*database* *quicklisp-database*)
    (pretty-print-matched (search-phrase query
                                         :start-boundary start-boundary
                                         :end-boundary end-boundary))))
