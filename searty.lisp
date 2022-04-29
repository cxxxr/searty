(in-package :searty)

(defparameter *quicklisp-database* (asdf:system-relative-pathname :searty "db/quicklisp.db"))

(defun search-string (query &key start-boundary end-boundary)
  (with-database (*database* *quicklisp-database*)
    (pretty-print-matched
     (search-phrase query
                    :start-boundary start-boundary
                    :end-boundary end-boundary))))

(defun search-definitions (query)
  (with-database (*database* *quicklisp-database*)
    (multiple-value-bind (name prefix) (parse-symbol query)
      (append (search-symbol-definitions name prefix)
              (when (null prefix)
                (search-package-definitions name))))))
