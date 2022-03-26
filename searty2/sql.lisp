(in-package :searty2)

(defvar *sql-logger-stream* nil)

(defun log-sql (sql &optional params)
  (declare (ignorable params))
  (when *sql-logger-stream*
    ;; (format *sql-logger-stream* "~&~A ~S~%" sql params)
    (format *sql-logger-stream* "~&~A~%" sql)))

(defun execute-sxql (connection sxql)
  (multiple-value-bind (sql params) (sxql:yield sxql)
    (log-sql sql params)
    (dbi:do-sql connection sql params)))

(defun resolve-sxql (connection sxql)
  (multiple-value-bind (sql params) (sxql:yield sxql)
    (log-sql sql params)
    (dbi:fetch-all
     (dbi:execute (dbi:prepare connection sql)
                  params))))

(defun execute-sql (connection sql &optional params)
  (log-sql sql params)
  (dbi:do-sql connection sql params))
