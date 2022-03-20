(in-package :searty)

(defun execute-sxql (connection sxql)
  (multiple-value-bind (sql params) (sxql:yield sxql)
    (dbi:do-sql connection sql params)))

(defun resolve-sxql (connection sxql)
  (multiple-value-bind (sql params) (sxql:yield sxql)
    (dbi:fetch-all
     (dbi:execute (dbi:prepare connection sql)
                  params))))

(defun execute-sql (connection sql &optional params)
  (dbi:do-sql connection sql params))
