(in-package :searty-tests)

(defmacro with-temporary-file ((pathname text) &body body)
  (with-unique-names (stream)
    `(uiop:with-temporary-file (:stream ,stream :pathname ,pathname)
       (write-string ,text ,stream)
       :close-stream
       ,@body)))

(defun call-with-test-database (function)
  (let ((database-file "/tmp/searty-test.sqlite3"))
    (uiop:delete-file-if-exists database-file)
    (sqlite3-init-database database-file)
    (let ((connection (dbi:connect :sqlite3 :database-name database-file)))
      (funcall function connection))))

(defmacro with-test-database ((connection) &body body)
  `(call-with-test-database (lambda (,connection) ,@body)))
