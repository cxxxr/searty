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
    (funcall function database-file)))

(defmacro with-test-database ((database-file) &body body)
  `(call-with-test-database (lambda (,database-file) ,@body)))

(deftest indexer-test
  (with-test-database (database-file)
    (with-temporary-file (doc-file "foo bar baz")
      (let* ((analyzer (make-instance 'simple-analyzer))
             (database (make-instance 'database
                                      :connection (dbi:connect :sqlite3 :database-name database-file)))
             (indexer (make-instance 'indexer
                                     :analyzer analyzer
                                     :database database)))
        (add-document indexer doc-file)
        (print (searty::resolve-document-by-pathname database doc-file))))))