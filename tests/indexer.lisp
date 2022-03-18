(in-package :searty-tests)

(defmacro with-temporary-file ((pathname text) &body body)
  (with-unique-names (stream)
    `(uiop:with-temporary-file (:stream ,stream :pathname ,pathname)
       (write-line ,text ,stream)
       :close-stream
       ,@body)))

(deftest indexer-test
  (with-temporary-file (pathname "foo bar baz")
    (let* ((analyzer (make-instance 'simple-analyzer))
           (database (make-instance 'database
                                    :connection (dbi:connect :sqlite3 :database-name "/tmp/searty-test.sqlite3")))
           (indexer (make-instance 'indexer
                                   :analyzer analyzer
                                   :database database)))
      (add-document indexer (probe-file pathname)))))
