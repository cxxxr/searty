(in-package :searty-tests)

(defun add-temporary-document (indexer text)
  (with-temporary-file (doc-file text)
    (add-document indexer doc-file)))

(deftest and-matcher
  (with-test-database (database-file)
    (let* ((analyzer (make-instance 'simple-analyzer))
           (database (make-instance 'database
                                    :connection (dbi:connect :sqlite3 :database-name database-file)))
           (indexer (make-instance 'indexer
                                   :analyzer analyzer
                                   :database database))
           (searcher (make-instance 'searcher
                                    :database database
                                    :analyzer analyzer))
           (doc-1 (add-temporary-document indexer "foo xxx yyy bar baz"))
           (doc-2 (add-temporary-document indexer "foo hoge xxx yyy piyo bar"))
           (doc-3 (add-temporary-document indexer "aaaaaaa bbbbbbbb cccccc")))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "b"))))
        (ok (null result)))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "bbbbbbbb"))))
        (ok (length= result 1))
        (ok (find doc-3 result :test #'document-equal)))
      
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "foo"))))
        (ok (length= result 2))
        (ok (find doc-1 result :test #'document-equal))
        (ok (find doc-2 result :test #'document-equal)))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "xxx yyy"))))
        (ok (length= result 2))
        (ok (find doc-1 result :test #'document-equal))
        (ok (find doc-2 result :test #'document-equal))))))
