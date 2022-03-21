(in-package :searty-tests)

(defun add-temporary-document (indexer text)
  (with-temporary-file (doc-file text)
    (add-document indexer doc-file)))

(deftest and-matcher
  (with-test-database (connection)
    (let* ((analyzer (make-instance 'simple-analyzer))
           (database (make-instance 'database
                                    :connection connection))
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
        (ok (set-equal result
                       (list (searty::make-matched :token (resolve-token database "bbbbbbbb")
                                                   :document-id (document-id doc-3)
                                                   :positions '(1)))
                       :test #'searty::matched-equal)))

      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "foo"))))
        (ok (set-equal result
                       (list (searty::make-matched :token (resolve-token database "foo")
                                                   :document-id (document-id doc-1)
                                                   :positions '(0))
                             (searty::make-matched :token (resolve-token database "foo")
                                                   :document-id (document-id doc-2)
                                                   :positions '(0)))
                       :test #'searty::matched-equal)))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "xxx yyy"))))
        (ok (set-equal result
                       (list (searty::make-matched :token (resolve-token database "xxx")
                                                   :document-id (document-id doc-1)
                                                   :positions '(1))
                             (searty::make-matched :token (resolve-token database "yyy")
                                                   :document-id (document-id doc-1)
                                                   :positions '(2))
                             (searty::make-matched :token (resolve-token database "xxx")
                                                   :document-id (document-id doc-2)
                                                   :positions '(2))
                             (searty::make-matched :token (resolve-token database "yyy")
                                                   :document-id (document-id doc-2)
                                                   :positions '(3)))
                       :test #'searty::matched-equal))))))
