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
      (declare (ignorable doc-1 doc-2 doc-3))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "b"))))
        (ok (null result)))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "bbbbbbbb"))))
        (ok (length= result 1))
        (let ((matched (first result)))
          (ok (equal (token-id (resolve-token database "bbbbbbbb"))
                     (searty::matched-token-id matched)))
          (ok (doc-locations-equal (searty::matched-doc-locations matched)
                                   (list (searty::make-doc-location :document-id (document-id doc-3)
                                                                    :positions '(1)))))))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "foo"))))
        (ok (length= result 1))
        (let ((matched (first result)))
          (ok (equal (token-id (resolve-token database "foo"))
                     (searty::matched-token-id matched)))
          (ok (doc-locations-equal (searty::matched-doc-locations matched)
                                   (sort (list (searty::make-doc-location :document-id (document-id doc-1)
                                                                          :positions '(0))
                                               (searty::make-doc-location :document-id (document-id doc-2)
                                                                          :positions '(0)))
                                         #'searty::id<
                                         :key #'doc-location-document-id)))))
      (let ((result (execute-search searcher (make-instance 'and-matcher
                                                            :text "xxx yyy"))))
        (ok (length= result 2))
        (let ((matched (first result)))
          (ok (equal (token-id (resolve-token database "xxx"))
                     (searty::matched-token-id matched)))
          (ok (doc-locations-equal (searty::matched-doc-locations matched)
                                   (sort (list (searty::make-doc-location :document-id (document-id doc-1)
                                                                          :positions '(1))
                                               (searty::make-doc-location :document-id (document-id doc-2)
                                                                          :positions '(2)))
                                         #'searty::id<
                                         :key #'doc-location-document-id))))
        (let ((matched (second result)))
          (ok (equal (token-id (resolve-token database "yyy"))
                     (searty::matched-token-id matched)))
          (ok (doc-locations-equal (searty::matched-doc-locations matched)
                                   (sort (list (searty::make-doc-location :document-id (document-id doc-1)
                                                                          :positions '(2))
                                               (searty::make-doc-location :document-id (document-id doc-2)
                                                                          :positions '(3)))
                                         #'searty::id<
                                         :key #'doc-location-document-id))))))))
