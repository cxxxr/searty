(in-package :searty-tests)

(deftest indexer-test
  (with-test-database (connection)
    (with-temporary-file (doc-file "foo bar baz")
      (let* ((tokenizer (make-instance 'word-tokenizer))
             (database (make-instance 'database
                                      :connection connection))
             (indexer (make-instance 'indexer
                                     :tokenizer tokenizer
                                     :database database)))
        (add-document indexer doc-file)
        (let ((document (resolve-document-by-pathname database doc-file)))
          (assert document)
          ;; document
          (ok (uiop:pathname-equal doc-file (document-pathname document)))
          (ok (equal "foo bar baz" (document-body document)))
          ;; token
          (let ((tok-1 (resolve-token-by-term database "foo"))
                (tok-2 (resolve-token-by-term database "bar"))
                (tok-3 (resolve-token-by-term database "baz")))
            (ok tok-1)
            (ok tok-2)
            (ok tok-3)
            ;; inverted_index
            (let ((inverted-index
                    (resolve-inverted-index database
                                            (list (token-id tok-1)
                                                  (token-id tok-2)
                                                  (token-id tok-3)))))
              (let ((doc-locations (get-doc-locations inverted-index (token-id tok-1))))
                (ok (= 1 (length doc-locations)))
                (let ((loc (elt doc-locations 0)))
                  (ok (equal (doc-location-document-id loc)
                             (document-id document)))
                  (ok (equal '(0) (doc-location-positions loc)))))
              (let ((doc-locations (get-doc-locations inverted-index (token-id tok-2))))
                (ok (= 1 (length doc-locations)))
                (let ((loc (elt doc-locations 0)))
                  (ok (equal (doc-location-document-id loc)
                             (document-id document)))
                  (ok (equal '(1) (doc-location-positions loc)))))
              (let ((doc-locations (get-doc-locations inverted-index (token-id tok-3))))
                (ok (= 1 (length doc-locations)))
                (let ((loc (elt doc-locations 0)))
                  (ok (equal (doc-location-document-id loc)
                             (document-id document)))
                  (ok (equal '(2) (doc-location-positions loc))))))))))))

(defun create-index (indexer)
  (dolist (file (find-files (asdf:system-source-directory :searty) #'lisp-pathname-p))
    (let ((start (get-internal-real-time)))
      (format t "~&~A " file)
      (add-document indexer file)
      (format t "[~D ms]~%" (floor (- (get-internal-real-time) start)
                                   1000)))))

(deftest check-inverted-index-corruption-test
  (with-test-database (connection)
    (let* ((database (make-instance 'database :connection connection))
           (tokenizer (make-instance 'word-tokenizer))
           (indexer (make-instance 'indexer
                                   :tokenizer tokenizer
                                   :database database)))
      (create-index indexer)
      (ok (check-inverted-index-corruption (searty::resolve-whole-inverted-index database))))))
