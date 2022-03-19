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
        (let ((document (resolve-document-by-pathname database doc-file)))
          (assert document)
          ;; document
          (ok (uiop:pathname-equal doc-file (document-pathname document)))
          (ok (equal "foo bar baz" (document-body document)))
          ;; token
          (let ((tok-1 (resolve-token database "foo"))
                (tok-2 (resolve-token database "bar"))
                (tok-3 (resolve-token database "baz")))
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
