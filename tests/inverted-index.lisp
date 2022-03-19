(in-package :searty-tests)

(deftest inverted-index/insert-doc-location
  (let ((token-id (random-uuid))
        (doc-id (random-uuid))
        (inverted-index (make-inverted-index)))
    (loop :for pos :in (shuffle (iota 10))
          :do (insert-doc-location inverted-index token-id doc-id pos))
    (let* ((locs (get-doc-locations inverted-index token-id))
           (loc (first locs)))
      (ok (length= locs 1))
      (ok (equal doc-id (doc-location-document-id loc)))
      (ok (equal '(0 1 2 3 4 5 6 7 8 9)
                 (doc-location-positions loc)))))

  (let ((token-id-1 (random-uuid))
        (doc-id-1 (random-uuid))
        (doc-id-2 (random-uuid))
        (inverted-index (make-inverted-index)))

    (insert-doc-location inverted-index token-id-1 doc-id-1 1)
    (insert-doc-location inverted-index token-id-1 doc-id-1 2)
    (insert-doc-location inverted-index token-id-1 doc-id-1 3)

    (insert-doc-location inverted-index token-id-1 doc-id-2 1)
    (insert-doc-location inverted-index token-id-1 doc-id-2 5)
    (insert-doc-location inverted-index token-id-1 doc-id-2 3)

    (let* ((locs (get-doc-locations inverted-index token-id-1))
           (loc1 (second locs))
           (loc2 (first locs)))
      (ok (length= locs 2))

      (ok (equal doc-id-1 (doc-location-document-id loc1)))
      (ok (equal '(1 2 3) (doc-location-positions loc1)))

      (ok (equal doc-id-2 (doc-location-document-id loc2)))
      (ok (equal '(1 3 5) (doc-location-positions loc2))))))

(defun make-testing-inverted-index (spec)
  (let ((inverted-index (make-inverted-index)))
    (loop :for (token . locs) :in spec
          :do (loop :for (doc . positions) :in locs
                    :do (dolist (pos positions)
                          (insert-doc-location inverted-index token doc pos))))
    inverted-index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse (spec)
    `(list
      ,@(loop :for (token . locs) :in spec
              :collect `(list ,token ,@(loop :for (doc . positions) :in locs
                                             :collect `(list ,doc ,@positions)))))))

(defmacro index (spec)
  (let ((spec (parse spec)))
    `(make-testing-inverted-index ,spec)))

(deftest inverted-index/merge-inverted-index
  (macrolet ((test (input-1 input-2 output)
               (with-unique-names (result)
                 (let ((input-1 (parse input-1))
                       (input-2 (parse input-2))
                       (output (parse output)))
                   `(let ((,result
                            (merge-inverted-index (make-testing-inverted-index ,input-1)
                                                  (make-testing-inverted-index ,input-2))))
                      ;; (print (searty::dump-inverted-index ,result))
                      (inverted-index-equal ,result
                                            (make-testing-inverted-index ,output)))))))
    (let ((d1 "d1")
          (d2 "d2")
          (d3 "d3")
          (t1 "t1")
          (t2 "t2")
          (t3 "t3"))

      (ok (test ((t1 (d1 1 2 3)))
                ((t1 (d2 5 10)))
                ((t1 (d1 1 2 3)
                     (d2 5 10)))))

      (ok (test ((t1 (d1 1 2 3)))
                ((t2 (d1 1 2 3)))
                ((t1 (d1 1 2 3))
                 (t2 (d1 1 2 3)))))

      (ok (test ((t1 (d1 1 2))
                 (t2 (d2 2 3)))
                ((t1 (d1 2 10) (d2 4 5))
                 (t3 (d3 10)))
                ((t1 (d1 1 2 10) (d2 4 5))
                 (t2 (d2 2 3))
                 (t3 (d3 10))))))))

(deftest inverted-index/encode-decode
  (let ((t1 (random-uuid))
        (t2 (random-uuid))
        (d1 (random-uuid))
        (d2 (random-uuid))
        (d3 (random-uuid)))
    (let ((inverted-index (index ((t1 (d1 1 2 3 241099124) (d2 1 2))
                                  (t2 (d1 3) (d3 5 10 553))))))
      (do-inverted-index ((token-id doc-locations) inverted-index)
        (declare (ignorable token-id))
        (ok (doc-locations-equal doc-locations
                                 (decode-doc-locations-from-vector
                                  (encode-doc-locations-to-vector
                                   doc-locations))))))))
