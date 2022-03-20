(in-package :searty-tests)

(defun random-sorting-uuids (n)
  (sort (loop :repeat n :collect (random-uuid))
        #'string<))

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

  (let* ((token-id-1 (random-uuid))
         (doc-ids (random-sorting-uuids 2))
         (doc-id-1 (first doc-ids))
         (doc-id-2 (second doc-ids))
         (inverted-index (make-inverted-index)))

    (insert-doc-location inverted-index token-id-1 doc-id-1 1)
    (insert-doc-location inverted-index token-id-1 doc-id-1 2)
    (insert-doc-location inverted-index token-id-1 doc-id-1 3)

    (insert-doc-location inverted-index token-id-1 doc-id-2 1)
    (insert-doc-location inverted-index token-id-1 doc-id-2 5)
    (insert-doc-location inverted-index token-id-1 doc-id-2 3)

    (let* ((locs (get-doc-locations inverted-index token-id-1))
           (loc1 (first locs))
           (loc2 (second locs)))
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

(deftest encode/decode-positive-integer-list
  (let ((data
          '(5 15 24 33 42 50 53 57 59 61 62 65 68 70 137 143 163 170 173 177 201 206 209 241 252 284
            313 314 319 327 355 363 365 366 382 386 389 390 392 393 410 411 415 462 472 475 499 505 510
            513 517 522 525 528 534 537 549 552 555 561 570 571 580 592 596 600 603 606 609 612 618 628
            632 636 639 644 646 649 658 662 666 670 681 684 696 701 703 706 715 719 730 732 734 736 739
            742 744 748 752 754 760 763 768 779 797 810 814 819 823 828 832 841 851 854 858 862 873 877
            883 891 895 904 908 910 914 917 921 926 930 933 936 948 957 961 973 976 984 986 990 993 996
            1004 1006 1010 1015 1031 1035 1037 1041 1049 1053 1072 1077 1115 1142 1162 1176 1184 1188
            1210 1223 1240 1253 1284 1290 1298 1311 1316 1323 1331 1347 1356 1360 1373 1380 1387 1393
            1407 1416 1420 1441 1446 1459 1472 1481 1485 1506 1511 1520 1531 1540 1544 1561 1566 1571
            1581 1592 1601 1605 1622 1630 1634 1639 1650 1665 1674 1678 1684 1689 1699 1712 1718 1728
            1741)))
    (with-open-stream (out (flex:make-in-memory-output-stream))
      (searty::encode-positive-integer-list data out)
      (with-open-stream (in (flex:make-in-memory-input-stream
                             (flex:get-output-stream-sequence out)))
        (ok (equal data (searty::decode-positive-integer-list in)))))))
