(in-package :searty-tests)

(deftest inverted-index/insert-doc-location
  (testing "1つのトークンの1から9までのpositionを挿入するテスト"
    (testing "ランダムなtoken-id, doc-idがあり、新しくinverted_indexを作成した前提条件下で"
      (let ((token-id (random-uuid))
            (doc-id (random-uuid))
            (inverted-index (make-inverted-index)))
        (testing "token-idをキーとしdoc-idの1から9までのpositionをランダムに挿入したとき"
          (loop :for pos :in (shuffle (iota 10))
                :do (insert-doc-location inverted-index token-id doc-id pos)))
        (testing "token-idに対応するlocationsが"
          (let* ((locs (get-doc-locations inverted-index token-id))
                 (loc (first locs)))
            (ok (length= locs 1) "長さが1であるか")
            (ok (equal doc-id (doc-location-document-id loc))
                "doc-locationのdoc-idが挿入したときの値であるか")
            (ok (equal '(0 1 2 3 4 5 6 7 8 9)
                       (doc-location-positions loc))
                "0から9までのpositionsがソートされているか"))))))

  (testing "二つの異なるトークンをとうろく挿入するテスト"
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
        (ok (equal '(1 3 5) (doc-location-positions loc2)))))))

(deftest inverted-index/merge-inverted-index
  (let ((inverted-index (make-inverted-index)))
    ))

