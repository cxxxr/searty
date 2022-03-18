(in-package :searty-tests)

(deftest simple-analyzer-test
  (let ((analyzer (make-instance 'simple-analyzer)))
    (ok (equal '("foo")
               (analyze analyzer "foo")))
    (ok (equal '("foo" "bar" "baz")
               (analyze analyzer "foo bar baz")))
    (ok (equal '("abc123" "foo")
               (analyze analyzer "abc123 foo")))))
