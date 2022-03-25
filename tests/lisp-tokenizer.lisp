(in-package :searty-tests.lisp-tokenizer)

(defun test (actual expected)
  (testing (princ-to-string expected)
    (ok (length= actual expected))
    (loop :for lit1 :in actual
          :for lit2 :in expected
          :do (ok (equal (token-term lit1) (getf lit2 :term)))
              (ok (equal (token-position lit1) (getf lit2 :position))))))

(deftest lisp-tokenizer
  (test (with-input-from-string (in "foo") (tokenize in))
        '((:TERM "foo" :POSITION 0)))

  (test (with-input-from-string (in "   foo") (tokenize in))
        '((:TERM "foo" :POSITION 3)))

  (test (with-input-from-string (in "'foo") (tokenize in))
        '((:TERM "'" :POSITION 0)
          (:TERM "foo" :POSITION 1)))

  (test (with-input-from-string (in "'(xyz)") (tokenize in))
        '((:TERM "'" :POSITION 0)
          (:TERM "(" :POSITION 1)
          (:TERM "xyz" :POSITION 2)
          (:TERM ")" :POSITION 5)))

  (test (with-input-from-string (in "(cons car cdr)") (tokenize in))
        '((:TERM "(" :POSITION 0)
          (:TERM "cons" :POSITION 1)
          (:TERM "car" :POSITION 6)
          (:TERM "cdr" :POSITION 10)
          (:TERM ")" :POSITION 13)))

  (test (with-input-from-string (in "|f  \\|oo| xxx") (tokenize in))
        '((:TERM "|f  \\|oo|" :POSITION 0)
          (:TERM "xxx" :POSITION 10)))

  (test (with-input-from-string (in "\"abcd\"") (tokenize in))
        '((:TERM "\"abcd\"" :POSITION 0)))

  (test (with-input-from-string (in "\"foo\\\"xbar\"") (tokenize in))
        '((:TERM "\"foo\\\"xbar\"" :POSITION 0)))

  (test (with-input-from-string (in "foo; comment
")
          (tokenize in))
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM " comment" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (with-input-from-string (in "foo;
")
          (tokenize in))
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (with-input-from-string (in "foo;comment
")
          (tokenize in))
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (with-input-from-string (in "foo;comment
bar")
          (tokenize in))
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)
          (:TERM "bar" :POSITION 12 :KIND :SYMBOL)))

  (test (with-input-from-string (in "#\\space") (tokenize in))
        '((:TERM "space" :POSITION 0 :KIND :CHARACTER)))

  (test (with-input-from-string (in "#\\'") (tokenize in))
        '((:TERM "'" :POSITION 0 :KIND :CHARACTER)))

  (test (with-input-from-string (in "#'car") (tokenize in))
        '((:TERM "car" :POSITION 0 :KIND :FUNCTION-OBJECT)))

  (test (with-input-from-string (in "#'(lambda ())") (tokenize in))
        '((:TERM "#'" :POSITION 0 :KIND T)
          (:TERM "(" :POSITION 2 :KIND T)
          (:TERM "lambda" :POSITION 3 :KIND :SYMBOL)
          (:TERM "(" :POSITION 10 :KIND T)
          (:TERM ")" :POSITION 11 :KIND T)
          (:TERM ")" :POSITION 12 :KIND T)))

  (test (with-input-from-string (in "#(abc)") (tokenize in))
        '((:TERM "#(" :POSITION 0 :KIND T)
          (:TERM "abc" :POSITION 2 :KIND :SYMBOL)
          (:TERM ")" :POSITION 5 :KIND T)))

  (test (with-input-from-string (in "#:foo") (tokenize in))
        '((:TERM "foo" :POSITION 0 :KIND :UNINTERN-SYMBOL)))

  (test (with-input-from-string (in "#| foo #||# |#") (tokenize in))
        '((:TERM " foo #||# " :POSITION 0 :KIND :BLOCK-COMMENT)))

  (test (with-input-from-string (in "#1=(x y z)") (tokenize in))
        '((:TERM "#1=" :POSITION 0 :KIND T)
          (:TERM "(" :POSITION 3 :KIND T)
          (:TERM "x" :POSITION 4 :KIND :SYMBOL)
          (:TERM "y" :POSITION 6 :KIND :SYMBOL)
          (:TERM "z" :POSITION 8 :KIND :SYMBOL)
          (:TERM ")" :POSITION 9 :KIND T))))
