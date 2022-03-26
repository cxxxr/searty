(in-package :searty-tests.lisp-tokenizer)

(defun test (actual expected)
  (testing (prin1-to-string expected)
    (ok (length= actual expected))
    (loop :for lit1 :in actual
          :for lit2 :in expected
          :do (ok (equal (token-term lit1) (getf lit2 :term)))
              (ok (equal (token-position lit1) (getf lit2 :position))))))

(deftest lisp-tokenizer
  (test (tokenize "foo")
        '((:TERM "foo" :POSITION 0)))

  (test (tokenize "   foo")
        '((:TERM "foo" :POSITION 3)))

  (test (tokenize "'foo")
        '((:TERM "'" :POSITION 0)
          (:TERM "foo" :POSITION 1)))

  (test (tokenize "'(xyz)")
        '((:TERM "'" :POSITION 0)
          (:TERM "(" :POSITION 1)
          (:TERM "xyz" :POSITION 2)
          (:TERM ")" :POSITION 5)))

  (test (tokenize "(cons car cdr)")
        '((:TERM "(" :POSITION 0)
          (:TERM "cons" :POSITION 1)
          (:TERM "car" :POSITION 6)
          (:TERM "cdr" :POSITION 10)
          (:TERM ")" :POSITION 13)))

  (test (tokenize "|f  \\|oo| xxx")
        '((:TERM "|f  \\|oo|" :POSITION 0)
          (:TERM "xxx" :POSITION 10)))

  (test (tokenize "\"abcd\"")
        '((:TERM "\"abcd\"" :POSITION 0)))

  (test (tokenize "\"foo\\\"xbar\"")
        '((:TERM "\"foo\\\"xbar\"" :POSITION 0)))

  (test (tokenize "foo; comment
")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM " comment" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (tokenize "foo;
")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (tokenize "foo;comment
")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (tokenize "foo;comment
bar")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)
          (:TERM "bar" :POSITION 12 :KIND :SYMBOL)))

  (test (tokenize "#\\space")
        '((:TERM "space" :POSITION 0 :KIND :CHARACTER)))

  (test (tokenize "#\\'")
        '((:TERM "'" :POSITION 0 :KIND :CHARACTER)))

  (test (tokenize "#'car")
        '((:TERM "car" :POSITION 0 :KIND :FUNCTION-OBJECT)))

  (test (tokenize "#'(lambda ())")
        '((:TERM "#'" :POSITION 0 :KIND T)
          (:TERM "(" :POSITION 2 :KIND T)
          (:TERM "lambda" :POSITION 3 :KIND :SYMBOL)
          (:TERM "(" :POSITION 10 :KIND T)
          (:TERM ")" :POSITION 11 :KIND T)
          (:TERM ")" :POSITION 12 :KIND T)))

  (test (tokenize "#(abc)")
        '((:TERM "#(" :POSITION 0 :KIND T)
          (:TERM "abc" :POSITION 2 :KIND :SYMBOL)
          (:TERM ")" :POSITION 5 :KIND T)))

  (test (tokenize "#:foo")
        '((:TERM "foo" :POSITION 0 :KIND :UNINTERN-SYMBOL)))

  (test (tokenize "#| foo #||# |#")
        '((:TERM " foo #||# " :POSITION 0 :KIND :BLOCK-COMMENT)))

  (test (tokenize "#1=(x y z)")
        '((:TERM "#1=" :POSITION 0 :KIND T)
          (:TERM "(" :POSITION 3 :KIND T)
          (:TERM "x" :POSITION 4 :KIND :SYMBOL)
          (:TERM "y" :POSITION 6 :KIND :SYMBOL)
          (:TERM "z" :POSITION 8 :KIND :SYMBOL)
          (:TERM ")" :POSITION 9 :KIND T)))
  (test (tokenize "abc あいうえお xyz")
        '((:TERM "abc" :POSITION 0 :KIND :SYMBOL)
          (:TERM "あいうえお" :POSITION 4 :KIND :SYMBOL)
          (:TERM "xyz" :POSITION 10 :KIND :SYMBOL))))
