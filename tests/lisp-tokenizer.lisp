(in-package :searty-tests)

(defun test (actual expected)
  (testing (prin1-to-string expected)
    (ok (length= actual expected))
    (loop :for lit1 :in actual
          :for lit2 :in expected
          :do (ok (equal (searty::token-term lit1) (getf lit2 :term)))
              (ok (equal (searty::token-position lit1) (getf lit2 :position))))))

(deftest lisp-tokenizer
  (test (searty::tokenize "foo")
        '((:TERM "foo" :POSITION 0)))

  (test (searty::tokenize "   foo")
        '((:TERM "foo" :POSITION 3)))

  (test (searty::tokenize "'foo")
        '((:TERM "'" :POSITION 0)
          (:TERM "foo" :POSITION 1)))

  (test (searty::tokenize "'(xyz)")
        '((:TERM "'" :POSITION 0)
          (:TERM "(" :POSITION 1)
          (:TERM "xyz" :POSITION 2)
          (:TERM ")" :POSITION 5)))

  (test (searty::tokenize "(cons car cdr)")
        '((:TERM "(" :POSITION 0)
          (:TERM "cons" :POSITION 1)
          (:TERM "car" :POSITION 6)
          (:TERM "cdr" :POSITION 10)
          (:TERM ")" :POSITION 13)))

  (test (searty::tokenize "|f  \\|oo| xxx")
        '((:TERM "|f  \\|oo|" :POSITION 0)
          (:TERM "xxx" :POSITION 10)))

  (test (searty::tokenize "\"abcd\"")
        '((:TERM "\"abcd\"" :POSITION 0)))

  (test (searty::tokenize "\"foo\\\"xbar\"")
        '((:TERM "\"foo\\\"xbar\"" :POSITION 0)))

  (test (searty::tokenize "#:\\|")
        '((:ID NIL :TERM "\\|" :POSITION 0 :KIND :UNINTERN-SYMBOL)))

  (test (searty::tokenize "foo; comment
")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM " comment" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (searty::tokenize "foo;
")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (searty::tokenize "foo;comment
")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)))

  (test (searty::tokenize "foo;comment
bar")
        '((:TERM "foo" :POSITION 0 :KIND :SYMBOL)
          (:TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)
          (:TERM "bar" :POSITION 12 :KIND :SYMBOL)))

  (test (searty::tokenize "#\\space")
        '((:TERM "space" :POSITION 0 :KIND :CHARACTER)))

  (test (searty::tokenize "#\\'")
        '((:TERM "'" :POSITION 0 :KIND :CHARACTER)))

  (test (searty::tokenize "#'car")
        '((:TERM "car" :POSITION 0 :KIND :FUNCTION-OBJECT)))

  (test (searty::tokenize "#'(lambda ())")
        '((:TERM "#'" :POSITION 0 :KIND T)
          (:TERM "(" :POSITION 2 :KIND T)
          (:TERM "lambda" :POSITION 3 :KIND :SYMBOL)
          (:TERM "(" :POSITION 10 :KIND T)
          (:TERM ")" :POSITION 11 :KIND T)
          (:TERM ")" :POSITION 12 :KIND T)))

  (test (searty::tokenize "#(abc)")
        '((:TERM "#(" :POSITION 0 :KIND T)
          (:TERM "abc" :POSITION 2 :KIND :SYMBOL)
          (:TERM ")" :POSITION 5 :KIND T)))

  (test (searty::tokenize "#:foo")
        '((:TERM "foo" :POSITION 0 :KIND :UNINTERN-SYMBOL)))

  (test (searty::tokenize "#| foo #||# |#")
        '((:TERM " foo #||# " :POSITION 0 :KIND :BLOCK-COMMENT)))

  (test (searty::tokenize "#1=(x y z)")
        '((:TERM "#1=" :POSITION 0 :KIND T)
          (:TERM "(" :POSITION 3 :KIND T)
          (:TERM "x" :POSITION 4 :KIND :SYMBOL)
          (:TERM "y" :POSITION 6 :KIND :SYMBOL)
          (:TERM "z" :POSITION 8 :KIND :SYMBOL)
          (:TERM ")" :POSITION 9 :KIND T)))
  (test (searty::tokenize "abc あいうえお xyz")
        '((:TERM "abc" :POSITION 0 :KIND :SYMBOL)
          (:TERM "あいうえお" :POSITION 4 :KIND :SYMBOL)
          (:TERM "xyz" :POSITION 10 :KIND :SYMBOL)))

  (test (searty::tokenize "#|#|foo|#|#")
        '((:TERM "#|foo|#" :POSITION 0 :KIND :BLOCK-COMMENT))))
