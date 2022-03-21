(in-package :searty.lisp-tokenizer)

(defun test (actual expected)
  (assert (length= actual expected))
  (loop :for tok1 :in actual
        :for tok2 :in expected
        :do (assert (equal (token-term tok1) (token-term tok2)))
            (assert (equal (token-position tok1) (token-position tok2)))))

(test (with-input-from-string (in "foo") (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 0)))

(test (with-input-from-string (in "   foo") (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 3)))

(test (with-input-from-string (in "'foo") (scan in))
      '(#S(TOKEN :TERM "'" :POSITION 0)
        #S(TOKEN :TERM "foo" :POSITION 1)))

(test (with-input-from-string (in "'(xyz)") (scan in))
      '(#S(TOKEN :TERM "'" :POSITION 0)
        #S(TOKEN :TERM "(" :POSITION 1)
        #S(TOKEN :TERM "xyz" :POSITION 2)
        #S(TOKEN :TERM ")" :POSITION 5)))

(test (with-input-from-string (in "(cons car cdr)") (scan in))
      '(#S(TOKEN :TERM "(" :POSITION 0)
        #S(TOKEN :TERM "cons" :POSITION 1)
        #S(TOKEN :TERM "car" :POSITION 6)
        #S(TOKEN :TERM "cdr" :POSITION 10)
        #S(TOKEN :TERM ")" :POSITION 13)))

(test (with-input-from-string (in "|f  \\|oo| xxx") (scan in))
      '(#S(TOKEN :TERM "|f  \\|oo|" :POSITION 0)
        #S(TOKEN :TERM "xxx" :POSITION 10)))

(test (with-input-from-string (in "\"abcd\"") (scan in))
      '(#S(TOKEN :TERM "\"abcd\"" :POSITION 0)))

(test (with-input-from-string (in "\"foo\\\"xbar\"") (scan in))
      '(#S(TOKEN :TERM "\"foo\\\"xbar\"" :POSITION 0)))

(test (with-input-from-string (in "foo; comment
")
                 (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 0 :KIND :SYMBOL)
        #S(TOKEN :TERM " comment" :POSITION 3 :KIND :LINE-COMMENT)))

(test (with-input-from-string (in "foo;
")
        (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 0 :KIND :SYMBOL)
        #S(TOKEN :TERM "" :POSITION 3 :KIND :LINE-COMMENT)))

(test (with-input-from-string (in "foo;comment
")
        (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 0 :KIND :SYMBOL)
        #S(TOKEN :TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)))

(test (with-input-from-string (in "foo;comment
bar")
        (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 0 :KIND :SYMBOL)
        #S(TOKEN :TERM "comment" :POSITION 3 :KIND :LINE-COMMENT)
        #S(TOKEN :TERM "bar" :POSITION 12 :KIND :SYMBOL)))

(test (with-input-from-string (in "#\\space") (scan in))
      '(#S(TOKEN :TERM "space" :POSITION 0 :KIND :CHARACTER)))

(test (with-input-from-string (in "#'car") (scan in))
      '(#S(TOKEN :TERM "car" :POSITION 0 :KIND :FUNCTION-OBJECT)))

(test (with-input-from-string (in "#'(lambda ())") (scan in))
      '(#S(TOKEN :TERM "#'" :POSITION 0 :KIND T)
        #S(TOKEN :TERM "(" :POSITION 2 :KIND T)
        #S(TOKEN :TERM "lambda" :POSITION 3 :KIND :SYMBOL)
        #S(TOKEN :TERM "(" :POSITION 10 :KIND T)
        #S(TOKEN :TERM ")" :POSITION 11 :KIND T)
        #S(TOKEN :TERM ")" :POSITION 12 :KIND T)))

(test (with-input-from-string (in "#(abc)") (scan in))
      '(#S(TOKEN :TERM "#(" :POSITION 0 :KIND T)
        #S(TOKEN :TERM "abc" :POSITION 2 :KIND :SYMBOL)
        #S(TOKEN :TERM ")" :POSITION 5 :KIND T)))

(test (with-input-from-string (in "#:foo") (scan in))
      '(#S(TOKEN :TERM "foo" :POSITION 0 :KIND :UNINTERN-SYMBOL)))

(test (with-input-from-string (in "#| foo #||# |#") (scan in))
      '(#S(TOKEN :TERM " foo #||# " :POSITION 0 :KIND :BLOCK-COMMENT)))
