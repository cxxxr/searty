(in-package :searty2)

(deftype token-kind ()
  '(member
    t
    :string
    :symbol
    :character
    :function-object
    :unintern-symbol
    :line-comment
    :block-comment))

(defun encode-token-kind (token-kind)
  (ecase token-kind
    (t 0)
    (:string 1)
    (:symbol 2)
    (:character 3)
    (:function-object 4)
    (:unintern-symbol 5)
    (:line-comment 6)
    (:block-comment 7)))

(defun decode-token-kind (int)
  (ecase int
    (0 t)
    (1 :string)
    (2 :symbol)
    (3 :character)
    (4 :function-object)
    (5 :unintern-symbol)
    (6 :line-comment)
    (7 :block-comment)))

(defstruct token
  term
  position
  kind)
