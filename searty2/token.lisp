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

(defstruct token
  term
  position
  kind)
