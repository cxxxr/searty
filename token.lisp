(in-package :searty)

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
  (check-type token-kind token-kind)
  (if (eq token-kind t)
      "unknown"
      (string-downcase token-kind)))

(defun decode-token-kind (token-kind)
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
  id
  term
  position
  kind)
