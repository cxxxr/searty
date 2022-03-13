(in-package :searty)

(defun ngram (sequence n)
  (loop :for pos :from 0 :to (- (length sequence) n)
        :collect (loop :for i :from 0 :below n
                       :collect (elt sequence (+ pos i)))))

(defun word-tokenize (string)
  (let ((tokens '()))
    (ppcre:do-matches (match-start match-end "\\w+" string)
      (push (subseq string match-start match-end) tokens))
    (nreverse tokens)))
