(in-package :searty)

(defconstant +null-char+ (code-char 0))

(defun make-boundary-string (string start-boundary end-boundary)
  (cond ((and start-boundary end-boundary)
         (format nil
                 "~C~A~C"
                 +null-char+
                 string
                 +null-char+))
        (start-boundary
         (format nil
                 "~C~A"
                 +null-char+
                 string))
        (end-boundary
         (format nil
                 "~A~C"
                 string
                 +null-char+))
        (t
         string)))

(defun tokenize-trigram (token &key start-boundary end-boundary)
  (let ((kind (token-kind token)))
    (loop :for term :in (ngram (make-boundary-string (token-term token)
                                                     start-boundary
                                                     end-boundary)
                               3)
          :for pos :from (if start-boundary
                             (1- (token-position token))
                             (token-position token))
          :collect (make-token :term term :kind kind :position pos))))

(defun trigram-tokens (tokens)
  (mapcan (lambda (token)
            (tokenize-trigram token :start-boundary t :end-boundary t))
          tokens))
