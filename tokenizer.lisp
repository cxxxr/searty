(in-package :searty)

(defconstant +null-char+ (code-char 0))

(defun make-bounding-string (string start-bounding end-bounding)
  (cond ((and start-bounding end-bounding)
         (format nil
                 "~C~A~C"
                 +null-char+
                 string
                 +null-char+))
        (start-bounding
         (format nil
                 "~C~A"
                 +null-char+
                 string))
        (end-bounding
         (format nil
                 "~A~C"
                 string
                 +null-char+))
        (t
         string)))

(defun tokenize-trigram (token &key start-bounding end-bounding)
  (let ((kind (token-kind token)))
    (loop :for term :in (ngram (make-bounding-string (token-term token)
                                                     start-bounding
                                                     end-bounding)
                               3)
          :for pos :from (if start-bounding
                             (1- (token-position token))
                             (token-position token))
          :collect (make-token :term term :kind kind :position pos))))

(defun tokenize-file (text)
  (mapcan (lambda (token)
            (tokenize-trigram token :start-bounding t :end-bounding t))
          (tokenize text)))
