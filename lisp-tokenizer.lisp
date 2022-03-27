(in-package :searty)

(defstruct (lexer (:constructor make-lexer (text)))
  text
  (position 0))

(defun lexer-peek-char (lexer)
  (when (< (lexer-position lexer)
           (length (lexer-text lexer)))
    (char (lexer-text lexer)
          (lexer-position lexer))))

(defun lexer-read-char (lexer)
  (when-let ((c (lexer-peek-char lexer)))
    (incf (lexer-position lexer))
    c))

(defun lexer-unread-char (lexer c)
  (declare (ignore c))
  (decf (lexer-position lexer)))

(defun lexer-read-line (lexer)
  (with-output-to-string (out)
    (loop :for c := (lexer-read-char lexer)
          :until (member c '(#\newline nil))
          :do (write-char c out))))

(defun skip-whitespaces (lexer)
  (loop :for c := (lexer-peek-char lexer)
        :while (and c (whitespacep c))
        :do (lexer-read-char lexer)
        :finally (return c)))

(defvar *macro-character-table*
  (plist-hash-table '(#\( scan-one-char
                      #\) scan-one-char
                      #\' scan-one-char
                      #\` scan-one-char
                      #\, scan-one-char
                      #\; scan-line-comment
                      #\" scan-string
                      #\# scan-dispatch-macro-character)
                    :test #'equal))

(defvar *dispatch-macro-character-table*
  (plist-hash-table '(#\\ scan-character
                      #\' scan-function
                      #\( scan-sharp-others
                      #\: scan-unintern-symbol
                      #\+ scan-sharp-others
                      #\- scan-sharp-others
                      #\# scan-sharp-others
                      #\= scan-sharp-others
                      #\* scan-sharp-others
                      #\| scan-block-comment
                      #\b scan-sharp-others
                      #\. scan-sharp-others)
                    :test #'equal))

(defun exact-char (lexer expected)
  (assert (char= expected (lexer-read-char lexer))))

(defun scan-one-char (lexer)
  (make-token :position (lexer-position lexer)
              :term (string (lexer-read-char lexer))
              :kind t))

(defun scan-line-comment (lexer)
  (let ((position (lexer-position lexer)))
    (exact-char lexer #\;)
    (make-token :term (lexer-read-line lexer)
                :position position
                :kind :line-comment)))

(defun scan-string (lexer)
  (let ((position (lexer-position lexer))
        (term (with-output-to-string (out)
                (exact-char lexer #\")
                (write-char #\" out)
                (loop :for c := (lexer-read-char lexer)
                      :do (case c
                            (#\"
                             (write-char c out)
                             (return))
                            (#\\
                             (write-char c out)
                             (write-char (lexer-read-char lexer) out))
                            (otherwise
                             (write-char c out)))))))
    (make-token :position position
                :term term
                :kind :string)))

(defun scan-multiple-escape (lexer)
  (with-output-to-string (out)
    (exact-char lexer #\|)
    (write-char #\| out)
    (loop :for c := (lexer-read-char lexer)
          :do (case c
                (#\|
                 (write-char #\| out)
                 (return))
                (#\\
                 (write-char #\\ out)
                 (write-char (lexer-read-char lexer) out))
                (otherwise
                 (write-char c out))))))

(defun scan-symbol-name (lexer)
  (with-output-to-string (out)
    (loop :for c := (lexer-peek-char lexer)
          :do (cond ((null c)
                     (return))
                    ((whitespacep c)
                     (return))
                    ((gethash c *macro-character-table*)
                     (return))
                    ((char= c #\\)
                     (write-char (lexer-read-char lexer) out)
                     (write-char (lexer-read-char lexer) out))
                    ((char= c #\|)
                     (write-string (scan-multiple-escape lexer) out))
                    (t
                     (write-char c out)
                     (lexer-read-char lexer))))))

(defun scan-symbol (lexer)
  (let ((pos (lexer-position lexer))
        (term (scan-symbol-name lexer)))
    (make-token :position pos
                :term term
                :kind :symbol)))

(defun scan-dispatch-macro-arg (lexer)
  (let ((arg (with-output-to-string (out)
               (loop :for c := (lexer-peek-char lexer)
                     :while (and c (digit-char-p c))
                     :do (write-char c out)
                         (lexer-read-char lexer)))))
    (if (length= arg 0)
        nil
        (parse-integer arg))))

(defun scan-dispatch-macro-character (lexer)
  (let ((position (lexer-position lexer)))
    (exact-char lexer #\#)
    (let ((arg (scan-dispatch-macro-arg lexer))
          (scanner (gethash (lexer-peek-char lexer)
                            *dispatch-macro-character-table*
                            'scan-sharp-others)))
      (funcall scanner lexer arg position))))

(defun scan-sharp-others (lexer arg position)
  (make-token :term (if arg
                        (format nil "#~D~C" arg (lexer-read-char lexer))
                        (format nil "#~C" (lexer-read-char lexer)))
              :position position
              :kind t))

(defun delimiter-char-p (c)
  (or (null c)
      (whitespacep c)
      (gethash c *macro-character-table*)))

(defun scan-character (lexer arg position)
  (declare (ignore arg))
  (exact-char lexer #\\)
  (let ((char (lexer-read-char lexer))
        (next-char (lexer-peek-char lexer)))
    (cond ((delimiter-char-p next-char)
           (make-token :term (string char)
                       :position position
                       :kind :character))
          (t
           (lexer-unread-char lexer char)
           (make-token :term (scan-symbol-name lexer)
                       :position position
                       :kind :character)))))

(defun scan-function (lexer arg position)
  (declare (ignore arg))
  (exact-char lexer #\')
  (if (char= #\( (lexer-peek-char lexer))
      (make-token :term "#'"
                  :position position
                  :kind t)
      (make-token :term (scan-symbol-name lexer)
                  :position position
                  :kind :function-object)))

(defun scan-unintern-symbol (lexer arg position)
  (declare (ignore arg))
  (exact-char lexer #\:)
  (make-token :term (scan-symbol-name lexer)
              :position position
              :kind :unintern-symbol))

(defun scan-block-comment (lexer arg position)
  (declare (ignore arg))
  (exact-char lexer #\|)
  (let ((term (with-output-to-string (out)
                (loop :with depth := 1
                      :for prev := nil :then c
                      :and c := (lexer-read-char lexer)
                      :do (cond ((and prev
                                      (char= prev #\#)
                                      (char= c #\|))
                                 (incf depth))
                                ((and prev
                                      (char= prev #\|)
                                      (char= c #\#))
                                 (when (zerop (decf depth))
                                   (return))))
                          (when prev
                            (write-char prev out))))))
    (make-token :term term
                :position position
                :kind :block-comment)))

(defun scan-token (lexer)
  (unless (skip-whitespaces lexer)
    (return-from scan-token (values nil t)))
  (let ((scanner (gethash (lexer-peek-char lexer) *macro-character-table*)))
    (if scanner
        (funcall scanner lexer)
        (scan-symbol lexer))))

(defun tokenize (text)
  (let ((lexer (make-lexer text)))
    (let ((tokens '()))
      (loop
        (multiple-value-bind (token eof) (scan-token lexer)
          (when eof
            (return))
          (when token
            (push token tokens))))
      (nreverse tokens))))
