(in-package :searty.lisp-tokenizer)

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
                      #\* scan-sharp-others
                      #\| scan-block-comment
                      #\b scan-sharp-others
                      #\. scan-sharp-others)
                    :test #'equal))

(defun whitespacep (c)
  (member c '(#\Space #\Tab #\Linefeed #\Return #\Page)))

(defun exact-char (stream expected)
  (assert (char= expected (read-char stream))))

(defun scan-one-char (stream)
  (make-token :position (file-position stream)
              :term (string (read-char stream))
              :kind t))

(defun scan-line-comment (stream)
  (let ((position (file-position stream)))
    (exact-char stream #\;)
    (make-token :term (read-line stream)
                :position position
                :kind :line-comment)))

(defun scan-string (stream)
  (let ((pos (file-position stream))
        (term (with-output-to-string (out)
                (exact-char stream #\")
                (write-char #\" out)
                (loop :for c := (read-char stream)
                      :do (case c
                            (#\"
                             (write-char c out)
                             (return))
                            (#\\
                             (write-char c out)
                             (write-char (read-char stream) out))
                            (otherwise
                             (write-char c out)))))))
    (make-token :position pos
                :term term
                :kind :string)))

(defun scan-multiple-escape (stream)
  (with-output-to-string (out)
    (exact-char stream #\|)
    (write-char #\| out)
    (loop :for c := (read-char stream)
          :do (case c
                (#\|
                 (write-char #\| out)
                 (return))
                (#\\
                 (write-char #\\ out)
                 (write-char (read-char stream) out))
                (otherwise
                 (write-char c out))))))

(defun scan-symbol-name (stream)
  (with-output-to-string (out)
    (loop :for c := (peek-char nil stream nil)
          :do (cond ((null c)
                     (return))
                    ((whitespacep c)
                     (return))
                    ((gethash c *macro-character-table*)
                     (return))
                    ((char= c #\\)
                     (write-char c out)
                     (read-char stream))
                    ((char= c #\|)
                     (write-string (scan-multiple-escape stream) out))
                    (t
                     (write-char c out)
                     (read-char stream))))))

(defun scan-symbol (stream)
  (let ((pos (file-position stream))
        (term (scan-symbol-name stream)))
    (make-token :position pos
                :term term
                :kind :symbol)))

(defun scan-dispatch-macro-arg (stream)
  (let ((arg (with-output-to-string (out)
               (loop :for c := (peek-char nil stream)
                     :while (and c (digit-char-p c))
                     :do (write-char c out)))))
    (if (length= arg 0)
        nil
        (parse-integer arg))))

(defun scan-dispatch-macro-character (stream)
  (let ((position (file-position stream)))
    (exact-char stream #\#)
    (let ((arg (scan-dispatch-macro-arg stream))
          (scanner (gethash (peek-char nil stream)
                            *dispatch-macro-character-table*
                            'scan-sharp-others)))
      (funcall scanner stream arg position))))

(defun scan-sharp-others (stream arg position)
  (declare (ignore arg))
  (make-token :term (format nil "#~C" (read-char stream))
              :position position
              :kind t))

(defun scan-character (stream arg position)
  (declare (ignore arg))
  (exact-char stream #\\)
  (let ((char-name
          (with-output-to-string (out)
            (loop :for c := (peek-char nil stream nil)
                  :while (and c
                              (or (alphanumericp c)
                                  (member c '(#\- #\_))))
                  :do (write-char c out)
                      (read-char stream)))))
    (make-token :term char-name
                :position position
                :kind :character)))

(defun scan-function (stream arg position)
  (declare (ignore arg))
  (exact-char stream #\')
  (if (char= #\( (peek-char nil stream))
      (make-token :term "#'"
                  :position position
                  :kind t)
      (make-token :term (scan-symbol-name stream)
                  :position position
                  :kind :function-object)))

(defun scan-unintern-symbol (stream arg position)
  (declare (ignore arg))
  (exact-char stream #\:)
  (make-token :term (scan-symbol-name stream)
              :position position
              :kind :unintern-symbol))

(defun scan-block-comment (stream arg position)
  (declare (ignore arg))
  (exact-char stream #\|)
  (let ((term (with-output-to-string (out)
                (loop :with depth := 1
                      :for prev := nil :then c
                      :and c := (read-char stream)
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

(defun scan-token (stream)
  (unless (peek-char t stream nil)
    (return-from scan-token (values nil t)))
  (if-let ((scanner (gethash (peek-char nil stream) *macro-character-table*)))
    (funcall scanner stream)
    (scan-symbol stream)))

(defun tokenize (stream)
  (let ((tokens '()))
    (loop
      (multiple-value-bind (token eof) (scan-token stream)
        (when eof
          (return))
        (when token
          (push token tokens))))
    (nreverse tokens)))
