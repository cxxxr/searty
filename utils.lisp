(in-package :searty)

(defun whitespacep (c)
  (member c '(#\Space #\Tab #\Linefeed #\Return #\Page)))

(defun ngram (string n)
  (if (< (length string) n)
      (list string)
      (loop :for pos :from 0 :to (- (length string) n)
            :collect (with-output-to-string (out)
                       (loop :for i :from 0 :below n
                             :do (write-char (char string (+ pos i)) out))))))

(defun word-tokenize (string)
  (let ((tokens '()))
    (ppcre:do-matches (match-start match-end "\\w+" string)
      (push (subseq string match-start match-end) tokens))
    (nreverse tokens)))

(defun random-uuid ()
  (princ-to-string (uuid:make-v4-uuid)))

(defun coerce-unsigned-byte-vector (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents (coerce bytes 'list)))

(defun lisp-pathname-p (pathname)
  (equal "lisp" (pathname-type pathname)))

(defun find-files (directory test)
  (append (remove-if-not test (uiop:directory-files directory))
          (mapcan (lambda (dir)
                    (unless (search "/.git/" (namestring dir))
                      (find-files dir test)))
                  (uiop:subdirectories directory))))
