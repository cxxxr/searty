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

(defun lisp-pathname-p (pathname)
  (equal "lisp" (pathname-type pathname)))

(defun find-files (directory test)
  (append (remove-if-not test (uiop:directory-files directory))
          (mapcan (lambda (dir)
                    (unless (search "/.git/" (namestring dir))
                      (find-files dir test)))
                  (uiop:subdirectories directory))))

(defun insert-sort (item list predicate &key key)
  (merge 'list (list item) list predicate :key key))

(defun random-uuid ()
  (princ-to-string (uuid:make-v4-uuid)))

(defun coerce-unsigned-byte-vector (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents (coerce bytes 'list)))

(defmacro measure-time (form)
  (with-unique-names (start)
    `(let ((,start (get-internal-real-time)))
       ,form
       (floor (- (get-internal-real-time) ,start) 1000))))

(defun basename (filename)
  (string-right-trim "/"
                     (enough-namestring
                      filename
                      (uiop:pathname-parent-directory-pathname filename))))

(defun date ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format nil "[~A/~A/~A ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

(defun list-to-hash-table (list &key (value t) (test 'eql))
  (let ((ht (make-hash-table :test test)))
    (dolist (elt list)
      (setf (gethash elt ht) value))
    ht))

(defmacro do-hash-table ((key value hash-table) &body body)
  `(maphash (lambda (,key ,value)
              ,@body)
            ,hash-table))

(defun parse-symbol (string)
  (ppcre:register-groups-bind (package-name symbol-name) ("(.*)::(.*)" string)
    (when (and package-name symbol-name)
      (return-from parse-symbol
        (values (string-upcase symbol-name)
                (string-upcase package-name)))))
  (values (string-upcase string) nil))
