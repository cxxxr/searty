(in-package :searty2)

(defun insert-sort (item list predicate &key key)
  (merge 'list (list item) list predicate :key key))

(defconstant +null-char+ (code-char 0))

(defstruct token term kind pos)

(defun tokenize-trigram (token)
  (let ((kind (token-kind token)))
    (loop :for term :in (searty:ngram (format nil
                                              "~C~A~C"
                                              +null-char+
                                              (token-term token)
                                              +null-char+)
                                      3)
          :for pos :from (1- (token-pos token))
          :collect (make-token :term term :kind kind :pos pos))))


(defun convert-tokens (tokens)
  (loop :for token :in tokens
        :collect (make-token :term (searty.lisp-tokenizer:token-term token)
                             :kind (searty.lisp-tokenizer:token-kind token)
                             :pos (searty.lisp-tokenizer:token-position token))))

(defun tokenize (string)
  (with-input-from-string (in string)
    (convert-tokens
     (searty.lisp-tokenizer:tokenize in))))

(defun tokenize-file (file)
  (with-open-file (in file)
    (convert-tokens
     (searty.lisp-tokenizer:tokenize in))))

(defvar *document-counter* 0)
(defvar *document-table* (make-hash-table :test 'eql))

(defstruct (document (:constructor %make-document (pathname)))
  (id (incf *document-counter*))
  pathname)

(defun new-document (pathname)
  (let ((document (%make-document pathname)))
    (setf (gethash (document-id document) *document-table*)
          document)
    document))

(defstruct trigram-value kind locations)
(defstruct location document positions)

(defstruct inverted-index
  (trigram-table (make-hash-table :test 'equal))
  (symbol-table (make-hash-table :test 'equal)))

(defun inverted-index-get (inverted-index term to)
  (ecase to
    (:trigram
     (gethash term (inverted-index-trigram-table inverted-index)))
    (:symbol
     (gethash term (inverted-index-symbol-table inverted-index)))))

(defun (setf inverted-index-get) (value inverted-index term to)
  (ecase to
    (:trigram
     (setf (gethash term (inverted-index-trigram-table inverted-index))
           value))
    (:symbol
     (setf (gethash term (inverted-index-symbol-table inverted-index))
           value))))

(defun insert-inverted-index (inverted-index document token to)
  (let ((inverted-values (inverted-index-get inverted-index (token-term token) to)))
    (let ((trigram-value (find (token-kind token) inverted-values :key #'trigram-value-kind)))
      (if (null trigram-value)
          (push (make-trigram-value :kind (token-kind token)
                                    :locations (list (make-location :document document :positions (list (token-pos token)))))
                (inverted-index-get inverted-index (token-term token) to))
          (let ((loc (find (document-id document)
                           (trigram-value-locations trigram-value)
                           :key (lambda (loc) (document-id (location-document loc)))
                           :test #'=)))
            (if (null loc)
                (push (make-location :document document :positions (list (token-pos token)))
                      (trigram-value-locations trigram-value))
                (setf (location-positions loc)
                      (insert-sort (token-pos token) (location-positions loc) #'<))))))))

(defun add-token (inverted-index document token)
  (insert-inverted-index inverted-index document token :trigram))

(defun add-symbol (inverted-index document token)
  (insert-inverted-index inverted-index document token :symbol))

(defun add-file (inverted-index file)
  (let ((document (new-document file)))
    (dolist (token (tokenize-file file))
      (add-symbol inverted-index document token)
      (dolist (token (tokenize-trigram token))
        (add-token inverted-index document token)))))

(defun index-lisp-system (system-designator)
  (let ((inverted-index (make-inverted-index)))
    (dolist (file (searty:find-files (asdf:system-source-directory system-designator)
                                     #'searty:lisp-pathname-p))
      (add-file inverted-index file))
    inverted-index))


(defun search-all (inverted-index query)
  (declare (ignore inverted-index query)))
