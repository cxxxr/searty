(in-package :searty2)

(defun insert-sort (item list predicate &key key)
  (merge 'list (list item) list predicate :key key))

(defconstant +null-char+ (code-char 0))

(defstruct token term kind pos)

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
    (loop :for term :in (searty:ngram (make-bounding-string (token-term token)
                                                            start-bounding
                                                            end-bounding)
                                      3)
          :for pos :from (1- (token-pos token))
          :collect (make-token :term term :kind kind :pos pos))))


(defun convert-tokens (tokens)
  (loop :for token :in tokens
        :collect (make-token :term (searty.lisp-tokenizer:token-term token)
                             :kind (searty.lisp-tokenizer:token-kind token)
                             :pos (searty.lisp-tokenizer:token-position token))))

(defun tokenize (string)
  (convert-tokens
   (searty.lisp-tokenizer:tokenize string)))

(defun tokenize-file (file)
  (convert-tokens
   (searty.lisp-tokenizer:tokenize
    (read-file-into-string file))))

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

(defun document= (document1 document2)
  (= (document-id document1) (document-id document2)))

(defun document< (document1 document2)
  (< (document-id document1) (document-id document2)))

(defstruct trigram-value kind locations)
(defstruct location document positions)

(defstruct inverted-index
  (trigram-table (make-hash-table :test 'equal))
  (symbol-table (make-hash-table :test 'equal)))

(defun inverted-index-get (inverted-index term from)
  (ecase from
    (:trigram
     (gethash term (inverted-index-trigram-table inverted-index)))
    (:symbol
     (gethash term (inverted-index-symbol-table inverted-index)))))

(defun (setf inverted-index-get) (value inverted-index term from)
  (ecase from
    (:trigram
     (setf (gethash term (inverted-index-trigram-table inverted-index))
           value))
    (:symbol
     (setf (gethash term (inverted-index-symbol-table inverted-index))
           value))))

(defun insert-inverted-index (inverted-index document token from)
  (let* ((inverted-values (inverted-index-get inverted-index (token-term token) from))
         (trigram-value (find (token-kind token) inverted-values :key #'trigram-value-kind)))
    (if (null trigram-value)
        (push (make-trigram-value
               :kind (token-kind token)
               :locations (list (make-location :document document
                                               :positions (list (token-pos token)))))
              (inverted-index-get inverted-index (token-term token) from))
        (let ((loc (find (document-id document)
                         (trigram-value-locations trigram-value)
                         :key (lambda (loc) (document-id (location-document loc)))
                         :test #'=)))
          (if (null loc)
              (setf (trigram-value-locations trigram-value)
                    (insert-sort (make-location :document document :positions (list (token-pos token)))
                                 (trigram-value-locations trigram-value)
                                 #'document<
                                 :key #'location-document))
              (setf (location-positions loc)
                    (insert-sort (token-pos token) (location-positions loc) #'<)))))))

(defun add-token (inverted-index document token)
  (insert-inverted-index inverted-index document token :trigram))

(defun add-symbol (inverted-index document token)
  (insert-inverted-index inverted-index document token :symbol))

(defun add-file (inverted-index file)
  (let ((document (new-document file)))
    (dolist (token (tokenize-file file))
      (add-symbol inverted-index document token)
      (dolist (token (tokenize-trigram token :start-bounding t :end-bounding t))
        (add-token inverted-index document token)))))

(defun index-lisp-system (system-designator)
  (let ((inverted-index (make-inverted-index)))
    (dolist (file (searty:find-files (asdf:system-source-directory system-designator)
                                     #'searty:lisp-pathname-p))
      (add-file inverted-index file))
    inverted-index))

;;;
(defstruct (range (:constructor make-range (start end))) start end)

(defstruct matched
  (document-positions-map (make-hash-table :test 'equal)))

(defun add-matched (matched document position length)
  (push (make-range position (+ position length))
        (gethash (document-pathname document)
                 (matched-document-positions-map matched))))

(defun normalize-ranges (ranges)
  (let* ((ranges (sort ranges #'< :key #'range-start))
        (head-ranges ranges))
    (loop
      (let ((current (first ranges))
            (next (second ranges)))
        (when (null next) (return))
        (cond ((<= (range-start current)
                   (range-start next)
                   (range-end current))
               (setf (range-end current) (range-end next))
               (setf (rest ranges) (rest (rest ranges))))
              (t
               (setf ranges (rest ranges))))))
    head-ranges))

(defun normalize-matched (matched &key start-bounding end-bounding)
  (let ((ht (matched-document-positions-map matched)))
    (maphash (lambda (document ranges)
               (setf ranges (sort ranges #'< :key #'range-start))
               (setf (gethash document ht)
                     (normalize-ranges ranges)))
             ht)
    (maphash (lambda (document ranges)
               (when (or start-bounding end-bounding)
                 (setf (gethash document ht)
                       (mapcar (lambda (range)
                                 (make-range (if start-bounding
                                                 (1+ (range-start range))
                                                 (range-start range))
                                             (if end-bounding
                                                 (1- (range-end range))
                                                 (range-end range))))
                               ranges))))
             ht))
  matched)

;;;
(defstruct (posting (:constructor %make-posting (token locations))) token locations)

(defun make-posting (inverted-index token)
  (or (dolist (trigram-value (inverted-index-get inverted-index
                                                 (token-term token)
                                                 :trigram))
        (when (eq :symbol (trigram-value-kind trigram-value))
          (let ((locations (trigram-value-locations trigram-value)))
            (return (%make-posting token locations)))))
      (%make-posting token nil)))

(defun make-postings (inverted-index tokens)
  (let ((postings (make-array (length tokens))))
    (loop :for token :in tokens
          :for i :from 0
          :do (setf (aref postings i)
                    (make-posting inverted-index token)))
    postings))

(defun posting-null-p (posting)
  (null (posting-locations posting)))

(defun posting-location (posting)
  (first (posting-locations posting)))

(defun posting-document (posting)
  (location-document (posting-location posting)))

(defun posting-positions (posting)
  (location-positions (posting-location posting)))

(defun posting-next (posting)
  (setf (posting-locations posting)
        (rest (posting-locations posting)))
  posting)

(defun postings-next (postings)
  (loop :for posting :across postings
        :do (posting-next posting)))

(defun next-minimum-posting (postings)
  (let ((min-posting (aref postings 0)))
    (loop :for i :from 1 :below (length postings)
          :for posting := (aref postings i)
          :do (when (document< (posting-document posting)
                               (posting-document min-posting))
                (setf min-posting posting)))
    (posting-next min-posting)))

(defun same-document-p (postings)
  (let ((first-document (posting-document (aref postings 0))))
    (loop :for i :from 1 :below (length postings)
          :for posting := (aref postings i)
          :always (document= first-document
                             (posting-document posting)))))

(defun end-posting-one-or-more-p (postings)
  (some #'posting-null-p postings))

(defun convert-token-locations-map (token-locations-map)
  (loop :for term :being :each :hash-key :of token-locations-map :using (:hash-value locs)
        :collect (list term
                       (mapcar (lambda (loc)
                                 (list (document-pathname (location-document loc))
                                       (location-positions loc)))
                               locs))))

(defun search-and (inverted-index query)
  (let* ((tokens (mapcan #'tokenize-trigram (tokenize query)))
         (postings (make-postings inverted-index tokens))
         (matched (make-matched)))
    (loop :until (end-posting-one-or-more-p postings)
          :do (cond ((same-document-p postings)
                     (loop :for posting :across postings
                           :do (loop :for pos :in (posting-positions posting)
                                     :do (add-matched matched (posting-document posting) pos 1)))
                     (postings-next postings))
                    (t
                     (next-minimum-posting postings))))
    (normalize-matched matched)))

(defun compute-relative-positions-list (postings)
  (loop :for posting :across postings
        :for offset :from 0
        :collect (loop :for pos :in (location-positions (posting-location posting))
                       :collect (- pos offset))))

(defun intersection-positions (relative-positions-list)
  (loop :with set := (first relative-positions-list)
        :for positions :in (rest relative-positions-list)
        :do (setf set (intersection set positions))
        :finally (return set)))

(defun phrase-match-p (postings)
  (let ((relative-positions-list (compute-relative-positions-list postings)))
    (intersection-positions relative-positions-list)))

(defun search-phrase (inverted-index query &key start-bounding end-bounding)
  (let* ((tokens (mapcan (lambda (token)
                           (tokenize-trigram token
                                             :start-bounding start-bounding
                                             :end-bounding end-bounding))
                         (tokenize query)))
         (postings (make-postings inverted-index tokens))
         (matched (make-matched)))
    (loop :until (some #'posting-null-p postings)
          :do (cond ((same-document-p postings)
                     (when-let ((positions (phrase-match-p postings)))
                       (loop :for posting :across postings
                             :for offset :from 0
                             :do (dolist (pos positions)
                                   (add-matched matched
                                                (posting-document posting)
                                                (+ pos offset)
                                                3))))
                     (postings-next postings))
                    (t
                     (next-minimum-posting postings))))
    (normalize-matched matched
                       :start-bounding start-bounding
                       :end-bounding end-bounding)))

(defun read-file-range (file range)
  (let* ((buffer (lem:find-file-buffer file))
         (point (lem:buffer-point buffer)))
    (lem:move-to-position point (range-start range))
    (lem:with-point ((line-start (lem:line-start point)))
      (lem:with-point ((line-end (lem:line-end point)))
        (lem:with-point ((match-start (lem:move-to-position point (1+ (range-start range)))))
          (lem:with-point ((match-end (lem:move-to-position point (1+ (range-end range)))))
            (format t "~&~A:~A~A~A~%"
                    file
                    (lem:points-to-string line-start match-start)
                    (cl-ansi-text:red (lem:points-to-string match-start match-end))
                    (lem:points-to-string match-end line-end))))))))

(defun print-matched (matched)
  (maphash (lambda (pathname ranges)
             (dolist (range ranges)
               (read-file-range pathname range)))
           (matched-document-positions-map matched)))

#|

(defparameter $ (index-lisp-system :searty))
(print-matched (search-phrase $ "save-excursion"))

|#
