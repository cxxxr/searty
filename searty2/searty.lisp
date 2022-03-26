(in-package :searty2)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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
          :for pos :from (1- (token-position token))
          :collect (make-token :term term :kind kind :position pos))))

(defun tokenize-file (file)
  (mapcan (lambda (token)
            (tokenize-trigram token :start-bounding t :end-bounding t))
          (tokenize (read-file-into-string file))))

;; TODO: idとdocumentをマッピングするためにあるが、rdbmsがあれば不要になるので削除する
(defvar *document-table* (make-hash-table :test 'equal))

(defun save-inverted-index (inverted-index)
  (inverted-index-foreach
   inverted-index
   (lambda (token-id locations)
     (upsert-inverted-index *database*
                            token-id
                            locations))))

(defun flush-inverted-index (inverted-index)
  (let* ((storage-inverted-index
           (resolve-inverted-index *database* (inverted-index-token-ids inverted-index)))
         (merged-inverted-index
           (inverted-index-merge inverted-index storage-inverted-index)))
    (save-inverted-index merged-inverted-index)))

(defun create-document (pathname)
  (let ((document (make-document pathname (read-file-into-string pathname))))
    (insert-document *database* document)
    ;; (setf (gethash (document-id document) *document-table*) document)
    document))

(defun add-file (inverted-index file)
  (dbi:with-transaction (database-connection *database*)
    (let ((document (create-document file))
          (tokens (tokenize-file file)))
      (dolist (token tokens)
        ;; NOTE: このresolve-token, insert-token内でtoken-idがセットされる
        (unless (resolve-token *database* token)
          (insert-token *database* token))
        (inverted-index-insert inverted-index (document-id document) token))
      ;; TODO: index-lisp-systemで最後に一度だけ行うようにする
      (flush-inverted-index inverted-index))))

(defun index-lisp-system (system-designator)
  (let ((inverted-index (make-inverted-index)))
    (dolist (file (find-files (asdf:system-source-directory system-designator)
                              #'lisp-pathname-p))
      (format t "~&~A " file)
      (let ((ms (measure-time (add-file inverted-index file))))
        (format t "[~D ms]~%" ms)))
    inverted-index))

;;;
(defstruct (range (:constructor make-range (start end))) start end)

(defstruct matched
  (document-positions-map (make-hash-table :test 'equal)))

(defun add-matched (matched document-id position length)
  (push (make-range position (+ position length))
        (gethash document-id
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
  (let ((locations (inverted-index-get inverted-index token)))
    (%make-posting token locations)))

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

(defun posting-document-id (posting)
  (location-document-id (posting-location posting)))

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
          :do (when (id< (posting-document-id posting)
                         (posting-document-id min-posting))
                (setf min-posting posting)))
    (posting-next min-posting)))

(defun same-document-p (postings)
  (let ((first-document-id (posting-document-id (aref postings 0))))
    (loop :for i :from 1 :below (length postings)
          :for posting := (aref postings i)
          :always (id= first-document-id
                       (posting-document-id posting)))))

(defun end-posting-one-or-more-p (postings)
  (some #'posting-null-p postings))

(defun convert-token-locations-map (token-locations-map)
  (loop :for term :being :each :hash-key :of token-locations-map :using (:hash-value locs)
        :collect (list term
                       (mapcar (lambda (loc)
                                 (list (location-document-id loc)
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
                                     :do (add-matched matched (posting-document-id posting) pos 1)))
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
                                                (posting-document-id posting)
                                                (+ pos offset)
                                                3))))
                     (postings-next postings))
                    (t
                     (next-minimum-posting postings))))
    (normalize-matched matched
                       :start-bounding start-bounding
                       :end-bounding end-bounding)))

(defun read-file-range (file range)
  (with-open-file (in file)
    (loop :with pos := 0
          :for line := (read-line in)
          :for line-number :from 1
          :do (when (<= pos (range-start range) (+ pos (length line)))
                (let ((start (- (range-start range) pos))
                      (end (- (range-end range) pos)))
                  (format t "~&~A:~D:~D:~D:~A~A~A~%"
                          file
                          line-number
                          start
                          end
                          (subseq line 0 start)
                          (cl-ansi-text:red (subseq line start end))
                          (subseq line end))
                  (return)))
              (incf pos (1+ (length line))))))

(defun pretty-print-matched (matched)
  matched
  #+(or)
  (maphash (lambda (document-id ranges)
             (let ((document (gethash document-id *document-table*)))
               (dolist (range ranges)
                 (read-file-range (document-pathname document) range))))
           (matched-document-positions-map matched)))

(eval-when ()
  (setq *database* (make-instance 'database))
  (defparameter $ (index-lisp-system :searty))
  (pretty-print-matched (search-phrase $ "defun")))
