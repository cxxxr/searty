(in-package :searty)

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

(defstruct (posting (:constructor %make-posting (token locations))) token locations)

(defun make-posting (inverted-index token)
  (let ((locations (inverted-index-get inverted-index (token-id token))))
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
          :do (when (document-id< (posting-document-id posting)
                                  (posting-document-id min-posting))
                (setf min-posting posting)))
    (posting-next min-posting)))

(defun same-document-p (postings)
  (let ((first-document-id (posting-document-id (aref postings 0))))
    (loop :for i :from 1 :below (length postings)
          :for posting := (aref postings i)
          :always (document-id= first-document-id
                                (posting-document-id posting)))))

(defun search-and (query)
  (let ((tokens (mapcar (curry #'resolve-token *database*)
                        (mapcan #'tokenize-trigram (tokenize query)))))
    (unless (some #'null tokens)
      (let* ((inverted-index (resolve-inverted-index-by-token-ids *database*
                                                                  (mapcar #'token-id tokens)))
             (postings (make-postings inverted-index tokens))
             (document-ids '()))
        (loop :until (some #'posting-null-p postings)
              :do (cond ((same-document-p postings)
                         (push (posting-document-id (aref postings 0)) document-ids)
                         (postings-next postings))
                        (t
                         (next-minimum-posting postings))))
        (resolve-documents-by-ids *database* document-ids)))))

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

(defun search-phrase (query &key start-bounding end-bounding)
  (let ((matched (make-matched))
        (tokens (mapcar (curry #'resolve-token *database*)
                        (mapcan (lambda (token)
                                  (tokenize-trigram token
                                                    :start-bounding start-bounding
                                                    :end-bounding end-bounding))
                                (tokenize query)))))
    (unless (some #'null tokens)
      (let* ((inverted-index (resolve-inverted-index-by-token-ids
                              *database*
                              (mapcar #'token-id tokens)))
             (postings (make-postings inverted-index tokens)))
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
                         (next-minimum-posting postings))))))
    (normalize-matched matched
                       :start-bounding start-bounding
                       :end-bounding end-bounding)))

(defun read-file-range (file external-format range)
  (with-open-file (in file :external-format external-format)
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
  (let ((documents
          (let ((ids (hash-table-keys (matched-document-positions-map matched))))
            (resolve-documents-by-ids *database* ids)))
        (errors '()))
    (maphash (lambda (document-id ranges)
               (let ((document (find document-id documents :key #'document-id :test #'document-id=)))
                 (dolist (range ranges)
                   (handler-case
                       (read-file-range (document-pathname document)
                                        (make-keyword (string-upcase (document-external-format document)))
                                        range)
                     (error (e)
                       (push e errors))))))
             (matched-document-positions-map matched))
    errors))
