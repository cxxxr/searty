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

(defun normalize-matched (matched &key start-boundary end-boundary)
  (let ((ht (matched-document-positions-map matched)))
    (maphash (lambda (document ranges)
               (setf ranges (sort ranges #'< :key #'range-start))
               (setf (gethash document ht)
                     (normalize-ranges ranges)))
             ht)
    (maphash (lambda (document ranges)
               (when (or start-boundary end-boundary)
                 (setf (gethash document ht)
                       (mapcar (lambda (range)
                                 (make-range (if start-boundary
                                                 (1+ (range-start range))
                                                 (range-start range))
                                             (if end-boundary
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
                        (mapcan #'tokenize-trigram (tokenize-lisp query)))))
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

(defun search-phrase (query &key start-boundary end-boundary)
  (let ((matched (make-matched))
        (tokens (mapcar (curry #'resolve-token *database*)
                        (mapcan (lambda (token)
                                  (tokenize-trigram token
                                                    :start-boundary start-boundary
                                                    :end-boundary end-boundary))
                                (tokenize-lisp query)))))
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
                       :start-boundary start-boundary
                       :end-boundary end-boundary)))

(defun print-highlgiht-line (document line-number start end line)
  (format t "~&~A:~D:~D:~D:~A~A~A~%"
          (document-pathname document)
          line-number
          start
          end
          (subseq line 0 start)
          (cl-ansi-text:red (subseq line start end))
          (subseq line end)))

(defun print-matched-line (document line-number start end line)
  (declare (ignore start end))
  (format t "~&~A:~D:~D"
          (document-pathname document)
          line-number
          line))

(defun read-file-range (document range &key (printer #'print-highlgiht-line))
  (with-input-from-string (in (document-body document))
    (loop :with pos := 0
          :for line := (read-line in)
          :for length := (babel:string-size-in-octets line)
          :for line-number :from 1
          :do (when (<= pos (range-start range) (+ pos length))
                (let ((start (- (range-start range) pos))
                      (end (- (range-end range) pos)))
                  (funcall printer document line-number start end line)
                  (return)))
              (incf pos (1+ length)))))

(defun pretty-print-matched (matched)
  (let ((documents
          (let ((ids (hash-table-keys (matched-document-positions-map matched))))
            (resolve-documents-by-ids *database* ids)))
        (errors '()))
    (maphash (lambda (document-id ranges)
               (let ((document (find document-id documents :key #'document-id :test #'document-id=)))
                 (dolist (range ranges)
                   (handler-case
                       (read-file-range document range)
                     (error (e)
                       (push e errors))))))
             (matched-document-positions-map matched))
    errors))

(defun find-line-start-position (text pos)
  (loop :for i :downfrom (1- pos) :to 0
        :for c := (char text i)
        :until (char= c #\newline)
        :finally (return (if (minusp i) 0 (1+ i)))))

(defun get-matched-line (text position)
  (let* ((position (babel:string-size-in-octets text :end position))
         (start-line-pos (find-line-start-position text position))
         (end-line-pos (position #\newline text :start position)))
    (let ((start
            (babel:string-size-in-octets text :end start-line-pos))
          (end
            (when end-line-pos
              (babel:string-size-in-octets text :end end-line-pos))))
      (subseq text start end))))

(defun print-definitions (definitions)
  (loop :for definition :in definitions
        :for filename := (definition-filename definition)
        :for position := (definition-position definition)
        :for document := (first (resolve-documents-by-pathnames *database* (list filename)))
        :do (cond ((null document)
                   (warn "~A is not exist" filename))
                  (t
                   (format t "~A:~A~%"
                           filename
                           (get-matched-line (document-body document) position))))))

(defun search-symbol-definitions (symbol-name &optional package-name)
  (loop :for symbol-id :in (if package-name
                               (list (resolve-symbol-id *database* symbol-name package-name))
                               (resolve-symbol-ids-by-symbol-name *database* symbol-name))
        :append (resolve-symbol-definitions *database* symbol-id)))

(defun search-package-definitions (package-name)
  (when-let ((package-id (resolve-package-id *database* package-name)))
    (resolve-package-definitions *database* package-id)))
