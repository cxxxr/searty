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

(defun flush-inverted-index (inverted-index)
  (do-inverted-index (token-id locations inverted-index)
    (when-let ((storage-locations (resolve-locations *database* token-id)))
      (merge-locations locations storage-locations))
    (upsert-inverted-index *database* token-id locations))
  (inverted-index-clear inverted-index))

(defun create-document (pathname body)
  (let ((document (make-document :pathname pathname :body body)))
    (insert-document *database* document)
    (let ((id (resolve-document-id-by-pathname *database* pathname)))
      (setf (document-id document) id))
    document))

(defun read-file-into-string* (file)
  (or (ignore-errors (read-file-into-string file))
      (read-file-into-string file :external-format :cp932)))

(defun add-file (inverted-index file)
  (let* ((text (read-file-into-string* file))
         (document (create-document file text))
         (tokens (tokenize-file text)))
    (dolist (token tokens)
      ;; NOTE: このresolve-token, insert-token内でtoken-idがセットされる
      (unless (resolve-token *database* token)
        (insert-token *database* token))
      (inverted-index-insert inverted-index (document-id document) token))))

(defun add-file-with-time (inverted-index file)
  (format t "~&~A " file)
  (let ((ms (measure-time (add-file inverted-index file))))
    (format t "[~D ms]~%" ms)))

(defun date ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format nil "[~A/~A/~A ~2,'0D:~2,'0D:~2,'0D]" year month date hour minute second)))

(defun flush-inverted-index-with-time (inverted-index)
  (format t "~&index flush: ~A~%" (date))
  (let ((time (measure-time (flush-inverted-index inverted-index))))
    (format t "~&index flushed (~A ms): ~A~%" time (date))))

(defun index-lisp-files (files)
  (let ((inverted-index (make-inverted-index)))
    (dbi:with-transaction (database-connection *database*)
      (dolist (file files)
        ;; 重複を防ぐために既に登録されているファイルはインデックスしない
        ;; 例:
        ;; 3b-swf-20120107-gitは3b-swf-swc.asdと3b-swf.asdがあるが、
        ;; 3b-swf-swcが3b-swfに依存してるため、3b-swfを二重に見る問題がある
        (unless (resolve-document-id-by-pathname *database* file)
          (add-file-with-time inverted-index file)))
      (flush-inverted-index-with-time inverted-index))))

(defun set-compile-file (function)
  (sb-ext:without-package-locks
    (setf (fdefinition 'compile-file) function)))

(defun compile-system-and-collect-input-files (system)
  (let ((input-files '()))
    (let ((compile-file-function #'compile-file))
      (sb-ext:without-package-locks
        (unwind-protect
             (progn
               (set-compile-file
                (lambda (input-file &rest args)
                  (push input-file input-files)
                  (apply compile-file-function input-file args)))
               (asdf:compile-system system :force t))
          (set-compile-file compile-file-function))))
    (nreverse input-files)))

(defclass nop-plan (asdf:sequential-plan) ())

(defmethod asdf:perform-plan ((plan nop-plan) &key))

(defun collect-cl-source-files (system base-directory)
  (multiple-value-bind (operation plan)
      (asdf:operate 'asdf:compile-op system :plan-class 'nop-plan :force t)
    (declare (ignore operation))
    (loop :for action :in (asdf/plan:plan-actions plan)
          :for o := (asdf/action:action-operation action)
          :for c := (asdf/action:action-component action)
          :when (and (typep o 'asdf:compile-op)
                     (typep c 'asdf:cl-source-file))
          :append (let ((file (first (asdf::input-files o c))))
                    (when (uiop:subpathp file base-directory)
                      (list file))))))

(defun index-lisp-system (system base-directory)
  (let ((files (collect-cl-source-files system base-directory))
        (*database* (make-instance 'sqlite3-database)))
    (index-lisp-files files)))

(defun init-index ()
  (let ((index-dir (namestring (asdf:system-relative-pathname :searty "index/"))))
    (uiop:run-program (list "rm" "-rf" index-dir))
    (uiop:run-program (list "mkdir" index-dir))
    (uiop:run-program (list "sqlite3" "-init" *sqlite3-schema-file* *sqlite3-database-file*))))

(defun call-with-asdf (root-directory function)
  (let (#+(or)
        (asdf:*system-definition-search-functions*
          '(asdf/system-registry:sysdef-source-registry-search))
        (asdf/source-registry:*source-registry* nil))
    (asdf::initialize-source-registry
     `(:source-registry
       (:tree ,root-directory)
       #+sbcl (:directory (:home ".sbcl/systems/"))
       :ignore-inherited-configuration))
    (funcall function)))

(defmacro with-asdf ((root-directory) &body body)
  `(call-with-asdf ,root-directory (lambda () ,@body)))

(defun collect-asd-files (directory)
  (let ((asd-files '()))
    (asdf::register-asd-directory directory
                                  :recurse t
                                  :collect (lambda (asd-file) (push asd-file asd-files)))
    asd-files))

(defun index-quicklisp-repository (directory)
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (root-directory (uiop:pathname-parent-directory-pathname directory)))
    (with-asdf (root-directory)
      (dolist (asd-file (collect-asd-files directory))
        (index-lisp-system (pathname-name asd-file) directory)))))

(defun index-quicklisp-releases (root-directory)
  (init-index)
  (let ((root-directory (uiop:ensure-directory-pathname root-directory)))
    (with-asdf (root-directory)
      (dolist (dir (uiop:subdirectories root-directory))
        (dolist (asd-file (collect-asd-files dir))
          (format t "~&--- ~A~%" asd-file)
          (index-lisp-system (pathname-name asd-file) dir))))))

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
  (let ((documents
          (let ((ids (hash-table-keys (matched-document-positions-map matched))))
            (resolve-documents-by-ids *database* ids))))
    (maphash (lambda (document-id ranges)
               (let ((document (find document-id documents :key #'document-id :test #'document-id=)))
                 (dolist (range ranges)
                   (read-file-range (document-pathname document) range))))
             (matched-document-positions-map matched))))

;; for debug
(defun dump-inverted-index (inverted-index)
  (let ((table (make-hash-table :test 'equal))
        (tokens (resolve-tokens-by-ids *database* (inverted-index-token-ids inverted-index)))
        (documents (resolve-documents-by-ids *database* (collect-all-document-ids inverted-index))))
    (inverted-index-foreach inverted-index
                            (lambda (token-id locations)
                              (let ((token (find token-id tokens :test #'uuid= :key #'token-id)))
                                (setf (gethash (token-term token) table)
                                      (mapcar (lambda (loc)
                                                (let ((document (find (location-document-id loc)
                                                                      documents
                                                                      :test #'document-id=
                                                                      :key #'document-id)))
                                                  (cons document
                                                        (location-positions loc))))
                                              locations)))))
    (hash-table-alist table)))
