(in-package :searty)

(defun flush-inverted-index (inverted-index &optional (database *database*))
  (flet ((body ()
           (do-inverted-index (token-id locations inverted-index)
             (when-let ((storage-locations (resolve-locations database token-id)))
               (merge-locations locations storage-locations))
             (upsert-inverted-index database token-id locations))
           (inverted-index-clear inverted-index)))
    (format t "~&index flush: ~A~%" (date))
    (let ((time (measure-time (body))))
      (format t "~&index flushed (~A ms): ~A~%" time (date)))))

(defun create-document (pathname external-format text &optional (*database* *database*))
  (let ((document (make-document :pathname pathname :external-format external-format :body text)))
    (insert-document *database* document)
    (let ((id (resolve-document-id-by-pathname *database* pathname)))
      (setf (document-id document) id))
    document))

(defun read-file-into-string* (file)
  (flet ((try (external-format)
           (ignore-errors
             (list (read-file-into-string file :external-format external-format) external-format))))
    (apply #'values
           (or (try :utf-8)
               (try :cp932)))))

(defun resolve-or-insert-symbol-id (database symbol-name package-name)
  (or (resolve-symbol-id database symbol-name package-name)
      (insert-symbol database symbol-name package-name)))

(defun resolve-or-insert-package-id (database package-name system-id)
  (or (resolve-package-id database package-name)
      (insert-package database package-name system-id)))

(defun flatten-definitions (specifier-and-locations-list)
  (let ((acc '()))
    (dolist (specifier-and-locations specifier-and-locations-list)
      (destructuring-bind (specifier &rest locations) specifier-and-locations
        (loop :for (filename position) :in locations
              :do (push (list specifier filename position) acc))))
    acc))

(defun index-definitions (spec-definitions system-id)
  (loop :for (object . specifier-and-locations-list) :in spec-definitions
        :do (destructuring-bind (&key type name package) object
              (ecase type
                (:symbol
                 (let ((symbol-id (resolve-or-insert-symbol-id *database* name package)))
                   (loop :for (specifier filename position) :in (flatten-definitions specifier-and-locations-list)
                         :do (insert-symbol-definition *database*
                                                       symbol-id
                                                       specifier
                                                       filename
                                                       position))))
                (:package
                 (let ((package-id (resolve-or-insert-package-id *database* name system-id)))
                   (loop :for (specifier filename position) :in (flatten-definitions specifier-and-locations-list)
                         :do (insert-package-definition *database*
                                                        package-id
                                                        specifier
                                                        filename
                                                        position))))))))

(defun index-from-spec (spec)
  (let ((system-id (insert-asd-system *database* spec)))
    (index-definitions (spec-definitions spec) system-id)))

(defun index-file (inverted-index file)
  (multiple-value-bind (text external-format) (read-file-into-string* file)
    (let* ((document (create-document file external-format text))
           (tokens (tokenize-lisp text))
           (tokens (trigram-tokens tokens)))
      (dolist (token tokens)
        ;; NOTE: このresolve-token, insert-token内でtoken-idがセットされる
        (unless (resolve-token *database* token)
          (insert-token *database* token))
        (inverted-index-insert inverted-index (document-id document) token)))))

(defun index-lisp-files (files)
  (let ((inverted-index (make-inverted-index)))
    (dolist (file files)
      (format t "~&~A " file)
      (let ((ms (measure-time (index-file inverted-index file))))
        (format t "[~D ms]~%" ms)))
    (flush-inverted-index inverted-index)))

(defun index-system (filename database-file)
  (with-database (*database* database-file :initialize t :without-disconnect t)
    (let ((spec (load-spec filename)))
      (index-lisp-files (spec-files spec))
      (index-from-spec spec))))

;;;
(defun collect-index-files (index-directory)
  (uiop:directory-files index-directory "*.db"))

(defun merge-document (dst-database database-files)
  (let ((document-id-per-database-map (make-hash-table :test 'equal)))
    (dolist (database-file database-files)
      (let ((document-id-map (make-hash-table :test 'equal)))
        (with-database (src-database database-file)
          (let ((src-documents (resolve-whole-documents src-database)))
            (dolist (document src-documents)
              (insert-document dst-database document))
            (dolist (dst-document (resolve-documents-by-pathnames
                                   dst-database
                                   (mapcar #'document-pathname src-documents)))
              (let ((src-document (find (document-pathname dst-document)
                                        src-documents
                                        :key #'document-pathname
                                        :test #'equal)))
                (setf (gethash (document-id src-document) document-id-map)
                      (document-id dst-document))))))
        (setf (gethash (pathname-name database-file) document-id-per-database-map)
              document-id-map)))
    document-id-per-database-map))

(defun merge-token (dst-database database-files)
  (let ((token-map (make-hash-table :test 'equal))
        (token-id-map (make-hash-table :test 'equal)))
    (dolist (database-file database-files)
      (format t "merge-token: process ~A~%" database-file)
      (with-database (src-database database-file)
        (let ((src-tokens (resolve-whole-tokens src-database)))
          (dolist (token src-tokens)
            (push (token-id token)
                  (gethash (cons (token-term token)
                                 (token-kind token))
                           token-map))))))
    (let ((total (hash-table-count token-map))
          (progress 0))
      (maphash (lambda (key src-ids)
                 (format t "merge-token: ~D/~D~%" (incf progress) total)
                 (destructuring-bind (term . kind) key
                   (let ((dst-token (make-token :term term :kind kind)))
                     (insert-token dst-database dst-token)
                     (dolist (src-id src-ids)
                       (setf (gethash src-id token-id-map)
                             (token-id dst-token))))))
               token-map))
    token-id-map))

(defun replace-id-from-inverted-index (inverted-index token-id-map document-id-map)
  (let ((new-inverted-index (make-inverted-index)))
    (do-inverted-index (token-id locations inverted-index)
      (let ((replaced-token-id (gethash token-id token-id-map)))
        (setf (inverted-index-get new-inverted-index replaced-token-id)
              (mapcar (lambda (location)
                        (make-location :document-id (gethash (location-document-id location) document-id-map)
                                       :positions (location-positions location)))
                      locations))))
    new-inverted-index))

(defun merge-inverted-index (dst-database database-files token-id-map document-id-per-database-map)
  (let ((dst-inverted-index (make-inverted-index)))
    (loop :with total := (length database-files)
          :for database-file :in database-files
          :for database-name := (pathname-name database-file)
          :for n :from 1
          :do (format t "merge-inverted-index: process ~A ~D/~D~%" database-file n total)
              (with-database (src-database database-file)
                (let ((src-inverted-index (resolve-whole-inverted-index src-database)))
                  (inverted-index-merge dst-inverted-index
                                        (replace-id-from-inverted-index
                                         src-inverted-index
                                         token-id-map
                                         (gethash database-name
                                                  document-id-per-database-map))))))
    (flush-inverted-index dst-inverted-index dst-database)))

(defun merge-index-1 (output-database-file database-files)
  (with-database (dst-database output-database-file :initialize t :without-disconnect t)
    (let ((document-id-per-database-map (merge-document dst-database database-files))
          (token-id-map (merge-token dst-database database-files)))
      (merge-inverted-index dst-database
                            database-files
                            token-id-map
                            document-id-per-database-map)
      (dolist (database-file database-files)
        (with-database (src-database database-file)
          (copy-symbol-table dst-database src-database)
          (copy-symbol-definition-table dst-database src-database)
          (copy-packages dst-database src-database)
          (copy-package-definition-table dst-database src-database))))))

(defun merge-index (index-directory output-database-file &optional limit)
  (let ((database-files (collect-index-files index-directory)))
    (when limit
      (setq database-files (subseq database-files 0 limit)))
    (merge-index-1 output-database-file database-files)))
