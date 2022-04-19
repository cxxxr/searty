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

(defun collect-defpackage-forms (text)
  (with-input-from-string (in text)
    (loop :with eof-value := '#:eof
          :for form := (ignore-errors
                         (let ((*read-eval* nil))
                           (read in nil eof-value)))
          :until (eq form eof-value)
          :when (and (consp form)
                     (member (first form) '(cl:defpackage uiop:define-package)))
          :collect form)))

(defstruct definition filename position)

(defun definition-equal (definition1 definition2)
  (and (equal (definition-filename definition1)
              (definition-filename definition2))
       (equal (definition-position definition1)
              (definition-position definition2))))

(defun parse-location (location)
  (destructuring-case location
    ((:location file position hints)
     (declare (ignore hints))
     (destructuring-case file
       ((:file filename)
        (destructuring-case position
          ((:position pos)
           (make-definition :filename filename :position pos))))))))

(defun find-definitions (symbol)
  (let ((definitions '()))
    (loop :for (title location) :in (swank/backend:find-definitions symbol)
          :do (let ((loc (parse-location location)))
                (cond ((null loc)
                       (warn "find-definitions: unexpected location ~S" location))
                      (t
                       (pushnew loc definitions :test #'definition-equal)))))
    definitions))

(defun collect-symbol-definitions (package-name)
  (let ((symbol-definitions-table (make-hash-table)))
    (let ((package (find-package package-name)))
      (do-symbols (symbol package-name)
        (when (eq package (symbol-package symbol))
          (setf (gethash symbol symbol-definitions-table)
                (find-definitions symbol)))))
    symbol-definitions-table))

(defun defpackage-name (form)
  (second form))

(defun index-package (defpackage-forms)
  (dolist (defpackage-form defpackage-forms)
    (let ((package-name (defpackage-name defpackage-form)))
      (do-hash-table (symbol definitions (collect-symbol-definitions package-name))
        (let ((symbol-id (or (resolve-symbol-id *database*
                                                (symbol-name symbol)
                                                (package-name (symbol-package symbol)))
                             (insert-symbol *database* symbol))))
          (dolist (definition definitions)
            (insert-symbol-definition *database*
                                      symbol-id
                                      (definition-filename definition)
                                      (definition-position definition))))))))

(defun index-file (inverted-index file)
  (multiple-value-bind (text external-format) (read-file-into-string* file)
    (let* ((document (create-document file external-format text))
           (tokens (tokenize-lisp text))
           (tokens (trigram-tokens tokens)))
      (dolist (token tokens)
        ;; NOTE: このresolve-token, insert-token内でtoken-idがセットされる
        (unless (resolve-token *database* token)
          (insert-token *database* token))
        (inverted-index-insert inverted-index (document-id document) token))
      (index-package (collect-defpackage-forms text)))))

(defun index-file-with-time (inverted-index file)
  (format t "~&~A " file)
  (let ((ms (measure-time (index-file inverted-index file))))
    (format t "[~D ms]~%" ms)))

(defun index-lisp-files (files)
  (let ((inverted-index (make-inverted-index)))
    (dolist (file files)
      ;; 重複を防ぐために既に登録されているファイルはインデックスしない
      ;; 例:
      ;; 3b-swf-20120107-gitは3b-swf-swc.asdと3b-swf.asdがあるが、
      ;; 3b-swf-swcが3b-swfに依存してるため、3b-swfを二重に見る問題がある
      (unless (resolve-document-id-by-pathname *database* file)
        (index-file-with-time inverted-index file)))
    (flush-inverted-index inverted-index)))

(defclass nop-plan (asdf:sequential-plan) ())

(defmethod asdf:perform-plan ((plan nop-plan) &key))

(defun match-component (system-name component)
  (let ((parent (asdf:component-parent component)))
    (cond ((null parent) nil)
          ((equal system-name (asdf:component-name parent))
           t)
          (t
           (match-component system-name parent)))))

(defun collect-cl-source-files (system)
  (let ((system-name (asdf:component-name (asdf:find-system system))))
    (declare (ignorable system-name))
    (multiple-value-bind (operation plan)
        (asdf:operate 'asdf:compile-op system #|:plan-class 'nop-plan|# :force t)
      (declare (ignore operation))
      (loop :for action :in (asdf/plan:plan-actions plan)
            :for o := (asdf/action:action-operation action)
            :for c := (asdf/action:action-component action)
            :when (and (typep o 'asdf:compile-op)
                       (typep c 'asdf:cl-source-file)
                       (match-component system-name c))
            :collect (truename (first (asdf::input-files o c)))))))

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

(defun index-system (system-name dist-dir database-file)
  (with-database (*database* database-file :initialize t :without-disconnect t)
    (with-asdf (dist-dir)
      (let ((files (collect-cl-source-files system-name)))
        (index-lisp-files files)))))

;;;
(defun collect-index-files (index-directory)
  (let* ((success.txt (merge-pathnames "success.txt" index-directory))
         (system-names (list-to-hash-table (split-sequence #\newline (read-file-into-string success.txt))
                                           :test 'equal)))
    (loop :for file :in (uiop:directory-files index-directory)
          :when (and (equal (pathname-type file) "db")
                     (gethash (pathname-name file) system-names))
          :collect file)))

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

(defun merge-symbol-and-definitions (dst-database database-files)
  (dolist (database-file database-files)
    (with-database (src-database database-file)
      (copy-symbol-table dst-database src-database)
      (copy-symbol-definition-table dst-database src-database))))

(defun merge-index-1 (output-database-file database-files)
  (with-database (dst-database output-database-file :initialize t :without-disconnect t)
    (let ((document-id-per-database-map (merge-document dst-database database-files))
          (token-id-map (merge-token dst-database database-files)))
      (merge-inverted-index dst-database
                            database-files
                            token-id-map
                            document-id-per-database-map)
      (merge-symbol-and-definitions dst-database database-files))))

(defun merge-index (index-directory output-database-file &optional limit)
  (let ((database-files (collect-index-files index-directory)))
    (when limit
      (setq database-files (subseq database-files 0 limit)))
    (merge-index-1 output-database-file database-files)))
