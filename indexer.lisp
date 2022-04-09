(in-package :searty)

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

(defclass nop-plan (asdf:sequential-plan) ())

(defmethod asdf:perform-plan ((plan nop-plan) &key))

(defun collect-cl-source-files (system)
  (let ((system-name (asdf:component-name (asdf:find-system system))))
    (declare (ignorable system-name))
    (multiple-value-bind (operation plan)
        (asdf:operate 'asdf:compile-op system :plan-class 'nop-plan :force t)
      (declare (ignore operation))
      (loop :for action :in (asdf/plan:plan-actions plan)
            :for o := (asdf/action:action-operation action)
            :for c := (asdf/action:action-component action)
            :when (and (typep o 'asdf:compile-op)
                       (typep c 'asdf:cl-source-file)
                       (equal system-name (asdf:component-name (asdf:component-parent c))))
            :collect (first (asdf::input-files o c))))))

(defun index-lisp-system (system)
  (let ((files (collect-cl-source-files system))
        (*database* (make-instance 'sqlite3-database)))
    (index-lisp-files files)))

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

(defun index-system (system-name dist-dir output-dir)
  (let ((dist-dir dist-dir)
        (*index-directory* (format nil "~A/~A/" output-dir system-name))
        (*sqlite3-database-file* (format nil "~A/~A/searty.db" output-dir system-name)))
    (ensure-directories-exist *index-directory*)
    (sqlite3-init-database)
    (with-asdf (dist-dir)
      (index-lisp-system system-name))))
