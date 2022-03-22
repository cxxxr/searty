(in-package :searty)

(defparameter *sqlite3-database-name* "/tmp/searty.sqlite3")

(defparameter *tokenizer* (make-instance 'lisp-tokenizer))

(defparameter *database*
  (make-instance 'database :connection (dbi:connect :sqlite3 :database-name *sqlite3-database-name*)))

(defparameter *indexer* (make-instance 'indexer
                                       :tokenizer *tokenizer*
                                       :database *database*))

(defparameter *searcher* (make-instance 'lisp-searcher
                                        :tokenizer *tokenizer*
                                        :database *database*))

(defun index-lisp-sources (system-designator)
  (dolist (file (find-files (asdf:system-source-directory system-designator) #'lisp-pathname-p))
    (let ((start (get-internal-real-time)))
      (format t "~&~A " file)
      (add-document *indexer* file)
      (format t "[~D ms]~%" (floor (- (get-internal-real-time) start)
                                   1000)))))

(defun search-code (query)
  (execute-search *searcher* (make-instance 'and-matcher :text query)))
