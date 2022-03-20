(in-package :searty)

(defparameter *sqlite3-database-name* "/tmp/searty.sqlite3")

(defparameter *analyzer* (make-instance 'simple-analyzer))
(defparameter *database* (make-instance 'database
                                        :connection (dbi:connect :sqlite3
                                                                 :database-name *sqlite3-database-name*)))
(defparameter *indexer* (make-instance 'indexer
                                       :analyzer *analyzer*
                                       :database *database*))

(defparameter *searcher* (make-instance 'searcher
                                        :analyzer *analyzer*
                                        :database *database*))

(defun index-lisp-sources (system-designator)
  (dolist (file (find-files (asdf:system-source-directory system-designator) #'lisp-pathname-p))
    (let ((start (get-internal-real-time)))
      (format t "~&~A " file)
      (add-document *indexer* file)
      (format t "[~D ms]~%" (floor (- (get-internal-real-time) start)
                                   1000)))))

(defun search-code (text)
  (execute-search *searcher* (make-instance 'and-matcher :text text)))
