(in-package :searty)

(defparameter *sqlite3-database-name* "/tmp/searty.sqlite3")

(defvar $analyzer)
(defvar $database)
(defvar $indexer)
(defvar $searcher)

(defun init ()
  (setf $analyzer (make-instance 'simple-analyzer))
  (setf $database (make-instance 'database
                                 :connection (dbi:connect :sqlite3 :database-name *sqlite3-database-name*)))
  (setf $indexer (make-instance 'indexer
                                :analyzer $analyzer
                                :database $database))
  (setf $searcher (make-instance 'searcher
                                 :analyzer $analyzer
                                 :database $database)))

(defun index ()
  (dolist (file (find-files (asdf:system-source-directory :lem) #'lisp-pathname-p))
    (let ((start (get-internal-real-time)))
      (format t "~&~A " file)
      (add-document $indexer file)
      (format t "[~D ms]~%" (floor (- (get-internal-real-time) start)
                                   1000)))))

(defun search-test (text)
  (execute-search $searcher (make-instance 'and-matcher :text text)))
