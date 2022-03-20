(in-package :searty)

(defvar $analyzer)
(defvar $database)
(defvar $indexer)
(defvar $searcher)

(defun init ()
  (setf $analyzer (make-instance 'simple-analyzer))
  (setf $database (make-instance 'database
                                 :connection (dbi:connect :sqlite3 :database-name "/tmp/searty.sqlite3")))
  (setf $indexer (make-instance 'indexer
                                :analyzer $analyzer
                                :database $database))
  (setf $searcher (make-instance 'searcher
                                 :analyzer $analyzer
                                 :database $database)))

(defun index ()
  (dolist (file (find-files "/home/user/common-lisp/lem/" #'lisp-pathname-p))
    (let ((start (get-internal-real-time)))
      (format t "~&~A " file)
      (add-document $indexer file)
      (format t "[~D ms]~%" (floor (- (get-internal-real-time) start)
                                   1000)))))
