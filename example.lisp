(in-package :searty)

(defun test-index (system-name)
  (format t "index: ~D ms~%"
          (measure-time (index-system system-name "/home/user/quicklisp-dist/2022-04-01/" "/tmp/searty.db"))))

(defun test-search (query &rest args &key start-bounding end-bounding)
  (declare (ignore start-bounding end-bounding))
  (with-database (*database* "/tmp/searty.db")
    (format t "search: ~D ms~%"
            (measure-time (pretty-print-matched (apply #'search-phrase query args))))))

(defun test (system-name)
  (test-index system-name)
  (test-search "defun"))
