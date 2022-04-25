(in-package :searty)

(defstruct spec
  system-name
  asd-file
  files
  definitions
  time)

(defun load-spec (filename)
  (let ((initargs (uiop:read-file-form filename)))
    (apply #'make-spec initargs)))
