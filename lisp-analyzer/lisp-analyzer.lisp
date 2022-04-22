(ql:quickload :swank :silent t)

(defpackage :searty/lisp-analyzer
  (:use :cl))
(in-package :searty/lisp-analyzer)

(defun call-with-asdf (root-directory function)
  (let ((asdf/source-registry:*source-registry* nil))
    (asdf::initialize-source-registry
     `(:source-registry
       (:tree ,root-directory)
       #+sbcl (:directory (:home ".sbcl/systems/"))
       :ignore-inherited-configuration))
    (funcall function)))

(defmacro with-asdf ((root-directory) &body body)
  `(call-with-asdf ,root-directory (lambda () ,@body)))

(defmacro with-muffle-output (() &body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
     ,@body))

(defun match-component (system-name component)
  (let ((parent (asdf:component-parent component)))
    (when parent
      (or (equal system-name (asdf:component-name parent))
          (match-component system-name parent)))))

(defun collect-cl-source-files (system)
  (let ((system-name (asdf:component-name (asdf:find-system system))))
    (multiple-value-bind (operation plan)
        (with-muffle-output ()
          (asdf:operate 'asdf:compile-op system :force t))
      (declare (ignore operation))
      (loop :for action :in (asdf/plan:plan-actions plan)
            :for o := (asdf/action:action-operation action)
            :for c := (asdf/action:action-component action)
            :when (and (typep o 'asdf:compile-op)
                       (typep c 'asdf:cl-source-file)
                       (match-component system-name c))
            :collect (truename (first (asdf::input-files o c)))))))

(defun load-files (files)
  (with-muffle-output ()
    (map nil #'load files)))

(defun collect-defpackage-forms (file)
  (with-open-file (in file)
    (loop :with eof-value := '#:eof
          :for form := (ignore-errors
                         (let ((*read-eval* nil))
                           (read in nil eof-value)))
          :until (eq form eof-value)
          :when (and (consp form)
                     (member (first form) '(cl:defpackage uiop:define-package)))
          :collect form)))

(defun defpackage-name (form)
  (second form))

(defun parse-location (location)
  (when (and (consp location)
             (eq :location (first location)))
    (let ((file (getf (second location) :file))
          (position (getf (third location) :position)))
      (when (and file position)
        (list file position)))))

(defun coerce-to-symbol (x)
  (etypecase x
    (string (make-symbol x))
    (symbol x)))

(defun find-definitions (symbol)
  (loop :for ((specifier name) location) :in (swank/backend:find-definitions (coerce-to-symbol symbol))
        :for loc := (parse-location location)
        :when loc
        :collect (list (string specifier) loc)))

(defun encode-symbol (symbol)
  (list :type :symbol :name (symbol-name symbol) :package (package-name (symbol-package symbol))))

(defun encode-package (package)
  (list :type :package :name (package-name package)))

(defun collect-definitions (package-name)
  (let ((symbol-definitions-table (make-hash-table :test 'equal))
        (package (find-package package-name)))
    (do-symbols (symbol package-name)
      (when (eq package (symbol-package symbol))
        (setf (gethash symbol symbol-definitions-table)
              (find-definitions symbol))))
    (cons (cons (encode-package package)
                (find-definitions package-name))
          (loop :for symbol :being :each :hash-key :of symbol-definitions-table
                :using (:hash-value definitions)
                :collect (cons (encode-symbol symbol) definitions)))))

(defun main (system-name root-directory output-file)
  (with-asdf (root-directory)
    (let ((files (collect-cl-source-files system-name))
          (definitions '()))
      (load-files files)
      (dolist (file files)
        (dolist (defpackage-form (collect-defpackage-forms file))
          (let ((package-name (defpackage-name defpackage-form)))
            (setf definitions (nconc (collect-definitions package-name) definitions)))))
      (with-open-file (out output-file
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (pprint `(:files ,files :definitions ,definitions) out)
        (terpri out)))))

;; TODO:
;; - find-references
