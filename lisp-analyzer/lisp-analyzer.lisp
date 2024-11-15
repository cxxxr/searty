(ql:quickload :swank :silent t)

(defpackage :searty/lisp-analyzer
  (:use :cl))
(in-package :searty/lisp-analyzer)

;;;
(defstruct (system (:type list)) project system-file system-name)
(defstruct (release (:type list)) project url size file-md5 content-sha1 prefix system-files)

(defun load-systems (system-file)
  (with-open-file (in system-file)
    (read-line in nil)
    (let ((system-map (make-hash-table :test 'equal)))
      (loop :for line := (read-line in nil)
            :while line
            :do (let ((system (uiop:split-string line)))
                  (push system (gethash (system-project system) system-map))))
      system-map)))

(defun load-releases (releases-file)
  (with-open-file (in releases-file)
    (read-line in nil)
    (let ((releases-map (make-hash-table :test 'equal)))
      (loop :for line := (read-line in nil)
            :while line
            :do (destructuring-bind (project url size file-md5 content-sha1 prefix &rest system-files)
                    (uiop:split-string line)
                  (let ((release (list project url size file-md5 content-sha1 prefix system-files)))
                    (setf (gethash project releases-map) release))))
      releases-map)))

(defun make-system-map (dist-directory)
  (let* ((quicklisp-directory (uiop:pathname-parent-directory-pathname dist-directory))
         (systems-map (load-systems (merge-pathnames "systems.txt" quicklisp-directory)))
         (releases-map (load-releases (merge-pathnames "releases.txt" quicklisp-directory)))
         (system-name-file-map (make-hash-table :test 'equal)))
    (maphash (lambda (project release)
               (let ((systems (gethash project systems-map))
                     (project-directory
                       (uiop:ensure-directory-pathname
                        (merge-pathnames (release-prefix release) dist-directory))))
                 (loop :for system :in systems
                       :for asd-file := (find (system-system-file system)
                                              (release-system-files release)
                                              :key #'pathname-name
                                              :test #'equal)
                       :do (setf (gethash (system-system-name system) system-name-file-map)
                                 (merge-pathnames asd-file project-directory)))))
             releases-map)
    system-name-file-map))

(defun find-asd-file (dist-directory system-name)
  (gethash system-name (make-system-map dist-directory)))

;;;
(defstruct symbol-identifier (type "symbol") name package)
(defstruct package-identifier (type "package") name)
(defstruct location specifier file position)
(defstruct definition identifier locations)
(defstruct json
  system-name
  asd-file
  root-directory
  files
  definitions
  time)

(defun class-instance-to-alist (instance)
  (loop :for slot-definition :in (sb-mop:class-direct-slots (class-of instance))
        :for slot-name := (sb-mop:slot-definition-name slot-definition)
        :for slot-value := (slot-value instance slot-name)
        :collect (cons slot-name slot-value)))

(defun snake-case (symbol)
  (substitute #\_ #\- (string-downcase symbol)))

(defmethod encode-json ((instance structure-object) stream)
  (write-char #\{ stream)
  (loop :for (slot-name . slot-value) :in (class-instance-to-alist instance)
        :for firstp := t :then nil
        :do (unless firstp (write-string ", " stream))
            (format stream "\"~A\": " (snake-case slot-name))
            (encode-json slot-value stream))
  (write-char #\} stream))

(defmethod encode-json ((list list) stream)
  (write-char #\[ stream)
  (loop :for item :in list
        :for firstp := t :then nil
        :do (unless firstp (write-string ", " stream))
            (encode-json item stream))
  (write-char #\] stream))

(defparameter *char-replacements*
  '(#\\ "\\\\"
    #\" "\\\""
    #\Backspace "\\b"
    #\Page "\\f"
    #\Newline "\\n"
    #\Return "\\r"
    #\Tab "\\t"))

(defun write-surrogate-pair-escape (code stream)
  (let ((upper (+ (ldb (byte 10 10) (- code #x10000))
                  #xD800))
        (lower (+ (ldb (byte 10 0) (- code #x10000))
                  #xDC00)))
    (format stream "\\u~4,'0X\\u~4,'0X" upper lower)))

(defmethod encode-json ((string string) stream)
  (write-char #\" stream)
  (loop :for char :across string
        :for replacement := (getf *char-replacements* char)
        :do (cond (replacement (write-string replacement stream))
                  ((<= #x0000 (char-code char) #x001F)
                   (format stream "\\u~4,'0X" (char-code char)))
                  ;; Non-BMP characters must be escaped as a UTF-16 surrogate pair.
                  ((<= #x010000 (char-code char) #x10FFFF)
                   (write-surrogate-pair-escape (char-code char) stream))
                  (t (write-char char stream))))
  (write-char #\" stream)
  (values))

(defmethod encode-json ((pathname pathname) stream)
  (encode-json (namestring pathname) stream))

(defmethod encode-json ((value float) stream)
  (let ((*read-default-float-format* 'double-float))
    (format stream "~F" (coerce value 'double-float))))

(defmethod encode-json ((value integer) stream)
  (princ value stream))

;;;
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

(defun parse-location (specifier location)
  (when (and (consp location)
             (eq :location (first location)))
    (let ((file (getf (second location) :file))
          (position (getf (third location) :position)))
      (when (and file position)
        (make-location :specifier specifier :file file :position position)))))

(defun coerce-to-symbol (x)
  (etypecase x
    (string (make-symbol x))
    (symbol x)))

(defun find-locations (symbol)
  (loop :for ((specifier name) location) :in (swank/backend:find-definitions (coerce-to-symbol symbol))
        :for loc := (parse-location (string specifier) location)
        :when loc
        :collect :it))

(defun encode-symbol (symbol)
  (make-symbol-identifier :name (symbol-name symbol) :package (package-name (symbol-package symbol))))

(defun encode-package (package)
  (make-package-identifier :name (package-name package)))

(defun collect-definitions (package-name definitions)
  (let ((package (find-package package-name)))
    (do-symbols (symbol package-name)
      (when (eq package (symbol-package symbol))
        (push (make-definition :identifier (encode-symbol symbol)
                               :locations (find-locations symbol))
              definitions)))
    (push (make-definition :identifier (encode-package package)
                           :locations (find-locations package-name))
          definitions)))

(defun project-root-directory (root-directory system-directory)
  (labels ((rec (current)
             (let ((parent (uiop:pathname-parent-directory-pathname current)))
               (cond ((uiop:pathname-equal parent current)
                      nil)
                     ((uiop:pathname-equal parent root-directory)
                      current)
                     (t
                      (rec parent))))))
    (rec system-directory)))

(defun main (system-name root-directory output-file)
  (let ((start-time (get-internal-real-time)))
    (with-asdf (root-directory)
      (let* ((asd-file (find-asd-file root-directory system-name))
             (system-source-directory (uiop:pathname-directory-pathname asd-file))
             (project-dir (project-root-directory root-directory system-source-directory)))
        (assert project-dir)
        (asdf:load-asd asd-file)
        (let ((files (collect-cl-source-files system-name))
              (definitions '()))
          (unless (equal system-name "lime-test")
            (load-files files))
          (dolist (file files)
            (dolist (defpackage-form (collect-defpackage-forms file))
              (let ((package-name (defpackage-name defpackage-form)))
                (setf definitions (collect-definitions package-name definitions)))))
          (with-open-file (out output-file
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (encode-json (make-json
                          :system-name system-name
                          :asd-file asd-file
                          :root-directory (princ-to-string project-dir)
                          :files files
                          :definitions definitions
                          :time (float (/ (- (get-internal-real-time) start-time)
                                          internal-time-units-per-second)))
                         out))))))
  0)

;; TODO:
;; - find-references
