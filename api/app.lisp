(defpackage :searty/web
  (:use :cl))
(in-package :searty/web)

(defvar *app* (make-instance 'ningle:app))
(defvar *handler*)

(defun render-json (definitions)
  (st-json:write-json-to-string
   (loop :for definition :in definitions
         :collect (st-json:jso "filename" (searty::definition-filename definition)
                               "position" (searty::definition-position definition)))))

(defun search-query (params)
  (let ((query (cdr (assoc "query" params :test #'string=))))
    (unless query
      (setf (lack.response:response-status ningle:*response*) 400)
      (return-from search-query))
    (render-json (searty:search-definitions query))))

(defun start ()
  (setq *handler* (clack:clackup *app*)))

(defun stop ()
  (clack:stop *handler*))

(setf (ningle:route *app* "/search") 'search-query)
