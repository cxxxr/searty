(in-package :searty)

(defun merge-positions (positions1 positions2)
  (delete-duplicates (merge 'list positions1 positions2 #'<)))

(defstruct doc-location
  document-id
  positions)

(defun doc-location-equal (doc-location-1 doc-location-2)
  (and (equal (doc-location-document-id doc-location-1)
              (doc-location-document-id doc-location-2))
       (equal (doc-location-positions doc-location-1)
              (doc-location-positions doc-location-2))))

(defun doc-locations-equal (doc-locations-1 doc-locations-2)
  (set-equal doc-locations-1 doc-locations-2 :test #'doc-location-equal))

(defun insert-sort-doc-location (doc-location doc-locations)
  (merge 'list
         (list doc-location)
         doc-locations
         #'string<
         :key #'doc-location-document-id))

(defun merge-doc-location (doc-locations document-id pos)
  (dolist (loc doc-locations)
    (when (equal document-id (doc-location-document-id loc))
      (setf (doc-location-positions loc)
            (merge-positions (list pos) (doc-location-positions loc)))
      (return-from merge-doc-location doc-locations)))
  (let ((merged-doc-locations
          (insert-sort-doc-location (make-doc-location :document-id document-id
                                                       :positions (list pos))
                                    doc-locations)))
    merged-doc-locations))

(defun merge-doc-locations (destination-doc-locations source-doc-locations)
  (dolist (source-doc-location source-doc-locations)
    (if-let ((destination-doc-location
              (find (doc-location-document-id source-doc-location)
                    destination-doc-locations
                    :test #'equal
                    :key #'doc-location-document-id)))
      (setf (doc-location-positions destination-doc-location)
            (merge-positions (doc-location-positions destination-doc-location)
                             (doc-location-positions source-doc-location)))
      (setf destination-doc-locations
            (insert-sort-doc-location source-doc-location
                                      destination-doc-locations))))
  destination-doc-locations)

(defstruct inverted-index
  (map (make-hash-table :test 'equal)))

(defun dump-inverted-index (inverted-index)
  (let ((table '()))
    (maphash (lambda (token locs)
               (push (cons token
                           (loop :for loc :in locs
                                 :collect (cons (doc-location-document-id loc)
                                                (doc-location-positions loc))))
                     table))
             (inverted-index-map inverted-index))
    table))

(defun clear-inverted-index (inverted-index)
  (clrhash (inverted-index-map inverted-index)))

(defun inverted-index-equal (inverted-index-1 inverted-index-2)
  (let ((map-1 (inverted-index-map inverted-index-1))
        (map-2 (inverted-index-map inverted-index-2)))
    (and (set-equal (hash-table-keys map-1)
                    (hash-table-keys map-2)
                    :test #'equal)
         (set-equal (hash-table-values map-1)
                    (hash-table-values map-2)
                    :test #'doc-locations-equal))))

(defun insert-doc-location (inverted-index token-id document-id pos)
  (let* ((map (inverted-index-map inverted-index))
         (doc-locations (gethash token-id map)))
    (setf (gethash token-id map)
          (merge-doc-location doc-locations document-id pos)))
  (values))

(defun get-doc-locations (inverted-index token-id)
  (gethash token-id (inverted-index-map inverted-index)))

(defun (setf get-doc-locations) (doc-locations inverted-index token-id)
  (setf (gethash token-id (inverted-index-map inverted-index))
        doc-locations))

(defmacro do-inverted-index (((token-id doc-locations) inverted-index) &body body)
  `(maphash (lambda (,token-id ,doc-locations)
              ,@body)
            (inverted-index-map ,inverted-index)))

(defun inverted-index-tokens (inverted-index)
  (hash-table-keys (inverted-index-map inverted-index)))

(defun collect-inverted-index-values (inverted-index make-function)
  (let ((values '()))
    (do-inverted-index ((token-id locs) inverted-index)
      (declare (ignore token-id))
      (push (funcall make-function locs) values))
    values))

(defun merge-inverted-index (destination source)
  (do-inverted-index ((token-id doc-locations) source)
    (setf (get-doc-locations destination token-id)
          (merge-doc-locations (get-doc-locations destination token-id)
                               doc-locations)))
  destination)

;;; encode/decode
(defun encode-positive-integer (v stream)
  (let ((bytes '()))
    (push (logand v #x7f) bytes)
    (loop
      (setf v (ash v -7))
      (unless (plusp v) (return))
      (push (+ #x80 (logand v #x7f)) bytes))
    (write-sequence bytes stream)))

(defun encode-positive-integer-list (integers stream)
  (encode-positive-integer (length integers) stream)
  (dolist (integer integers)
    (encode-positive-integer integer stream)))

(defun encode-uuid (uuid stream)
  (assert (and (stringp uuid) (= 36 (length uuid))))
  (loop :for c :across uuid
        :unless (char= c #\-)
        :do (write-byte (char-code c) stream)))

(defun encode-doc-location (doc-location stream)
  (encode-uuid (doc-location-document-id doc-location) stream)
  (encode-positive-integer-list (doc-location-positions doc-location) stream))

(defun encode-doc-locations (doc-locations stream)
  (encode-positive-integer (length doc-locations) stream)
  (dolist (loc doc-locations)
    (encode-doc-location loc stream)))

(defun encode-doc-locations-to-vector (doc-locations)
  (with-open-stream (stream (flex:make-in-memory-output-stream))
    (encode-doc-locations doc-locations stream)
    (flex:get-output-stream-sequence stream)))

(defun decode-positive-integer (stream)
  (let ((v 0))
    (loop :for byte := (read-byte stream)
          :for continue := (logbitp 7 byte)
          :do (setf v
                    (logior (ash v 7)
                            (if continue
                                (logxor byte 128)
                                byte)))
          :while continue)
    v))

(defun decode-positive-integer-list (stream)
  (let ((length (decode-positive-integer stream)))
    (loop :repeat length
          :collect (decode-positive-integer stream))))

(defun decode-uuid (stream)
  (let ((uuid (make-array 36 :element-type '(unsigned-byte 8) :initial-element (char-code #\-))))
    (read-sequence uuid stream :start 0 :end 8)
    (read-sequence uuid stream :start 9 :end 13)
    (read-sequence uuid stream :start 14 :end 18)
    (read-sequence uuid stream :start 19 :end 23)
    (read-sequence uuid stream :start 24)
    (map 'string #'code-char uuid)))

(defun decode-doc-location (stream)
  (let ((document-id (decode-uuid stream))
        (positions (decode-positive-integer-list stream)))
    (make-doc-location :document-id document-id
                       :positions positions)))

(defun decode-doc-locations (stream)
  (let ((length (decode-positive-integer stream)))
    (loop :repeat length
          :collect (decode-doc-location stream))))

(defun decode-doc-locations-from-vector (bytes)
  (with-open-stream (stream (flex:make-in-memory-input-stream bytes))
    (decode-doc-locations stream)))
