(in-package :searty2)

(defstruct inverted-value kind locations)
(defstruct location document-id positions)

(defstruct inverted-index
  (table (make-hash-table :test 'equal)))

(defun token-hashkey (token)
  (list (token-kind token) (token-term token)))

(defun inverted-index-get (inverted-index token)
  (gethash (token-hashkey token) (inverted-index-table inverted-index)))

(defun (setf inverted-index-get) (value inverted-index token)
  (setf (gethash (token-hashkey token) (inverted-index-table inverted-index))
        value))

(defun inverted-index-insert (inverted-index document-id token)
  (let ((locations (inverted-index-get inverted-index token)))
    (let ((loc (find document-id
                     locations
                     :key #'location-document-id
                     :test #'id=)))
      (if (null loc)
          (setf (inverted-index-get inverted-index token)
                (insert-sort (make-location :document-id document-id
                                            :positions (list (token-position token)))
                             locations
                             #'id<
                             :key #'location-document-id))
          (setf (location-positions loc)
                (insert-sort (token-position token) (location-positions loc) #'<))))))

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

(defun encode-location (location stream)
  (encode-uuid (location-document-id location) stream)
  (encode-positive-integer-list (location-positions location) stream))

(defun encode-locations (locations stream)
  (encode-positive-integer (length locations) stream)
  (dolist (loc locations)
    (encode-location loc stream)))

(defun encode-locations-to-vector (locations)
  (with-open-stream (stream (flex:make-in-memory-output-stream))
    (encode-locations locations stream)
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

(defun decode-location (stream)
  (let ((document-id (decode-uuid stream))
        (positions (decode-positive-integer-list stream)))
    (make-location :document-id document-id
                   :positions positions)))

(defun decode-locations (stream)
  (let ((length (decode-positive-integer stream)))
    (loop :repeat length
          :collect (decode-location stream))))

(defun decode-doc-locations-from-vector (bytes)
  (with-open-stream (stream (flex:make-in-memory-input-stream bytes))
    (decode-locations stream)))
